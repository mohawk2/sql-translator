package SQL::Translator::Parser::MySQL;

=head1 NAME

SQL::Translator::Parser::MySQL - parser for MySQL

=head1 SYNOPSIS

  use SQL::Translator;
  use SQL::Translator::Parser::MySQL;

  my $translator = SQL::Translator->new;
  $translator->parser("SQL::Translator::Parser::MySQL");

=head1 DESCRIPTION

The grammar is influenced heavily by Tim Bunce's "mysql2ora" grammar.

Here's the word from the MySQL site
(http://www.mysql.com/doc/en/CREATE_TABLE.html):

  CREATE [TEMPORARY] TABLE [IF NOT EXISTS] tbl_name [(create_definition,...)]
  [table_options] [select_statement]

  or

  CREATE [TEMPORARY] TABLE [IF NOT EXISTS] tbl_name LIKE old_table_name;

  create_definition:
    col_name type [NOT NULL | NULL] [DEFAULT default_value] [AUTO_INCREMENT]
              [PRIMARY KEY] [reference_definition]
    or    PRIMARY KEY (index_col_name,...)
    or    KEY [index_name] (index_col_name,...)
    or    INDEX [index_name] (index_col_name,...)
    or    UNIQUE [INDEX] [index_name] (index_col_name,...)
    or    FULLTEXT [INDEX] [index_name] (index_col_name,...)
    or    [CONSTRAINT symbol] FOREIGN KEY [index_name] (index_col_name,...)
              [reference_definition]
    or    CHECK (expr)

  type:
          TINYINT[(length)] [UNSIGNED] [ZEROFILL]
    or    SMALLINT[(length)] [UNSIGNED] [ZEROFILL]
    or    MEDIUMINT[(length)] [UNSIGNED] [ZEROFILL]
    or    INT[(length)] [UNSIGNED] [ZEROFILL]
    or    INTEGER[(length)] [UNSIGNED] [ZEROFILL]
    or    BIGINT[(length)] [UNSIGNED] [ZEROFILL]
    or    REAL[(length,decimals)] [UNSIGNED] [ZEROFILL]
    or    DOUBLE[(length,decimals)] [UNSIGNED] [ZEROFILL]
    or    FLOAT[(length,decimals)] [UNSIGNED] [ZEROFILL]
    or    DECIMAL(length,decimals) [UNSIGNED] [ZEROFILL]
    or    NUMERIC(length,decimals) [UNSIGNED] [ZEROFILL]
    or    CHAR(length) [BINARY]
    or    VARCHAR(length) [BINARY]
    or    DATE
    or    TIME
    or    TIMESTAMP
    or    DATETIME
    or    TINYBLOB
    or    BLOB
    or    MEDIUMBLOB
    or    LONGBLOB
    or    TINYTEXT
    or    TEXT
    or    MEDIUMTEXT
    or    LONGTEXT
    or    ENUM(value1,value2,value3,...)
    or    SET(value1,value2,value3,...)

  index_col_name:
          col_name [(length)]

  reference_definition:
          REFERENCES tbl_name [(index_col_name,...)]
                     [MATCH FULL | MATCH PARTIAL]
                     [ON DELETE reference_option]
                     [ON UPDATE reference_option]

  reference_option:
          RESTRICT | CASCADE | SET NULL | NO ACTION | SET DEFAULT

  table_options:
          TYPE = {BDB | HEAP | ISAM | InnoDB | MERGE | MRG_MYISAM | MYISAM }
  or      ENGINE = {BDB | HEAP | ISAM | InnoDB | MERGE | MRG_MYISAM | MYISAM }
  or      AUTO_INCREMENT = #
  or      AVG_ROW_LENGTH = #
  or      [ DEFAULT ] CHARACTER SET charset_name
  or      CHECKSUM = {0 | 1}
  or      COLLATE collation_name
  or      COMMENT = "string"
  or      MAX_ROWS = #
  or      MIN_ROWS = #
  or      PACK_KEYS = {0 | 1 | DEFAULT}
  or      PASSWORD = "string"
  or      DELAY_KEY_WRITE = {0 | 1}
  or      ROW_FORMAT= { default | dynamic | fixed | compressed }
  or      RAID_TYPE= {1 | STRIPED | RAID0 } RAID_CHUNKS=#  RAID_CHUNKSIZE=#
  or      UNION = (table_name,[table_name...])
  or      INSERT_METHOD= {NO | FIRST | LAST }
  or      DATA DIRECTORY="absolute path to directory"
  or      INDEX DIRECTORY="absolute path to directory"


A subset of the ALTER TABLE syntax that allows addition of foreign keys:

  ALTER [IGNORE] TABLE tbl_name alter_specification [, alter_specification] ...

  alter_specification:
          ADD [CONSTRAINT [symbol]]
          FOREIGN KEY [index_name] (index_col_name,...)
             [reference_definition]

A subset of INSERT that we ignore:

  INSERT anything

=head1 ARGUMENTS

This parser takes a single optional parser_arg C<mysql_parser_version>, which
provides the desired version for the target database. Any statement in the processed
dump file, that is commented with a version higher than the one supplied, will be stripped.

The default C<mysql_parser_version> is set to the conservative value of 40000 (MySQL 4.0)

Valid version specifiers for C<mysql_parser_version> are listed L<here|SQL::Translator::Utils/parse_mysql_version>

More information about the MySQL comment-syntax: L<http://dev.mysql.com/doc/refman/5.0/en/comments.html>


=cut

use strict;
use warnings;

our $VERSION = '1.60';

use Storable qw(dclone);
use DBI qw(:sql_types);
use SQL::Translator::Utils qw/parse_mysql_version/;
use SQL::Translator::Parser::SQLCommon qw(
  $DQSTRING_BS
  $SQSTRING_BS
  $BQSTRING_BS
  $NUMBER
  $NULL
  $BLANK_LINE
  $COMMENT_DD
  $COMMENT_HASH
  $COMMENT_SSTAR
  parse_super
);

use base qw(Exporter);
our @EXPORT_OK = qw(parse);

our %type_mapping = ();

use constant DEFAULT_PARSER_VERSION => 40000;

our $GRAMMAR = << 'END_OF_GRAMMAR' . join "\n", $DQSTRING_BS, $SQSTRING_BS, $BQSTRING_BS, $NUMBER, $NULL, $BLANK_LINE, $COMMENT_DD, $COMMENT_HASH, $COMMENT_SSTAR;

{
    my ( $database_name, %tables, $table_order, @views, @procedures );
    my $delimiter = ';';
}

#
# The "eofile" rule makes the parser fail if any "statement" rule
# fails.  Otherwise, the first successful match by a "statement"
# won't cause the failure needed to know that the parse, as a whole,
# failed. -ky
#
startrule : statement(s) eofile {
    {
        schema_name   => $database_name,
        tables        => \%tables,
        views         => \@views,
        procedures    => \@procedures,
    }
}

eofile : /^\Z/

statement : use
    | set
    | drop
    | comment(s?) BLANK_LINE
    | create
    | comment
    | alter
    | insert
    | delimiter
    | empty_statement
    | <error>

use : /use/i NAME "$delimiter"
    {
        $database_name = $item[2];
    }

set : /set/i not_delimiter "$delimiter"

drop : /drop/i TABLE not_delimiter "$delimiter"

drop : /drop/i NAME(s) "$delimiter"

bit:
    / 0b([01]{1,64}) | [bB]'([01]{1,64})' /x
    {
        $return  = length $1 ? $1 : $2;
    }

string :
  # MySQL strings, unlike common SQL strings, can be double-quoted or
  # single-quoted.

  SQSTRING_BS | DQSTRING_BS

nonstring : /[^;\'"]+/

statement_body : string | nonstring

insert : /insert/i  statement_body(s?) "$delimiter"

delimiter : /delimiter/i /[\S]+/
    { $delimiter = $item[2] }

empty_statement : "$delimiter"

alter : ALTER TABLE table_name alter_specification(s /,/) "$delimiter"
    {
        my $table_name                       = $item{'table_name'};
    die "Cannot ALTER table '$table_name'; it does not exist"
        unless $tables{ $table_name };
        for my $definition ( @{ $item[4] } ) {
        $definition->{'extra'}->{'alter'} = 1;
        push @{ $tables{ $table_name }{'constraints'} }, $definition;
    }
    }

alter_specification : ADD foreign_key_def
    { $return = $item[2] }

create : CREATE /database/i NAME "$delimiter"

create : comment(s?) CREATE TEMPORARY(?) TABLE opt_if_not_exists(?) table_name '(' create_definition(s /,/) /(,\s*)?\)/ table_option(s?) "$delimiter"
    {
        my $table_name                       = $item{'table_name'};
        die "There is more than one definition for $table_name"
            if ($tables{$table_name});

        $tables{ $table_name }{'order'}      = ++$table_order;
        $tables{ $table_name }{'table_name'} = $table_name;

        if ( @{ $item[1] } ) {
            $tables{ $table_name }{'comments'} = [ @{ $item[1] } ];
        }

        my @field_constraints; # previous parser puts these last so segregate
        my $i = 1;
        for my $definition ( @{ $item[8] } ) {
            my $supertype = delete $definition->{supertype};
            if ( $supertype eq 'field' ) {
                my $field_name = $definition->{'name'};
                if ( $definition->{'is_primary_key'} ) {
                    push @{ $tables{ $table_name }{'constraints'} },
                        {
                            type   => 'primary_key',
                            fields => [ $field_name ],
                        }
                    ;
                }
                push @field_constraints,
                    @{ delete $definition->{constraints} || [] };
                push @{ $tables{ $table_name }{indices} },
                    @{ delete $definition->{indices} || [] };
                $tables{ $table_name }{'fields'}{ $field_name } =
                    { %$definition, order => $i };
                $i++;
            }
            elsif ( $supertype eq 'constraint' ) {
                push @{ $tables{ $table_name }{'constraints'} }, $definition;
            }
            elsif ( $supertype eq 'index' ) {
                push @{ $tables{ $table_name }{'indices'} }, $definition;
            }
        }
        push @{ $tables{ $table_name }{constraints} }, @field_constraints;

        if ( my @options = @{ $item{'table_option(s?)'} } ) {
            for my $option ( @options ) {
                my ( $key, $value ) = each %$option;
                if ( $key eq 'comment' ) {
                    push @{ $tables{ $table_name }{'comments'} }, $value;
                }
                else {
                    push @{ $tables{ $table_name }{'table_options'} }, $option;
                }
            }
        }

        1;
    }

opt_if_not_exists : /if not exists/i

create : CREATE UNIQUE(?) /(index|key)/i index_name /on/i table_name '(' field_name(s /,/) ')' "$delimiter"
    {
        push @{ $tables{ $item{'table_name'} }{'indices'} },
            {
                name   => $item[4],
                type   => $item[2][0] ? 'unique' : 'normal',
                fields => $item[8],
            }
        ;
    }

create : CREATE /trigger/i NAME not_delimiter "$delimiter"

create : CREATE PROCEDURE NAME not_delimiter "$delimiter"
    {
        my $func_name = $item[3];
        my $owner = '';
        my $sql = "$item[1] $item[2] $item[3] $item[4]";

        push @procedures, {
          name => $func_name,
          owner => $owner,
          sql => $sql,
        };
    }

PROCEDURE : /procedure/i
    | /function/i

create : CREATE or_replace(?) create_view_option(s?) /view/i NAME /as/i view_select_statement "$delimiter"
    {
        my $view_name   = $item{'NAME'};
        my $select_sql  = $item{'view_select_statement'};
        my $options     = $item{'create_view_option(s?)'};

        my $sql = join(q{ },
            grep { defined and length }
            map  { ref $_ eq 'ARRAY' ? @$_ : $_ }
            $item{'CREATE'},
            $item{'or_replace(?)'},
            $options,
            $view_name,
            'as select',
            join(', ',
                map {
                    sprintf('%s%s',
                        $_->{'name'},
                        $_->{'alias'} ? ' as ' . $_->{'alias'} : ''
                    )
                }
                @{ $select_sql->{'columns'} || [] }
            ),
            ' from ',
            join(', ',
                map {
                    sprintf('%s%s',
                        $_->{'name'},
                        $_->{'alias'} ? ' as ' . $_->{'alias'} : ''
                    )
                }
                @{ $select_sql->{'from'}{'tables'} || [] }
            ),
            $select_sql->{'from'}{'where'}
                ? 'where ' . $select_sql->{'from'}{'where'}
                : ''
            ,
        );

        # Hack to strip database from function calls in SQL
        $sql =~ s#`\w+`\.(`\w+`\()##g;

        my @flds = map { $_->{'alias'} || $_->{'name'} }
                   @{ $item{'view_select_statement'}{'columns'} || [] };
        my @from = map { $_->{'alias'} || $_->{'name'} }
                   @{ $item{'view_select_statement'}{'from'}{'tables'} || [] };
        push @views, {
          name => $view_name,
          sql => $sql,
          options => $options,
          fields => \@flds,
          tables => \@from,
        };
    }

create_view_option : view_algorithm | view_sql_security | view_definer

or_replace : /or replace/i

view_algorithm : /algorithm/i /=/ WORD
    {
        $return = "$item[1]=$item[3]";
    }

view_definer : /definer=\S+/i

view_sql_security : /sql \s+ security  \s+ (definer|invoker)/ixs

not_delimiter : /.*?(?=$delimiter)/is

view_select_statement : /[(]?/ /select/i view_column_def /from/i view_table_def /[)]?/
    {
        $return = {
            columns => $item{'view_column_def'},
            from    => $item{'view_table_def'},
        };
    }

view_column_def : /(.*?)(?=\bfrom\b)/ixs
    {
        # split on commas not in parens,
        # e.g., "concat_ws(\' \', first, last) as first_last"
        my @tmp = $1 =~ /((?:[^(,]+|\(.*?\))+)/g;
        my @cols;
        for my $col ( @tmp ) {
            my ( $name, $alias ) = map {
              s/^\s+|\s+$//g;
              s/[`]//g;
              $_
            } split /\s+as\s+/i, $col;

            push @cols, { name => $name, alias => $alias || '' };
        }

        $return = \@cols;
    }

not_delimiter : /.*?(?=$delimiter)/is

view_table_def : not_delimiter
    {
        my $clause = $item[1];
        my $where  = $1 if $clause =~ s/\bwhere \s+ (.*)//ixs;
        $clause    =~ s/[)]\s*$//;

        my @tables;
        for my $tbl ( split( /\s*,\s*/, $clause ) ) {
            my ( $name, $alias ) = split /\s+as\s+/i, $tbl;
            push @tables, { name => $name, alias => $alias || '' };
        }

        $return = {
            tables => \@tables,
            where  => $where || '',
        };
    }

view_column_alias : /as/i NAME
    { $return = $item[2] }

create_definition : constraint
    | index
    | field
    | comment
    | <error>

comment : COMMENT_DD | COMMENT_HASH | COMMENT_SSTAR

field : /\s*/ comment(s?) field_name data_type field_meta(s?) /\s*/ comment(s?)
    {
        my @comments = ( @{ $item[2] }, @{ $item[7] } );
        my ( @constraints, @indices );
        my %fieldspec = (
            supertype   => 'field',
            name        => $item{'field_name'},
            is_nullable => 1,
        );
        for my $meta ( $item{data_type}, @{ $item{'field_meta(s?)'} } ) {
            my $supertype = delete $meta->{supertype};
            if ($supertype eq 'comment') {
                push @comments, $meta->{value};
            } elsif ($supertype eq 'constraint') {
                $meta = { %$meta, fields => [ $item{'field_name'} ] };
                push @constraints, $meta;
            } elsif ($supertype eq 'index') {
                push @indices, { %$meta, fields => [ $item{'field_name'} ] };
            } elsif ($supertype eq 'fieldextra') {
                $fieldspec{extra} = { %{ $fieldspec{extra} || {} }, %$meta };
            } elsif ($supertype eq 'fieldspec') {
                %fieldspec = (%fieldspec, %$meta);
            } else {
                die "Unknown supertype: '$supertype'";
            }
        }
        $fieldspec{comments} = \@comments if @comments;
        $fieldspec{constraints} = \@constraints if @constraints;
        $fieldspec{indices} = \@indices if @indices;
        $return = \%fieldspec;
    }
    | <error>

field_meta : field_qualifier | reference_definition

field_qualifier : /not/i /null/i
    { $return = { supertype => 'fieldspec', is_nullable => 0 } }
    |
    /null/i
    { $return = { supertype => 'fieldspec', is_nullable => 1 } }

field_qualifier : /default/i ( CURRENT_TIMESTAMP | bit | VALUE | /[\w\d:.-]+/ )
    { $return = { supertype => 'fieldspec', default => $item[2] } }

field_qualifier : /auto_increment/i
    { $return = { supertype => 'fieldspec', is_auto_increment => 1 } }

field_qualifier : primary_key

field_qualifier : /unsigned/i
    { $return = { supertype => 'fieldextra', unsigned => 1 } }


field_qualifier : /character set/i WORD
    { $return = { supertype => 'fieldextra', 'character set' => $item[2] } }

field_qualifier : /collate/i WORD
    { $return = { supertype => 'fieldextra', collate => $item[2] } }

field_qualifier : /on update/i CURRENT_TIMESTAMP
    { $return = { supertype => 'fieldextra', 'on update' => $item[2] } }

field_qualifier : /unique/i KEY(?)
    { $return = { supertype => 'constraint', type => 'unique' } }

field_qualifier : KEY
    { $return = { supertype => 'index' } }

field_qualifier : /comment/i string
    { $return = { supertype => 'comment', value => $item[2] } }

reference_definition : /references/i table_name parens_field_list(?) match_type(?) on_delete(?) on_update(?)
    {
        $return = {
            supertype        => 'constraint',
            type             => 'foreign_key',
            reference_table  => $item[2],
            reference_fields => $item[3][0],
            match_type       => $item[4][0],
            on_delete        => $item[5][0],
            on_update        => $item[6][0],
        }
    }

match_type : /match full/i { 'full' }
    |
    /match partial/i { 'partial' }

on_delete : /on delete/i reference_option
    { $item[2] }

on_update :
    /on update/i CURRENT_TIMESTAMP
    { $item[2] }
    |
    /on update/i reference_option
    { $item[2] }

reference_option: /restrict/i |
    /cascade/i   |
    /set null/i  |
    /no action/i |
    /set default/i

index : normal_index
    | fulltext_index
    | spatial_index
    | <error>

table_name   : NAME

field_name   : NAME

index_name   : NAME

data_type    : WORD parens_value_list(s?) type_qualifier(s?)
    {
        my $type = $item[1];
        my $size; # field size, applicable only to non-set fields
        my @list; # set list, applicable only to sets (duh)

        if ( uc($type) =~ /^(SET|ENUM)$/ ) {
            @list = @{ $item[2][0] };
        } else {
            $size = $item[2][0];
        }
        my %extra;
        $extra{list} = \@list if @list;
        $extra{$_} = 1 for @{ $item[3] };

        $return        = {
            supertype  => 'fieldspec',
            data_type  => $type,
            size       => $size,
            keys(%extra) ? (extra => \%extra) : (),
        }
    }

parens_field_list : '(' field_name(s /,/) ')'
    { $item[2] }

parens_value_list : '(' VALUE(s /,/) ')'
    { $item[2] }

type_qualifier : /(BINARY|UNSIGNED|ZEROFILL)/i
    { lc $item[1] }

field_type   : WORD

create_index : /create/i /index/i

primary_key : /primary/i /key/i
    { $return = { supertype => 'fieldspec', is_primary_key => 1 } }

constraint : primary_key_def
    | unique_key_def
    | foreign_key_def
    | <error>

foreign_key_def : foreign_key_def_begin parens_field_list reference_definition
    {
        $return              =  {
            supertype        => 'constraint',
            type             => 'foreign_key',
            name             => $item[1],
            fields           => $item[2],
            %{ $item{'reference_definition'} },
        }
    }

foreign_key_def_begin : /constraint/i /foreign key/i NAME
    { $return = $item{NAME} }
    |
    /constraint/i NAME(?) /foreign key/i
    { ($return) = @{$item{'NAME(?)'} || ['']} }
    |
    /foreign key/i NAME(?)
    { ($return) = @{$item{'NAME(?)'} || ['']} }

primary_key_def : primary_key index_type(?) '(' name_with_opt_paren(s /,/) ')' index_type(?)
    {
        $return       = {
            supertype => 'constraint',
            type      => 'primary_key',
            fields    => $item[4],
            options   => $item[2][0] || $item[6][0],
        };
    }
    # In theory, and according to the doc, names should not be allowed here, but
    # MySQL accept (and ignores) them, so we are not going to be less :)
    | primary_key index_name_not_using(?) '(' name_with_opt_paren(s /,/) ')' index_type(?)
    {
        $return       = {
            supertype => 'constraint',
            type      => 'primary_key',
            fields    => $item[4],
            options   => $item[6][0],
        };
    }

unique_key_def : UNIQUE KEY(?) index_name_not_using(?) index_type(?) '(' name_with_opt_paren(s /,/) ')' index_type(?)
    {
        $return       = {
            supertype => 'constraint',
            name      => $item[3][0],
            type      => 'unique',
            fields    => $item[6],
            options   => $item[4][0] || $item[8][0],
        }
    }

normal_index : KEY index_name_not_using(?) index_type(?) '(' name_with_opt_paren(s /,/) ')' index_type(?)
    {
        $return       = {
            supertype => 'index',
            type      => 'normal',
            name      => $item[2][0],
            fields    => $item[5],
            options   => $item[3][0] || $item[7][0],
        }
    }

index_name_not_using : QUOTED_NAME
    | /(\b(?!using)\w+\b)/ { $return = ($1 =~ /^using/i) ? undef : $1 }

index_type : /using (btree|hash|rtree)/i { $return = uc $1 }

fulltext_index : /fulltext/i KEY(?) index_name(?) '(' name_with_opt_paren(s /,/) ')'
    {
        $return       = {
            supertype => 'index',
            type      => 'fulltext',
            name      => $item{'index_name(?)'}[0],
            fields    => $item[5],
        }
    }

spatial_index : /spatial/i KEY(?) index_name(?) '(' name_with_opt_paren(s /,/) ')'
    {
        $return       = {
            supertype => 'index',
            type      => 'spatial',
            name      => $item{'index_name(?)'}[0],
            fields    => $item[5],
        }
    }

name_with_opt_paren : NAME parens_value_list(s?)
    { $item[2][0] ? "$item[1]($item[2][0][0])" : $item[1] }

UNIQUE : /unique/i

KEY : /key/i | /index/i

table_option : /comment/i /=/ string
    {
        $return     = { comment => $item[3] };
    }
    | /(default )?(charset|character set)/i /\s*=?\s*/ NAME
    {
        $return = { 'CHARACTER SET' => $item[3] };
    }
    | /collate/i NAME
    {
        $return = { 'COLLATE' => $item[2] }
    }
    | /union/i /\s*=\s*/ '(' table_name(s /,/) ')'
    {
        $return = { $item[1] => $item[4] };
    }
    | WORD /\s*=\s*/ table_option_value
    {
        $return = { $item[1] => $item[3] };
    }

table_option_value : VALUE
                   | NAME

default : /default/i

ADD : /add/i

ALTER : /alter/i

CREATE : /create/i

TEMPORARY : /temporary/i

TABLE : /table/i

WORD : /\w+/

DIGITS : /\d+/

COMMA : ','

QUOTED_NAME : BQSTRING_BS
    | SQSTRING_BS
    | DQSTRING_BS

NAME: QUOTED_NAME
    | /\w+/

VALUE : NUMBER
    | SQSTRING_BS
    | DQSTRING_BS
    | NULL

# always a scalar-ref, so that it is treated as a function and not quoted by consumers
CURRENT_TIMESTAMP :
      /current_timestamp(\(\))?/i { \'CURRENT_TIMESTAMP' }
    | /now\(\)/i { \'CURRENT_TIMESTAMP' }

END_OF_GRAMMAR

sub parse {
    my ( $translator, $data ) = @_;

    # Preprocess for MySQL-specific and not-before-version comments
    # from mysqldump
    my $parser_version = parse_mysql_version(
      $translator->parser_args->{mysql_parser_version}, 'mysql'
    ) || DEFAULT_PARSER_VERSION;

    while ( $data =~
        s#/\*!(\d{5})?(.*?)\*/#($1 && $1 > $parser_version ? '' : $2)#es
    ) {
      # do nothing; is there a better way to write this? -- ky
    }

    my $retval = parse_super($translator, $data, 'MySQL');
    return $retval if !$retval;

    my $schema = $translator->schema;
    for my $table ($schema->get_tables) {
      # After the constrains and PK/idxs have been created,
      # we normalize fields
      normalize_field($_) for $table->get_fields;
    }

    return $retval;
}

# Takes a field, and returns
sub normalize_field {
    my ($field) = @_;
    my ($size, $type, $list, $unsigned, $changed);

    $size = $field->size;
    $type = $field->data_type;
    $list = $field->extra->{list} || [];
    $unsigned = defined($field->extra->{unsigned});

    if ( !ref $size && $size eq 0 ) {
        if ( lc $type eq 'tinyint' ) {
            $changed = $size != 4 - $unsigned;
            $size = 4 - $unsigned;
        }
        elsif ( lc $type eq 'smallint' ) {
            $changed = $size != 6 - $unsigned;
            $size = 6 - $unsigned;
        }
        elsif ( lc $type eq 'mediumint' ) {
            $changed = $size != 9 - $unsigned;
            $size = 9 - $unsigned;
        }
        elsif ( $type =~ /^int(eger)?$/i ) {
            $changed = $size != 11 - $unsigned || $type ne 'int';
            $type = 'int';
            $size = 11 - $unsigned;
        }
        elsif ( lc $type eq 'bigint' ) {
            $changed = $size != 20;
            $size = 20;
        }
        elsif ( lc $type =~ /(float|double|decimal|numeric|real|fixed|dec)/ ) {
            my $old_size = (ref $size || '') eq 'ARRAY' ? $size : [];
            $changed     = @$old_size != 2
                        || $old_size->[0] != 8
                        || $old_size->[1] != 2;
            $size        = [8,2];
        }
    }

    if ( $type =~ /^tiny(text|blob)$/i ) {
        $changed = $size != 255;
        $size = 255;
    }
    elsif ( $type =~ /^(blob|text)$/i ) {
        $changed = $size != 65_535;
        $size = 65_535;
    }
    elsif ( $type =~ /^medium(blob|text)$/i ) {
        $changed = $size != 16_777_215;
        $size = 16_777_215;
    }
    elsif ( $type =~ /^long(blob|text)$/i ) {
        $changed = $size != 4_294_967_295;
        $size = 4_294_967_295;
    }

    if ( $field->data_type =~ /(set|enum)/i && !$field->size ) {
        my %extra = $field->extra;
        my $longest = 0;
        for my $len ( map { length } @{ $extra{'list'} || [] } ) {
            $longest = $len if $len > $longest;
        }
        $changed = 1;
        $size = $longest if $longest;
    }


    if ( $changed ) {
        # We only want to clone the field, not *everything*
        {
            local $field->{table} = undef;
            $field->parsed_field( dclone( $field ) );
            $field->parsed_field->{table} = $field->table;
        }
        $field->size( $size );
        $field->data_type( $type );
        $field->sql_data_type( $type_mapping{ lc $type } )
            if exists $type_mapping{ lc $type };
        $field->extra->{list} = $list if @$list;
    }
}

1;

# -------------------------------------------------------------------
# Where man is not nature is barren.
# William Blake
# -------------------------------------------------------------------

=pod

=head1 AUTHOR

Ken Youens-Clark E<lt>kclark@cpan.orgE<gt>,
Chris Mungall E<lt>cjm@fruitfly.orgE<gt>.

=head1 SEE ALSO

Parse::RecDescent, SQL::Translator::Schema.

=cut
