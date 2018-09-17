package SQL::Translator::Parser::SQLCommon;

use strict;
use warnings;

our $VERSION = '1.59';

use base qw(Exporter);
use Data::Dumper;
use SQL::Translator::Utils qw/ddl_parser_instance/;
our @EXPORT_OK;
our $DEBUG;
$DEBUG   = 0 unless defined $DEBUG;

=head1 NAME

SQL::Translator::Parser::SQLCommon - Common fragments of P::RD SQL grammars

=head1 SYNOPSIS

  package SQL::Translator::Parser::Something;
  use SQL::Translator::Parser::SQLCommon qw(
    $DQSTRING_BS
    parse_super
  );

  # will have DQSTRING_BS entity available
  our $GRAMMAR = <<EOF . join "\n", $DQSTRING_BS;
  ...

  sub parse {
    my ($tr, $data) = @_;
    parse_super($tr, $data, 'Something');
  }
  EOF

=head1 DESCRIPTION

This is extracted from L<SQL::Translator::Parser::MySQL> and its
descendants to effect code/data reuse. The various exportable variables
are intended to be interpolated into L<Parse::RecDescent> grammar strings.

=head1 PACKAGE VARIABLES

All exportable package variables create an entity with the same name. None
are exported by default.

=over

=item $DQSTRING_BS

Double-quoted string which can backslash-quote C<\> or C<"> as well as
double the C<">.

=cut

push @EXPORT_OK, qw($DQSTRING_BS);
our $DQSTRING_BS = <<'EOF';
DQSTRING_BS: '"' <skip: ''> /(?:[^\\"]|""|\\.)*/ '"'
    { ($return = $item[3]) =~ s/(\\[\\"]|"")/substr($1,1)/ge }
EOF

=item $DQSTRING

Double-quoted string which doubles any C<">.

=cut

push @EXPORT_OK, qw($DQSTRING);
our $DQSTRING = <<'EOF';
DQSTRING : '"' <skip: ''> /((?:[^"]|"")+)/ '"'
    { ($return = $item[3]) =~ s/""/"/g; }
EOF

=item $SQSTRING_BS

Single-quoted string which can backslash-quote C<\> or C<'> as well as
double the C<'>.

=cut

push @EXPORT_OK, qw($SQSTRING_BS);
our $SQSTRING_BS = <<'EOF';
SQSTRING_BS: "'" <skip: ''> /(?:[^\\']|''|\\.)*/ "'"
    { ($return = $item[3]) =~ s/(\\[\\']|'')/substr($1,1)/ge }
EOF

=item $SQSTRING

Single-quoted string which doubles any C<'>.

=cut

push @EXPORT_OK, qw($SQSTRING);
our $SQSTRING = <<'EOF';
SQSTRING : "'" <skip: ''> /((?:[^']|'')*)/ "'"
    { ($return = $item[3]) =~ s/''/'/g }
EOF

=item $SBSTRING

Square-bracket-quoted string which doubles any C<]>.

=cut

push @EXPORT_OK, qw($SBSTRING);
our $SBSTRING = <<'EOF';
SBSTRING : '[' <skip: ''> /(?:[^]]|]])+/ ']'
    { ($return = $item[3]) =~ s/]]/]/g; }
EOF

=item $BQSTRING_BS

Back-quoted string which can backslash-quote C<\> or C<`> as well as
double the C<`>.

=cut

push @EXPORT_OK, qw($BQSTRING_BS);
our $BQSTRING_BS = <<'EOF';
BQSTRING_BS: '`' <skip: ''> /(?:[^\\`]|``|\\.)*/ '`'
    { ($return = $item[3]) =~ s/(\\[\\`]|``)/substr($1,1)/ge }
EOF

=item $BQSTRING

Back-quoted string which doubles any C<`>.

=cut

push @EXPORT_OK, qw($BQSTRING);
our $BQSTRING = <<'EOF';
BQSTRING : "`" <skip: ''> /((?:[^`]|``)*)/ "`"
    { ($return = $item[3]) =~ s/``/`/g }
EOF

=item $NUMBER

A scientific-notation number.

=cut

push @EXPORT_OK, qw($NUMBER);
our $NUMBER = <<'EOF';
NUMBER : /[-+]?\d*\.?\d+(?:[eE]\d+)?/
EOF

=item $NULL

The word C<NULL>, case-insensitive.

=cut

push @EXPORT_OK, qw($NULL);
our $NULL = <<'EOF';
NULL : /null/i { 'NULL' }
EOF

=item $BLANK_LINE

Blank line. This is so that comments before a C<CREATE TABLE> will only
be one "paragraphs"'s worth.

=cut

push @EXPORT_OK, qw($BLANK_LINE);
our $BLANK_LINE = <<'EOF';
BLANK_LINE : <skip: ''> /\A\n/
EOF

=item $COMMENT_DD

Single-line comment preceded by C<-->.

=cut

push @EXPORT_OK, qw($COMMENT_DD);
our $COMMENT_DD = <<'EOF';
COMMENT_DD : <skip: ''> /^[ \t]*--[ \t]*(.*?)[ \t]*\n/
    { $return = $1; }
EOF

=item $COMMENT_HASH

Single-line comment preceded by C<#>.

=cut

push @EXPORT_OK, qw($COMMENT_HASH);
our $COMMENT_HASH = <<'EOF';
COMMENT_HASH : <skip: ''> /^[ \t]*#[ \t]*(.*?)[ \t]*\n/
    { $return = $1; }
EOF

=item $COMMENT_SSTAR

Multi-line comment surrounded by C</*> and C<*/> minus starting/finishing
whitespace, and any C<*> at the start of each line. Also absorbs any
spaces or tabs up to optional next C<\n>.

=cut

push @EXPORT_OK, qw($COMMENT_SSTAR);
our $COMMENT_SSTAR = <<'EOF';
COMMENT_SSTAR : <skip: ''> m{^[ \t]*/\* \s*}x m{(.*?) \s* \*/[ \t]*\n?}xs
    {
        ($return = $1) =~ s/^\s*\**\s*//mg;
    }
EOF

=item $COMMENT_DS

Single-line comment preceded by C<//>.

=cut

push @EXPORT_OK, qw($COMMENT_DS);
our $COMMENT_DS = <<'EOF';
COMMENT_DS : <skip: ''> /^[ \t]*\/\/[ \t]*(.*?)[ \t]*\n/
    { $return = $1; }
EOF

=item $COMMENT_PERCENT

Single-line comment preceded by C<%>.

=cut

push @EXPORT_OK, qw($COMMENT_PERCENT);
our $COMMENT_PERCENT = <<'EOF';
COMMENT_PERCENT : <skip: ''> /^[ \t]*%[ \t]*(.*?)[ \t]*\n/
    { $return = $1; }
EOF

=back

=head1 FUNCTIONS

All exportable.

=head2 parse_super

Same parameters as L<SQL::Translator::Parser/parse> plus the parser
instance-class name. Relies on the parser "result" being a hash with keys
C<tables>, C<procedures>, C<views>, and C<triggers>.

The values for C<tables> will be a hash-ref, the others will be
array-refs.

It will set the schema's C<name> property from the C<schema_name> value,
if returned. It also parses the C<ignore_opts> argument mentioned
in L<sqlt>, treating it as a comma-separated list of table options to ignore.

=cut

push @EXPORT_OK, qw(parse_super);
sub parse_super {
  my ( $translator, $data, $instance_name ) = @_;

  # Enable warnings within the Parse::RecDescent module.
  local $::RD_ERRORS = 1 unless defined $::RD_ERRORS; # Make sure the parser dies when it encounters an error
  local $::RD_WARN   = 1 unless defined $::RD_WARN; # Enable warnings. This will warn on unused rules &c.
  local $::RD_HINT   = 1 unless defined $::RD_HINT; # Give out hints to help fix problems.

  local $::RD_TRACE  = $translator->trace ? 1 : undef;
  local $DEBUG       = $translator->debugging;

  my $parser = ddl_parser_instance($instance_name);

  my $result = $parser->startrule($data);
  return $translator->error( "Parse failed." ) unless defined $result;
  if ($DEBUG) {
    require Data::Dumper;
    my $dumper = Data::Dumper->new( [$result] );
    $dumper->Indent(1)->Terse(1);
    $dumper->Sortkeys(1) if $dumper->can("Sortkeys");
    warn $dumper->Dump;
  }

  my %ignore_opt = map { $_ => 1 }
    $translator->parser_args->{'ignore_opts'}
      ? split( /,/, $translator->parser_args->{'ignore_opts'} )
      : ();

  my $schema = $translator->schema;
  $schema->name($result->{'schema_name'}) if $result->{'schema_name'};
  my @tables = sort {
      ( $result->{tables}{ $a }{'order'} || 0 ) <=> ( $result->{tables}{ $b }{'order'} || 0 )
  } keys %{ $result->{tables} };

  for my $table_name ( @tables ) {
    my $tdata =  $result->{tables}{ $table_name };
    my $table =  $schema->add_table(
      #schema => $tdata->{'schema_name'},
      name   => $tdata->{'table_name'},
    ) or die "Couldn't create table '$table_name': " . $schema->error;

    $table->extra(temporary => 1) if $tdata->{'temporary'};

    $table->comments( $tdata->{'comments'} );

    my @fields = sort {
      $tdata->{'fields'}{ $a }{'order'}
      <=>
      $tdata->{'fields'}{ $b }{'order'}
    } keys %{ $tdata->{'fields'} };

    for my $fname ( @fields ) {
      my $fdata = $tdata->{'fields'}{ $fname };
      next if $fdata->{'drop'};
      my $field = $table->add_field(
        name              => $fdata->{'name'},
        data_type         => $fdata->{'data_type'},
        size              => $fdata->{'size'},
        default_value     => $fdata->{'default'},
        is_auto_increment => $fdata->{'is_auto_increment'},
        is_nullable       => $fdata->{'is_nullable'},
        comments          => $fdata->{'comments'},
      ) or die $table->error;

      $table->primary_key( $field->name ) if $fdata->{'is_primary_key'};

      my %extra = %{ $fdata->{extra} || {} };
      $field->extra( $_, $extra{$_} ) for keys %extra;

      for my $cdata ( @{ $fdata->{'constraints'} } ) {
        next unless $cdata->{'type'} eq 'foreign_key';
        $cdata->{'fields'} ||= [ $field->name ];
        push @{ $tdata->{'constraints'} }, $cdata;
      }
    }

    for my $idata ( @{ $tdata->{'indices'} || [] } ) {
      my @options = ();
      push @options, { using => $idata->{'method'} } if $idata->{method};
      push @options, { where => $idata->{'where'} }  if $idata->{where};
      my $index  =  $table->add_index(
        name    => $idata->{'name'},
        type    => uc($idata->{'type'} || ''),
        fields  => $idata->{'fields'},
        options => \@options
      ) or die $table->error . ' ' . $table->name;
    }

    if ( my @options = @{ $tdata->{'table_options'} || [] } ) {
      $table->options(
        [ grep !$ignore_opt{ (keys %$_)[0] }, @options ]
      ) or die $table->error;
    }

    for my $cdata ( @{ $tdata->{'constraints'} || [] } ) {
      my $constraint     =  $table->add_constraint(
        name             => $cdata->{'name'},
        type             => $cdata->{'type'},
        fields           => $cdata->{'fields'},
        reference_table  => $cdata->{'reference_table'},
        reference_fields => $cdata->{'reference_fields'},
        match_type       => $cdata->{'match_type'} || '',
        on_delete        => $cdata->{'on_delete'} || $cdata->{'on_delete_do'},
        on_update        => $cdata->{'on_update'} || $cdata->{'on_update_do'},
        expression       => $cdata->{'expression'},
      ) or die "Can't add constraint of type '" .
        $cdata->{'type'} .  "' to table '" . $table->name .
        "': " . $table->error;
    }
  }

  foreach my $pdata (@{ $result->{procedures} }) {
    $schema->add_procedure(
      name  => $pdata->{name},
      owner => $pdata->{owner},
      sql   => $pdata->{sql},
    );
  }

  for my $vinfo (@{$result->{views}}) {
    my $sql = $vinfo->{sql};
    $sql =~ s/\A\s+|\s+\z//g;
    my $view = $schema->add_view (
      #schema => $vinfo->{schema_name},
      name => $vinfo->{name},
      sql => $sql,
      fields => $vinfo->{fields},
      tables  => $vinfo->{'tables'},
      options => $vinfo->{'options'}
    ) or die "Couldn't create view '$vinfo->{name}': " . $schema->error;
    $view->extra ( temporary => 1 ) if $vinfo->{is_temporary};
  }

  for my $trigger (@{ $result->{triggers} }) {
    $schema->add_trigger( %$trigger );
  }

  return 1;
}

1;

=head1 SEE ALSO

L<Parse::RecDescent>

=cut
