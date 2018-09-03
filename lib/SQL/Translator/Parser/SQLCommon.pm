package SQL::Translator::Parser::SQLCommon;

use strict;
use warnings;

our $VERSION = '1.59';

use base qw(Exporter);
our @EXPORT_OK;

=head1 NAME

SQL::Translator::Parser::SQLCommon - Common fragments of P::RD SQL grammars

=head1 SYNOPSIS

  package SQL::Translator::Parser::Something;
  use SQL::Translator::Parser::SQLCommon qw(
    $DQSTRING_BS
  );

  # will have DQSTRING_BS entity available
  our $GRAMMAR = <<EOF . join "\n", $DQSTRING_BS;
  ...
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

=cut

1;

=head1 SEE ALSO

L<Parse::RecDescent>

=cut
