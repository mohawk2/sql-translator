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

=back

=cut

1;

=head1 SEE ALSO

L<Parse::RecDescent>

=cut
