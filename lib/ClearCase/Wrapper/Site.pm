package ClearCase::Wrapper;

# A list of users who are exempt from certain restrictions.
use vars '@Admins';
@Admins = qw(vobadm);

# Override the user's preferences while interacting with clearcase.
umask 002 if defined(umask) && umask > 2 &&
			    $ENV{LOGNAME} && !grep(/^$ENV{LOGNAME}$/, @Admins);

# Similar to above but would withstand competition from settings in
# .kshrc et al (e.g. in a setview). It's critical to build DO's with
# generous umasks in case they get winked in. We allow it to be
# overridden lower than 002 but not higher.
$ENV{CLEARCASE_BLD_UMASK} = 2
	if !defined($ENV{CLEARCASE_BLD_UMASK}) || $ENV{CLEARCASE_BLD_UMASK} > 2;

#############################################################################
# Usage Message Extensions
#############################################################################
{
   local $^W = 0;
   no strict 'vars';

   # Usage message additions for actual cleartool commands that we extend.

   # Usage messages for pseudo cleartool commands that we implement here.
   local $0 = $ARGV[0] || '';
   $site_example	= "$0 [-aflag] [-bflag] ...";
}

=head1 NAME

ClearCase::Wrapper::Site - site-specific overrides for ClearCase::Wrapper

=head1 SYNOPSIS

Provides a place to put site-specific enhancements to ClearCase::Wrapper,
in order to ease merging of updates.

=head1 CLEARTOOL ENHANCEMENTS

=cut

=over 4

=item * SITE_EXAMPLE

This is an example of a site-specific enhancement. It's nothing but a
demo which prints out its arguments and flags when run.

=cut

# You should be able to run "ct site_example -aflag xx yy" to see that
# it works. Other extensions are equally simple: just make a subroutine
# by that name.
sub site_example {
    my %opt;
    my $name = shift @ARGV;
    print "In $name (args='@ARGV')!\n";
    GetOptions(\%opt, qw(aflag bflag));
    for (qw(aflag bflag)) {
	print "-$_ used\n" if $opt{$_};
    }
}

1;

__END__

=back

=head1 SEE ALSO

perl(1), ClearCase::Wrapper

=cut
