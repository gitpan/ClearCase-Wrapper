#!/usr/local/bin/perl -w

# The bulk of the code comes from here ...
BEGIN {
    if (!$ENV{CLEARCASE_WRAPPER_NATIVE}) {
	# The "standard" set of overrides supplied with the package.
	# These are autoloaded and thus fairly cheap to 'require'
	# even though there's lots of code down here ...
	require ClearCase::Wrapper;

	# A site admin may optionally put local overrides here.
	# These are not autoloaded due to a limitation of AutoLoader
	# that more than one file may not contribute to a single
	# autoloaded package. But upgrades to new ClearCase::Wrapper
	# versions will be substantially easier if all local tweaking
	# in this file. A subroutine declared here will eclipse one
	# of the same name autoloaded above.
	local $^W = 0;	# in case a sub is redefined in Site.pm
	require ClearCase::Wrapper::Site;
    }
}

use strict;

# If Wrapper.pm defines an AutoLoad-ed subroutine to handle $ARGV[0], call it.
# That subroutine may or may not return.
if (@ARGV && !$ENV{CLEARCASE_WRAPPER_NATIVE} &&
	    (defined($ClearCase::Wrapper::{$ARGV[0]}) || $ARGV[0] eq 'help')) {
    # This provides support for writing extensions.
    require ClearCase::Argv;
    ClearCase::Argv->inpathnorm(0);	# backward compat with Argv<1.03
    ClearCase::Argv->attropts;		# this is what parses -/dbg=1 et al
    {
	# "Import" these interfaces in case they're wanted.
	# Doubled up to suppress a spurious warning.
	*ClearCase::Wrapper::ctsystem = \&ClearCase::Argv::ctsystem;
	*ClearCase::Wrapper::ctsystem = \&ClearCase::Argv::ctsystem;
	*ClearCase::Wrapper::ctexec = \&ClearCase::Argv::ctexec;
	*ClearCase::Wrapper::ctexec = \&ClearCase::Argv::ctexec;
	*ClearCase::Wrapper::ctqx = \&ClearCase::Argv::ctqx;
	*ClearCase::Wrapper::ctqx = \&ClearCase::Argv::ctqx;
    }
    my $cmd = "ClearCase::Wrapper::$ARGV[0]";
    no strict 'refs';
    # This block handles "ct <cmd> -help" and "ct help <cmd>".
    if ($ARGV[0] eq 'help' || grep /^-h(elp)?$/, @ARGV) {
	my @help;
	my $hlp = ClearCase::Argv->new({-stderr=>0});
	if ($ARGV[0] eq 'help') {
	    $cmd = "ClearCase::Wrapper::$ARGV[-1]";
	    @help = $hlp->argv(@ARGV)->qx;
	} else {
	    @help = $hlp->argv($ARGV[0], '-h')->qx;
	}
	if (defined ${$cmd}) {
	    @help = ('Usage: *') if !@help;
	    chomp(my $text = $$cmd);
	    chomp $help[-1];
	    my($indent) = ($help[-1] =~ /^(\s*)/);
	    substr($indent, -2, 2) = '';
	    $text =~ s/\n/\n$indent/gs;
	    push(@help, $text);
	} elsif (!@help) {
	    $hlp->stderr(2)->exec;
	}
	print @help, "\n";
	exit 0;
    }
    # Call the override subroutine ...
    my $rc = &$cmd(@ARGV);
    # ... and exit unless it returned zero.
    exit $rc if $rc;
}

# Either there was no override defined for this command or the override
# decided to let us finish up by exec-ing the current @ARGV.
# If we're on Windows we need ClearCase::Argv to avoid the weird
# behavior of native exec() there. If we're already using ClearCase::Argv
# we continue to do so, and if any -/foo flags are directed at it
# we drag it in in order to allow it to parse them. But otherwise, in
# order to not unduly slow down a cmd that isn't being overridden anyway,
# we skip all that overhead and just exec.
if ($^O =~ /MSWin32|Windows/ || defined $Argv::{new} || grep(m%^-/%, @ARGV)) {
    if (grep !m%^-/%, @ARGV) {
	require ClearCase::Argv;
	ClearCase::Argv->attropts;
	ClearCase::Argv->new(@ARGV)->inpathnorm(0)->exec;
    } else {
	exit system 'cleartool';
    }
} else {
    die "Error: no ClearCase on this system!\n" unless -d '/usr/atria';
    exec('/usr/atria/bin/cleartool', @ARGV) && exit $?;
}
