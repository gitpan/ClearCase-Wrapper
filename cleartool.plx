#!/usr/local/bin/perl -w

# The bulk of the code comes from here ...
use ClearCase::Wrapper;

use strict;

# Technically we should use Getopt::Long::Configure() to set these but
# there's a tangled version history involved and this way is faster anyway.
local $Getopt::Long::passthrough = 1; # required for wrapper programs
local $Getopt::Long::ignorecase = 0;  # global override for dumb default

# If Wrapper.pm defines an AutoLoad-ed subroutine to handle $ARGV[0], call it.
# That subroutine may or may not return.
if (@ARGV && !$ENV{CLEARCASE_WRAPPER_NATIVE} &&
	    (defined($ClearCase::Wrapper::{$ARGV[0]}) || $ARGV[0] eq 'help')) {
    require ClearCase::Argv;
    ClearCase::Argv->inpathnorm(0);	# unset an unfortunate default
    ClearCase::Argv->attropts;		# this is what parses -/dbg=1 et al
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
	ClearCase::Argv->inpathnorm(0);
	ClearCase::Argv->attropts;
	ClearCase::Argv->new(@ARGV)->exec;
    } else {
	exit system 'cleartool';
    }
} else {
    die "Error: no ClearCase on this system!\n" unless -d '/usr/atria';
    exec('/usr/atria/bin/cleartool', @ARGV) && exit $?;
}
