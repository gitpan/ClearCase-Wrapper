#!/usr/local/bin/perl -w

use ClearCase::Wrapper;

use strict;

# Technically we should use Getopt::Long::Configure() to set these but
# there's a tangled version history involved and it's faster anyway.
local $Getopt::Long::passthrough = 1; # required for wrapper programs
local $Getopt::Long::ignorecase = 0;  # global override for dumb default

# If Wrapper.pm defines an AutoLoad-ed subroutine to handle $ARGV[0], call it.
# This subroutine may or may not return.
if (defined $ClearCase::Wrapper::{$ARGV[0]}) {
    require ClearCase::Argv;
    ClearCase::Argv->inpathnorm(0);
    ClearCase::Argv->attropts;
    my $cmd = "ClearCase::Wrapper::$ARGV[0]";
    no strict 'refs';
    # This block handles -help extensions.
    if ($ARGV[0] eq 'help' || grep /^-h(elp)?$/, @ARGV) {
	my @help;
	if ($ARGV[0] eq 'help') {
	    @help = ClearCase::Argv->new(@ARGV)->stderr(0)->qx;
	} else {
	    @help = ClearCase::Argv->new($ARGV[0], '-h')->stderr(0)->qx;
	}
	if (defined ${$cmd}) {
	    @help = ('Usage: *') if !@help;
	    chomp(my $text = $$cmd);
	    chomp $help[-1];
	    my($indent) = ($help[-1] =~ /^(\s*)/);
	    substr($indent, -2, 2) = '';
	    $text =~ s/\n/\n$indent/gs;
	    push(@help, $text);
	}
	print @help, "\n";
	exit 0;
    }
    my $rc = &$cmd(@ARGV);
    exit $rc if $rc;
}

# Either there was no override defined for this command or the override
# decided to let us finish up by exec-ing the current @ARGV.
# If we're on Windows we need ClearCase::Argv to avoid the weird
# behavior of native exec() there. If we're already using ClearCase::Argv
# we continue to do so, and if any -/foo flags are directed at it
# we drag it in in order to allow it to parse them. But otherwise, in
# order to not unduly slow down a command we aren't overriding anyway,
# skip all that overhead and just exec.
if ($^O =~ /MSWin32|Windows/ || defined $Argv::{new} || grep(m%^-/%, @ARGV)) {
    require ClearCase::Argv;
    ClearCase::Argv->inpathnorm(0);
    ClearCase::Argv->attropts;
    ClearCase::Argv->new(@ARGV)->exec;
} else {
    exec('/usr/atria/bin/cleartool', @ARGV);
}
