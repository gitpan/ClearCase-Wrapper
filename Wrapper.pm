package ClearCase::Wrapper;

$VERSION = '0.24';

require 5.004;

use constant MSWIN => $^O =~ /MSWin32|Windows_NT/i;

use AutoLoader 'AUTOLOAD';
# Suppress the Getopt::Long->import(), we need our own GetOptions().
require Getopt::Long;

# Technically we should use Getopt::Long::Configure() for these but
# there's a tangled version history and this is faster anyway.
$Getopt::Long::passthrough = 1; # required for wrapper programs
$Getopt::Long::ignorecase = 0;  # global override for dumb default

# Determine where this module was found so we can look there for other files.
BEGIN { ($libdir = $INC{'ClearCase/Wrapper.pm'}) =~ s%\.pm$%% }

# Hacks for portability with NT env vars.
BEGIN {
    $ENV{LOGNAME} ||= $ENV{USERNAME};
    $ENV{HOME} ||= "$ENV{HOMEDRIVE}/$ENV{HOMEPATH}";
}

# Unless the user has their own CLEARCASE_PROFILE, set it to the global one.
BEGIN {
    if (defined $ENV{CLEARCASE_PROFILE}) {
	$ENV{_CLEARCASE_WRAPPER_PROFILE} = $ENV{CLEARCASE_PROFILE};
    } elsif ($ENV{_CLEARCASE_WRAPPER_PROFILE}) {
	$ENV{CLEARCASE_PROFILE} = $ENV{_CLEARCASE_WRAPPER_PROFILE};
    } elsif (! -f "$ENV{HOME}/.clearcase_profile") {
	my $rc = join('/', $libdir, 'clearcase_profile');
	$ENV{CLEARCASE_PROFILE} = $rc if -r $rc;
    }
}

use strict;

use vars qw($prog $libdir);
$prog = $ENV{CLEARCASE_WRAPPER_PROG} || (split m%[/\\]+%, $0)[-1];

# Getopt::Long::GetOptions() respects '--' but strips it, while
# we want to respect '--' and leave it in. Thus this override.
sub GetOptions {
    @ARGV = map {/^--$/ ? qw(=--= --) : $_} @ARGV;
    my $ret = Getopt::Long::GetOptions(@_);
    @ARGV = map {/^=--=$/ ? qw(--) : $_} @ARGV;
    return $ret;
}

# Take a string and an array, return the index of the 1st occurrence
# of the string in the array.
sub FirstIndex {
    my $flag = shift;
    for my $i (0..$#_) {
       return $i if $flag eq $_[$i];
    }
    return undef;
}

# Implements the -me -tag convention (see POD).
if (my $me = FirstIndex('-me', @ARGV)) {
    if ($ARGV[0] =~ /^(?:set|start|end)view$|^(?:workon|rdl)$/) {
	my $delim = 0;
	for (@ARGV) {
	    last if /^--$/;
	    $delim++;
	}
	for (reverse @ARGV[0..$delim-1]) {
	    if (/^\w+$/) {
		$_ = join('_', $ENV{LOGNAME}, $_);
		last;
	    }
	}
	splice(@ARGV, $me, 1);
    } elsif (my $tag = FirstIndex('-tag', @ARGV)) {
	$ARGV[$tag+1] = join('_', $ENV{LOGNAME}, $ARGV[$tag+1]);
	splice(@ARGV, $me, 1);
    }
}

# Implements the -M flag (see POD).
if (my $mflag = FirstIndex('-M', @ARGV) || $ENV{CLEARCASE_WRAPPER_MORE}) {
    splice(@ARGV, $mflag, 1) if $mflag && !$ENV{CLEARCASE_WRAPPER_MORE};
    die Msg('E', "sorry, can't do -M on @*&# Windows") if MSWIN;
    pipe(READER, WRITER);
    my $pid;
    if ($pid = fork) {
	close WRITER;
	open(STDIN, ">&READER") || die Msg('E', "STDIN: $!");
	my $pager = $ENV{PAGER} || 'more';
	exec $pager || warn Msg('W', "can't run $pager: $!");
    } else {
	die Msg('E', "can't fork") if !defined($pid);
	close READER;
	open(STDOUT, ">&WRITER") || die Msg('E', "STDOUT: $!");
    }
}

# Turn symbolic links into their targets so CC will "do the right thing".
# But only for checkin/checkout so we don't break e.g. rmname.
if (@ARGV && $ARGV[0] =~ /^(?:ci|co|unc|check|diff|edit)/) {
    for (@ARGV[1..$#ARGV]) { $_ = readlink if -l && defined readlink }
}

#############################################################################
# Usage Message Extensions
#############################################################################
{
   local $^W = 0;
   no strict 'vars';

   # Extended messages for actual cleartool commands that we extend.
   $catcs	= "\n* [-cmnt|-expand|-sources|-start]";
   $checkin	= "\n* [-dir|-rec|-all|-avobs] [-diff [diff-opts]] [-revert]";
   $ci		= $checkin;
   $diff	= "\n* [-<n>] [-dir|-rec|-all|-avobs]";
   $lock	= "\n* [-allow|-deny login-name[,...]] [-iflocked]";
   $lsprivate	= "\n* [-dir|-rec|-all] [-rel/ative] [-ext] [-type d|f] [pname]";
   $lsp		= $lsprivate;
   $lsview	= "* [-me]";
   $mkelem	= "\n* [-dir|-rec] [-do] [-ok]";
   $mklabel	= "\n* [-up]";
   $mkview	= "\n* [-me] [-clone]";
   $setcs	= "\n\t     * [-clone view-tag] [-expand] [-sync]";
   $setview	= "* [-me] [-drive drive:] [-persistent]";
   $uncheckout	= "* [-nc]";
   $unco	= $uncheckout;
   $winkin	= "\n* [-vp] [-tag view-tag]";

   # Extended messages for pseudo cleartool commands that we implement here.
   local $0 = $ARGV[0] || '';
   $comment	= "$0 [-new] [-element] object-selector ...";
   $diffcs	= "$0 view-tag-1 [view-tag-2]";
   $edattr	= "$0 [-element] object-selector ...";
   $edit	= "$0 <co-flags> [-ci] <ci-flags> pname ...";
   $grep	= "$0 [grep-flags] pattern element";
   $recheckout	= "$0 pname ...\n";
   $reco	= $recheckout;
   $winkout	= "$0 [-dir|-rec|-all] [-f file] [-pro/mote] [-do]
		[-meta file [-print] file ...";
   $workon	= "$0 [-me] [-login] [-exec command-invocation] view-tag\n";
}

#############################################################################
# Command Aliases
#############################################################################
*ci		= \&checkin;
*co		= \&checkout;
*egrep		= \&grep;
*lsp		= \&lsprivate;
*lspriv		= \&lsprivate;
*unco		= \&uncheckout;
*mkbrtype	= \&mklbtype;	# not synonyms but the code's the same
*reco		= \&recheckout;
*edcmnt		= \&comment;
*vi		= \&edit;

#############################################################################
# Allow per-user configurability. Give the individual access to @ARGV just
# before we hand it off to the local wrapper function and/or cleartool.
# Access to this feature is suppressed if the 'no_overrides' file exists.
#############################################################################
if (-r "$ENV{HOME}/.clearcase_profile.pl" && ! -e "$libdir/NO_OVERRIDES") {
    require "$ENV{HOME}/.clearcase_profile.pl";
}

# This is an enhancement like the ones below but is kept "above the
# fold" because wrapping of cleartool man is an integral and generic
# part of the module. It runs "cleartool man <cmd>" as requested,
# followed by "perldoc ClearCase::Wrapper" iff <cmd> is extended below.
sub man {
    my $page = pop @ARGV;
    my $module = __PACKAGE__;
    ClearCase::Argv->man($page)->system unless $page eq $prog;
    my $override = exists $ClearCase::Wrapper::{$page};
    exit $? unless $override || $page eq $prog;
    if ($override) {
	# We need to determine which .pm file to run perldoc on.
	# Delete the function in question from the symbol table,
	# then drag in the Site.pm file again and see if it came back.
	delete $ClearCase::Wrapper::{$page};
	delete $INC{'ClearCase/Wrapper/Site.pm'};
	{
	    local $^W = 0;
	    require ClearCase::Wrapper::Site;
	}
	$module = 'ClearCase::Wrapper::Site'
		    if exists $ClearCase::Wrapper::{$page};
	# This EV hack causes perldoc to search for the right keyword
	# within the perldoc.
	if (!MSWIN) {
	    require Config;
	    my $pager = $Config::Config{pager};
	    $ENV{PERLDOC_PAGER} ||= "$pager +/" . uc($page)
		if $pager =~ /more|less/;
	}
    }
    my $psep = MSWIN ? ';' : ':';
    require File::Basename;
    $ENV{PATH} = join($psep, File::Basename::dirname($^X), $ENV{PATH});
    Argv->perldoc($module)->exec;
    exit $?;
}

1;

__END__

=head1 NAME

ClearCase::Wrapper - general-purpose wrapper for B<cleartool>

=head1 SYNOPSIS

This perl module functions as a wrapper for B<cleartool>, allowing the
command-line interface of B<cleartool> to be extended or modified. It
allows defaults to be changed, new flags to be added to existing
B<cleartool> commands, or entirely new B<cleartool> commands to be
synthesized.

=cut

###########################################################################
## Internal service routines, autoloaded since not always needed.
###########################################################################

sub Msg {
    my $type = shift;
    $type = {W=>Warning, E=>Error}->{$type} || $type if defined($type);
    chomp(my $msg = "@_");
    if ($type) {
	return "$prog: $type: $msg\n";
    } else {
	return "$prog: $msg\n";
    }
}

# Function to parse 'include' stmts recursively.  Used by
# config-spec parsing meta-commands. The first arg is a
# "magic incrementing string", the second a filename,
# the third an "action" which is eval-ed
# for each line.  It can be as simple as 'print' or as
# complex a regular expression as desired. If the action is
# null, only the names of traversed files are printed.
sub Burrow {
    local $input = shift;
    my($filename, $action) = @_;
    print $filename, "\n" if !$action;
    $input++;
    if (!open($input, $filename)) {
	warn "$filename: $!";
	return;
    }
    while (<$input>) {
	if (/^include\s+(.*)/) {
	    Burrow($input, $1, $action);
	    next;
	}
	eval $action if $action;
    }
}

sub Pred {
    my($vers, $count, $ct) = @_;
    if ($count) {
	$ct ||= ClearCase::Argv->new;
	(my $elem = $vers) =~ s/@@.*//;
	chomp(my $pred = $ct->desc([qw(-pred -s)], $vers)->qx);
	return Pred("$elem@\@$pred", $count-1, $ct);
    } else {
	return $vers;
    }
}

# Examines supplied arg vector, returns the explicit or implicit working view.
sub ViewTag {
    my $vtag;
    if (@_) {
	local(@ARGV) = @_;
	GetOptions("tag=s" => \$vtag);
    }
    if (!$vtag) {
	require Cwd;
	my $cwd = Cwd::fastgetcwd;
	if (MSWIN) {
	    $cwd =~ s/^[A-Z]://i;
	    $cwd =~ s%\\%/%g;
	}
	if ($cwd =~ m%/+view/([^/]+)%) {
	    $vtag ||= $1;
	}
    }
    if (!$vtag && $ENV{CLEARCASE_ROOT}) {
	$vtag = (split(m%[/\\]%, $ENV{CLEARCASE_ROOT}))[-1];
    }
    $vtag ||= ClearCase::Argv->pwv(['-s'])->qx;
    chomp $vtag if $vtag;
    return $vtag;
}

# Print out the list of elements derived as 'eligible', whatever
# that means for the current op.
sub ShowFound {
    my $n = @_;
    if ($n == 0) {
	print STDERR Msg(undef, "no eligible elements found");
    } elsif ($n == 1) {
	print STDERR Msg(undef, "found 1 file: @_");
    } elsif ($n <= 10) {
	print STDERR Msg(undef, "found $n files: @_");
    } else {
	print STDERR Msg(undef, "found $n files: @_[0..3] ...");
    }
}

# Return the list of checked-out elements according to
# the -dir/-rec/-all/-avobs flags. Passes the supplied
# args to lsco, returns the result.
sub AutoCheckedOut {
    return () unless @_;
    my @args = @_;
    my @auto = grep /^-(?:dir|rec|all|avo)/, @args;
    return @args unless @auto;
    die Msg('E', "mutually exclusive flags: @auto") if @auto > 1;
    my $lsco = ClearCase::Argv->new('lsco', [qw(-cvi -s)],
						    grep !/^-(d|cvi)/, @args);
    $lsco->stderr(0) if grep !/^-/, @args; # in case v-p files are listed
    chomp(my @co = $lsco->qx);
    ShowFound(@co);
    exit 0 unless @co;
    return @co;
}

# Return the list of not-checked-out elements according to
# the -dir/-rec flags (-all/-avobs not supported).
sub AutoNotCheckedOut {
    return () unless @_;
    my @args = @_;
    my @agg = grep /^-(?:dir|rec|all|avo)/, @args;
    return @args unless @agg;
    die Msg('E', "mutually exclusive flags: @agg") if @agg > 1;
    die Msg('E', "only -dir/-recurse supported: @agg") if $agg[0] =~ /^-a/;
    # First derive a list of all FILE elements under the cwd.
    my @e = ClearCase::Argv->new(qw(find . -type f -cvi -nxn -print))->qx;
    chomp @e;
    # Turn the list into a hash while removing the leading "./".
    my %elems = map {substr($_, 2) => 1} @e;
    # Then, narrow it to elems WITHIN the cwd unless -rec.
    if ($agg[0] !~ /^-rec/) {
	for (keys %elems) {
	    delete $elems{$_} if m%[/\\]%;
	}
    }
    # Remove those which are already checked out to this view.
    if (%elems) {
	my $lsco = ClearCase::Argv->new('lsco', [qw(-cvi -s)]);
	for ($lsco->args(keys %elems)->qx) {
	    chomp;
	    delete $elems{$_};
	}
    }
    # Done: we have a list of all file elems that are not checked out.
    my @not_co = sort keys %elems;
    ShowFound(@not_co);
    exit 0 unless @not_co;
    return @not_co;
}

###########################################################################
## Beginning of command enhancements ...
###########################################################################

=head1 CLEARTOOL ENHANCEMENTS

=over 4

=item * CATCS

=over 4

=item 1. New B<-expand> flag

Follows all include statements recursively in order to print a complete
config spec. When used with the B<-cmnt> flag, comments are stripped
from this listing.

=item 2. New B<-sources> flag

Prints all files involved in the config spec (the I<config_spec> file
itself plus any files it includes).

=item 3. New B<-start> flag

Prints the preferred I<initial working directory> of a view by
examining its config spec. If the conventional string C"<##:Start:
I<dir>"> is present then the value of I<dir> is printed. Otherwise no
output is produced. The B<workon> command (see) uses this value.  E.g.,
using B<workon> instead of B<setview> with the config spec:

    ##:Start: /vobs_fw/src/java
    element * CHECKEDOUT
    element * /main/LATEST

would automatically cd to C</vobs_fw/src/java> within the set view.

=item 4. New B<-rdl> flag

Prints the value of the config spec's C"<##:RDL:> attribute.

=back

=cut

sub catcs {
    my(%opt, $op);
    GetOptions(\%opt, qw(cmnt expand rdl start sources viewenv vobs));
    if ($opt{sources}) {
	$op = '';
    } elsif ($opt{expand}) {
	$op = 'print';;
    } elsif ($opt{'rdl'}) {
	$op = 's%##:RDL:\s*(.+)%print "$+\n";exit 0%ie';
    } elsif ($opt{viewenv}) {
	$op = 's%##:ViewEnv:\s+(\S+)%print "$+\n";exit 0%ie';
    } elsif ($opt{start}) {
	$op = 's%##:Start:\s+(\S+)|^\s*element\s+(\S*)/\.{3}\s%print "$+\n";exit 0%ie';
    } elsif ($opt{vobs}) {
	$op = 's%^element\s+(\S+)/\.{3}\s%print "$1\n"%e';
    }
    if (defined $op) {
	$op .= ' unless /^\s*#/' if $op && $opt{cmnt};
	my $tag = ViewTag(@ARGV);
	die Msg('E', "no view tag specified or implicit") if !$tag;;
	my($vws) = reverse split '\s+', ClearCase::Argv->lsview($tag)->qx;
	exit Burrow('CATCS_00', "$vws/config_spec", $op);
    }
}

=item * CI/CHECKIN

Extended to handle the B<-dir/-rec/-all/-avobs> flags.

Extended to allow B<symbolic links> to be checked in (by operating on
the target of the link instead).

Extended to implement a B<-diff> flag, which runs a B<I<diff -pred>>
command before each checkin so the user can see his/her changes while
typing the comment.

Automatically supplies B<-nc> to checkins if the element list consists
of only directories (since directories get a default comment).

Implements a new B<-revert> flag. This causes identical (unchanged)
elements to be unchecked-out instead of being checked in.

Since checkin is such a common operation, an unadorned I<ci> is
"promoted" to I<ci -diff -dir -revert> to save typing.

=cut

sub checkin {
    # Allows 'ct ci' to be shorthand for 'ct ci -diff -revert -dir'.
    push(@ARGV, qw(-diff -revert -dir)) if @ARGV == 1;

    my %opt;
    # -re999 isn't a real flag, it's to disambiguate -rec from -rev
    GetOptions(\%opt, qw(diff revert re999)) if grep /^-(dif|rev)/, @ARGV;

    my $ci = ClearCase::Argv->new(@ARGV);

    # Parse checkin and (potential) diff flags into different optsets.
    $ci->parse(qw(c|cfile=s cqe|nc
		    nwarn|cr|ptime|identical|rm|cact|cwork from=s));
    if ($opt{'diff'} || $opt{revert}) {
	$ci->optset('DIFF');
	$ci->parseDIFF(qw(serial_format|diff_format|window columns|options=s
			    graphical|tiny|hstack|vstack|predecessor));
    }

    # Now do auto-aggregation on the remaining args.
    $ci->args(AutoCheckedOut($ci->args));
    my @elems = $ci->args;

    # Default to -nc if checking in directories only.
    if (!grep(/^-c$|^-cq|^-nc$|^-cfi/, @ARGV)) {
	$ci->opts('-nc', $ci->opts) if !grep {!-d} @elems;
    }

    # Unless -diff or -revert in use, we're done.
    $ci->exec unless $opt{'diff'} || $opt{revert};

    # Make sure the -pred flag is there as we're going one at a time.
    my $diff = $ci->clone->prog('diff');
    $diff->optsDIFF(qw(-pred -serial), $diff->optsDIFF);

    # In case ~/.clearcase_profile makes ci -nc the default, make sure
    # we prompt for a comment - unless checking in dirs only.
    $ci->opts('-cqe', $ci->opts)
			if !grep(/^-c|^-nc$/, $ci->opts) && grep(-f, @elems);

    $diff->stdout(0) if !$opt{'diff'};  # if -revert we only care about retcode
    for $elem (@elems) {
	my $chng = $diff->args($elem)->system('DIFF');
	if ($opt{revert} && !$chng) {
	    # if -revert and no changes, unco instead of checkin
	    ClearCase::Argv->unco(['-rm'], $elem)->system;
	} else {
	    $ci->args($elem)->system;
	}
    }

    # All done, no need to return to wrapper program.
    exit $?>>8;
}

=item * CO/CHECKOUT

Extended to handle the B<-dir/-rec> flags. NOTE: the B<-all/-avobs>
flags are disallowed for checkout.

=cut

sub checkout {
    return 0 unless grep /^-(dir|rec)/, @ARGV;

    # Remove the aggregation flag, push the aggregated list of
    # not-checked-out file elements onto argv, and return.
    my @added = AutoNotCheckedOut(@ARGV);
    {
	my %rm;
	GetOptions(\%rm, qw(directory recurse all avobs do));
    }
    push(@ARGV, @added);
    return 0;
}

=item * COMMENT

For each ClearCase object specified, dump the current comment into a
temp file, allow the user to edit it with his/her favorite editor, then
change the objects's comment to the results of the edit. This is
useful if you mistyped a comment and want to correct it.

The B<-new> flag causes it to ignore the previous comment.

See B<edattr> for editor selection rules.

=cut

sub comment {
    shift @ARGV;
    my %opt;
    GetOptions(\%opt, qw(element new));
    my $retstat = 0;
    my $editor = $ENV{WINEDITOR} || $ENV{VISUAL} || $ENV{EDITOR} ||
						    (MSWIN ? 'notepad' : 'vi');
    my $ct = ClearCase::Argv->new;
    # Checksum before and after edit - only update if changed.
    my($csum_pre, $csum_post) = (0, 0);
    for my $obj (@ARGV) {
	my @input = ();
	$obj .= '@@' if $opt{element};
	if (!$opt{new}) {
	    @input = $ct->desc([qw(-fmt %c)], $obj)->qx;
	    next if $?;
	}
	my $edtmp = ".$prog.comment.$$";
	open(EDTMP, ">$edtmp") || die Msg('E', "$edtmp: $!");
	for (@input) {
	    next if /^~\w$/;  # Hack - allow ~ escapes for ci-trigger a la mailx
	    $csum_pre += unpack("%16C*", $_);
	    print EDTMP $_;
	}
	close(EDTMP) || die Msg('E', "$edtmp: $!");

	# Run editor on temp file
	Argv->new($editor, $edtmp)->system;

	open(EDTMP, $edtmp) || die Msg('E', "$edtmp: $!");
	while (<EDTMP>) { $csum_post += unpack("%16C*", $_); }
	close(EDTMP) || die Msg('E', "$edtmp: $!");
	unlink $edtmp, next if $csum_post == $csum_pre;
	$retstat++ if $ct->chevent([qw(-replace -cfi), $edtmp], $obj)->system;
	unlink $edtmp;
    }
    exit $retstat;
}

=item * DIFF

Extended to handle the B<-dir/-rec/-all/-avobs> flags.

Improved default: if given just one element and no flags, assume B<-pred>.

Extended to implement B<-n>, where I<n> is an integer requesting that
the diff take place against the I<n>'th predecessor.

=cut

sub diff {
    # Allows 'ct diff' to be shorthand for 'ct diff -dir'.
    push(@ARGV, qw(-dir)) if @ARGV == 1;

    my $limit = 0;
    if (my @num = grep /^-\d+$/, @ARGV) {
	@ARGV = grep !/^-\d+$/, @ARGV;
	die Msg('E', "incompatible flags: @num") if @num > 1;
	$limit = -int($num[0]);
    }
    my $diff = ClearCase::Argv->new(@ARGV);
    $diff->parse(qw(options=s serial_format|diff_format|window
		    graphical|tiny|hstack|vstack|predecessor));
    my @args = $diff->args;
    my $auto = grep /^-(?:dir|rec|all|avo)/, @args;
    my @elems = AutoCheckedOut(@args);
    $diff->args(@elems);
    my @opts = $diff->opts;
    my @extra = ('-serial') if !grep(/^-(?:ser|dif|col|g)/, @opts);
    if ($limit && @elems == 1) {
	$diff->args(Pred($elems[0], $limit, ClearCase::Argv->new), @elems);
    } else {
	push(@extra, '-pred') if ($auto || @elems < 2) && !grep(/^-pre/, @opts);
    }
    $diff->opts(@opts, @extra) if @extra;
    if ($auto && @elems > 1) {
	for (@elems) { $diff->args($_)->system }
	exit $?;
    } else {
	$diff->exec;
    }
}

=item * DIFFCS

New command.  B<Diffcs> dumps the config specs of two specified views
into temp files and diffs them. If only one view is specified, compares
against the current working view's config spec.

=cut

sub diffcs {
    my %opt;
    GetOptions(\%opt, qw(tag=s@));
    my @tags = @{$opt{tag}} if $opt{tag};
    push(@tags, @ARGV[1..$#ARGV]);
    if (@tags == 1) {
	my $cwv = ViewTag();
	push(@tags, $cwv) if $cwv;
    }
    die Msg('E', "two view-tag arguments required") if @tags != 2;
    my $ct = ClearCase::Argv->cleartool;
    my @cstmps = map {"$_.cs"} @tags;
    for my $i (0..1) {
	Argv->new("$ct catcs -tag $tags[$i] >$cstmps[$i]")->autofail(1)->system;
    }
    Argv->new('diff', @cstmps)->dbglevel(1)->system;
    unlink(@cstmps);
    exit 0;
}

=item * EDATTR

New command, inspired by the I<edcs> cmd.  B<Edattr> dumps the
attributes of the specified object into a temp file, then execs your
favorite editor on it, and adds, removes or modifies the attributes as
appropriate after you exit the editor.  Attribute types are created and
deleted automatically.  This is particularly useful on Unix platforms
because as of CC 3.2 the Unix GUI doesn't support modification of
attributes and the quoting rules make it difficult to use the
command line.

The environment variables WINEDITOR, VISUAL, and EDITOR are checked
in that order for editor names. If none of the above are set, the
default editor used is vi on UNIX and notepad on Windows.

=cut

sub edattr {
    my %opt;
    GetOptions(\%opt, qw(element));
    shift @ARGV;
    my $retstat = 0;
    my $editor = $ENV{WINEDITOR} || $ENV{VISUAL} || $ENV{EDITOR} ||
						    (MSWIN ? 'notepad' : 'vi');
    my $ct = ClearCase::Argv->new;
    my $ctq = $ct->clone({-stdout=>0, -stderr=>0});
    for my $obj (@ARGV) {
	my %indata = ();
	$obj .= '@@' if $opt{element};
	my @lines = $ct->desc([qw(-aattr -all)], $obj)->qx;
	if ($?) {
	    $retstat++;
	    next;
	}
	for my $line (@lines) {
	    next unless $line =~ /\s*(\S+)\s+=\s+(.+)/;
	    $indata{$1} = $2;
	}
	my $edtmp = ".$prog.edattr.$$";
	open(EDTMP, ">$edtmp") || die Msg('E', "$edtmp: $!");
	print EDTMP "# $obj (format: attr = \"val\"):\n\n" if ! keys %indata;
	for (sort keys %indata) { print EDTMP "$_ = $indata{$_}\n" }
	close(EDTMP) || die Msg('E', "$edtmp: $!");

	# Run editor on temp file
	Argv->new($editor, $edtmp)->system;

	open(EDTMP, $edtmp) || die Msg('E', "$edtmp: $!");
	while (<EDTMP>) {
	    chomp;
	    next if /^\s*$|^\s*#.*$/;	# ignore null and comment lines
	    if (/\s*(\S+)\s+=\s+(.+)/) {
		my($attr, $newval) = ($1, $2);
		my $oldval;
		if (defined($oldval = $indata{$attr})) {
		    delete $indata{$attr};
		    # Skip if data unchanged.
		    next if $oldval eq $newval;
		}
		# Figure out what type the new attype needs to be.
		# Sorry, didn't bother with -vtype time.
		if ($ctq->lstype("attype:$attr")->system) {
		    if ($newval =~ /^".*"$/) {
			$ct->mkattype([qw(-nc -vty string)], $attr)->system;
		    } elsif ($newval =~ /^[+-]?\d+$/) {
			$ct->mkattype([qw(-nc -vty integer)], $attr)->system;
		    } elsif ($newval =~ /^-?\d+\.?\d*$/) {
			$ct->mkattype([qw(-nc -vty real)], $attr)->system;
		    } else {
			$ct->mkattype([qw(-nc -vty opaque)], $attr)->system;
		    }
		    next if $?;
		}
		# Deal with broken quoting on &^&@# Windows.
		if (MSWIN && $newval =~ /^"(.*)"$/) {
		    $newval = qq("\\"$1\\"");
		}
		# Make the new attr value.
		if (defined($oldval)) {
		    $retstat++ if $ct->mkattr([qw(-rep -c)],
			 "(Was: $oldval)", $attr, $newval, $obj)->system;
		} else {
		    $retstat++ if $ct->mkattr([qw(-rep)],
			 $attr, $newval, $obj)->system;
		}
	    } else {
		warn Msg('W', "incorrect line format: '$_'");
		$retstat++;
	    }
	}
	close(EDTMP) || die Msg('E', "$edtmp: $!");
	unlink $edtmp;

	# Now, delete any attrs that were deleted from the temp file.
	# First we do a simple rmattr; then see if it was the last of
	# its type and if so remove the type too.
	for (sort keys %indata) {
	    if ($ct->rmattr($_, $obj)->system) {
		$retstat++;
	    } else {
		# Don't remove the type if its vob serves as an admin vob!
		my @deps = grep /^<-/,
				$ct->desc([qw(-s -ahl AdminVOB)], 'vob:.')->qx;
		next if $? || @deps;
		$ct->rmtype(['-rmall'], "attype:$_")->system;
	    }
	}
    }
    exit $retstat;
}

=item * EDIT

Convenience command. Same as 'checkout' but execs your favorite editor
afterwards. Takes all the same flags as checkout, plus B<-ci> to check
the element back in afterwards. When B<-ci> is used in conjunction with
B<-diff> the file will be either checked in or un-checked out depending
on whether it was modified.

The aggregation flags B<-dir/-rec/-all/-avo> may be used, with the
effect being to run the editor on all checked-out files in the named
scope. Example: I<"ct edit -all">.

=cut

sub edit {
    # Allows 'ct edit' to be shorthand for 'ct edit -dir'.
    push(@ARGV, qw(-dir)) if @ARGV == 1;
    my %opt;
    # -c999 isn't a real flag, it's there to disambiguate -c vs -ci
    GetOptions(\%opt, qw(ci c999)) if grep /^-ci$/, @ARGV;
    my $co = ClearCase::Argv->new('co', @ARGV[1..$#ARGV]);
    $co->optset('CI');
    $co->parse(qw(out|branch=s reserved|unreserved|ndata|version|nwarn));
    $co->parseCI(qw(nwarn|cr|ptime|identical|rm from=s c|cfile=s cq|nc diff|revert));
    my $editor = $ENV{WINEDITOR} || $ENV{VISUAL} || $ENV{EDITOR} ||
						    (MSWIN ? 'notepad' : 'vi');
    # Handle -dir/-rec/etc
    $co->args(AutoCheckedOut($co->args)) if grep /^-(?:dir|rec|all|avo)/, @ARGV;
    my $ed = $co->clone->prog([$editor]);
    $co->args(grep !-w, $co->args);
    $co->opts('-nc', $co->opts);
    $co->autofail(1)->system if $co->args;
    $ed->system('-');
    exit $? unless $opt{'ci'};
    # Use the wrapper for checkin in case of special flags.
    $ed->optsCI('-revert') unless $ed->optsCI;
    $ed->prog([$^X, '-S', $0, 'ci'])->exec('CI');
}

=item * GREP

New command. Greps through past revisions of a file for a pattern, so
you can see which revision introduced a particular function or a
particular bug. By analogy with I<lsvtree>, I<grep> searches only
"interesting" versions unless B<-all> is specified.

Flags B<-nnn> are accepted where I<nnn> represents the number of versions
to go back. Thus C<grep -1 foo> would search only the predecessor.

=cut

sub grep {
    my %opt;
    GetOptions(\%opt, 'all');
    my $elem = pop(@ARGV);
    my $limit = 0;
    if (my @num = grep /^-\d+$/, @ARGV) {
	@ARGV = grep !/^-\d+$/, @ARGV;
	die Msg('E', "incompatible flags: @num") if @num > 1;
	$limit = -int($num[0]);
    }
    my $lsvt = ClearCase::Argv->new('lsvt', ['-s'], $elem);
    $lsvt->opts('-all', $lsvt->opts) if $opt{all} || $limit > 1;
    chomp(my @vers = sort {($b =~ m%/(\d+)%)[0] <=> ($a =~ m%/(\d+)%)[0]}
						grep {m%/\d+$%} $lsvt->qx);
    splice(@vers, $limit) if $limit;
    splice(@ARGV, 0, 1, 'egrep');
    Argv->new(@ARGV, @vers)->dbglevel(1)->exec;
}

=item * LOCK

New B<-allow> and B<-deny> flags. These work like I<-nuser> but operate
incrementally on an existing I<-nuser> list rather than completely
replacing it. When B<-allow> or B<-deny> are used, I<-replace> is
implied.

When B<-iflocked> is used, no lock will be created where one didn't
previously exist; the I<-nusers> list will only be modified for
existing locks.

=cut

sub lock {
    my %opt;
    GetOptions(\%opt, qw(allow=s deny=s iflocked));
    return 0 unless %opt;
    my $lock = ClearCase::Argv->new(@ARGV);
    $lock->parse(qw(c|cfile=s c|cquery|cqeach nusers=s
						    pname=s obsolete replace));
    die Msg('E', "cannot specify -nusers along with -allow or -deny")
					if $lock->flag('nusers');
    die Msg('E', "cannot use -allow or -deny with multiple objects")
					if $lock->args > 1;
    my $lslock = ClearCase::Argv->lslock([qw(-fmt %c)], $lock->args);
    my($currlock) = $lslock->autofail(1)->qx;
    if ($currlock && $currlock =~ m%^Locked except for users:\s+(.*)%) {
	my %nusers = map {$_ => 1} split /\s+/, $1;
	if ($opt{allow}) {
	    for (split /,/, $opt{allow}) { $nusers{$_} = 1 }
	}
	if ($opt{deny}) {
	    for (split /,/, $opt{deny}) { delete $nusers{$_} }
	}
	$lock->opts($lock->opts, '-nusers', join(',', sort keys %nusers))
								    if %nusers;
    } elsif (!$currlock && $opt{iflocked}) {
	exit 0;
    } elsif ($opt{allow}) {
	$lock->opts($lock->opts, '-nusers', $opt{allow});
    }
    $lock->opts($lock->opts, '-replace') unless $lock->flag('replace');
    $lock->exec;
}

=item * LSPRIVATE

Extended to recognize B<-dir/-rec/-all/-avobs>.  Also allows a
directory to be specified such that 'ct lsprivate .' restricts output
to the cwd, etc. This directory arg may be used in combination with
B<-dir> etc. Output in these cases is relative to the current or
specified directory if the B<-rel/ative> flag is used.

The flag B<-type d|f> is also supported with the usual semantics.

The flag B<-visible> flag removes files not currently visible in the
view.

The B<-ext> flag sorts the output by extension.

=cut

sub lsprivate {
    my %opt;
    GetOptions(\%opt, qw(directory recurse all avobs
						ext relative type=s visible));

    my $lsp = ClearCase::Argv->new(@ARGV);
    $lsp->parse(qw(co|do|other|short|long tag|invob=s));

    my $pname = '.';

    # Extension: allow [dir] argument
    if ($lsp->args) {
	chomp(($pname) = $lsp->args);
	$lsp->args;
	# Default to -rec but accept -dir.
	$opt{recurse} = 1 unless $opt{directory} || $opt{all} || $opt{avobs};
    }

    # Extension: implement [-dir|-rec|-all|-avobs]
    if ($opt{directory} || $opt{recurse} || $opt{all} || $opt{ext}) {
	require Cwd;
	my $dir = Cwd::abs_path($pname);
	my $tag = $lsp->flag('tag');
	if ($opt{directory} || $opt{recurse}) {
	    if ($dir =~ s%/+view/([^/]+)%%) {	# UNIX view-extended path
		$tag ||= $1;
	    } elsif ($dir =~ s%^[A-Z]:%%) {	# WIN view-extended path
		if ($tag) {
		    $dir =~ s%^/$tag%%i;
		} else {
		    $tag = ViewTag(@ARGV);
		}
	    } elsif (!$tag) {
		$tag = ViewTag(@ARGV);
	    }
	    $lsp->opts($lsp->opts, '-tag', $tag) if !$lsp->flag('tag');
	} elsif ($opt{all}) {
	    $lsp->opts($lsp->opts, '-invob', $pname);
	}
	chomp(my @privs = sort $lsp->qx);
	exit $? if $? || !@privs;
	my @results = @privs;
	if ($opt{directory} || $opt{recurse}) {
	    for (@privs) {
		if (MSWIN) {
		    s/^[A-Z]://i;
		    s%\\%/%g;
		}
		s%(/+view)?/$tag%%;
	    }
	    @privs = map {$_ eq $dir ? "$_/" : $_} @privs;
	    my $job = $opt{relative} ? 'map ' : 'grep ';
	    $job .= $opt{recurse} ? '{m%^$dir/(.*)%}' : '{m%^$dir/([^/]*)$%s}';
	    $opt{type} ||= 'e' if $opt{visible};
	    $job = "grep {-$opt{type}} $job" if $opt{type};
	    eval qq(\@results = $job \@privs);
	    exit 0 if !@results;
	}
	if ($opt{ext}) {	# sort by extension
	    require File::Basename;
	    @results = map  { $_->[0] }
	       sort { "$a->[1]$a->[2]$a->[3]" cmp "$b->[1]$b->[2]$b->[3]" }
	       map  { [$_, (File::Basename::fileparse($_, '\.\w+'))[2,0,1]] }
	       @results;
	}
	for (@results) { print $_, "\n" }
	exit 0;
    }
    $lsp->exec;
}

=item * LSVIEW

Extended to recognize the general B<-me> flag, restricting the search
namespace to E<lt>B<username>E<gt>_*.

=cut

sub lsview {
    my @args = grep !/^-me/, @ARGV;
    push(@args, "$ENV{LOGNAME}_*") if @args != @ARGV;
    ClearCase::Argv->new(@args)->autoquote(0)->exec;
}

=item * LSVTREE

Modified default to always use B<-a> flag.

=cut

sub lsvtree {
    splice(@ARGV, 1, 0, '-a') if !grep(/^-a/, @ARGV);
}

=item * MKELEM

Extended to handle the B<-dir/-rec> flags, enabling automated mkelems
with otherwise the same syntax as original. Directories are also
automatically checked out as required in this mode. B<Note that this
automatic directory checkout is only enabled when the candidate list is
derived via the B<-dir/-rec> flags>.  If the B<-ci> flag is present,
any directories automatically checked out are checked back in too.

By default, only regular (I<-other>) view-private files are considered
by I<-dir|-rec>.  The I<-do> flag causes derived objects to be made
into elements as well.

If I<-ok> is specified, the user will be prompted to continue after the
list of eligible files is determined.

=cut

sub mkelem {
    my %opt;
    GetOptions(\%opt, qw(directory recurse all avobs do ok));
    die Msg('E', "-all|-avobs flags not supported for mkelem")
					if $opt{all} || $opt{avobs};
    return unless $opt{directory} || $opt{recurse};

    # Derive the list of view-private files to work on.
    my $scope = $opt{recurse} ? '-rec' : '-dir';
    my @vps;
    # Can't use lsprivate in a snapshot view ...
    if (-e '.@@/main/0') {
	my $lsp = Argv->new([$^X, '-S', $0, 'lsp'], [qw(-s -oth), $scope]);
	$lsp->opts($lsp->opts, '-do') if $opt{'do'};
	chomp(@vps = $lsp->qx);
    } else {
	require File::Spec;
	File::Spec->VERSION(0.82);
	die Msg('E', "-do not supported in a snapshot view") if $opt{'do'};
	my $ls = Argv->new([$^X, '-S', $0, 'ls'], [qw(-s -view), $scope]);
	chomp(@vps = $ls->qx);
	@vps = map {File::Spec->rel2abs($_)} @vps;
    }
    # Certain files we don't ever want to put under version control...
    @vps = grep !/(?:\.(?:n|mv)fs_|\.(?:abe|cmake)\.state)$/, @vps;
    ShowFound(@vps);
    exit 0 unless @vps;
    if ($opt{ok}) {
	print "Continue? [yes] ";
	chomp(my $resp = <STDIN>);
	exit 0 if $resp && $resp !~ /^y/i;
    }

    my $ct = ClearCase::Argv->new({-autofail=>1});

    # We'll be separating the elements-to-be into files and directories.
    my(@files, %dirs);

    # If the parent directories of any of the candidates are already
    # versioned elements we may need to check them out.
    require File::Basename;
    my %seen;
    for (@vps) {
	my $d = File::Basename::dirname($_);
	next if ! $d || $dirs{$d};
	next if $seen{$d}++;
	my $lsd = $ct->ls(['-d'], $d)->qx;
	# If no version selector was given it's a view-private dir and
	# will be handled below.
	next unless $lsd =~ /\sRule:\s/;
	# If already checked out, nothing to do.
	next if $lsd =~ /CHECKEDOUT$/;
	# Now we know it's an element and needs to be checked out.
	$dirs{$d}++;
    }
    $ct->co(['-nc'], keys %dirs)->system if %dirs;

    # Process candidate directories here, then do files below.
    for my $cand (@vps) {
	if (! -d $cand) {
	    push(@ARGV, $cand);
	    next;
	}
	# Now we know we're dealing with directories.  These must not
	# exist at mkelem time so we move them aside, make
	# a versioned dir, then move all the files from the original
	# back into the new dir (still as view-private files).
	my $tmpdir = "$cand.$$.keep.d";
	die Msg('E', "$cand: $!") unless rename($cand, $tmpdir);
	$ct->mkdir(['-nc'], $cand)->system;
	opendir(DIR, $tmpdir) || die Msg('E', "$tmpdir: $!");
	while (defined(my $i = readdir(DIR))) {
	    next if $i eq '.' || $i eq '..';
	    rename("$tmpdir/$i", "$cand/$i") || die Msg('E', "$cand/$i: $!");
	}
	closedir DIR;
	warn Msg('W', "$tmpdir: $!") unless rmdir $tmpdir;
	# Keep a record of directories to be checked in when done.
	$dirs{$cand}++;
    }

    # Now we've made all the directories, do the files in one fell swoop.
    $ct->argv(@ARGV)->system if grep -f, @ARGV;

    # Last - if the -ci flag was supplied, check the dirs back in.
    $ct->ci(['-nc'], keys %dirs)->exec if %dirs && grep /^-ci$/, @ARGV;

    # Done - don't drop back to main program.
    exit $?;
}

=item * MKLABEL

The new B<-up> flag, when combined with B<-recurse>, also labels the parent
directories of the specified I<pname>s all the way up to their vob tags.

=cut

sub mklabel {
    my %opt;
    GetOptions(\%opt, qw(up));
    return 0 unless $opt{up};
    die Msg('E', "-up requires -recurse") if !grep /^-re?$|^-rec/, @ARGV;
    my $mkl = ClearCase::Argv->new(@ARGV);
    my $dsc = ClearCase::Argv->new({-autochomp=>1});
    $mkl->parse(qw(replace|recurse|ci|cq|nc
				version|c|cfile|select|type|name|config=s));
    $mkl->syfail(1)->system;
    require File::Basename;
    require File::Spec;
    File::Spec->VERSION(0.82);
    my($label, @elems) = $mkl->args;
    my %ancestors;
    for my $pname (@elems) {
	my $vobtag = $dsc->desc(['-s'], "vob:$pname")->qx;
	for (my $dad = File::Basename::dirname(File::Spec->rel2abs($pname));
		    length($dad) >= length($vobtag);
			    $dad = File::Basename::dirname($dad)) {
	    $ancestors{$dad}++;
	}
    }
    exit(0) if !%ancestors;
    $mkl->opts(grep !/^-r(ec)?$/, $mkl->opts);
    $mkl->args($label, sort {$b cmp $a} keys %ancestors)->exec;
}

=item * MKBRTYPE,MKLBTYPE

Modification: if user tries to make a type in the current VOB without
explicitly specifying -ordinary or -global, and if said VOB is
associated with an admin VOB, then by default create the type as a
global type in the admin VOB instead. B<I<In effect, this makes -global
the default iff a suitable admin VOB exists>>.

=cut

sub mklbtype {
    return if grep /^-ord|^-glo|vob:/i, @ARGV;
    if (my($ahl) = grep /^->/,
		    ClearCase::Argv->desc([qw(-s -ahl AdminVOB vob:.)])->qx) {
	if (my $avob = (split /\s+/, $ahl)[1]) {
	    # Save aside all possible flags for mkxxtype,
	    # then add the vob selector to each type selector
	    # and add the new -global to opts before exec-ing.
	    my $ntype = ClearCase::Argv->new(@ARGV);
	    $ntype->parse(qw(replace|global|ordinary
			    vpelement|vpbranch|vpversion
			    pbranch|shared
			    gt|ge|lt|le|enum|default|vtype=s
			    cqe|nc c|cfile=s));
	    my @args = $ntype->args;
	    for (@args) {
		next if /\@/;
		$_ = "$_\@$avob";
		warn Msg('W', "making global type $_ ...");
	    }
	    $ntype->args(@args);
	    $ntype->opts('-global', $ntype->opts);
	    $ntype->exec;
	}
    }
}

=item * MKVIEW

Extended in the following ways:

=over 4

=item 1. New B<-me> flag

Supports the B<-me> flag to prepend $LOGNAME to the view name,
separated by an underscore. This enables the convention that all user
views be named B<E<lt>usernameE<gt>_E<lt>whateverE<gt>>.

=item 2. Default view-storage location

Provides a standard default view-storage path which includes the user's
name. Thus a user can simply type B<"mkview -me -tag foo"> and the view
will be created as E<lt>usernameE<gt>_foo with the view storage placed
in a default location determined by the sysadmin.

=item 3. New B<-clone> flag

This allows you to specify another view from which to copy the config
spec and other properties. Note that it does I<not> copy view-private
files from the old view, just the view properties.

=back

=cut

sub mkview {
    # For now at least, take the easy way out on Windows.
    die Msg('E', "please use the GUI to make views on Windows!") if MSWIN;

    # Policy: print warnings if new view's tag does not match the
    # local naming convention, or if the storage location is not
    # one of the approved areas.
    # Extension: if no view-storage area specified, use a standard one.
    my %opt;
    GetOptions(\%opt, qw(clone=s));

    if (!grep(/^-sna/, @ARGV)) {
	my $gstg;

	# Site-specific preference for where we like to locate our views.
	# If there's a local /*/vwstore area which is shared and automountable,
	# put the view there. Otherwise require an explicit choice.
	# This array holds (<global> <local>) storage path pairs.
	my $vhost = 'sparc5';
	my @vwsmap = qw(/data/ccase/vwstore/personal
						/data/ccase/vwstore/personal);
	{
	    local(@ARGV) = @ARGV;	# operate on temp argv
	    my %ignore;
	    GetOptions(\%ignore, qw(ncaexported|shareable_dos|nshareable_dos
		tcomment|tmode|region|ln|host|hpath|gpath|cachesize|stream=s));
	    GetOptions(\%opt, q(tag=s));
	    return if !$opt{tag};
	    if ($opt{tag} && ($#ARGV == 0) && @vwsmap) {
		$gstg = "$vwsmap[0]/$ENV{LOGNAME}/$opt{tag}.vws";
	    }
	}
	if ($gstg) {
	    my $lstg = "$vwsmap[1]/$ENV{LOGNAME}/$opt{tag}.vws";
	    push(@ARGV, '-gpa', $gstg, '-hpa', $lstg, '-host', $vhost, $gstg);
	}

	if ($opt{tag}) {
	    # Policy: view-storage areas should be in a std place.
	    if (@vwsmap) {
		require File::Path;
		my $stgpat = "$ENV{LOGNAME}/$opt{tag}.vws";
		if ($ARGV[-1] =~ m%$stgpat$%) {
		    my($vwbase) = ($ARGV[-1] =~ m%(.+)/[^/]+\.vws$%);
		    File::Path::mkpath($vwbase, 0, 0755) unless -d $vwbase;
		} else {
		    warn Msg("standard view storage path is /vws/.../$stgpat");
		}
	    }
	}
    }

    # Policy: users' views should be prefixed by username.
    die Msg('E', "personal view names must match $ENV{LOGNAME}_*")
		    if defined(@Admins) && !grep(/^$ENV{LOGNAME}$/, @Admins) &&
						$opt{tag} !~ /^$ENV{LOGNAME}_/;

    # If an option was used requiring a special config spec, make the
    # view here, change the cspec, then exit. Must be done this way
    # because mkview provides no way to specify the initial config spec.
    # Also clone other properties such as cache size and text mode.
    if ($opt{clone}) {
	my $ct = ClearCase::Argv->new;
	chomp(my @data = $ct->lsview([qw(-prop -full)], $opt{clone})->qx);
	my %lsview = map {(split /:\s*/)[0,1]} @data;
	splice(@ARGV, 1, 0, '-tmode', $lsview{'Text mode'})
		   if $lsview{'Text mode'};
	my %properties = map {$_ => 1} split(/\s+/, $lsview{Properties});
	for (keys %properties) { splice(@ARGV, 1, 0, "-$_") if /shareable_do/ }
	my($cachebytes) = map {(split /\s+/)[0]} map {(split /:\s*/)[1]}
		    reverse $ct->getcache([qw(-view -s)], $opt{clone})->qx;
	splice(@ARGV, 1, 0, '-cachesize', $cachebytes);
	$ct->autofail(1);
	$ct->argv(@ARGV)->system;
	my $cstmp = ".mkview.$$.cs.$opt{tag}";
	my $ctx = ClearCase::Argv->cleartool;
	Argv->new("$ctx catcs -tag $opt{clone} > $cstmp")->system;
	$ct->setcs('-tag', $opt{tag}, $cstmp)->system;
	unlink($cstmp);
	exit 0;
    }

    # Generally speaking user's views should be -nshareable while
    # standard nightly-build views should be -shareable. Since by
    # convention the former are prefixed with username, we make
    # the shareability default contingent on that while always
    # allowing a literal flag to win.
    if (!grep /^-(?:sna|sha|nsh)/, @ARGV) {
	splice(@ARGV, 1, 0, ($opt{tag} =~ /^$ENV{LOGNAME}_/) ? '-nsh' : '-sha');
    }

    return 0;
}

=item * MOUNT

This is a Windows-only enhancement: on UNIX, I<mount> behaves correctly
and we do not mess with its behavior. On Windows, I<cleartool mount
-all> gives an error for already-mounted VOBs for some reason; these
are now ignored as on UNIX. At the same time, VOB tags containing I</>
are normalized to I<\> so they'll match the registry, and an extension
is made to allow multiple VOB tags to be passed to one I<mount>
command.

=cut

sub mount {
    return 0 if !MSWIN || @ARGV < 2;
    my %opt;
    GetOptions(\%opt, qw(all));
    my $mount = ClearCase::Argv->new(@ARGV);
    $mount->autofail(1);
    $mount->parse(qw(persistent options=s));
    die Msg('E', qq(Extra arguments: "@{[$mount->args]}"))
						if $mount->args && $opt{all};
    my @tags = $mount->args;
    for (@tags) { s%[/\\]%\\\\%g }	# VOB tags are /-vs-\ sensitive
    my $lsvob = ClearCase::Argv->lsvob(@tags);
    # The set of all known public VOBs.
    my @public = grep /\spublic\b/, $lsvob->qx;
    # The subset which are not mounted.
    my @todo = map {(split /\s+/)[1]} grep /^\s/, @public;
    # If no vobs are mounted, let the native mount -all proceed.
    if ($opt{all} && @public == @todo) {
	push(@ARGV, '-all');
	return 0;
    }
    # Otherwise mount what's needed one by one.
    for (@todo) {
	$mount->args($_)->system;
    }
    exit 0;
}

=item * RECO/RECHECKOUT

Redoes a checkout without the database operations by simply copying the
contents of the existing checkout's predecessor over the view-private
checkout file. The previous contents are moved aside to "<element>.reco".

=cut

sub recheckout {
    shift;
    require File::Copy;
    for (@_) {
	if (! -w $_) {
	    Msg('W', "$_: not checked out");
	    next;
	}
	my $pred = Pred($_, 1);
	my $keep = "$_.reco";
	unlink $keep;
	if (rename($_, $keep)) {
	    if (File::Copy::copy($pred, $_)) {
		chmod 0644, $_;
	    } else {
		Msg('E', (-r $_ ? $keep : $_) . ": $!");
	    }
	} else {
	    Msg('E', "cannot rename $_ to $keep: $!");
	}
    }
    exit 0;
}

=item * SETVIEW

ClearCase 4.0 for Windows completely removed I<setview> functionality,
but this wrapper emulates it by attaching the view to a drive letter
and cd-ing to that drive. It supports all the flags I<setview> for
CC 3.2.1/Windows supported (B<-drive>, B<-exec>, etc.) and adds a
new one: B<-persistent>.

If the view is already mapped to a drive letter that drive is used.
If not, the first available drive working backwards from Z: is used.
Without B<-persistent> a drive mapped by setview will be unmapped
when the setview process is existed.

The setview emulation sets I<CLEARCASE_ROOT> for compatibility and adds
a new EV I<CLEARCASE_VIEWDRIVE>.

UNIX setview functionality is left alone.

=cut

sub setview {
    # Clean up whatever EV's we might have used to communicate from
    # parent (pre-setview) to child (in-setview) processes.
    $ENV{CLEARCASE_PROFILE} = $ENV{_CLEARCASE_WRAPPER_PROFILE}
				if defined($ENV{_CLEARCASE_WRAPPER_PROFILE});
    delete $ENV{_CLEARCASE_WRAPPER_PROFILE};
    delete $ENV{_CLEARCASE_PROFILE};
    for (grep /^(CLEARCASE_)?ARGV_/, keys %ENV) { delete $ENV{$_} }

    return 0 if !MSWIN;

    my %opt;
    GetOptions(\%opt, qw(exec=s drive=s login ndrive persistent));
    my $child = $opt{'exec'};
    if ($ENV{SHELL}) {
	$child ||= $ENV{SHELL};
    } else {
	delete $ENV{LOGNAME};
    }
    $child ||= $ENV{ComSpec} || $ENV{COMSPEC} || 'cmd.exe';
    my $vtag = $ARGV[-1];
    my @net_use = grep /\s[A-Z]:\s/i, Argv->new(qw(net use))->qx;
    my $drive = $opt{drive} || (map {/(\w:)/ && uc($1)}
				grep /\s+\\\\view\\$vtag\b/,
				grep !/unavailable/i, @net_use)[0];
    my $mounted = 0;
    my $pers = $opt{persistent} ? '/persistent:yes' : '/persistent:no';
    if (!$drive) {
	ClearCase::Argv->startview($vtag)->autofail(1)->system
						    if ! -d "//view/$vtag";
	$mounted = 1;
	my %taken = map { /\s([A-Z]:)\s/i; $1 => 1 } @net_use;
	for (reverse 'G'..'Z') {
	    next if $_ eq 'X';	# X: is reserved (for CDROM?) on Citrix
	    $drive = $_ . ':';
	    if (!$taken{$drive}) {
		local $| = 1;
		print "Connecting $drive to \\\\view\\$vtag ... "
							    if !$opt{'exec'};
		my $netuse = Argv->new(qw(net use),
					    $drive, "\\\\view\\$vtag", $pers);
		$netuse->stdout(0) if $opt{'exec'};
		last if !$netuse->system;
	    }
	}
    } elsif ($opt{drive}) {
	$drive .= ':' if $drive !~ /:$/;
	$drive = uc($drive);
	if (! -d $drive) {
	    $mounted = 1;
	    local $| = 1;
	    print "Connecting $drive to \\\\view\\$vtag ... ";
	    Argv->new(qw(net use), $drive, "\\\\view\\$vtag", $pers)->system;
	    exit $?>>8 if $?;
	}
    }
    chdir "$drive/" || die Msg('E', "chdir $drive $!");
    $ENV{CLEARCASE_ROOT} = "\\\\view\\$vtag";
    $ENV{CLEARCASE_VIEWDRIVE} = $ENV{VD} = $drive;
    if ($mounted && !$opt{persistent}) {
	my $rc = Argv->new($child)->system;
	my $netuse = Argv->new(qw(net use), $drive, '/delete');
	$netuse->stdout(0) if $opt{'exec'};
	$netuse->system;
	exit $rc;
    } else {
	Argv->new($child)->exec;
    }
}

=item * SETCS

Adds a B<-clone> flag which lets you specify another view from which to copy
the config spec.

Adds a B<-sync> flag. This is similar to B<-current> except that it
analyzes the view dependencies and only flushes the view cache if the
I<compiled_spec> file is out of date with respect to the I<config_spec>
source file or any file it includes. In other words: B<-sync> is to
B<-curr> as C<make foo.o> is to C<cc -c foo.c>.

Adds a B<-expand> flag, which "flattens out" the config spec by
inlining the contents of any include files.

=cut

sub setcs {
    my %opt;
    GetOptions(\%opt, qw(clone=s expand sync));
    die Msg('E', "-expand and -sync are mutually exclusive")
					    if $opt{expand} && $opt{sync};
    my $tag = ViewTag(@ARGV) if $opt{expand} || $opt{sync} || $opt{clone};
    if ($opt{expand}) {
	my $ct = Argv->new([$^X, '-S', $0]);
	my $settmp = ".$prog.setcs.$$";
	open(EXP, ">$settmp") || die Msg('E', "$settmp: $!");
	print EXP $ct->opts(qw(catcs -expand -tag), $tag)->qx;
	close(EXP);
	$ct->opts('setcs', $settmp)->system;
	unlink $settmp;
	exit $?;
    } elsif ($opt{sync}) {
	chomp(my @srcs = qx($^X -S $0 catcs -sources -tag $tag));
	exit 2 if $?;
	(my $obj = $srcs[0]) =~ s/config_spec/.compiled_spec/;
	die Msg('E', "$obj: no such file") if ! -f $obj;
	die Msg('E', "no permission to update $tag's config spec") if ! -w $obj;
	my $otime = (stat $obj)[9];
	for (@srcs) {
	    ClearCase::Argv->setcs(qw(-current -tag), $tag)->exec
						    if (stat $_)[9] > $otime;
	}
	exit 1;
    } elsif ($opt{clone}) {
	my $ct = ClearCase::Argv->new;
	my $ctx = $ct->cleartool;
	my $cstmp = ".$ARGV[0].$$.cs.$tag";
	Argv->autofail(1);
	Argv->new("$ctx catcs -tag $opt{clone} > $cstmp")->system;
	$ct->setcs('-tag', $tag, $cstmp)->system;
	unlink($cstmp);
	exit 0;
    }
}

=item * WORKON

New command, similar to I<setview> but provides hooks to cd to a
preferred I<initial working directory> within the view and to set
up any required environment variables. The I<initial working directory>
is defined as the output of B<ct catcs -start> (see).

If a file called I<.viewenv.pl> exists in the I<initial working
directory>, it's read before starting the user's shell. This file uses
Perl syntax and must end with a "1;" like any C<require-d> file.  Any
unrecognized arguments given to I<workon> following the view name will
be passed on to C<.viewenv.pl> in C<@ARGV>. Environment variables
required for builds within the setview may be set here.

=cut

sub workon {
    shift @ARGV;	# get rid of pseudo-cmd
    my(%opt, @sv_argv);
    # Strip flags intended for 'setview' out of @ARGV, hold them in @sv_argv.
    GetOptions(\%opt, qw(drive=s exec=s login ndrive persistent));
    push(@sv_argv, '-drive', $opt{drive}) if $opt{drive};
    push(@sv_argv, map {"-$_"} grep !/^(drive|exec)/, keys %opt);
    # Now dig the tag out of @ARGV, wherever it might happen to be.
    # Assume it's the last entry in ARGV matching a legal view-tag pattern.
    my $tag;
    for (my $i=$#ARGV; $i >= 0; $i--) {
	if ($ARGV[$i] !~ /^-|^\w+=.+/) {
	    $tag = splice(@ARGV, $i, 1);
	    last;
	}
    }
    die Msg('E', "no tag argument found in '@ARGV'") if !$tag;
    # If anything left in @ARGV has whitespace, quote it against its
    # journey through the "setview -exec" shell.
    for (@ARGV) {
	if (/\s/ && !/^(["']).*\1$/) {
	    $_ = qq('$_');
	}
    }
    # Last, run the setview cmd we've so laboriously constructed.
    unshift(@ARGV, '_inview');
    if ($opt{'exec'}) {
	push(@ARGV, '-_exec', qq("$opt{'exec'}"));
    }
    my $vwcmd = "$^X -S $0 @ARGV";
    # This next line is required because 5.004 and 5.6 do something
    # different with quoting on Windows, no idea exactly why or what.
    $vwcmd = qq("$vwcmd") if MSWIN && $] > 5.005;
    push(@sv_argv, '-exec', $vwcmd, $tag);
    # Prevent \'s from getting lost in subsequent interpolation.
    for (@sv_argv) { s%\\%/%g }
    # Hack - assume presence of $ENV{_} means we came from a UNIX-style
    # shell (e.g. MKS on Windows) so set quoting accordingly.
    my $cmd_exe = (MSWIN && !$ENV{_});
    Argv->new($^X, '-S', $0, 'setview', @sv_argv)->autoquote($cmd_exe)->exec;
}

## undocumented helper function for B<workon>
sub _inview {
    my $tag = (split(m%[/\\]%, $ENV{CLEARCASE_ROOT}))[-1];
    #Argv->new([$^X, '-S', $0, 'setcs'], [qw(-sync -tag), $tag])->system;

    # If -exec foo was passed to workon it'll show up as -_exec foo here.
    my %opt;
    GetOptions(\%opt, qw(_exec=s)) if grep /^-_/, @ARGV;

    my @cs = Argv->new([$^X, '-S', $0, 'catcs'], [qw(--expand -tag), $tag])->qx;
    chomp @cs;
    my($iwd, $venv, @viewenv_argv);
    for (@cs) {
	if (/^##:Start:\s+(\S+)/) {
	    $iwd = $1;
	} elsif (/^##:ViewEnv:\s+(\S+)/) {
	    $venv = $1;
	} elsif (/^##:([A-Z]+=.+)/) {
	    push(@viewenv_argv, $1);
	}
    }

    # If an initial working dir is supplied cd to it, then check for
    # a viewenv file and require it if so.
    if ($iwd) {
	print "+ cd $iwd\n";
	# ensure $PWD is set to $iwd within req'd file
	require Cwd;
	Cwd::chdir($iwd) || warn "$iwd: $!\n";
	my($cli) = grep /^viewenv=/, @ARGV;
	$venv = (split /=/, $cli)[1] if $cli;
	$venv ||= '.viewenv.pl';
	if (-f $venv) {
	    local @ARGV = grep /^\w+=/, @ARGV;
	    push(@ARGV, @viewenv_argv) if @viewenv_argv;
	    print "+ reading $venv ...\n";
	    eval { require $venv };
	    warn Msg('W', $@) if $@;
	}
    }

    # A reasonable default for everybody.
    $ENV{CLEARCASE_MAKE_COMPAT} ||= 'gnu';

    for (grep /^(CLEARCASE_)?ARGV_/, keys %ENV) { delete $ENV{$_} }

    # Exec the default shell or the value of the -_exec flag.
    if (! $opt{_exec}) {
	if (MSWIN) {
	    $opt{_exec} = $ENV{SHELL} || $ENV{ComSpec} || $ENV{COMSPEC}
				|| (-x '/bin/sh.exe' ? '/bin/sh' : 'cmd');
	} else {
	    $opt{_exec} = $ENV{SHELL} || (-x '/bin/sh' ? '/bin/sh' : 'sh');
	}
    }
    Argv->new($opt{_exec})->exec;
}

=item * UNCO

Extended to accept (and ignore) the standard comment flags for
consistency with other cleartool cmds.

Extended to handle the -dir/-rec/-all/-avobs flags.

=cut

sub uncheckout {
    my $unco = ClearCase::Argv->new(@ARGV);
    $unco->parse(qw(keep rm cact cwork));
    $unco->optset('IGNORE');
    $unco->parseIGNORE(qw(c|cfile=s cqe|nc));
    $unco->args(sort {$b cmp $a} AutoCheckedOut($unco->args));
    $unco->exec;
}

=item * WINKIN

The B<-tag> flag allows you specify a local file path plus another view;
the named DO in the named view will be winked into the current view.

The B<-vp> flag, when used with B<-tag>, causes the "remote" file to be
converted into a DO if required before winkin is attempted. See the
B<winkout> extension for details.

=cut

sub winkin {
    my %opt;
    local $Getopt::Long::autoabbrev = 0; # so -rm and -r/ecurse don't collide
    GetOptions(\%opt, qw(rm tag=s vp));
    return 0 if !$opt{tag};
    my $wk = ClearCase::Argv->new(@ARGV);
    $wk->parse(qw(print|noverwrite|siblings|adirs|recurse|ci out|select=s));
    $wk->quote;
    my @files = $wk->args;
    unlink @files if $opt{rm};
    if ($opt{vp}) {
	my @winkout = ($^X, '-S', $0, 'winkout', '-pro');
	ClearCase::Argv->new(qw(setview -exe), "@winkout @files",
					    $opt{tag})->autofail(1)->system;
    }
    my $rc = 0;
    for my $file (@files) {
	if ($wk->flag('recurse') || $wk->flag('out')) {
	    $wk->args;
	} else {
	    $wk->args('-out', $file);
	}
	$rc ||= $wk->args($wk->args, "/view/$opt{tag}$file")->system;
    }
    exit $rc;
}

=item * WINKOUT

The B<winkout> pseudo-cmd takes a set of view-private files as
arguments and, using clearaudit, makes them into derived objects. The
config records generated are meaningless but the mere fact of being a
DO makes a file eligible for forced winkin.

If the B<-promote> flag is given, the view scrubber will be run on the
new DO's. This has the effect of promoting them to the VOB and winking
them back into the current view.

If a meta-DO filename is specified with B<-meta>, this file is created
as a DO and caused to reference all the other new DO's, thus defining a
I<DO set> and allowing the entire set to be winked in using the meta-DO
as a hook. E.g. assuming view-private files X, Y, and Z already exist:

	ct winkout -meta .WINKIN X Y Z

will make them into derived objects and create a 4th DO ".WINKIN"
containing references to the others. A subsequent

	ct winkin -recurse -adirs /view/extended/path/to/.WINKIN

from a different view will wink all four files into the current view.

Accepts B<-dir/-rec/-all/-avobs>, a file containing a list of files
with B<-flist>, or a literal list of view-private files. When using
B<-dir/-rec/-all/-avobs> to derive the file list only the output of
C<lsprivate -other> is considered unless B<-do> is used; B<-do> causes
existing DO's to be re-converted.

The B<"-flist -"> flag can be used to read the file list from stdin.

=cut

sub winkout {
    warn Msg('E', "if you can get this working on &%@# Windows you're a better programmer than me!") if MSWIN;
    my %opt;
    GetOptions(\%opt, qw(directory recurse all avobs flist=s
					do meta=s print promote));
    my $ct = ClearCase::Argv->new({-autochomp=>1, -syfail=>1});

    my $dbg = Argv->dbglevel;

    my $cmd = shift @ARGV;
    my @list;
    if (my @scope = grep /^(dir|rec|all|avo|f)/, keys %opt) {
	die Msg('E', "mutually exclusive flags: @scope") if @scope > 1;
	if ($opt{flist}) {
	    open(LIST, $opt{flist}) || die Msg('E', "$opt{flist}: $!");
	    @list = <LIST>;
	    close(LIST);
	} else {
	    my @type = $opt{'do'} ? qw(-other -do) : qw(-other);
	    @list = Argv->new([$^X, '-S', $0, 'lsp'],
		    ['-s', @type, "-$scope[0]"])->qx;
	}
    } else {
	@list = @ARGV;
    }
    chomp @list;
    my %set = map {$_ => 1} grep {-f}
		    grep {!m%\.(?:mvfs|nfs)\d+|cmake\.state%} @list;
    exit 0 if ! %set;
    if ($opt{'print'}) {
	for (keys %set) {
	    print $_, "\n";
	}
	print $opt{meta}, "\n" if $opt{meta};
	exit 0;
    }
    # Shared DO's should be g+w!
    (my $egid = $)) =~ s%\s.*%%;
    for (keys %set) {
	my($mode, $uid, $gid) = (stat($_))[2,4,5];
	if (!defined($mode)) {
	    warn Msg('W', "no such file: $_");
	    delete $set{$_};
	    next;
	}
	next if $uid != $> || ($mode & 0222) || ($mode & 0220 && $gid == $egid);
	#print STDERR "+ chmod ug+w $_\n" if $dbg;
	chmod(($mode & 07777) | 0220, $_);
    }
    my @dolist = sort keys %set;
    # Add the -meta file to the list of DO's if specified.
    if ($opt{meta}) {
	if ($dbg) {
	    my $num = @dolist;
	    print STDERR "+ associating $num files with $opt{meta} ...\n";
	}
	open(META, ">$opt{meta}") || die Msg('E', "$opt{meta}: $!");
	for (@dolist) { print META $_, "\n" }
	close(META);
	push(@dolist, $opt{meta});
    }
    # Convert regular view-privates into DO's by opening them
    # under clearaudit control.
    {
	my $clearaudit = '/usr/atria/bin/clearaudit';
	local $ENV{CLEARAUDIT_SHELL} = $^X;
	my $ecmd = 'chomp; open(DO, ">>$_") || warn "Error: $_: $!\n"';
	my $cmd = qq($clearaudit -n -e '$ecmd');
	$cmd = "set -x; $cmd" if $dbg;
	open(AUDIT, "| $cmd") || die Msg('E', "$cmd: $!");
	for (@dolist) {
	    print AUDIT  $_, "\n";
	    print STDERR $_, "\n" if $dbg;
	}
	close(AUDIT) || die Msg('E', $! ?
				"Error closing clearaudit pipe: $!" :
				"Exit status @{[$?>>8]} from clearaudit");
    }
    if ($opt{promote}) {
	my $scrubber = '/usr/atria/etc/view_scrubber';
	my $cmd = "$scrubber -p";
	$cmd = "set -x; $cmd" if $dbg;
	open(SCRUBBER, "| $cmd") || die Msg('E', "$scrubber: $!");
	for (@dolist) { print SCRUBBER $_, "\n" }
	close(SCRUBBER) || die Msg('E', $! ?
				"Error closing $scrubber pipe: $!" :
				"Exit status $? from $scrubber");
    }
    exit 0;
}

=back

=head1 GENERAL FEATURES

=over 4

=item * symlink expansion

Before processing a checkin or checkout command, any symbolic links on
the command line are replaced with the file they point to. This allows
allowd developers to operate directly on symlinks for ci/co.

=item * -M flag

As a convenience feature, the B<-M> flag runs all output through your
pager. Of course C<"ct lsh -M foo"> saves only a few keystrokes over
"ct lsh foo | more" but for heavy users of shell history the more
important feature is that it preserves the value of ESC-_ (C<ksh -o
vi>) or !$ (csh).

=item * -me -tag

Introduces a global convenience/standardization feature: the flag
B<-me> in the context of a command which takes a B<-tag view-tag>
causes I<"$LOGNAME"> to be prefixed to the tag name with an
underscore.  This relies on the fact that even though B<-me> is a
native cleartool flag, at least through CC4.0 no command which takes
B<-tag> also takes B<-me> natively. For example:

    % <wrapper-context> mkview -me -tag myview ... 

The commands I<setview, startview, endview, and lsview> also take B<-me>,
such that the following commands are equivalent:

    % <wrapper-context> setview dboyce_myview
    % <wrapper-context> setview -me myview

=back

=head1 CONFIGURATION

Various degrees of configurability are supported:

=over 4

=item * Global Enhancements and Extensions

To add a global override for 'cleartool xxx', simply define a
subroutine 'xxx' after the __END__ token in Wrapper.pm or in
lib/ClearCase/Wrapper/Site.pm and re-run 'make install'.  When doing
so it's a good idea to document it in POD format right above the sub
and make the appropriate addition to the "Usage Message Extensions"
section.  Also, if the command has an abbreviation (e.g.  checkout/co)
you should add that to the "Command Aliases" section.

This override subroutine is called with @ARGV as its parameter list (and
@ARGV is also available directly of course). The sub can do whatever it
likes but it's strongly recommended that I<ClearCase::Argv> be used to
run any cleartool subcommands and its base class I<Argv> be used to run
other programs. These modules provide value for UNIX/Windows
portability and aid in parsing flags into different categories where
required. See their PODs for full documentation.

=item * Personal Preference Setting

As well as allowing for site-wide enhancements to be made in
Wrapper.pm, a hook is also provided for individual users (who must be
knowledgeable about both ClearCase and Perl) to set their own
defaults.  If the file C<~/.clearcase_profile.pl> exists it will be
read before launching any of the sitewide enhancements. Note that this
file is passed to the Perl interpreter and thus has access to the full
array of Perl syntax.

=item * Sitewide ClearCase Comment Defaults

This distribution comes with a file called I<clearcase_profile> which
is installed as part of the module. If the user has no
I<clearcase_profile> file in his/her home directory and if
CLEARCASE_PROFILE isn't already set, CLEARCASE_PROFILE will
automatically be pointed at this supplied file. This allows the
administrator to set sitewide defaults of checkin/checkout comment
handling using the syntax supported by ClearCase natively but without
each user needing to maintain their own config file or set their own
EV.

=item * CLEARCASE_WRAPPER_NATIVE

This environment variable may be set to suppress all extensions,
causing the wrapper to behave just like an alias to cleartool (but
slower).

=back

=head1 DIAGNOSTICS

The flag B<-/dbg=1> prints all "real" cleartool operations executed
by the wrapper to stderr as long as the extension in use was coded
with ClearCase::Argv, which is the case for all supplied extensions.

=head1 INSTALLATION

I recommend you install the I<cleartool.plx> file to some global dir
(e.g. /usr/local/bin), then symlink it to I<ct> or whatever short name
you prefer.  Unfortunately, there's no equivalent mechanism for
wrapping GUI access to clearcase. For Windows the strategy is similar
but requires a "ct.bat" redirector instead of a symlink. See
"ct.bat.sample" in the distribution.

To install or update a global enhancement you must re-run "make
install".  Also, don't forget to check that the contents of
C<lib/ClearCase/Wrapper/clearcase_profile> are what you want users to
have by default.

=head1 COPYRIGHT

Copyright (c) 1997-2001 David Boyce (dsb@boyski.com). All rights
reserved.  This Perl program is free software; you may redistribute it
and/or modify it under the same terms as Perl itself.

=cut

## Please put enhancement code above the CONFIGURATION POD section.
## Alphabetical order is the standard.
