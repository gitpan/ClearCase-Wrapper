use ExtUtils::MakeMaker;

# Explain a little background at install time.
print q(
    This module installs a wrapper script 'cleartool.plx'.  Typically
    you'll want to access it by a shorthand such as 'ct', 'cl', 'clt',
    'Ct', or even just 'c'. This must be set up by hand and may be
    done on UNIX via a symbolic link, e.g.:

	% cd /usr/local/bin; ln -s cleartool.plx ct

    or on Windows by making a tiny ct.bat file containing:

	@echo off
	perl //UNC/path/to/cleartool.plx %*

);

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile.
WriteMakefile(
    'NAME'		=> 'ClearCase::Wrapper',
    'VERSION_FROM'	=> 'Wrapper.pm', # finds $VERSION
    'EXE_FILES'		=> [ 'cleartool.plx' ],
    'PREREQ_PM' => {
	  File::Spec		=> 0.82,
	  Getopt::Long		=> 2.17,
	  ClearCase::Argv	=> 0.23,
	  Argv			=> 0.53,
    },
    'dist' => {
	  DIST_DEFAULT	=> 'all tardist',
	  COMPRESS	=> 'gzip --force --best',
	  TAR		=> 'gtar',
	  TARFLAGS	=> '--mode=u+w -cvf',
    },
);
