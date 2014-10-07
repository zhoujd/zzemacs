#!/usr/bin/env perl

use warnings;
use strict;
use Getopt::Std;
use Cwd;
use FindBin;

$| = 1;

print "git diff setup start ...\n";

my $setup_home=getcwd;
my $contents = <<EOF;
#
# This is the config file, and
# a '#' or ';' character indicates
# a comment
#

[user]
	name = zhoujd
	email = zjd-405\@163.com
[color]
	ui = true
[core]
	autocrlf = false
	safecrlf = true
	filemode = false
[alias]
	st = status
	ci = commit
	br = branch
	co = checkout
	df = difftool
	dc = difftool --cached
	nlog = log -n
	llog = log -n 10
	lg = log -p
	lol = log --graph --decorate --pretty=oneline --abbrev-commit
	lola = log --graph --decorate --pretty=oneline --abbrev-commit --all
	ls = ls-files
	ign = ls-files -o -i --exclude-standard
	glog = log --graph --pretty=format:'%Cred%h%Creset %C(cyan)%an%Creset -%C(blue)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative
[http]
	postBuffer = 524288000
[diff]
	tool = extdiff
[difftool "extdiff"]
	cmd = perl $setup_home/git-diff-wrapper.sh \"\$LOCAL\" \"\$REMOTE\"
[difftool]
	prompt = false
[merge]
	tool = extmerge
[mergetool "extmerge"]
	cmd = perl $setup_home/git-merge-wrapper.sh \"\$BASE\" \"\$LOCAL\" \"\$REMOTE\" \"\$MERGED\"
	trustExitCode = true
[mergetool]
	keepBackup = false
EOF

print "$contents\n";
print "$^O\n";

print "git diff setup end ...\n";
