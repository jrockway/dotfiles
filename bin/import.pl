#!/usr/bin/env perl

use strict;
use warnings;
use feature ':5.10';

use Path::Class;
use Carp qw(confess);

my $home = dir($ENV{HOME});
confess "NO HOMEDIR!?" unless -d $home;

my $dotdir = $home->subdir(".dotfiles");
confess "$dotdir does not exist" unless -e -d $dotdir;
confess "$dotdir is not a git repository" unless -d $dotdir->subdir('.git');

my $dotfile = shift @ARGV || confess 'Need a dotfile to import';
$dotfile = $home->file($dotfile);
confess "$dotfile does not exist" unless -e $dotfile;
confess "refusing to import a symlink" if -l $dotfile;
confess "$dotfile does not start with a dot" unless $dotfile->basename =~ /^[.]/;

my ($name) = ($dotfile->basename =~ m{[.](.+)$});
my $dest = $dotdir->file($name);

confess "destination $dest already exists!" if -e $dest;

rename $dotfile, $dest or confess "Failed to rename $dotfile to $dest: $!";

chdir $dotdir or confess "Failed to chdir to $dotdir: $!";
# if git fails, its own messages should make that clear.  we don't
# lose any data in that case.
system(qw/git add/, $dest);
system(qw/git commit -m/, 'adding '. $dotfile->basename);

symlink $dest, $dotfile or warn "WARNING: Failed to link $dest to $dotfile: $!";
