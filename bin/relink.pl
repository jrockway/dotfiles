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

my @files =
  map  { $dotdir->file($_) }
  grep { !/^[.]/ && !/^bin$/ }
  do   {
      opendir my $dh, $dotdir or confess "Failed to open $dotdir: $!";
      readdir $dh;
  };

relink:
for my $original (@files){
    no warnings 'exiting';
    my $link = $home->file('.'. $original->basename);
    #say "looking at $link -> $original";
    given($link){
        when( -l $_ ){
            my $dest = file(readlink $link);
            if($dest->absolute ne $original->absolute){
                say {*STDERR} "warning: destination link '$link' exists, ".
                  "but points to the wrong place.  (should point to ".
                    "$original, points to $dest).";
            }
            next relink;
        }
        when( -e $_ ){
            say {*STDERR} "warning: destination link '$link' exists as a regular file!";
            next relink;
        }
    }
    say "linking $original to $link";
    symlink $original, $link or say {*STDERR} "Error linking $original to $link: $!";
}
