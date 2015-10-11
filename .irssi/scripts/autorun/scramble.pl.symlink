#!/usr/bin/perl

use strict;
use warnings;
use vars qw($VERSION %IRSSI);

$VERSION = "0.1";
%IRSSI = (
    authors     => 'Michal Rus',
    contact     => 'm@michalrus.com',
    name        => 'scramble',
    description => 'Scrambles text.',
    license     => 'Apache2',
);

use Irssi;
use Irssi::Irc;
use Encode;
use List::Util qw(shuffle);

sub randomize_character_order {
  return join('', shuffle(split '', shift));
}

sub scramble_word {
  my $word = shift;
  $word =~ s/^(.)(.*)(.)$/$1 . randomize_character_order($2) . $3/e;
  return $word;
}

sub scramble {
  my $text = shift;
  Encode::_utf8_on($text);
  $text =~ s/([\p{L}]+)/scramble_word($1)/ge;
  return $text;
}

sub ssay {
  my ($text, $server, $dest) = @_;
  return unless $dest;
  $dest->command("/say " . scramble($text));
}

sub sme {
  my ($text, $server, $dest) = @_;
  return unless $dest;
  $dest->command("/me " . scramble($text));
}

sub stopic {
  my ($text, $server, $dest) = @_;
  return unless $dest;
  $dest->command("/topic " . scramble($text));
}

sub skick {
  my ($text, $server, $dest) = @_;
  return unless $dest;
  my ($nick, $reason) = split(/ +/, $text, 2);
  return unless $nick;
  $dest->command("/kick " . $nick . " " . scramble($reason));
}

sub sknockout {
  my ($text, $server, $dest) = @_;
  return unless $dest;
  my ($time, $nick, $reason) = split(/ +/, $text, 3);
  ($time, $nick, $reason) = (300, $time, $nick . " " . $reason) if ($time !~ m/^\d+$/);
  return unless $nick;
  $dest->command("/knockout " . $time . " " . $nick . " " . scramble($reason));
}

Irssi::command_bind("ssay", "ssay");
Irssi::command_bind("stopic", "stopic");
Irssi::command_bind("sme", "sme");
Irssi::command_bind("skick", "skick");
Irssi::command_bind("sknockout", "sknockout");
