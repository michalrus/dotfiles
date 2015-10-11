#
# Loosely based on https://github.com/cjdev/irssi-email-notifier .
#

use strict;
use warnings;

use vars qw($VERSION %IRSSI);

use Irssi;
$VERSION = '0.0.1';
%IRSSI = (
	authors     => 'Michał Rus',
	contact     => 'm@michalrus.com',
	name        => 'libnotify-client',
	description => 'Runs all hilights through libnotify-client.',
	license     => 'Apache License, Version 2.0'
);

sub notify {
	my ($title, $summary) = @_;
	my @args = ('libnotify-client', '--notify', 'irssi', $title, $summary);
	system @args;
}

sub priv_msg {
	my ($server, $msg, $nick, $address) = @_;
	notify($server->{chatnet}, '<' . $nick . '> ' . $msg);
}

sub hilight {
	my ($dest, $text, $stripped) = @_;
	if ($dest->{level} & MSGLEVEL_HILIGHT) {
		notify($dest->{server}->{chatnet} . '/' . $dest->{target}, $stripped);
	}
}

Irssi::signal_add_last("message private", "priv_msg");
Irssi::signal_add_last("print text", "hilight");
