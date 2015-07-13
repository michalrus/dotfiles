#
# Copyright 2013 Michał Rus <https://michalrus.com/>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

use strict;
use warnings;

use Irssi;
use Irssi::Irc;
use vars qw($VERSION %IRSSI);

$VERSION = "0.01";
%IRSSI = (
  authors     => 'Michał Rus',
  contact     => 'https://michalrus.com/',
  name        => 'stupid-nicks',
  description => 'Ignore stupid changes in nicks (away, off, afk, symbols)',
  license     => 'Apache License, Version 2.0',
  url         => 'https://github.com/michalrus/irssi-stupid-nicks/',
  changed     => '2013-12-14'
);

sub event_nick {
  my ($server, $newnick, $oldnick, $address) = @_;

  $newnick = substr($newnick, 1) if ($newnick =~ /^:/);
  $oldnick = substr($oldnick, 1) if ($oldnick =~ /^:/);

  my $ign = 0;

  $ign = 1 if (!$ign && $newnick =~ m/[^a-z](away|gone|afk|off)$/i);
  $ign = 1 if (!$ign && $oldnick =~ m/[^a-z](away|gone|afk|off)$/i);

  $ign = 1 if (!$ign && $newnick =~ m/^(zz+)[^a-z]/i);
  $ign = 1 if (!$ign && $oldnick =~ m/^(zz+)[^a-z]/i);

  $ign = 1 if (!$ign && $newnick =~ m/^Guest\d\d\d+$/);
  $ign = 1 if (!$ign && $oldnick =~ m/^Guest\d\d\d+$/);

  if (!$ign) {
    my $o = $oldnick; my $n = $newnick;
    $o =~ s/[^a-z0-9]//ig; $n =~ s/[^a-z0-9]//ig; # strip special chars
    $o = lc $o; $n = lc $n;                       # make case the same
    $o =~ s/[0-9]+$//; $n =~ s/[0-9]+$//;         # strip number at ends
    $ign = 1 if ($o eq $n);
  }

  if ($ign) {
    # Irssi::print('stupid-nicks: ignoring "' . $oldnick . '" -> "' . $newnick . '"');
    Irssi::signal_stop();
  }
}

Irssi::signal_add('event nick', 'event_nick');
