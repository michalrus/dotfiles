# dotfiles

## How to install

```
$ git clone https://github.com/michalrus/dotfiles.git ~/.dotfiles
$ ~/.dotfiles/setup
```

## How to update

```
$ dotfiles
```

## About

1. Files/directories ending with `.symlink` will be symbolically linked at the corresponding place in your `~`.
1. All needed parent directories will be created (if non-existent).
1. If a symlink would overwrite another file in a particular location, this file will be renamed by adding a suffix of `.dotfiles-bak.$date-$time-$nanosecs`.
1. Machine-local changes are reflected in `~/.dotfiles/setup.excluded` and `~/.dotfiles/setup.local`, see their `.example` files.
1. OpenSSH config resides in `~/.ssh/{authorized_keys,config}.d/` directories. Config files (OpenSSH knows no `include`) are regenrated at each run of `ssh` (`~/.bin/ssh`, really).

## License

```
Copyright 2015 Michal Rus <https://michalrus.com/>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```
