# We need to do two things here:

# 1. Ensure ~/.bash/env gets run first
. ~/.bash/env

# 2. Prevent it from being run later, since we need to use $BASH_ENV for
# non-login non-interactive shells.
# We don't export it, as we may have a non-login non-interactive shell as
# a child.
BASH_ENV=

# 3. Join the spanish inquisition. ;)
# so much for only two things...

# 4. Run ~/.bash/login
. ~/.bash/login

# 5. Run ~/.bash/interactive if this is an interactive shell.
if [ "$PS1" ]; then
    . ~/.bash/interactive
fi

