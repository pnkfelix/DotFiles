# Note that this file should be linked to from ~/.profile
# not ~/.bash_profile.
# (Though for all I know the latter might work.)

export EDITOR=emacsclient

# Instead, see p4config
export P4CONFIG=~/.p4config

if type -P brew > /dev/null ; then
   BASH_COMPLETION=$(brew --prefix)/etc/bash_completion
else
   BASH_COMPLETION=/etc/bash_completion
fi
if [ -f $BASH_COMPLETION ]; then
   . $BASH_COMPLETION
fi

stty -ixon -ixoff

# Adds rustup-installed binaries to PATH
if [ -f ~/.cargo/env ]; then
   source ~/.cargo/env
fi

if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi

# OPAM configuration
. /Users/fklock/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

export PATH="$HOME/.cargo/bin:$PATH"
