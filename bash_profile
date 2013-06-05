# Note that this file should be linked to from ~/.profile
# not ~/.bash_profile.
# (Though for all I know the latter might work.)

export EDITOR=emacsclient

# Instead, see p4config
export P4CONFIG=~/.p4config

if type -P brew ; then
   BASH_COMPLETION=$(brew --prefix)/etc/bash_completion
else
   BASH_COMPLETION=/etc/bash_completion
fi
if [ -f $BASH_COMPLETION ]; then
   . $BASH_COMPLETION
fi

stty -ixon -ixoff

if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi
