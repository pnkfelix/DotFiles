# Note that this file should be linked to from ~/.profile
# not ~/.bash_profile.
# (Though for all I know the latter might work.)

export EDITOR=emacsclient

# Instead, see p4config
export P4CONFIG=~/.p4config

if type -P brew ; then
    if [ -f $(brew --prefix)/etc/bash_completion ]; then
        . $(brew --prefix)/etc/bash_completion
    fi
fi

if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi
