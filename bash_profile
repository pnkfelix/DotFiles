# Note that this file should be linked to from ~/.profile
# not ~/.bash_profile.
# (Though for all I know the latter might work.)

export EDITOR=emacsclient

# Instead, see p4config
export P4CONFIG=~/.p4config

if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi
