# -*- mode: sh; indent-tabs-mode: nil -*-

# Working around Emacs PATH issues :
# http://hea-www.harvard.edu/~fine/OSX/path_helper.html
if [ -x /usr/libexec/path_helper ]; then
	eval `/usr/libexec/path_helper -s`
fi

## NOTE: The instructions for this actually ask for it to
## be added to .bash_profile, not .bashrc.  I should check
## whether *all* of my PATH manipulations should be going
## into .bash_profile (and if so, why).

# Add GHC 7.8.2 to the PATH, via http://ghcformacosx.github.io/
export GHC_DOT_APP="/Applications/ghc-7.8.2.app"
if [ -d "$GHC_DOT_APP" ]; then
    export PATH="${HOME}/.cabal/bin:${GHC_DOT_APP}/Contents/bin:${PATH}"
fi

# We want the rbenv shims, and we want them at the *front* of the
# PATH, so we need to either:
#
# * Stop using path_helper (...tempting...)
#
# * Hack ~/.rbenv/shims into /etc/paths (...probably bad idea...)
#
# * Manually change the PATH here
#   (it won't be present everywhere, but that's okay, I only need it for
#    shell interactions.)

if [ -d ~/.rbenv/shims  ]; then
   export PATH=~/.rbenv/shims:$PATH
fi

if [ -d ~/.gem/ruby/1.8/bin ]; then
    export PATH=~/.gem/ruby/1.8/bin:$PATH
fi

if [ -d ~/.cabal/bin  ]; then
   export PATH=~/.cabal/bin:$PATH
fi

#if [ -d /usr/local/bin  ]; then
#    export PATH=/usr/local/bin:$PATH
#fi

# Following same reasoning as for rbenv above, I will add ~/bin to the
# path since I want it for *shell* interactions.
if [ -d ~/bin ]; then
    export PATH=~/bin:$PATH
fi

## To placate moz-central/spider-monkey `mach bootstrap`, putting
## homebrew's local bin before the standard bin directories...
# export PATH=/usr/local/bin:$PATH
## ...but then the libtool in /usr/local/bin may be questionable,
## e.g. it might not be compatible with /usr/bin/libtool which
## supports e.g. libtool -static
## So I took the above back out again.  :(

# % brew install hg:
# For non-homebrew python (2.x), you need to amend your PYTHONPATH like so:
export PYTHONPATH=/usr/local/lib/python2.7/site-packages:$PYTHONPATH

export PKG_CONFIG_PATH=$(find "$HOME/opt" -name pkgconfig | xargs | sed -e 's@ @/:@g')

### Inspired by a verbose prompt shown at:

## http://stevelosh.com/blog/2009/05/what-i-hate-about-mercurial/

### I first came up with this:

# PS1='$(basename $(dirname $(pwd)))/$(basename $(pwd)) $(if [ -e .hg ] ; then echo hg:$(hg branch) ; fi )% '

### but then I decided to extend with git, and attempting to extract
### the branch name there led me to this:

## http://www.jonmaddox.com/2008/03/13/show-your-git-branch-name-in-your-prompt/

. ~/ConfigFiles/Bash/search_for_repo.sh
# above exports: search_parents_for_dotgit, parse_git_branch,
#                search_parents_for_dothg, parse_hg_branch
# (at least I hope it does)


function top_two_dirs {
   CWD="$(pwd)"
   DIR=$(dirname "$CWD")
   echo $(basename "$DIR")/$(basename "$CWD")
}

function check_test {
   errstate=1
   gudstate=0
   if [ $errstate -eq 0 ] ; then
      echo "\$errstate -eq 0" is true
   else
      echo "\$errstate -eq 0" is false
   fi
   if [ $gudstate -eq 0 ] ; then
      echo "\$gudstate -eq 0" is true
   else
      echo "\$gudstate -eq 0" is false
   fi
}

function echo_error_status {
  errstate=$?
  if [ $errstate -ne 0 ] ; then
    echo " $errstate"
  fi
  return $errstate
}

function datetime {
    date +%m-%d:%H:%M:%S
}

function justtime {
    date +%H-%M-%S
}

# PS1='$(basename $(dirname $(pwd)))/$(basename $(pwd)) hg:$(parse_hg_branch) git:$(parse_git_branch) % '

# BASE_PS1='$(top_two_dirs)$(parse_hg_branch)$(parse_git_branch) $(datetime)'
BASE_PS1='$(justtime) $(top_two_dirs)$(parse_hg_branch)$(parse_git_branch)'

function prompt_err {
  if test "$?" -eq 0; then PS1="$BASE_PS1 % "; else PS1="$BASE_PS1 [ERROR#$?] % "; fi
}
function simplify_prompt {
    unset PROMPT_COMMAND;
    PS1='% ';
}
function fsk_prompt {
    PROMPT_COMMAND=prompt_err
}

fsk_prompt

export P4CONFIG=.fsk_perforce

if [ -f /usr/local/etc/bash_completion ] ; then
    . /usr/local/etc/bash_completion
fi

# See: http://www.simplicidade.org/notes/archives/2008/02/bash_completion.html
bash=${BASH_VERSION%.*}; bmajor=${bash%.*}; bminor=${bash#*.}
if [ "$PS1" ] && [ $bmajor -eq 2 ] && [ $bminor '>' 04 ] ; then
  if [ -f ~/bin/bash_completion && -f ~/.bash_completion.d  ] ; then
    BASH_COMPLETION=~/bin/bash_completion
    BASH_COMPLETION_DIR=~/.bash_completion.d
    export BASH_COMPLETION BASH_COMPLETION_DIR
    . ~/bin/bash_completion
  fi
fi  
unset bash bmajor bminor

if [ -f ~/.bash_completion.d ] ; then
    . ~/.bash_completion.d/rake.sh
fi

PERL_MB_OPT="--install_base \"/Users/fklock/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/Users/fklock/perl5"; export PERL_MM_OPT;

# Set locale so that Octopress does not get upset about non-ASCII text in my posts.
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# if there are machine-private settings (e.g. experimental
# or secret things I do not want in a public repo), then they
# can go into ~/.bashrc_private
#
# The concrete example that I want to document here: I want to
# map CapsLock to Ctrl when I login at the console, but I do
# not want to do it via /etc/vconsole.conf because I have found
# that too risky (namely when I accidentally corrupt the keymap,
# I want to be able to login as root via a conservative keymap
# after a reboot). So the solution I have adopted is:
#
# 1. Make a script `~/bin/ctrlcaps`:
# ```
# #!/bin/sh
# 
# loadkeys <<EOF
# keymaps 0-2,4-6,8-9,12
# keycode 58 = Control
# EOF
# ```
#
# 2. Give above root privileges `/etc/sudoers.d/11-allow-fsk-ctrlcaps`:
# ```
# pnkfelix ALL = (root) NOPASSWD: /home/pnkfelix/bin/ctrlcaps
# ```
#
# 3. Do sudo'ed invocation on login, `~/.bash_private`:
# ```
# sudo ctrlcaps
# ```

if [ -f ~/.bashrc_private ]; then
    source ~/.bashrc_private
fi
