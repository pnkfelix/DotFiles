# -*- mode: sh; indent-tabs-mode: nil -*-

# Working around Emacs PATH issues :
# http://hea-www.harvard.edu/~fine/OSX/path_helper.html
if [ -x /usr/libexec/path_helper ]; then
	eval `/usr/libexec/path_helper -s`
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

# Following same reasoning as for rbenv above, I will add ~/bin to the
# path since I want it for *shell* interactions.
if [ -d ~/bin ]; then
    export PATH=~/bin:$PATH
fi

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
