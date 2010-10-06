# Working around Emacs PATH issues :
# http://hea-www.harvard.edu/~fine/OSX/path_helper.html
if [ -x /usr/libexec/path_helper ]; then
	eval `/usr/libexec/path_helper -s`
fi


### Inspired by a verbose prompt shown at:

## http://stevelosh.com/blog/2009/05/what-i-hate-about-mercurial/

### I first came up with this:

# PS1='$(basename $(dirname $(pwd)))/$(basename $(pwd)) $(if [ -e .hg ] ; then echo hg:$(hg branch) ; fi )% '

### but then I decided to extend with git, and attempting to extract
### the branch name there led me to this:

## http://www.jonmaddox.com/2008/03/13/show-your-git-branch-name-in-your-prompt/

function search_parents_for_dotgit {
  curpath=$(pwd)
  found_dotgit=0
  while [ "$curpath" != "/" ] ; do
    if [ -e "$curpath/.git" ] ; then
      found_dotgit=1
      export last_git_path="$curpath/.git"
      break
    fi
    curpath=$(dirname "$curpath")
  done
  return $found_dotgit
}

function parse_git_branch {
  if ! search_parents_for_dotgit ; then
    output=$(git branch --no-color 2>/dev/null)
    errstate=$?
    if [ $errstate -eq 0 ] ; then
      output2=$(echo "$output" | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/')
      echo \ \(git:$output2\)
    fi
  fi
}

function parse_hg_branch_orig {
  output=$(hg branch 2> /dev/null)
  errstate=$?
  if [ $errstate -eq 0 ] ; then
     echo \ \(hg:$output\)
  fi
}

function search_parents_for_dothg {
  curpath=$(pwd)
  found_dothg=0
  while [ "$curpath" != "/" ] ; do
    if [ -e "$curpath/.hg" ] ; then
      found_dothg=1
      export last_hg_path="$curpath/.hg"
      break
    fi
    curpath=$(dirname "$curpath")
  done
  return $found_dothg
}

function parse_hg_branch_prepass {
  if ! search_parents_for_dothg ; then
     parse_hg_branch_orig
  fi
}

# avoids invoking python
function parse_hg_branch {
  if ! search_parents_for_dothg ; then
    if [ -e "$last_hg_path/branch" ] && [ -s "$last_hg_path/patches/status" ] ; then
      echo \ \(hg:$(cat "$last_hg_path/branch")\,$(tail -1 "$last_hg_path/patches/status" | sed -e 's/.*://')\)
    else if [ -e "$last_hg_path/branch" ] ; then
      echo \ \(hg:$(cat "$last_hg_path/branch")\)
    fi
    fi
  fi
}

function top_two_dirs {
   CWD=$(pwd)
   echo $(basename $(dirname $CWD))/$(basename $CWD)
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

# PS1='$(basename $(dirname $(pwd)))/$(basename $(pwd)) hg:$(parse_hg_branch) git:$(parse_git_branch) % '

# BASE_PS1='$(top_two_dirs)$(parse_hg_branch)$(parse_git_branch) $(datetime)'
BASE_PS1='$(top_two_dirs)$(parse_hg_branch)$(parse_git_branch)'

function prompt_err {
  if test "$?" -eq 0; then PS1="$BASE_PS1 % "; else PS1="$BASE_PS1 [ERROR#$?] % "; fi
}
PROMPT_COMMAND=prompt_err

export P4CONFIG=.fsk_perforce
