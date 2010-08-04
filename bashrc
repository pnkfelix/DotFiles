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

function parse_git_branch {
  if git branch 2>/dev/null 1> /dev/null ; then
    echo \ \(git:$(git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/')\)
  fi
}

function parse_hg_branch {
  if hg branch 2> /dev/null 1>/dev/null ; then
    echo \ \(hg:$(hg branch)\)
  fi
}

# PS1='$(basename $(dirname $(pwd)))/$(basename $(pwd)) hg:$(parse_hg_branch) git:$(parse_git_branch) % '

PS1='$(basename $(dirname $(pwd)))/$(basename $(pwd))$(parse_hg_branch)$(parse_git_branch)% '
