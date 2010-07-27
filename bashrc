# Working around Emacs PATH issues :
# http://hea-www.harvard.edu/~fine/OSX/path_helper.html
if [ -x /usr/libexec/path_helper ]; then
	eval `/usr/libexec/path_helper -s`
fi
