This directory is a place to stash "dotfiles" [1] -- configuration
files that one usually finds in a user's home directory.

The reason to have a directory like this is that I can put files under
version control here without cluttering up my home directory.  Maybe
that is a mistake, but I don't think so, since a lot of the modern
version control systems (like Mercurial aka Hg and Git) assume that
they are managing every subdirectory beneath where they are located,
which is pretty much exactly the wrong behavior for this use-case
unless the dotfiles are moved to a separate subdirectory -- like this
one.

[1] I call these configuration files "dotfiles" because they (usually)
    start with a period as their first character, which tells the UNIX
    ls command and other utilities that they are not meant to show up
    on directory listings unless explicitly requested by the user.
