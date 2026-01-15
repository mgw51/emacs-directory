# Emacs Config

### Unix/Linux

Clone this repository into your home directory like so:

    git clone https://github.com/mgw51/emacs-directory .emacs.d

`init.el` is located in `.emacs.d` directory.  `.emacs` files take
precedence over 'init.el' files at startup.  Be sure your home directory
contains no '.emacs' file by either renaming or deleting it if one exists.

Beginning with version 27, Emacs respects the value of your `XDG_CONFIG_HOME` 
environment variable.  For Emacs 27 and above, place your config and related
files at that location.  For example:

    git clone https://github.com/mgw51/emacs-directory $XDG_CONFIG_HOME/emacs

### Things To Include

Include your `init.el` file, also other config files if you have broken your
configuration code into multiple files.  Include anything you have written
yourself, such as `.el` files (should be placed in the `lisp` directory).
Basically, any config files should be included in your repository and so
should any custom code--including themes, modes, and your own elisp files.

### Things Not To Include

Do not inlcude any package repository directories such as Melpa, Elpa, etc.
These do not need to be managed by git.  Instead, create a function that can
be run from `init.el` which will ensure all required packages are installed
on the local system when emacs is started, or use the excellent package
`use-package` to handle automatic installation of necessary packages for
you.
