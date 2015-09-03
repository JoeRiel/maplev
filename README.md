MapleV:  An Emacs Package for Maple Developers
==============================================

**Version 2.33**

MapleV is an Emacs package for developing Maple code.
Maple is computer algebra system sold by [Maplesoft](http://www.maplesoft.com/).
MapleV provides several major modes:

 - maplev-mode : edit Maple source files
 - maplev-mint : view the output of mint, a Maple syntax checker
 - maplev-help : view Maple help pages
 - maplev-view : view archived Maple modules and procedures
 - maplev-cmaple : interface with a Maple engine

Requirements
------------

* GNU Emacs 23.1+ (earlier versions may work)
* Maple 5+

Installation
------------

Download and unpack the latest [zip file](https://github.com/JoeRiel/maplev/archive/master.zip).

Instructions for installing MapleV are given in its info manual, `maplev.info`,
of which there is an html version, `maplev.html`.

On a Linux machine you might be able to do

    make install

If needed, assign EMACS on the command line; for example

	make install EMACS=/usr/bin/emacs24

Configuration
-------------

Add the following lines to the Emacs initialization file (`~/.emacs`):

	(add-to-list 'load-path (concat user-emacs-directory "maple"))
	(autoload 'maplev "maplev")
	(setq auto-mode-alist (cons (cons (concat "\\." (regexp-opt '("mpl" "tst") t)
	                                          "$")
			                           'maplev-mode)
	                             auto-mode-alist))

Use the Emacs custom group `maplev-group` to access MapleV's many customizations.

Usage
-----

Fire up Emacs.  If you configured `auto-mode-alist`, as above, opening
a Maple source file with extension `.mpl` should cause it to be opened
in `maplev-mode`.  If the `maplev.info` file was installed where it
can be found by Emacs, you can get help by calling
`maplev-got-info-node`.  An html version of the help, `maplev.html`, is
distributed with the source.
