<!--*- markdown -*-->
MapleV:  An Emacs Package for Maple Developers
==============================================

**Version 2.37**

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

* [GNU Emacs](https://www.gnu.org/software/emacs/) 23.1+ (earlier versions may work)
* [Maple](https://www.maplesoft.com) 5+

Installation
------------

Download and unpack the latest [zip file](https://github.com/JoeRiel/maplev/archive/master.zip).

For convenience, the maplev-built.zip contains a pre-built Maple
archive file (maplev.mla) and documentation: doc/maplev.info,
doc/maplev.html, and doc/maplev.pdf.  The doc files contain the same
information but in different formats.  The maplev.info is the most
useful as it can be accessed from Emacs if properly installed.  These
files can be built from the source.

Instructions for installing MapleV are given in its info manual, `maplev.info`,
of which there is an html version, `maplev.html`.

On a Linux machine you might be able to do

    make install

If needed, assign EMACS on the command line; for example

	make install EMACS=/usr/bin/emacs24

Configuration
-------------

Add the following lines to the Emacs initialization file, which is
given by the value of user-init-file.

	(add-to-list 'load-path (concat user-emacs-directory "maple"))
	(autoload 'maplev-mode "maplev" "Maple editing mode" 'interactive)
	(add-to-list 'auto-mode-alist '("\\.mpl\\'" . maplev-mode))

Use the Emacs custom group `maplev-group` to access MapleV's many customizations.

Usage
-----

Fire up Emacs.  If you configured `auto-mode-alist`, as above, opening
a Maple source file with extension `.mpl` should cause it to be opened
in `maplev-mode`.  If the `maplev.info` file was installed where it
can be found by Emacs, you can get help by calling
`maplev-got-info-node`.  An html version of the help, `maplev.html`,
is distributed with the source.
