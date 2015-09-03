MapleV:  An Emacs Package for Maple Developers
==============================================

**Version 2.32**

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
Download the latest tarball from [the github download page][download],
then MapleV can be installed with

    make install

This requires `emacs` and `makeinfo` binaries, so please make sure the relevant
packages (generally `emacs` and `texinfo`) are installed on your system.
This will put maplev.el and friends into `~/.emacs.d/maple`.
Then add the following lines to the Emacs initialization file (`~/.emacs`):

	(add-to-list 'load-path (concat user-emacs-directory "maple"))
	(autoload 'maplev "maplev")
	(setq auto-mode-alist (cons (cons (concat "\\." (regexp-opt '("mpl" "tst") t)
	                                          "$")
			                           'maplev-mode)
	                             auto-mode-alist))

Use the Emacs custom group `maplev-group` to access MapleV's many customizations.




