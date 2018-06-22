##TOPIC(help,label=Intro) maplev[Intro]
##TITLE MapleV
##HALFLINE An Emacs Package for Maple Developers
##AUTHOR   Joe Riel
##DATE     Feb 2017
##DESCRIPTION
##-(nolead) **MapleV** is an Emacs package for developing source code for Maple.
##  The complete source for MapleV is available at "github",
##  however, building the package from source is not straightforward.
##  This package provides a simpler method to install MapleV.
##
##SECTION Requirements
##- "GNU Emacs" 25+.  Earlier versions may work
##- Maple 2017+.  Earlier versions are supported but may lack some features.
##
##SECTION Installation
##
##-(nolead) A few pieces must be installed:
##-- the Maple library and help files for MapleV;
##-- a binary executable, **pmaple**, that is used by Emacs
##   to communicate with the Maple engine;
##-- the Emacs lisp and info files.
##
##SUBSECTION Maple
##- First, install the Maple-side of this package.
##  Use either the MapleCloud install command, or execute the following command.
##
##>(noexecute) PackageTools:-Install(5079594903273472,'overwrite'):
##
##- To install the **pmaple** binary executable,
##  the backup doc files (the primary documentation is the info file
##  which, if properly installed, is accessible through Emacs)
##  and unpack the tar file of the Emacs package, execute the following command.
##
##>(noexecute) maplev:-Unpack();
##
##ENDSUBSECTION
##SUBSECTION Emacs
##- To install the lisp and info files,
##  open Emacs, execute the command  ~M-x package-install-file~,
##  and then enter the path to the tar file,
##  which is printed by the ~maplev:-Unpack~ command.
##  ~M-x~ means hold down the alt/meta key and press ~x~.
##
##- Execute the following command to print
##  elisp code that can be added to your Emacs
##  initialization file to configure MapleV.
##
##>(noexecute) maplev:-EmacsInitialization();
##
##
##ENDSUBSECTION
##XREFMAP
##- "github" : https://github.com/JoeRiel/maplev
##- "GNU Emacs" : https://www.gnu.org/software/emacs
