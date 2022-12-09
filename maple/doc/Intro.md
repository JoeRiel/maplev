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
##- "GNU Emacs" 27+.  Earlier versions may work
##- Maple 2022+.  Earlier versions are supported but may lack some features.
##
##SECTION Installation
##
##-(nolead) A few pieces must be installed and configured.
##-- the Maple library and help files for MapleV;
##-- **pmaple**, a binary executable;
##-- the Emacs lisp and info files.
##
##SUBSECTION Maple
##- Install the Maple-side of this package.
##  Use either the "MapleCloud" install command, or execute the following command.
##
##>(noexecute) PackageTools:-Install("this://",'overwrite'):
##
##ENDSUBSECTION
##SUBSECTION pmaple
##- To install **pmaple**, a binary executable
##  used by Emacs to provide access to Maple help pages and to run Maple commands,
##  execute the following command:
##
##>(noexecute) maplev:-Install:-pmaple();
##
##SUBSUBSECTION Linux and Mac
##- Assign the operating system environment variable
##  ~MAPLE~ to the directory in which Maple is installed; it is the value
##  returned by
##
##>(noexecute) kernelopts('mapledir');
##
##- Assign the environment variable ~LD_LIBRARY_PATH~ to include the
##  path to the Maple binaries; it is the value returned by
##>(noexecute) kernelopts('bindir');
##ENDSUBSUBSECTION
##ENDSUBSECTION
##SUBSECTION Emacs Lisp
##- Execute the following command to unpack the tar file
##  that contains the lisp and info files for MapleV.
##
##>(noexecute) maplev:-Install:-lisp();
##
##- To install the lisp and info files,
##  open Emacs, execute the command  ~M-x package-install-file~,
##  and then enter the path to the tar file,
##  which is printed by the previous command.
##  ~M-x~ means hold down the alt/meta key and press ~x~.
##
##- At this point you should be able to read the info pages
##  for MapleV from inside Emacs by executing ~C-h i~
##  and selecting the **maplev** entry.
##  ~C-h~ means hold down the control key and press ~h~.
##
##- Execute the following command to print
##  elisp code that can be added to your Emacs
##  initialization file to configure MapleV.
##
##>(noexecute) maplev:-Install:-EmacsInitialization();
##
##
##ENDSUBSECTION
##SUBSECTION(collapsed) Maintainance
##- This section is for the package maintainer's usage.
##>(noexecute) PackageTools:-GetProperty("this://","X-CloudId");
##>(noexecute) PackageTools:-GetProperty("this://","X-CloudGroup");
##
##ENDSUBSECTION
##XREFMAP
##- "github" : https://github.com/JoeRiel/maplev
##- "GNU Emacs" : https://www.gnu.org/software/emacs
##- "MapleCloud" : help:worksheet/cloud/login
