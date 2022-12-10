##TOPIC(help,label=Intro) maplev[Intro]
##TITLE MapleV
##HALFLINE An Emacs Package for Maple Developers
##AUTHOR   Joe Riel
##DATE     Dec 2022
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
##  Use either the "MapleCloud" install command, or execute the following.
##
##>(noexecute) PackageTools:-Install("this://",'overwrite'):
##
##ENDSUBSECTION
##SUBSECTION pmaple
##- To install **pmaple**, a binary executable used by Emacs to access Maple,
##  execute the following.
##
##>(noexecute) maplev:-Install:-pmaple();
##
##ENDSUBSECTION
##SUBSECTION Emacs Lisp
##- MapleV uses the Emacs package ~button-lock~,
##  which is available from the Melpa stable distribution.
##  To obtain it, add the following lines to the "Emacs InitFile"
##  and restart Emacs.
##SET(noshow)
##> printf("(require 'package)\n"):
##> printf("(add-to-list 'package-archives '(\"MELPA Stable\" . \"https://stable.melpa.org/packages/\"))\n"):
##UNSET
##- Execute the following command to unpack the tar file
##  that contains the lisp and info files for MapleV.
##>(noexecute) maplev:-Install:-lisp();
##
##
##- To install the lisp and info files,
##  open Emacs, execute the command  ~M-x package-install-file~,
##  and then enter the path to the tar file,
##  shown in the printed output of ~maplev:-Install:-lisp()~, above.
##
##- At this point you should be able to read the info pages
##  for MapleV from inside Emacs by executing ~C-h i~
##  and selecting the **maplev** entry.
##  ~C-h~ means hold down the control key and press ~h~.
##
##- Execute the following command to print
##  elisp code that can be added to your "Emacs InitFile"
##  to configure MapleV.
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
##- "Emacs InitFile" : https://www.emacswiki.org/emacs/InitFile
