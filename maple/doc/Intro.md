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
##- "Maple" 2022+.  Earlier versions are supported but may lack some features.
##
##SECTION Installation
##
##-(nolead) A few pieces must be installed and configured.
##-- the Maple library and help files for MapleV;
##-- **pmaple**, a binary executable;
##-- the Emacs lisp and info files.
##
##SUBSECTION Maple
##- Install the Maple library and help files
##  by executing the following command:
##
##>(noexecute) PackageTools:-Install("this://",'overwrite'):
##
##ENDSUBSECTION
##SUBSECTION pmaple
##- Install **pmaple**, a binary executable used by Emacs to access Maple,
##  by executing the following command:
##
##>(noexecute) maplev:-Install('pmaple');
##
##ENDSUBSECTION
##SUBSECTION Emacs
##- Unpack the tar file that contains the lisp and info files for MapleV
##  by executing the following command:
##
##>(noexecute) maplev:-Install('emacs');
##
##- To install the lisp and info files,
##  launch Emacs, execute the command  ~M-x package-install-file~,
##  and then enter the path to the tar file,
##  shown in the printed output of ~maplev:-Install('emacs')~, above.
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
##>(noexecute) maplev:-Install('print_emacs');
##
##ENDSUBSECTION
##SUBSECTION(collapsed) Maintainance
##- This section is for the package maintainer's usage.
##>(noexecute) PackageTools:-GetProperty("this://","X-CloudId");
##>(noexecute) PackageTools:-GetProperty("this://","X-CloudGroup");
##
##ENDSUBSECTION
##XREFMAP
##- "Emacs InitFile" : https://www.emacswiki.org/emacs/InitFile
##- "github"         : https://github.com/JoeRiel/maplev
##- "GNU Emacs"      : https://www.gnu.org/software/emacs
##- "Maple"          : https://maplesoft.com/products/Maple
##
##ENDMPLDOC
