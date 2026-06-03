##INCLUDE ../include/version.mpi
##TOPIC(help,label="Intro") maplev[Intro]
##TITLE MapleV
##HALFLINE An Emacs Package for Maple Developers (version \PKG_VERSION)
##AUTHOR   Joe Riel
##DATE     Nov 2024
##DESCRIPTION
##-(nolead) **MapleV** is an Emacs package for developing source code for Maple.
###  The complete source for MapleV is available at "github",
###  however, building the package from source is not straightforward.
###  This package provides a simpler method to install MapleV.
##
##SECTION Requirements
##- "GNU Emacs" 27+.  Earlier versions may work
##- "Maple" 2022+.  Earlier versions are supported but may lack some features.
##
##SECTION Installation
##SET(noexecute)
##
##SUBSECTION Maple
##- Install the Maple library and help files
##  by executing the following command:
##
##> maplev:-Install('binary');
##
##- Install the doc directory, which contains a pdf and html of the documentation for MapleV.
##  This may be skipped; the documentation for MapleV should be available in the Emacs info
##  reader after the steps in the Emacs section, below, are completed.
##
##> maplev:-Install('doc'):
##
##- Install the Maple source files for MapleV.
##  This may be skipped as the source files
##  are not needed for using the tool.
##
##> maplev:-Install('maple'):
##
##> maplev:-Install('rebuild'):
##
##ENDSUBSECTION
##SUBSECTION Emacs
##- Unpack the tar file that contains the lisp and info files for MapleV
##  by executing the following command:
##
##> maplev:-Install('emacs'):
##
##- To install the lisp and info files,
##  launch Emacs, and in it execute the command ~M-x package-install-file~,
##  then enter the path to the tar file,
##  shown in the printed output of ~maplev:-Install('emacs')~, above.
##
##- At this point you should be able to read the info pages
##  for MapleV from inside Emacs by executing ~C-h i~
##  and selecting the MapleV entry.
##  ~C-h~ means hold down the control key and press ~h~.
##
##- Execute the following command to print
##  elisp code that can be added to your "Emacs InitFile"
##  to configure MapleV.
##
##> maplev:-Install('emacs_init'):
##
##CODEEDITREGION(name="emacs_init",display="code",autofit="false")
##ENDCODEEDITREGION
##
##ENDSUBSECTION
##
##SUBSECTION(collapsed) Maintainance
##- This section is for the package maintainer's usage.
##> PackageTools:-GetProperty("this://","X-CloudId");
##> PackageTools:-GetProperty("this://","X-CloudGroup");
##
##ENDSUBSECTION
##XREFMAP
##- "Emacs InitFile" : https://www.emacswiki.org/emacs/InitFile
##- "github"         : https://github.com/JoeRiel/maplev
##- "GNU Emacs"      : https://www.gnu.org/software/emacs
##- "Maple"          : https://maplesoft.com/products/Maple
##
##ENDMPLDOC
