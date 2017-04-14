##TOPIC(help,label=Intro) maplev[Intro]
##TITLE MapleV
##HALFLINE An Emacs Major Mode for Maple Source Files
##AUTHOR   Joe Riel
##DATE     Feb 2017
##DESCRIPTION
##-(nolead) "MapleV" is an Emacs major mode for developing source code for Maple.
##  The complete source for MapleV is available at "github",
##  however, building the package from source is not straightforward.
##  This package provides an alternative way to install MapleV.
##
##SECTION Requirements
##- "GNU Emacs" 24+.  Earlier versions may work
##- Maple 2017+.  Earlier versions are supported.
##SECTION Installation
##- First, install the Maple-side of this package.
##  Use either the MapleCloud install command, or execute the following command.
##>(noexecute) PackageTools:-Install("this://",'overwrite'):
##- Next, execute the following command to install the Emacs files.
##  See "maplev[Install]" for options and details about this command.
##>(noexecute) maplev:-Install();
##
##XREFMAP
##- "github" : https://github.com/JoeRiel/maplev
##- "GNU Emacs" : https://www.gnu.org/software/emacs
