#LINK ../../Makefile
#LINK ../.maplev

##PACKAGE(help) maplev
##TITLE Overview of the maplev Package
###HALFLINE module used with Emacs maplev-mode
##DESCRIPTION
##- The `maplev` package
##  provides the Maple code for "maplev",
##  an "Emacs" major-mode for editing Maple source files.
##
##
##SUBSECTION Exports
##SHOWINDEX(table="maplev[Exports]")
##ENDSUBSECTION
##
##
##SEEALSO
##- "mdc"
##
##XREFMAP
##- "maplev" : https://maple.cloud/app/4677254699810816/maplev?activeGroup=public
##- "Emacs"  : https://www.gnu.org/software/emacs
##- "mdc"    : Help:mdc
##
##ENDMPLDOC

$ifdef MINTONLY
$define MAIN
$endif

unprotect('maplev'):
maplev := module()

option package;

export Emacs, GetSource, Install, Plot, Print, Setup; # , _pexports;

local pmaple_buffer := "unknown";  # pmaple buffer modified by Setup

$include <maple/Install/Install.mpl>  # module used to install maplev
$include <maple/src/Emacs.mm>         # send lisp to Emacs
$include <maple/src/GetSource.mm>     # return source file and line number of a procedure
$include <maple/src/Plot.mm>          # display plots
$include <maple/src/Print.mm>         # used to display maple library code
$include <maple/src/Setup.mm>         # setup the pmaple kernel; called from Emacs
$include <maple/src/pmaple.md>        # help page for pmaple

end module:

protect('maplev'):
#savelib('maplev'):
