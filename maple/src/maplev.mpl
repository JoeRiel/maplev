#LINK ../../Makefile
#LINK ../.maplev

##PACKAGE(help) maplev
##HALFLINE module used with Emacs maplev-mode
##DESCRIPTION
##- The `maplev` package
##  provides the Maple code for "maplev",
##  an "Emacs" major-mode for editing Maple source files.
##
##XREFMAP
##- "maplev" : https://maple.cloud/app/4677254699810816/maplev?activeGroup=public
##- "Emacs"  : https://www.gnu.org/software/emacs
##
##ENDMPLDOC

$ifdef MINTONLY
$define MAIN
$endif

unprotect('maplev'):
maplev := module()

option package;

export Emacs, GetSource, Install, Plot, Print, Setup, _pexports;

    _pexports := () -> [':-Plot'];

local pmaple_buffer := "unknown";  # assigned pmaple buffer by Setup

$include <Install/Install.mpl>  # module used to install maplev
$include <src/Emacs.mm>         # send lisp to Emacs
$include <src/GetSource.mm>     # return source file and line number of a procedure
$include <src/Plot.mm>          # display plots
$include <src/Print.mm>         # used to display maple library code
$include <src/Setup.mm>         # setup the pmaple kernel; called from Emacs

end module:

protect('maplev'):
#savelib('maplev'):
