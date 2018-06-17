#LINK ../../Makefile
#LINK ../.maplev

##MODULE(help) maplev
##HALFLINE module used with Emacs maplev-mode
##DESCRIPTION
##- The `maplev` package
##  provides the Maple code for MapleV,
##  an Emacs major-mode for editing Maple source files.
##
##- The Maple code is called by Emacs
##  to display Maple help pages and procedures.

unprotect('maplev'):
maplev := module()

export Copy, GetSource, Print, Setup, Unpack;

$include <Copy.mm>
$include <GetSource.mm>
$include <Print.mm>
$include <Setup.mm>>
$include <Unpack.mm>

##
end module:

protect('maplev'):
#savelib('maplev'):
