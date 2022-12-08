#LINK ../.maplev
#LINK ../../Makefile

Installer := module()

export Copy
    ,  EmacsInitialization
    ,  Unpack
    ;

$include <Installer/Copy.mm>
# $include <Installer/CreateInstaller.mm>
$include <Installer/EmacsIonitialization.mm>
$include <Installer/Unpack.mm>

end module:
