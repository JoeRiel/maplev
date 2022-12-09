#LINK ../.maplev
#LINK ../../Makefile

Install := module()

export Copy
    ,  EmacsInitialization
    ,  Unpack
    ,  lisp
    ,  pmaple
    ;

$include <Install/Copy.mm>
$include <Install/lisp.mm>
$include <Install/pmaple.mm>
$include <Install/EmacsInitialization.mm>
$include <Install/Unpack.mm>

end module:
