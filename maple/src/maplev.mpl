#LINK ../Makefile
#LINK .maplev

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

export GetSource, Install, Print, Setup;

$include <GetSource.mm>
$include <Install.mm>
$include <Print.mm>

##PROCEDURE maplev[Setup]
##DESCRIPTION
##- The `Setup` command calls "kernelopts" and "interface"
##  to assign the appropriate settings for interfacing with
##  the Emacs **maplev-mode**.

    Setup := proc()
        kernelopts('printbytes' = false);
        interface('prettyprint'    = 1
                  , 'verboseproc'  = 2
                  , 'errorbreak'   = 0
                  , 'warnlevel'    = 2
                  , 'errorcursor'  = false
                  , 'screenheight' = infinity
                 );
        NULL;
    end proc;
##
end module:

protect('maplev'):
#savelib('maplev'):
