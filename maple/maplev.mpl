#LINK ../Makefile
##MODULE maplev
##HALFLINE module used with Emacs maplev mode

maplev := module()

export GetSource, Print, Setup;

$include <Print.mm>
$include <GetSource.mm>

##PROCEDURE maplev[Setup]
##DESCRIPTION
##- The `Setup` command calls "kernelopts" and "interface"
##  to assign the appropriate settings for interfacing with
##  the Emacs **maplev-mode**.

    Setup := proc()
        kernelopts(printbytes = false);
        interface(prettyprint    = 1
                  , verboseproc  = 2
                  , errorbreak   = 0
                  , warnlevel    = 2
                  , errorcursor  = false
                  , screenheight = infinity
                 );
        NULL;
    end proc;
##
end module:

LibraryTools:-Save('maplev', "maplev.mla");
