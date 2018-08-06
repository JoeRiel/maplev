#LINK maplev.mpl

##PROCEDURE maplev[Setup]
##HALFLINE Setup maple for communicating with the Emacs  ~maplev-mode~.
##CALLINGSEQUENCE
##- maplev:-Setup()
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
