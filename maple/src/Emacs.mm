#LINK maplev.mpl

##INCLUDE ../include/mpldoc_macros.mpi
##PROCEDURE(help,label="Emacs") maplev:-Emacs
##HALFLINE send a string of lisp code to Emacs
##INDEXPAGE maplev[Exports],Emacs,send a lisp string to Emacs
##CALLINGSEQUENCE
##- maplev:-Emacs('lisp')
##PARAMETERS
##- 'lisp' : ::string::
##RETURNS
##- 'NULL'
##DESCRIPTION
##- The `Emacs` command
##  sends a string of lisp code to Emacs.
##
##- To successfully run this command, there must be an Emacs server running.
##  One way to do this is with the elisp code ~(server-start)~
##  in the Emacs initialization file (see **Using Emacs as a Server**
##  in your Emacs editor manual).
##
##EXAMPLE(notest)
##> with(maplev):
##- Display the info page for ~maplev~.
##> Emacs("(info \"maplev\")");
##
##SEEALSO
##- "maplev"
##
##TEST(notest)
## $include <maple/include/test_macros.mi>
## AssignFUNC(Emacs):
## AssignLocalProc(Setup,Setup):
## Setup(sprintf("Maple (%s/bin/maple)", kernelopts('mapledir'))):
### mdc(FUNC):
### The following can be debugged, but doesn't return when tested.
### That's because the tester uses the same mechanism as the debugger.
## Try("1.1", FUNC("(info \"emacs\")"));

Emacs := proc(lisp :: string)

local cmd, result;

    if pmaple_buffer <> "unknown" then
        cmd := sprintf("emacsclient --eval '%s'", lisp);
        result := ssystem(cmd);
        if result[1] <> 0 then
            error "problem contacting emacs: %1", result[2];
        end if;
    end if;

    NULL;

end proc;

