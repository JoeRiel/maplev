#LINK maplev.mpl

##PROCEDURE(help,label="Setup") maplev:-Setup
##HALFLINE Setup Maple for communicating with Emacs ~maplev-mode~
##INDEXPAGE maplev[Exports],Setup,setup Maple for communicating with Emacs ~maplev-mode~
##CALLINGSEQUENCE
##- maplev:-Setup('buffer')
##PARAMETERS
##- 'buffer' : ::string::; name of the interface buffer
##RETURNS
##- 'NULL'
##DESCRIPTION
##- The `Setup` command calls "kernelopts" and "interface"
##  to assign appropriate settings for interfacing with
##  Emacs **maplev-mode**.
##
##- This command is not intended to be directly called by the user.
##  It is used by the lisp code for "pmaple".
##
##- The module-local variable 'pmaple_buffer' is assigned
##  the value of the argument 'buffer'.
##
##- The "kernelopts" command is called with argument ~'printbytes' = false~,
##  which suppresses the garbage collection messages.
##
##- The "interface" command is called with the following assignments:
##
##TABLE(width="70%",colwidth="3|3|5")
##ROW **Name**     | **Value** |**Purpose**
##ROW errorbreak   | 0         | Continue if an error occurs while reading
##ROW errorcursor  | false     | Do not place cursor on location of syntax error
##ROW prettyprint  | 1         | Ensure character-based output
##ROW screenheight | infinity  | Do not limit height
##ROW verboseproc  | 2         | Print the body of all procedures
##ROW warnlevel    | 2         | Print library and kernel warnings
##ENDTABLE
##
##NOTES(nohelp)
##- Is this procecure used?
##  Yes, it used.  I just ran it by doing something which started maple
##  which failed because of the error, see below, but instantly forgot
##  what keystrokes I used.
##
##- The keystroke sequence was C-c C-c g (maplev-pmaple-pop-to-buffer).
##  A related keystroke sequence is C-c C-c k (maplev-pmaple-kill).
##  The value for buffer was ~"Maple (/home/joe/maplesoft/sandbox/main/bin/maple)"~.
##  Elisp code that calls Setup is in ~maplev-pmaple--get-pmaple-and-options~;
##  but what is the purpose?  It is called from ~maplev-pmaple--start-process~.
##
##SEEALSO
##- "interface"
##- "kernelopts"
##- "maplev"
##- "pmaple"
##TEST
## $include <maple/include/test_macros.mi>
## AssignFUNC(Setup):
### mdc(FUNC):
##
## Try("1.1", FUNC("Maple (maple)"));

# (setq maplev-config nil)

Setup := proc( buffer :: string )

    pmaple_buffer := buffer;

    kernelopts('printbytes' = false);      # suppress garbage collection messages (bytes used...)
    interface(NULL
              , 'errorbreak'   = 0         # continue if Maple encounters an error while reading
              , 'errorcursor'  = false     # do not place cursor with a syntax error
              , 'prettyprint'  = 1         # ensure character-based output
              , 'screenheight' = infinity  # do not limit the height
              , 'verboseproc'  = 2         # print the body of all procedures
              , 'warnlevel'    = 2         # print library- and kernel-generated warnings
             );
end proc;
