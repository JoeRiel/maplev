##MODULE maplev
##HALFLINE module used with Emacs maplev mode

maplev := module()

##PROCEDURE maplev[PrintProc]
##HALFLINE print a procedure
##INDEXPAGE maplev[Exports],PrintProc,
##CALLINGSEQUENCE
##- PrintProc('P', 'lines', 'doublespaces')
##PARAMETERS
##- 'P'            : ::string::; identifies procedure to print
##- 'lines'        : (optional) ::{posint, posint..posint}::; lines to print
##- 'doubleindent' : (optional keyword) ::truefalse::; true means double the spaces used to indent
##RETURNS
##- `NULL`
##
##DESCRIPTION
##- The `PrintProc` command
##  prints a procedure, like "showstat", but without the line numbers
##  and with the options and description fields (they are elided by showstat).
##
##- The 'P' parameter is a string identifying the procedure to print.
##  Module local procedures can be passed.
##OPTIONS
##- 'doubleindent' :: truefalse
##-(noindent) True means double the two-space indent used by showstat.
##  The default is true.

export
    PrintProc := proc(P :: string
                      , lines::{posint,posint..posint} := NULL
                      , { doubleindent :: truefalse := true }
                      , $
                     )
    local p,width,opacity,dummy_name,str,pos,opts,desc,extra;
    option `Copyright (C) 2004 by Joseph S. Riel. All rights reserved.`;
    description "Print like showstat, but without line numbers";
    uses ST = StringTools;

        try

            # Save and reset configuration.
            opacity := kernelopts('opaquemodules'=false);
            width := interface('screenwidth'=9999);

            p := parse(P);

            if not p::procedure then
                lprint(eval(p));
                return NULL;
            end if;

            # Create string of procedure listing, with statement
            # numbers removed and indenting doubled.
            str := sprintf("%s:"
                           , foldr(StringTools:-RegSubs
                                   , debugopts('procdump'=`if`(lines = NULL
                                                               , p
                                                               , [p, lines]
                                                              ))
                                   , `if`(doubleindent
                                          , "\n( +)" = "\n\\1\\1"  # double spaces used for indenting
                                          , NULL
                                         )
                                   , "\n ...." = "\n"              # remove statement numbers
                                  )[..-2]);                        # remove trailing \n

            # Insert option and description statements, if assigned.
            opts := op(3, eval(p));
            desc := op(5, eval(p));

            extra := "";
            if opts <> NULL then
                extra := sprintf("option %q;\n", opts);
            end if;
            if desc <> NULL then
                extra := sprintf("%sdescription %q;\n", extra, desc);
            end if;

            if extra <> "" then
                # Insert options/description after the first line (the procedure
                # header).  The Maple print procedure inserts them after the
                # local/global statements, however, that takes slightly more
                # work.
                pos := ST:-Search("\n", str);
                str := ST:-Insert(str, pos, extra);
            end if;

            # Print the string.
            printf("%s\n", str);

        catch "cannot debug the debugger":
            dummy_name := eval(p);
            procname(dummy_name,args[2..-1])
        catch:
            error "'%1' is not a procedure name",P;
        finally
            interface(screenwidth = width);
            kernelopts('opaquemodules' = opacity);
            return NULL;
        end try;
    end proc;

##PROCEDURE maplev[Setup]
##DESCRIPTION
##- The `Setup` command calls "kernelopts" and "interface"
##  to assign the appropriate settings for interfacing with
##  the Emacs **maplev-mode**.

export
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
