maplev := module()

export MemberToIndexed
    ,  PrintProc
    ;

    MemberToIndexed := proc(s::string)
    description "Convert member operator (:-) to indices in a string";
    local words;
    uses ST = StringTools;
        words := ST:-Split(s, ":-");
        words := remove(`=`, words, "");
        if nops(words) > 1 then
            sprintf("%s[':-%s']", words[1], ST:-Join(words[2..], "'][':-"));
        else
            words[1];
        end if;
    end proc:


    PrintProc := proc(P :: string
                      , lines::{posint,posint..posint} := NULL
                      , { doublespaces :: truefalse := true }
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
                                   , `if`(doublespaces
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

        catch "procedure name expected":
            error "%1 is not a procedure name",p;
        catch "cannot debug the debugger":
            dummy_name := eval(p);
            procname(dummy_name,args[2..-1])
        finally
            interface('screenwidth' = width);
            kernelopts('opaquemodules' = opacity);
        end try;
        return NULL;
    end proc;

end module:

