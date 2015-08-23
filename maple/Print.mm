#LINK maplev.mpl

Print := module()

export ModuleApply;
local Dispatch, PrintModule, PrintProc;

##PROCEDURE maplev[Print][ModuleApply]

    ModuleApply := proc(s :: string
                        , { return_string :: truefalse := false }
                       )

    local expr, opacity, width, str;

        try
            # Save and reset configuration.
            opacity := kernelopts('opaquemodules'=false);
            width := interface('screenwidth'=9999);

            expr := parse(s);

            str := Dispatch(expr, 0);

        finally
            # restore configuration
            interface(screenwidth = width);
            kernelopts('opaquemodules' = opacity);
        end try;

        if return_string then
            str;
        else
            printf("%s\n", str);
        end if;

    end proc;

##PROCEDURE maplev[Print][Dispatch]

    Dispatch := proc(expr
                     , indent_level :: nonnegint
                     , nomen := NULL
                     , { is_static :: truefalse := false }
                    )
    local indent;
        if   expr :: procedure then
            PrintProc(expr, indent_level, nomen, _options['is_static']);
        elif expr :: '`module`' and not expr :: 'record' then
            PrintModule(expr, indent_level, nomen);
        else
            indent := 4 * indent_level;
            sprintf("%*s%a", indent, "", eval(expr));
        end if;
    end proc;


##PROCEDURE maplev[Print][PrintModule]

    PrintModule := proc(m
                        , indent_level :: nonnegint
                        , nomen := m
                       )
    local buf, em, ex, indent, moddef,nm, obj;
    uses %ST = StringTools;

        em := eval(m);
        moddef := op(2,em);

        buf := %ST:-StringBuffer();
        indent := indent_level * 4;
        buf:-appendf("%*s%a := module ()\n", indent, "", nomen);
        if op(2,moddef) <> NULL then
            buf:-appendf("%*slocal %q;\n", indent, "", op(2,moddef));
        end if;
        if op(4,moddef) <> NULL then
            buf:-appendf("%*sexport %q;\n", indent, "", op(4,moddef));
        end if;
        if op(5,moddef) <> NULL then
            buf:-appendf("%*sdescription %q;\n", indent, "", op(5,moddef));
        end if;
        if op(6,moddef) <> NULL then
            buf:-appendf("%*sglobals %q;\n", indent, "", op(6,moddef));
        end if;
        if op(3,moddef) <> NULL then
            buf:-appendf("%*soptions %q;\n", indent, "", op(3,moddef));
        end if;
        # print exports
        if eval(m) :: 'object' then
            obj := eval(m);
            # print exports
            for ex in exports(obj,'static','instance') do
                buf:-appendf("\n%s\n", Dispatch(ex, indent_level+1
                                                , convert(convert(ex,string),name)
                                                , 'is_static'
                                               ));
            end do;
        else
            local str;
            # print exports
            for ex in exports(m) do
                str := Dispatch(m[ex], indent_level+1, ex);
                buf:-appendf("\n%s\n", str);
            end do;
            # print locals;
            for ex in op(3,em) do
                if ex :: '{procedure,`module`}' then
                    nm := convert(StringTools:-StringSplit(ex,":-")[-1],name);
                    str := Dispatch(ex, indent_level+1, nm);
                    buf:-appendf("\n%s\n", str);
                end if;
            end do;
        end if;
        buf:-appendf("%*send module;", indent, "");

        buf:-value();
    end proc;

##PROCEDURE maplev[Print][PrintProc]
##HALFLINE print a procedure


    PrintProc := proc(p
                      , indent_level :: nonnegint
                      , nomen := p
                      , { is_static :: truefalse := false }
                     )
    description "Print like showstat, but without line numbers";
    uses %ST = StringTools;

    local desc, extra, indent, opts, pos, str, rep;

        indent := indent_level * 4;

        if p :: 'builtin' then
            return sprintf("%*s%a\n", indent, "", eval(p));
        end if;

        str := substring(debugopts('procdump' = p), 1..-2);

        # Create name replacement
        rep := `if`(is_static
                    , sprintf("%a :: static :=\\1", nomen)
                    , sprintf("%a :=\\1", nomen)
                   );

        # escape ampersand (maybe other stuff)
        rep := StringTools:-RegSubs("&" = "\\\\&", rep);

        # Create string of procedure listing, with statement
        # numbers removed and indenting doubled.
        str := sprintf("%*s%s;"
                       , indent, ""
                       , foldr(StringTools:-RegSubs
                               , str
                               , "^[^ ]* :=" = rep
                               , "\n"      = sprintf("\n%*s", indent, "") # indent
                               , "\n( +)"  = "\n\\1\\1" # double spaces used for indenting
                               , "\n ...." = "\n"       # remove statement numbers
                              )
                      );

        # Insert option and description statements, if assigned.
        opts := op(3, eval(p));
        desc := op(5, eval(p));

        extra := "";
        if opts <> NULL then
            extra := sprintf("%*soption %q;\n", indent, "", opts);
        end if;
        if desc <> NULL then
            extra := sprintf("%s%*sdescription %q;\n", extra, indent, "", desc);
        end if;

        if extra <> "" then
            # Insert options/description after the first line (the procedure
            # header).  The Maple print procedure inserts them after the
            # local/global statements, however, that takes slightly more
            # work.
            pos := %ST:-Search("\n", str);
            str := %ST:-Insert(str, pos, extra);
        end if;

        str;

    end proc;

##

end module;
