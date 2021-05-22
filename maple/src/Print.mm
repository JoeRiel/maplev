#LINK maplev.mpl

##MODULE Print
##HALFLINE appliable module for printing a Maple expression
##DESCRIPTION
##- The `Print` module is used by the Emacs `maplev-view-mode` to display
##  Maple library code in a buffer.

Print := module()

export ModuleApply;
local Dispatch, ModuleLoad, PrintModule, PrintProc, PrintRecord
    , buf # StringBuffer
    , indent_amount := 4
    ;

    ModuleLoad := proc()
        buf := StringTools:-StringBuffer();
    end proc;

##PROCEDURE maplev[Print][ModuleApply]

    ModuleApply := proc(s :: string
                        , { file :: string := "" }
                        , { return_string :: truefalse := false }
                        , { keep_line_numbers :: truefalse := false }
                       )

    local expr, opacity, str, width;

        try
            # Save and reset configuration.
            opacity := kernelopts('opaquemodules'=false);
            width := interface('screenwidth'=9999);

            buf:-clear();
            expr := parse(s);
            Dispatch(0, expr, ":=", keep_line_numbers, expr);

        finally
            # restore configuration
            interface(screenwidth = width);
            kernelopts('opaquemodules' = opacity);
        end try;

        str := buf:-value('clear');

        if return_string then
            return str;
        elif file = "" then
            printf("%s\n", str);
        else
            FileTools:-Text:-WriteFile(file, str);
        end if;

    end proc;

##PROCEDURE maplev[Print][Dispatch]

    Dispatch := proc(indent :: nonnegint
                     , nomen
                     , rel :: string # = or :=
                     , keep_line_numbers :: truefalse
                    )
    local expr;
        expr := _rest;
        if expr :: procedure then
            PrintProc(args);
        elif expr :: 'record' then
            PrintRecord(args);
        elif expr :: '`module`' then
            PrintModule(args);
        else
            buf:-appendf("%*s%a %s %q;", indent, "", nomen, rel, eval(expr));
        end if;
    end proc;


##PROCEDURE maplev[Print][PrintModule]
##DESCRIPTION
##- The `PrintModule` commands prints module 'm',
##  which can be either a regular module, or an object.
##  A record is not handled.


    PrintModule := proc(indent :: nonnegint
                        , nomen
                        , rel :: string
                        , keep_line_numbers :: truefalse
                        , m
                       )
    local em, ex, moddef, nm, obj;
    uses %ST = StringTools;

        em := eval(m);
        moddef := op(2,em);

        buf:-appendf("%*s%a %s module ()\n", indent, "", nomen, rel);
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
            buf:-appendf("%*sglobal %q;\n", indent, "", op(6,moddef));
        end if;
        if op(3,moddef) <> NULL then
            buf:-appendf("%*soptions %q;\n", indent, "", op(3,moddef));
        end if;
        # print exports
        if eval(m) :: 'object' then
            obj := eval(m);
            # print exports
            for ex in exports(obj,'static','instance') do
                buf:-newline();
                Dispatch(indent + indent_amount
                         , convert(convert(ex,string),name)
                         , ":: static :="
                         , ex
                         , keep_line_numbers
                        );
                buf:-newline();
            end do;
        else
            # print exports
            for ex in exports(m) do
                buf:-newline();
                Dispatch(indent + indent_amount
                         , ex
                         , ":="
                         , keep_line_numbers
                         , m[ex]
                        );
                buf:-newline();
            end do;
            # print locals;
            for ex in op(3,em) do
                if ex :: '{procedure,`module`}' then
                    nm := convert(StringTools:-StringSplit(ex,":-")[-1],name);
                    buf:-newline();
                    Dispatch(indent + indent_amount
                             , nm
                             , ":="
                             , keep_line_numbers
                             , ex
                            );
                    buf:-newline();
                end if;
            end do;
        end if;
        buf:-appendf("%*send module;", indent, "");

    end proc;

##PROCEDURE maplev[Print][PrintProc]
##HALFLINE print a procedure


    PrintProc := proc(indent :: nonnegint
                      , nomen
                      , rel :: string
                      , keep_line_numbers :: truefalse
                      , p
                     )
    description "Print like showstat, but without line numbers";
    uses %ST = StringTools;

    local desc, extra, opts, pos, str, rep;

        if p :: 'builtin' then
            buf:-appendf("%*s%a\n", indent, "", eval(p));
            return;
        end if;

        str := substring(debugopts('procdump' = p), 1..-2);

        # Create name replacement
        rep := sprintf("%a %s\\1", nomen, rel);

        # Escape special characters (skip \1, etc.; a backslash in a name is a bad idea)
        rep := StringTools:-RegSubs("&" = "\\\\&", rep);

        # Create string of procedure listing, with statement
        # numbers removed and indenting doubled.
        str := sprintf("%*s%s;"
                       , indent, ""
                       , foldr(StringTools:-RegSubs
                               , str
                               (* the following are applied in reverse order *)
                               , "^[^ ]* :=" = rep
                               , "\n"      = sprintf("\n%*s", indent, "") # indent
                               , ifelse(keep_line_numbers
                                        , NULL
                                        , "\n (......)" = "\n    "  # remove numbers
                                       )
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

        buf:-append(str);

    end proc;


##PROCEDURE maplev[Print][PrintRecord]
##DESCRIPTION

    PrintRecord := proc(indent :: nonnegint
                        , nomen
                        , rel :: string
                        , keep_line_numbers :: truefalse
                        , rec
                       )
    local ex;
        buf:-appendf("%*s%a %s record(\n", indent, "", nomen, rel);
        # print exports
        for ex in exports(rec) do
            if rec[ex] = NULL then
                Dispatch(indent + indent_amount
                         , ex
                         , ""
                         , keep_line_numbers
                        );
            else
                Dispatch(indent + indent_amount
                         , ex
                         , "="
                         , keep_line_numbers
                         , rec[ex]
                        );
            end if;
            buf:-newline();
        end do;

        buf:-appendf("%*s);", indent, "");
    end proc;

##

end module:
