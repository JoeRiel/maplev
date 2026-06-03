#LINK maplev.mpl

##INCLUDE ../include/mpldoc_macros.mpi

##DEFINE CommonParams 'indent','nomen','rel','keep_statement_numbers'
##DEFINE COMMONPARAMDEFS
##- 'indent'            : ::nonnegint::; the indentation of the procedure
##- 'nomen'             : ::string::; the name of the expression; may also be of type name
##- 'rel'               : ::string::; the assignment operator (~:=~ or ~:: static :=~)
##- 'keep_statement_numbers' : ::truefalse::; true means keep the statement numbers
##ENDDEFINE

Print := module()

export ModuleApply;
local Dispatch, ModuleLoad, PrintModule, PrintProc, PrintRecord
    , buf # StringBuffer
    , indent_amount := 4
    ;

    ModuleLoad := proc()
        buf := StringTools:-StringBuffer();
    end proc;

##PROCEDURE(help,label="Print") maplev:-Print
##HALFLINE appliable module for printing a Maple expression
##INDEXPAGE maplev[Exports],Print,appliable module for printing a Maple expression
##CALLINGSEQUENCE
##- maplev:-Print('s','opts')
##PARAMETERS
##- 's'    : ::string::; string representation of a Maple expression to print
##param_opts(Print)
##RETURNS
##- ::string:: or NULL
##DESCRIPTION
##- The `Print` command
##  parses and prints a string of a Maple expression.
##  This procedure is used by "mds", part of the mdcs debugger,
##  to print requested procedures and modules.
##SUBSECTION Exports
##SHOWINDEX(table="maplev:-Print[Exports]")
##ENDSUBSECTION
##OPTIONS
##opt(file,string)
##  The name (path) of the file to write.
##  The default is the empty string, which prints the output to the screen.
##opt(return_string,truefalse)
##  True means return the string that would otherwise be written or printed.
##  The default is false.
##opt(keep_statement_numbers,truefalse)
##  True means keep (display) the statement numbers.
##  The default is false.
##EXAMPLES
##>(noexecute) maplev:-Print("cos", 'keep_statement_numbers');
##> maplev:-Print("proc(x) sin(x); end proc");
##SEEALSO
##- "mds"
##XREFMAP
##- "mdcs" : Help:mdc,Intro
##TEST
## $include <maple/include/test_macros.mi>
## AssignFUNC(Print):
## foo := module() end module:
### mdc(FUNC):
## Try("1.1", FUNC("proc(x) sin(x); end proc", 'return_string'), "expr := proc(x)\n    sin(x)\nend proc;" );
## Try("1.2", FUNC("Record(a=1,b=2)", 'return_string'), "Record(a = 1,b = 2) := Record('a' = 1, 'b' = 2);" );
## Try("1.3", FUNC("foo", 'return_string','keep_statement_numbers'), "foo := module ()\nend module;" );


    ModuleApply := proc(s :: string
                        , { file :: string := "" }
                        , { return_string :: truefalse := false }
                        , { keep_statement_numbers :: truefalse := false }
                       )
    local expr, opacity, str, width;
        try
            # Save and reset configuration.
            opacity := kernelopts('opaquemodules'=false);
            width := interface('screenwidth'=9999);

            buf:-clear();
            expr := parse(s);
            Dispatch(0, expr, ":=", keep_statement_numbers, expr);

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

##PROCEDURE maplev:-Print:-Dispatch
##HALFLINE dispatch the given expression to the appropriate procedure
##INDEXPAGE maplev:-Print[Exports],Dispatch,dispatch the given expression to the appropriate procedure
##CALLINGSEQUENCE
##- maplev:-Print:-Dispatch('indent','nomen','rel','keep_statement_numbers')
##PARAMETERS
##COMMONPARAMDEFS
##RETURNS
##- TBD
##DESCRIPTION
##- The `Dispatch` procedure
##TEST
## $include <maple/include/test_macros.mi>
## AssignFUNC(Print:-Dispatch):
### mdc(FUNC):
## Try("1.1", FUNC(0,"nomen","=",false));

    Dispatch := proc(indent :: nonnegint
                     , nomen
                     , rel :: string # = or :=
                     , keep_statement_numbers :: truefalse
                    )
    local expr;
        expr := _rest;
        if expr :: procedure then
            PrintProc(_passed);
        elif expr :: 'record' then
            PrintRecord(_passed);
        elif expr :: '`module`' then
            PrintModule(_passed);
        else
            buf:-appendf("%*s%a %s %q;", indent, "", nomen, rel, eval(expr));
        end if;
        NULL;
    end proc;

##PROCEDURE maplev:-Print:-PrintModule
##HALFLINE print a module
##INDEXPAGE maplev:-Print[Exports],PrintModule,print a module
##CALLINGSEQUENCE
##- maplev:-Print:-PrintModule(\CommonParams,'m')
##PARAMETERS
##COMMONPARAMDEFS
##- 'm' : a module or an object
##DESCRIPTION
##- The `PrintModule` commands prints module 'm',
##  which can be either a regular module, or an object.
##  A record is not handled.
##TEST
## $include <maple/include/test_macros.mi>
## AssignFUNC(Print:-PrintModule):
## M := module() export ex; local loc; end module:
### mdc(FUNC):
## Try[NE]("1.1.1", FUNC(1,foo,"::static :=",true,M), 'assign'='buf');
## Try("1.1.2", buf:-value(), " foo ::static := module ()\n local loc;\n export ex;\n\n     ex := ex;\n end module;" );


    PrintModule := proc(indent :: nonnegint
                        , nomen
                        , rel :: string
                        , keep_statement_numbers :: truefalse
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
                Dispatch(indent + indent_amount             # indent
                         , convert(convert(ex,string),name) # nomen
                         , ":: static :="                   # rel
                         , keep_statement_numbers           # keep_statement_numbers
                         , ex                               # expr
                        );
                buf:-newline();
            end do;
        else
            # print exports
            for ex in exports(m) do
                buf:-newline();
                Dispatch(indent + indent_amount   # indent
                         , ex                     # nomen
                         , ":="                   # rel
                         , keep_statement_numbers # keep_statement_numbers
                         , m[ex]                  # expr
                        );
                buf:-newline();
            end do;
            # print locals;
            for ex in op(3,em) do
                if ex :: '{procedure,`module`}' then
                    nm := convert(StringTools:-StringSplit(ex,":-")[-1],name);
                    buf:-newline();
                    Dispatch(indent + indent_amount   # indent
                             , nm                     # nomen
                             , ":="                   # rel
                             , keep_statement_numbers # keep_statement_numbers
                             , ex                     # expr
                            );
                    buf:-newline();
                end if;
            end do;
        end if;
        buf:-appendf("%*send module;", indent, "");

    end proc;

##PROCEDURE maplev:-Print:-PrintProc
##HALFLINE print a procedure
##INDEXPAGE maplev:-Print[Exports],PrintProc,print a procedure
##CALLINGSEQUENCE
##- maplev:-Print:-PrintProc(\CommonParams,'p')
##PARAMETERS
##COMMONPARAMDEFS
##- 'p' : ::procedure::; the procedure to print
##TEST
## $include <maple/include/test_macros.mi>
## AssignFUNC(Print:-PrintProc):
## P := proc() end proc:
### mdc(FUNC):
## Try[NE]("1.1.1", FUNC(0,nomen,":=",false,P), 'assign' = 'buf');
## Try("1.1.2", buf:-value(), "nomen := proc()\n    NULL\nend proc;" );

    PrintProc := proc(indent :: nonnegint
                      , nomen
                      , rel :: string
                      , keep_statement_numbers :: truefalse
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
                               , ifelse(keep_statement_numbers
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


##PROCEDURE maplev:-Print:-PrintRecord
##HALFLINE print a record
##INDEXPAGE maplev:-Print[Exports],PrintRecord,print a record
##CALLINGSEQUENCE
##- maplev:-Print:-PrintRecord(\CommonParams,'rec')
##PARAMETERS
##COMMONPARAMDEFS
##- 'rec' : ::record::
##DESCRIPTION
##TEST
## $include <maple/include/test_macros.mi>
## AssignFUNC(Print:-PrintRecord):
## R := Record(a=1,b=2):
### mdc(FUNC):
## Try[NE]("1.1.1", FUNC(0,nomen,":=",false,R), 'assign' = 'buf');
## Try("1.1.2", buf:-value(), "nomen := record(\n    a = 1;\n    b = 2;\n);" );

    PrintRecord := proc(indent :: nonnegint
                        , nomen
                        , rel :: string
                        , keep_statement_numbers :: truefalse
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
                         , keep_statement_numbers
                        );
            else
                Dispatch(indent + indent_amount
                         , ex
                         , "="
                         , keep_statement_numbers
                         , rec[ex]
                        );
            end if;
            buf:-newline();
        end do;

        buf:-appendf("%*s);", indent, "");
    end proc;

##

end module:
