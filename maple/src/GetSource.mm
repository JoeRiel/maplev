#LINK maplev.mpl

##INCLUDE ../include/mpldoc_macros.mpi
##PROCEDURE(help,label="GetSource") maplev:-GetSource
##HALFLINE return the source file and line number of a procedure
##INDEXPAGE maplev[Exports],GetSource,return the source file and line number of a Maple procedure
##CALLINGSEQUENCE
##- GetSource('p')
##PARAMETERS
##- 'p' : ::string::; procedure for which source is desired
##RETURNS
##- `[file,line]`
##-- `file` : ::string::; file name
##-- `line` : ::posint::; line number
##
##DESCRIPTION
##- The `GetSource` command returns a two-element list
##  containing the source file and line number
##  for the Maple procedure 'p'.
##
##- The parameter 'p', the name of a procedure,
##  is a string that is parsed with ~kernelopts(opaquemodules)~
##  temporarily assigned false so that a local procedure is handled.
##
##- If no source is located, `NULL` is returned.
##
##- If 'p' is an appliable module,
##  the source for ~p:-ModuleApply~ is used.
##
##- If 'p' has been assigned with "overload"
##  using a list of procedures,
##  the source for the first procedure is returned.
##
##- A leading `>` in the source name
##  is replaced with the value of ~kernelopts(mapledir)~
##  followed by a directory separator.
##
##EXAMPLE(noexecute)
##- Load the package.
##> with(maplev):
##- Get the file name and starting line number for this procedure.
##> src := GetSource("maplev:-GetSource");
##
##SEEALSO
##- "maplev"
##- "ModuleApply"
##- "kernelopts"
##
##TEST
### These fail in tester because debugopts('lineinfo') returns NULL
### (that is a feature of the tester).  They also fail here if the mla
### was built with LINEINFO_RELPATH := true; that needs to be dealt with.
##
## $include <maple/include/test_macros.mi>
## kernelopts('keepdebuginfo'=true):
## AssignFUNC(GetSource):
### mdc(FUNC):
## Try("1.1", FUNC("maplev:-GetSource"), ["/home/joe/emacs/maplev/maple/src/GetSource.mm", 62] );
## Try("2.1", map(whattype,FUNC("simplify")), [string,integer]);
## Try("2.2", FUNC("simplify"), [FileTools:-JoinPath([kernelopts('mapledir'), "lib/simplify/src/simplify.mpl"]), 38]);

# (maplev-cmaple-direct "(maplev:-GetSource)(\"int:-Main\");")

GetSource := proc(p? :: string )
local base,file,li,line,mroot,opacity,p,src;
    opacity := kernelopts('opaquemodules'=false);
    try
        p := parse(p?);
        if p :: `module` then
            if p :: 'appliable' then
                # does anyone use an appliable module for ModuleApply?
                p := p:-ModuleApply;
            else
                return NULL;
            end if;
        elif not p :: 'procedure' then
            return NULL
        elif member('overload', [op(eval(p))]) then
            # overloaded procedure
            try
                # Hack to get first procedure in an overload list.
                # If p merely has option overload, this fails
                # so p is unchanged and should work.
                p := pointto(disassemble(disassemble(disassemble(addressof(eval(p)))[6])[2])[2]);
            catch:
            end try;
        end if;

        # get the lineinfo data
        li := [debugopts(':-lineinfo' = p)];

        if li = [] then
            # no info available
            src := NULL;
        else
            # extract file and line from first element in list
            (file,line) := op([1,1..2],li);

            # expand a leading > to the value of kernelopts(mapledir)
            if file[1] = ">" then
                base := file[2..-1];
                mroot := kernelopts('mapledir');
                file := FileTools:-JoinPath([mroot,base]);
            end if;
            src := [file,line];
        end if;
    finally
        kernelopts('opaquemodules' = opacity);
    end try;
    return src;
end proc;


