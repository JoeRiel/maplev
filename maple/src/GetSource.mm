#LINK maplev.mpl

##INCLUDE ../include/mpldoc_macros.mpi
##PROCEDURE maplev[GetSource]
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
##- If no source is located, `NULL` is returned.
##- If 'p' is an appliable module,
##  the source for ~p:-ModuleApply~ is used.
##- If 'p' has been assigned with "overload"
##  using a list of procedures,
##  the source for the first procedure is returned.
##- A leading `>` in the source name
##  is replaced with the value of the OS environment variable MAPLE_ROOT
##  followed by a directory separator.
##  If MAPLE_ROOT is not assigned, an error is raised.
##
##OPTIONS
##opt(download,truefalse)
##  True means download and install a source file from the perforce repository
##  if the file currently does not exist.
##  This only has an effect if the OS environment variable MAPLE_ROOT has `main`
##  as the child directory name.
##  The default is false.

GetSource := proc(p? :: string
                  , { subs_maple_root :: truefalse := false }
                  , { download :: truefalse := false}
                  , $
                 )
local base,cmd,file,li,line,mroot,opacity,p,res,src;
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

        # get the line/info data
        li := [debugopts(':-lineinfo' = p)];

        if li = [] then
            # no info available
            src := NULL;
        else
            # extract file and line from first element in list
            (file,line) := op([1,1..2],li);

            # expand a leading > to value of MAPLE_ROOT environment variable
            if file[1] = ">" and subs_maple_root then
                base := file[2..-1];
                mroot := getenv("MAPLE_ROOT");
                if mroot = NULL then
                    error "environment variable MAPLE_ROOT is not assigned";
                else
                    file := FileTools:-JoinPath([mroot,base]);
                    if download
                    and FileTools:-Filename(mroot) = "main"
                    and not FileTools:-Exists(file) then
                        # download and install the file from perforce
                        cmd := sprintf("p4 print -q //wmi/projects/mapleV/main/%s", base);
                        res := ssystem(cmd);
                        if res[1] = 0 then
                            FileTools:-MakeDirectory(FileTools:-ParentDirectory(file),'recurse');
                            FileTools:-Text:-WriteFile(file, res[2]);
                        end if;
                    end if;
                end if;
            end if;
            src := [file,line];
        end if;
    finally
        kernelopts('opaquemodules' = opacity);
    end try;
    return src;
end proc;

##TEST
## macro(FUNC = maplev:-GetSource):
### mdc(FUNC):
## Try("1", FUNC("maplev:-GetSource")
##  , ["/home/joe/emacs/maplev/maple/GetSource.mm", 20]);
## Try("2", map(whattype,FUNC("simplify")), [string,integer]);

# (maplev-cmaple-direct "(maplev:-GetSource)(\"int:-Main\");")

