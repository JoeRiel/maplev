#LINK maplev.mpl

##INCLUDE ../include/mpldoc_macros.mpi
##PROCEDURE(help,label="IsAbsolutePath") maplev:-IsAbsolutePath
##HALFLINE determine whether a path is absolute or relative
##INDEXPAGE maplev[Exports],IsAbsolutePath,determine whether a path is absolute or relative
##CALLINGSEQUENCE
##- maplev:-IsAbsolutePath('path')
##PARAMETERS
##- 'path' : ::string::; filepath
##RETURNS
##- ::truefalse::
##DESCRIPTION
##- The `IsAbsolutePath`(path) command returns true if 'path' is an absolute path, false otherwise.
##- The path must be a string, but does not have to exist.
##EXAMPLES
##- maplev:-IsAbsolutePath("foo");
##- maplev:-IsAbsolutePath("/foo");
##SEEALSO
##- "FileTools[AbsolutePath]"
##TEST
## $include <maple/include/test_macros.mi>
## AssignFUNC(IsAbsolutePath):
### mdc(FUNC):
## Try("1.1", FUNC("foo"),false);
## Try("1.2", FUNC("/foo"),true);
## Try("1.3", FUNC(""),false);
## Try("1.4", FUNC("."),false);

IsAbsolutePath := proc( path :: string )
local cleanpath, regex;
    cleanpath := StringTools:-SubstituteAll(path, "\\", "/");
    regex := ifelse(kernelopts('platform') = "windows"
                    , "^([A-Za-z]:/|//)"
                    , "^/");
    StringTools:-RegMatch(regex, cleanpath);
end proc:

