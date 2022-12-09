#LINK Install.mpl

##INCLUDE ../include/mpldoc_macros.mpi
##PROCEDURE(help) MapleVInstaller:-Copy
##HALFLINE copy file
##AUTHOR   Joe Riel
##DATE     Jun 2018
##CALLINGSEQUENCE
##- Copy('src', 'dst', 'opts')
##PARAMETERS
##- 'src'  : ::string::; path to source file
##- 'dst'  : ::string::; path to destination file
##RETURNS
##- `NULL`
##OPTIONS
##opt(force,truefalse)
##  True means overwrite an existing destination file.
##  The default is false.
##opt(verbose,truefalse)
##  True means print the action taken.
##  The default is false.
##DESCRIPTION
##- The `Copy` command copies a source file to a destination file.

Copy := proc(src :: string
             , dst :: string
             , { force :: truefalse := false }
             , { verbose :: truefalse := false }
             , $
            )
    FileTools:-Copy(src, dst, _options['force']);
    if verbose then
        printf("  %s --> %s\n", src, dst);
    end if;
    NULL;
end proc;

