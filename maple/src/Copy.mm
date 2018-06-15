#LINK maplev.mpl
##INCLUDE ../include/mpldoc_macros.mpi
##PROCEDURE(help) maplev[Copy]
##HALFLINE copy file
##AUTHOR   Joe Riel
##DATE     Jun 2018
##CALLINGSEQUENCE
##- Copy('src', 'dst', 'opts')
##PARAMETERS
##- 'src'  : ::string::; path to source file
##- 'dst'  : ::string::; path to destination file

Copy := proc(src :: string
             , dst :: string
             , { force :: truefalse := false }
             , { verbose :: truefalse := false }
             , $
            )

    FileTools:-Copy(src, dst, _options['force']);
    if verbose then
        printf("copied %s to %s\n", src, dst);
    end if;
    NULL;
end proc;

