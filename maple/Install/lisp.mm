#LINK Install.mpl

##INCLUDE ../include/mpldoc_macros.mpi
##PROCEDURE(help) maplev:-Install:-lisp
##HALFLINE unpack the lisp and info files for MapleV
##AUTHOR   Joe Riel
##DATE     Feb 2017
##CALLINGSEQUENCE
##- Unpack()
##DESCRIPTION
##- The `Unpack` command unpacks the lisp, info, and binary files for MapleV.
##  It also byte compiles the lisp files.
##
##- The lisp files are extracted from the Maple workbook to ~$HOME/maple/toolbox/maplev/lisp/~,
##  byte-compiled, then copied to `lispdir`.
##
##- The info file is extracted from the Maple workbook to ~$HOME/maple/toolbox/maplev/info/~,
##  then copied to `infodir`.
##
##
##EXAMPLES(notest,noexecute)
##> maplev:-Unpack();
##
##XREFMAP
##- "Emacs" : Help:www.gnu.org/software/emacs
##
##SEEALSO
##- "maplev"

lisp := proc( )

local book, dst, join, src, tboxdir;


uses FT = FileTools;

    join := proc()
        FileTools:-JoinPath([_passed]);
    end proc;

    tboxdir := kernelopts('toolboxdir' = 'maplev');

    book := join(tboxdir, "lib", "maplev.maple");

    if not FT:-Exists(book) then
        error "Maple book %1 does not exist", book;
    end if;

    book := sprintf("maple://%s", book);

    #{{{ Tar file

    printf("\nextracting tar file\n");

    src := FT:-ListDirectory(book, 'returnonly' = "*.tar");
    if src = [] then
        error "missing tar file";
    else
        src := src[1];
    end if;

    dst := join(tboxdir, src);
    src := join(book, src);
    Copy(src, dst, 'force', 'verbose');

    #}}}

    return NULL;

end proc:

$undef EMACS_PKG
