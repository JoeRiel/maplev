#LINK maplev.mpl
##INCLUDE ../include/mpldoc_macros.mpi
##PROCEDURE(help) maplev[Unpack]
##HALFLINE unpack the lisp and info files for MapleV
##AUTHOR   Joe Riel
##DATE     Feb 2017
##CALLINGSEQUENCE
##- Unpack('opts')
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

#LINK maplev.mpl

Unpack := proc( )

local binfile, book, dst, dstdir, join, platform, src, status, systype, tboxdir;


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

    #{{{ Doc (html and pdf)

    printf("\nextracting doc files\n");

    dstdir := join(tboxdir, "doc");

    if not FT:-Exists(dstdir) then
        FT:-MakeDirectory(dstdir, 'recurse');
    end if;

    src := join(book, "maplev.html");
    dst := join(dstdir, "maplev.html");
    Copy(src, dst, 'force', 'verbose');

    src := join(book, "maplev.pdf");
    dst := join(dstdir, "maplev.pdf");
    Copy(src, dst, 'force', 'verbose');

    #}}}
    #{{{ Binary file

    printf("\nextracting pmaple binary file\n");

    platform := kernelopts('platform');
    binfile := ifelse(platform = "windows"
                      , "pmaple.exe"
                      , "pmaple"
                     );

    systype  := FileTools:-Filename(kernelopts('bindir'));
    dstdir := join(tboxdir, systype);

    if not FT:-Exists(dstdir) then
        FT:-MakeDirectory(dstdir, 'recurse');
    end if;

    src := join(book, systype, binfile);
    dst := join(dstdir, binfile);

    Copy(src, dst, 'force', 'verbose');

    if platform = "unix" or platform = "mac" then
        status := ssystem(sprintf("chmod +x %s", dst));
        if not status[1] = 0
        or not FileTools:-IsExecutable(dst)
        then
            WARNING("could not make binary file %1 executable", dst);
        end if;
    end if;

    #}}}
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
