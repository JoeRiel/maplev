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

local binfile, book, dst, dstdir, platform, src, status, tboxdir;

uses FT = FileTools;

    tboxdir := kernelopts('toolboxdir' = 'maplev');

    book := FileTools:-JoinPath([tboxdir, "lib", "maplev.maple"]);
    if not FT:-Exists(book) then
        error "Maple book %1 does not exist", book;
    end if;

    book := sprintf("maple://%s", book);

    #{{{ Doc

    printf("\nextracting doc files\n");

    dstdir := cat(tboxdir, "/doc");

    if not FT:-Exists(dstdir) then
        FT:-MakeDirectory(dstdir, 'recurse');
    end if;

    Copy(cat(book, "/maplev.html"), cat(dstdir, "/maplev.html"), 'force', 'verbose');
    Copy(cat(book, "/maplev.pdf"),  cat(dstdir, "/maplev.pdf"),  'force', 'verbose');

    #}}}
    #{{{ Binary file

    printf("\nextracting pmaple binary file\n");

    dstdir := FileTools:-JoinPath([tboxdir, "bin"]);

    if not FT:-Exists(dstdir) then
        FT:-MakeDirectory(dstdir, 'recurse');
    end if;

    platform := kernelopts('platform');

    if   platform = "unix"    then binfile := "pmaple";
    elif platform = "windows" then binfile := "pmaple.exe";
    else
    end if;

    src := cat(book, "/", binfile);
    dst := FT:-JoinPath([dstdir, binfile], 'force');

    Copy(src, dst, 'force', 'verbose',NULL);

    if platform = "unix" then
        status := ssystem(sprintf("chmod +x %s", dst));
        if not status[1] = 0 then
            WARNING("could not make binary file %1 executable", dst);
        end if;
    end if;

    #}}}
    #{{{ Tar file

    printf("\nextracting tar file\n");

    src := FT:-ListDirectory(book, 'returnonly' = "*.tar")[1];

    dst := cat(tboxdir, "/", src);
    Copy(src, dst, 'force', 'verbose');

    #}}}

    return NULL;

end proc:
