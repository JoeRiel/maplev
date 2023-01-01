#LINK Install.mpl

##INCLUDE ../include/mpldoc_macros.mpi
##PROCEDURE(nohelp) maplev:-Install:-pmaple
##HALFLINE install the pmaple executable
##AUTHOR   Joe Riel
##DATE     Dec 2022
##CALLINGSEQUENCE
##- maplev:-install:-pmaple()
##DESCRIPTION
##- The `pmaple` command unpacks and installs the **pmaple** binary executable for MapleV.
##
##EXAMPLES(notest,noexecute)
##> maplev:-Install:-pmaple();
##
##XREFMAP
##- "Emacs" : Help:www.gnu.org/software/emacs
##
##SEEALSO
##- "maplev"

pmaple := proc( )

local binfile, book, dst, dstdir, platform, src, status, systype, tboxdir;


uses FT = FileTools;

    tboxdir := kernelopts('toolboxdir' = 'maplev');

    book := FT:-JoinPath([tboxdir, "lib", "maplev.maple"]);

    if not FT:-Exists(book) then
        error "Maple book %1 does not exist", book;
    end if;

    book := sprintf("maple://%s", book);

    printf("\nextracting pmaple binary file\n");

    platform := kernelopts('platform');
    binfile := ifelse(platform = "windows"
                      , "pmaple.exe"
                      , "pmaple"
                     );

    systype := FileTools:-Filename(kernelopts('bindir'));
    dstdir := FT:-JoinPath([tboxdir, systype]);

    if not FT:-Exists(dstdir) then
        FT:-MakeDirectory(dstdir, 'recurse');
    end if;

    src := FT:-JoinPath([book, systype, binfile]);
    dst := FT:-JoinPath([dstdir, binfile]);

    Copy(src, dst, 'force', 'verbose');

    if platform = "unix" or platform = "mac" then
        status := ssystem(sprintf("chmod +x %s", dst));
        if not status[1] = 0
        or not FileTools:-IsExecutable(dst)
        then
            error "could not make binary file %1 executable", dst;
        end if;
    end if;

    return NULL;

end proc:

