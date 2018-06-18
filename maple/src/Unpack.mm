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

local bindir, binfile, book, cmaple, dst, dstdir, elisp, join
    , mapledir, mint, platform, pmaple, src, status, tboxdir;

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

    #{{{ Doc

    printf("\nextracting doc files\n");

    dstdir := join(tboxdir, "doc");

    if not FT:-Exists(dstdir) then
        FT:-MakeDirectory(dstdir, 'recurse');
    end if;

    Copy(join(book, "maplev.html"), join(dstdir, "maplev.html"), 'force', 'verbose');
    Copy(join(book, "maplev.pdf"),  join(dstdir, "maplev.pdf"),  'force', 'verbose');

    #}}}
    #{{{ Binary file

    printf("\nextracting pmaple binary file\n");

    dstdir := join(tboxdir, "bin");

    if not FT:-Exists(dstdir) then
        FT:-MakeDirectory(dstdir, 'recurse');
    end if;

    platform := kernelopts('platform');

    if   platform = "unix"    then binfile := "pmaple";
    elif platform = "windows" then binfile := "pmaple.exe";
    else
    end if;

    src := join(book, binfile);
    dst := join(dstdir, binfile);

    Copy(src, dst, 'force', 'verbose',NULL);

    if platform = "unix" then
        status := ssystem(sprintf("chmod +x %s", dst));
        if not status[1] = 0 then
            WARNING("could not make binary file %1 executable", dst);
        end if;
    end if;

    pmaple := dst;

    #}}}
    #{{{ Tar file

    printf("\nextracting tar file\n");

    src := FT:-ListDirectory(book, 'returnonly' = "*.tar")[1];

    dst := join(tboxdir, src);
    src := join(book, src);
    Copy(src, dst, 'force', 'verbose');

    #}}}
    #{{{ Emacs initialization

    # Print elisp code that can be used in the user's Emacs initialization file.

    (bindir,mapledir) := kernelopts(':-bindir',':-mapledir');
    mint   := join(bindir, "mint");
    cmaple := join(bindir, "cmaple");
    if platform = "windows" then
        mint := cat(mint,".exe");
        cmaple := cat(cmaple,".exe");
    end if;

    if not FileTools:-Exists(mint)   then mint := 'nil'; end if;
    if not FileTools:-Exists(cmaple) then cmaple := 'nil'; end if;

    printf("\n\nUse the following lisp code in your Emacs initialization file:\n\n"
           ";; Open files with extension .mpl with maplev-mode\n"
           "(setq auto-mode-alist (cons `(\"\\\\.mpl\\\\'\" . maplev-mode) auto-mode-alist))\n"
           "\n"
           ";; Assign maplev-config-default; it can also be customized with M-x customize-group RET maplev\n"
           "(eval-after-load 'maplev-config\n"
           "  '(setq maplev-config-default\n"
           "       (make-instance 'maplev-config-class\n"
           "                      :bindir   %a\n"
           "                      :mapledir %a\n"
           "                      :maple    %a\n"
           "                      :mint     %a\n"
           "                      :pmaple   %a)))\n"
           , bindir
           , mapledir
           , cmaple
           , mint
           , pmaple
          );

    #}}}


    return NULL;

end proc:
