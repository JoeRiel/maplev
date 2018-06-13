#LINK maplev.mpl
##INCLUDE ../include/mpldoc_macros.mpi
##PROCEDURE(help) maplev[Install]
##HALFLINE install the lisp and info files for MapleV
##AUTHOR   Joe Riel
##DATE     Feb 2017
##CALLINGSEQUENCE
##- Install('opts')
##DESCRIPTION
##- The `Install` command installs the lisp, info, and binary files for MapleV.
##  It also byte compiles the lisp files.
##
##- The lisp files are extracted from the Maple workbook to ~$HOME/maple/toolbox/maplev/lisp/~,
##  byte-compiled, then copied to `lispdir`.
##
##- The info file is extracted from the Maple workbook to ~$HOME/maple/toolbox/maplev/info/~,
##  then copied to `infodir`.
##
##- If the final installation fails,
##  manually copy the files from their extracted locations to where
##  they need to be.
##
##OPTIONS
##opt(bytecompile,truefalse)
##  True means byte-compile the lisp files.
##  The default is true.
##opt(emacs,string)
##  The Emacs executable.
##  This is needed when 'bytecompile' is true.
##  The default is ~emacs~.
##opt(installinfo,string)
##  The command to update the ~dir~ node for ~info~.
##  The default is ~ginstall-info~.
##opt(infodir,string)
##  The directory into which ~maplev.info~ is installed.
##  The file, ~dir~, that is updated to include the MapleV topic
##  is located in this directory.
##  The default is ~$HOME/share/info~.
##opt(lispdir,string)
##  The directory into which the lisp files are installed.
##  The default is ~$HOME/.emacs.d/maple~.
##EXAMPLES(notest,noexecute)
##> maplev:-Install();
##
##XREFMAP
##- "Emacs" : Help:www.gnu.org/software/emacs
##
##SEEALSO
##- "maplev"


#LINK maplev.mpl

Install := proc( { bytecompile :: truefalse := true }
                 , { emacs :: string := "emacs" }
                 , { installinfo :: string := "ginstall-info" }
                 , { infodir :: string := FileTools:-JoinPath([kernelopts("homedir"), "share", "info"]) }
                 , { lispdir :: string := FileTools:-JoinPath([kernelopts("homedir"), ".emacs.d", "maple"]) }
                 , $
               )
local bindir, binfile, book, cmd, dir, dst, dstdir, elfiles, extractlispdir, file
    , platform, result, src, srcdir, status, tboxdir
    ;
uses FT = FileTools;

    tboxdir := kernelopts('toolboxdir' = 'maplev');

    book := FileTools:-JoinPath([tboxdir, "lib", "maplev.maple"]);
    if not FT:-Exists(book) then
        error "Maple book %1 does not exist", book;
    end if;

    book := sprintf("maple://%s", book);

    #{{{ Lisp Files

    #{{{ (*) extract lisp files

    printf("extracting lisp files\n");

    srcdir := cat(book, "/lisp");

    elfiles := FT:-ListDirectory(srcdir, 'returnonly' = "*.el");

    extractlispdir := cat(tboxdir, "/lisp");

    if not FT:-Exists(extractlispdir) then
        FT:-MakeDirectory(extractlispdir, 'recurse');
    end if;

    for file in elfiles do
        src := cat(srcdir, "/", file);
        dst := cat(extractlispdir, "/", file);
        Copy(src, dst, 'force', 'verbose');
    end do;

    #}}}
    #{{{ (*) byte-compile

    if bytecompile then

        printf("\nbyte compiling\n");

        # Use elisp symbol-name to avoid having to quote the directory
        dir := sprintf("(symbol-name '%A)", extractlispdir);

        cmd := sprintf("%s --batch --no-site-file --no-init-file "
                       "--eval \"(push %s load-path)\" "
                       "--eval \"(byte-recompile-directory %s 0)\""
                       , emacs
                       , dir
                       , dir
                      );

        result := ssystem(cmd);
        if result[1] <> 0 then
            WARNING("problem byte-compiling lisp files:\n%1"
                    , result[2]
                   );
        end if;
    end if;

    #}}}
    #{{{ (*) copy to destination

    printf("installing lisp files\n");

    dstdir := lispdir;

    # ensure destination directory exists
    if not FT:-Exists(lispdir) then
        FT:-MakeDirectory(lispdir, 'recurse');
    end if;

    for file in FT:-ListDirectory(extractlispdir) do
        src := cat(extractlispdir, "/", file);
        dst := cat(dstdir, "/", file);
        Copy(src, dst, 'force', 'verbose', NULL);
    end do;

    #}}}

    #}}}
    #{{{ Info File

    #{{{ (*) extract info, html, and pdf files

    printf("\nunpacking info file\n");

    dstdir := cat(tboxdir, "/info");

    if not FT:-Exists(dstdir) then
        FT:-MakeDirectory(dstdir, 'recurse');
    end if;

    Copy(cat(book, "/maplev.info"), cat(dstdir, "/maplev.info"), 'force', 'verbose');
    Copy(cat(book, "/maplev.html"), cat(dstdir, "/maplev.html"), 'force', 'verbose');
    Copy(cat(book, "/maplev.pdf"),  cat(dstdir, "/maplev.pdf"),  'force', 'verbose');

    #}}}
    #{{{ (*) install info file

    printf("\ninstalling info file\n");

    if not FT:-Exists(infodir) then
        FT:-MakeDirectory(infodir, 'recurse');
    end if;
    src := cat(tboxdir, "/info/maplev.info");
    dst := FT:-JoinPath([infodir, "maplev.info"]);
    Copy(src, dst, 'force', 'verbose',NULL,NULL);

    #}}}
    #{{{ (*) update dir file

    if kernelopts('platform') = "unix" then
        printf("\nUpdating info dir node...\n");
        try
            cmd := sprintf("%s --dir-file=%s/dir %s"
                           , installinfo
                           , infodir
                           , dst
                          );
            result := ssystem(cmd);
            if result[1] <> 0 then
                error "problem executing '%1':/n%2", cmd, result[2];
            end if;
        catch:
            WARNING("problem updating the dir file. "
                    "The following error occured:\n%1"
                    , result[2]
                   );
        end try;
    else
        WARNING("the dir file, used by Emacs help system to "
                "provide a menu of help topics, was not updated. "
                "You can do so manually. If you cannot figure out how to do so, "
                "an html version of the documentation for mds "
                "(the Emacs-based Maple Debugger Server) "
                "is provided in the doc subdirectory of the installation."
               );
    end if;

    #}}}
    #}}}
    #{{{ Binary Files

    #{{{ (*) extract pmaple binary

    printf("\nunpacking binary\n");

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

    Copy(src, dst, 'force', 'verbose',NULL,NULL,NULL);

    if platform = "unix" then
        status := ssystem(sprintf("chmod +x %s", dst));
        if not status[1] = 0 then
            WARNING("could not make binary file %1 executable", dst);
        end if;
    end if;

    #}}}
    #{{{ (*) install pmaple binary

    printf("\ninstalling binary\n");

    bindir := FT:-JoinPath([lispdir, "bin"]);
    if not FT:-Exists(bindir) then
        FT:-MakeDirectory(bindir, 'recurse');
    end if;

    src := dst;  # reuse last dst
    dst := FT:-JoinPath([bindir, binfile]);
    Copy(src, dst, 'force', 'verbose',NULL,NULL,NULL,NULL);

    if platform = "unix" then
        status := ssystem(sprintf("chmod +x %s", dst));
        if not status[1] = 0 then
            WARNING("could not make binary file %1 executable", dst);
        end if;
    end if;

    #}}}


    #}}}

    return NULL;

end proc:
