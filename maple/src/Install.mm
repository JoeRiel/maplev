#LINK maplev.mpl
##INCLUDE ../include/mpldoc_macros.mpi
##PROCEDURE(help) maplev[Install]
##HALFLINE install the lisp and info files for MapleV
##AUTHOR   Joe Riel
##DATE     Feb 2017
##CALLINGSEQUENCE
##- Install('opts')
##DESCRIPTION
##- The `Install` command installs the "Emacs" lisp and info files for MapleV.
##  It also byte compiles the lisp files.
##
##- The lisp files are extracted from the Maple workbook to ~$HOME/maple/toolbox/maplev/lisp/~,
##  byte-compiled, then copied to `lispdir`.
##
##- The info file is extracted from the Maple workbook to ~$HOME/maple/toolbox/maplev/info/~,
##  then copied to `infodir`.
##
##- If the final installation fails, you can always manually
##  copy the files from their extracted locations to where
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
local book, cmd, dir, dst, dstdir, elfiles, extractlispdir, file, result, src, srcdir, tboxdir;
uses FT = FileTools;

    tboxdir := kernelopts('toolboxdir' = 'maplev');

    book := sprintf("maple://%s/lib/maplev.maple", tboxdir);

    #{{{ Lisp Files

    #{{{ (*) extract lisp files

    srcdir := cat(book, "/lisp");
    elfiles := FT:-ListDirectory(srcdir, 'returnonly' = "*.el");

    extractlispdir := cat(tboxdir, "/lisp");

    if not FT:-Exists(extractlispdir) then
        FT:-MakeDirectory(extractlispdir, 'recurse');
    end if;

    for file in elfiles do
        src := cat(srcdir, "/", file);
        dst := cat(extractlispdir, "/", file);
        FT:-Copy(src, dst, 'force');
    end do;

    #}}}
    #{{{ (*) byte-compile

    if bytecompile then

        # Use elisp symbol-name to avoid having to quote the directory
        dir := sprintf("(symbol-name '%A)", lispdir);

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

    dstdir := FT:-JoinPath([kernelopts("homedir"), ".emacs.d", "maple"]);

    # ensure destination directory exists
    if not FT:-Exists(lispdir) then
        FT:-MakeDirectory(lispdir, 'recurse');
    end if;

    printf("Copying lisp files to %s\n", dstdir);

    for file in FT:-ListDirectory(extractlispdir) do
        src := cat(extractlispdir, "/", file);
        dst := cat(dstdir, "/", file);
        FT:-Copy(src, dst, 'force');
    end do;

    #}}}

    #}}}
    #{{{ Info File

    #{{{ (*) extract info file

    dstdir := cat(tboxdir, "/info");

    if not FT:-Exists(dstdir) then
        FT:-MakeDirectory(dstdir, 'recurse');
    end if;

    FT:-Copy(cat(book, "/maplev.info"), cat(dstdir, "/maplev.info"));

    #}}}
    #{{{ (*) install info file

    if not FT:-Exists(infodir) then
        FT:-MakeDirectory(infodir, 'recurse');
    end if;
    src := cat(tboxdir, "/info/maplev.info");
    dst := FT:-JoinPath([infodir, "maplev.info"]);
    FT:-Copy(src, dst, 'force');

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

end proc:
