#{{{ InstallScript

InstallScript := proc()
global Installer;
    Installer := module()

    export ByteCompile
        ,  ModuleApply
        ,  ModuleLoad
        ;

    local Install
        , MakePath
        , ToolboxDir
        , UpdateDir := UpdateDir
        ;

    uses %FT = FileTools, %ST = StringTools;

        #{{{ ByteCompile

        ByteCompile := proc()

        global Emacs, LispDir;

        local cmd, dir, elfiles, lispDir, result, srcdir;

            try
                printf("\nByte-compiling lisp files...\n");

                srcdir := MakePath(ToolboxDir, "lisp");
                elfiles := %FT:-ListDirectory(srcdir,'returnonly'="*.el");
                elfiles := map[3](cat, LispDir, kernelopts('dirsep'), elfiles);

                # Emacs requires forward-quotes
                if kernelopts('platform') <> "unix" then
                    lispDir := %ST:-SubstituteAll(LispDir, "\\", "/");
                else
                    lispDir := LispDir;
                end if;

                if SearchText(" ",lispDir) <> 0 then
                    error "cannot handle directory name with spaces: '%1'", lispDir;
                end if;

                # Use elisp symbol-name to avoid having to quote the directory
                dir := sprintf("(symbol-name '%A)", lispDir);

                cmd := sprintf("%s --batch --no-site-file --no-init-file "
                               "--eval \"(push %s load-path)\" "
                               "--eval \"(byte-recompile-directory %s 0)\""
                               , Emacs
                               , dir
                               , dir
                              );

                printf("%s\n", cmd);
                result := ssystem(cmd);
                if result[1] <> 0 then
                    WARNING("problem byte-compiling lisp files:\n%1"
                            , result[2]
                           );
                end if;
            catch:
                WARNING("the lisp files were not automatically byte-compiled. "
                        "Byte-compiling is not a requirement, but will "
                        "allow the code to run faster."
                       );
                error;
            end try;

        end proc;

        #}}}

        #{{{ Install
        Install := proc(srcdir, dstdir, files :: list, { clear :: truefalse := false } )
        local content, file, src, dst;
        uses %FT=FileTools;
            if not %FT:-Exists(dstdir) then
                printf("Creating directory %s\n", dstdir);
                %FT:-MakeDirectory(dstdir, 'recurse'=true);
            elif clear then
                content := %FT:-ListDirectory(dstdir);
                for file in content do
                    file := MakePath(dstdir,file);
                    printf("Deleting file %s\n", file);
                    %FT:-Remove(file);
                end do;
            end if;
            for file in files do
                src := MakePath(srcdir,file);
                dst := MakePath(dstdir,file);
                printf("Copying %s --> %s\n", src, dst);
                %FT:-Copy(src,dst,'force'=true);
            end do;
        end proc;
        #}}}
        #{{{ MakePath

        MakePath := proc()
            StringTools:-Join([_passed], kernelopts(':-dirsep'));
        end proc;

        #}}}

        #{{{ ModuleLoad

        ModuleLoad := proc()
        local is_system, pdir;
            is_system := ToolboxInstaller:-Data:-Get("system_installation");
            pdir := ToolboxInstaller:-Tools:-GetInstallDirectory(is_system);
            ToolboxDir := MakePath(pdir, ToolboxInstaller:-Data:-Get("toolbox_name"));
        end proc;

        #}}}
        #{{{ ModuleApply
        ModuleApply := proc()
        local cmd
            , config
            , elfiles
            , file
            , result
            , src
            , srcdir
            ;
        global Emacs, LispDir, InfoDir, DirFile, MapleLib, UpdateDir;

            #{{{ Assign Defaults

            # These global variables may be overridden by the config.mpl assignments
            # in the next section

            Emacs    := "emacs";
            MapleLib := MakePath(kernelopts('homedir'), "maple", "toolbox", "emacs", "lib");
            InfoDir  := MakePath(kernelopts('homedir'), "share", "info");
            DirFile  := MakePath(InfoDir, "dir");

            #}}}
            #{{{ Read configuration file
            config := MakePath(ToolboxDir, "config.mpl");
            if FileTools:-Exists(config) then
                printf("Reading configuration file %s\n", config);
                read config;
            else
                printf("Configuration file %s does not exist, using default locations.\n", config);
            end if;
            #}}}
            #{{{ Install Maple files

            if assigned(MapleLib) then
                srcdir := MakePath(ToolboxDir,"lib");
                if MapleLib <> srcdir then
                    printf("\nInstalling Maple files...\n");
                    Install(srcdir, MapleLib, ["mdc.mla", "mdc.hdb", "mdc.help"]);
                end if;
            end if;

            #}}}
            #{{{ Install lisp files

            printf("\nInstalling lisp files...\n");

            # Use emacs to extract user-emacs-directory (typically ~/.emacs.d)
            cmd := cat(Emacs, ( " --batch --no-init-file --no-site-file "
                                "--eval \"(princ (file-truename user-emacs-directory))\""));
            result := ssystem(cmd);
            if result[1] <> 0 then
                WARNING("problem executing '%1':\n%s", cmd, result[2]);
                LispDir := MakePath(kernelopts('homedir'),"/.emacs.d/maple");
            else
                LispDir := cat(result[2], "maple");
            end if;

            srcdir := MakePath(ToolboxDir, "lisp");
            elfiles := %FT:-ListDirectory(srcdir,'returnonly'="*.el");
            Install(srcdir, LispDir, elfiles);

            #}}}
            #{{{ Byte-compile lisp files

            ByteCompile();

            #}}}
            #{{{ Install info files

            printf("\nInstalling info files...\n");
            Install(MakePath(ToolboxDir, "info"), InfoDir, ["maplev.info"]);

            #}}}
            #{{{ Update dir node

            if kernelopts('platform') = "unix" then
                printf("\nUpdating info dir node...\n");
                try
                    for file in ["mds.info","maplev.info"] do
                        src := MakePath(ToolboxDir, "info", file);
                        UpdateDir(DirFile, src);
                    end do;
                catch:
                    WARNING("problem updating the dir node. "
                            "Edit config file '%1', or update the dir node manually; "
                            "see README-Installer.  The following error occured:\n%2"
                            , config
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

            NULL;

        end proc:
        #}}}
        #{{{ UpdateDir
        UpdateDir := proc(dirfile::string, file::string)
        local cmd,result;
            cmd := sprintf("ginstall-info --dir-file=%s %s"
                           , dirfile
                           , file
                          );
            result := ssystem(cmd);
            if result[1] <> 0 then
                error "problem executing '%1':/n%2", cmd, result[2];
            end if;
        end proc;
        #}}}

        ModuleLoad();

    end module:
    Installer();
end proc:
#}}}
#{{{ CreateInstaller

CreateInstaller := proc()

local file, installer, manifest, vers;
global InstallScript;
uses %FT = FileTools;

    vers := ssystem("git describe --match release\\* --abbrev=0 HEAD");
    vers := vers[2][9..-2];

    installer := sprintf("maplev-installer-%s.mla", vers);

    if %FT:-Exists(installer) then
        %FT:-Remove(installer);
    end if;

    manifest := [NULL
                 (* source = target *)
                 , "maplev.mla" = "lib/maplev.mla"

                 (* lisp files *)
                 , seq(`=`(cat("lisp/", file)$2)
                       , file = %FT:-ListDirectory("lisp", 'returnonly' = "*.el"))
                 (* doc *)
                 , "doc/maplev.info" = "info/maplev.info"
                 , "doc/maplev.html" = "doc/maplev.html"
                 , "doc/maplev.pdf"  = "doc/maplev.pdf"

                 #, ".emacs" = ".emacs"

                 # , "maple/installer/config.mpl" = "_config.mpl"

                ];


    InstallerBuilder:-Build(
        "emacs"
        , ':-target' = installer
        , ':-version' = vers
        , ':-author' = "Joe Riel"
        , ':-welcome' = ['text' = cat(sprintf("Welcome to the installer for "
                                              "MapleV, version %s.\n"
                                              , vers)
                                      , "\n"
                                      , ( "This installer unpacks the Maple archive "
                                          "and help database to a folder under the user's HOME directory."
                                        )
                                     )
                        ]
        , ':-manifest' = manifest
        , ':-installation' = [NULL
                              , 'text' = ( "Installing the Lisp and Info files...\n"
                                           "\n"
                                           "The default locations are\n"
                                           "  lisp files --> $HOME/.emacs.d/maple\n"
                                           "  info files --> $HOME/share/info\n"
                                           "where $HOME = kernelopts('homedir').\n"
                                           "\n"
                                           "To change these defaults, stop the installation,\n"
                                           "rename the file _config.mpl to config.mpl,\n"
                                           "edit it appropriately, and restart the installation.\n"
                                           "See the file README-installer for details."
                                         )
                              , 'script' = eval(InstallScript)
                             ]
        , ':-finish' = [NULL
                        , 'text' = ("To finish the installation, see the README-installer file.\n\n")
                        , 'script' = proc()
                                         printf("\n"
                                                "For help with the mode, look at the maplev node "
                                                "in info.  Pdf and html versions of the maplev documentation "
                                                "is available in $HOME/maple/toolbox/doc.\n"
                                               );
                                     end proc
                       ]

        , ':-width' = 1000 );
end proc:

#}}}

(CreateInstaller)();
