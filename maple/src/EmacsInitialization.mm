#LINK maplev.mpl
##INCLUDE ../include/mpldoc_macros.mpi
##PROCEDURE(help) maplev[EmacsInitialization]
##HALFLINE print elisp code that can be used to configure maplev.
##AUTHOR   Joe Riel
##DATE     Feb 2017
##CALLINGSEQUENCE
##- maplev:-EmacsInitialization()
##RETURNS
##- `NULL`
##DESCRIPTION
##- Print elisp code that can be used to configure the **maplev** emacs package.

#LINK maplev.mpl

EmacsInitialization := proc()

local bindir, cmaple, file, join, mapledir, mint, platform, pmaple, systype;

    join := proc()
        FileTools:-JoinPath([_passed]);
    end proc;

    # Print elisp code that can be used in the user's Emacs initialization file.

    (bindir,mapledir,platform) := kernelopts(':-bindir',':-mapledir',':-platform');

    systype := FileTools:-Filename(kernelopts('bindir'));

    cmaple := join(bindir, "cmaple");
    mint   := join(bindir, "mint");
    pmaple := join(kernelopts('toolboxdir' = 'maplev'), systype, "pmaple");

    if platform = "windows" then
        cmaple := cat(cmaple , ".exe");
        mint   := cat(mint   , ".exe");
        pmaple := cat(pmaple , ".exe");
    elif platform = "unix" then
        # use scripts so environment is properly set
        for file in ["maple", "smaple"] do
            file := join(mapledir, "bin", file);
            if FileTools:-Exists(file) then
                cmaple := file;
                break;
            end if;
        end do;
    end if;

    if not FileTools:-Exists(cmaple) then cmaple := 'nil'; end if;
    if not FileTools:-Exists(pmaple) then pmaple := 'nil'; end if;
    if not FileTools:-Exists(mint)   then mint   := 'nil'; end if;

    printf(";; Open files with extension .mpl with maplev-mode\n"
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

    return NULL;

end proc:
