# Maple script to create emaple and/or emaple.bat

proc()
local bindir, platform, pmaple, template;

    platform := kernelopts(':-platform');
    bindir := kernelopts(':-bindir');

    if platform = "unix" then

        template := ( "#!/bin/bash\n\n"
                      "# This script launches the pmaple executable, which\n"
                      "# uses OpenMaple to access the Maple engine.\n\n"
                      "export LD_LIBRARY_PATH=\"%s:$LD_LIBRARY_PATH\"\n"
                      "\n"
                      "%s \"$@\"\n"
                    );
        pmaple := FileTools:-JoinPath([kernelopts(':-homedir'), ".emacs.d", "maple", "bin", "pmaple"]);

    elif platform = "windows" then

        template := ( "@echo off\n\n"
                      "REM This script launches the pmaple executable, which\n"
                      "REM uses OpenMaple to access the Maple engine.\n\n"
                      "set PATH=\"%s;%%PATH%%\"\n"
                      "%s %%*"
                    );
        pmaple := FileTools:-JoinPath([kernelopts(':-homedir'), ".emacs.d", "maple", "bin", "pmaple.exe"]);

    end if;

    printf(template
           , bindir
           , pmaple
          );

end proc():
