#LINK ../src/maplev.mpl


$define EMACS_PKG "maplev"
$define TOOLBOX maplev

Install := module()

local Copy;
local ToolboxDir;

uses FT = FileTools;

$include <maple/Install/Copy.mm>

##PROCEDURE Install:-ModuleApply
##CALLINGSEQUENCE
##- Install('opts')
##DESCRIPTION
##- The `Install` command installs parts of the `maplev` package.
##
##OPTIONS
##opt(binary,truefalse)
##  True means install the Maple library and help.
##  The default if false.
##opt(doc,truefalse)
##  True means install the 'doc' directory,
##  which contains a pdf and html of the package.
##  The default if false.
##opt(emacs,truefalse)
##  Unpack the tar file that contains the lisp and info files for the package.
##  The default is false.
##opt(emacs_init,truefalse)
##  Display sample elisp code that can be copied into the "Emacs initialization file"
##  to configure the package.
##  The default is false.
##opt(maple,truefalse)
##  Install the source files for the Maple package.
##  The default is false.
##opt(rebuild,truefalse)
##  Create or recreate the mla, ~lib/maplev.mla~ that contains
##  the code for ~maplev~.
##  The default is false.
##
##XREFMAP
##- "Emacs initialization file" : https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html


export
    ModuleApply := proc( { binary :: truefalse := false }
                         , { doc  :: truefalse := false }
                         , { emacs :: truefalse := false }
                         , { emacs_init :: truefalse := false }
                         , { maple :: truefalse := false }
                         , { rebuild :: truefalse := false }
                       )

    local Book;
    local cmd, dir, dst, file, files, lisp, numchars, numlines, pixheight, pixwidth, reply, src;
    global TOOLBOX;

        ToolboxDir := kernelopts('toolboxdir' = 'TOOLBOX');
        Book := FileTools:-JoinPath(["maple:/", currentdir(), "maplev.maple" ]);

        if not FT:-Exists(Book) then
            error "Maple book %1 does not exist", Book;
        end if;

        #{{{ binary

        if binary then

            # Install the system
            PackageTools:-Install("this://", 'overwrite');

            # Make pmaple executable (for linux)
            cmd := sprintf("chmod +x %s/bin.X86_64_LINUX/pmaple", ToolboxDir);
            reply := ssystem(cmd);
            if reply[1] <> 0 then
                error "problem making pmaple executable: %1", reply[2];
            end if;

        end if;

        #}}}

        #{{{ doc

        if doc then

            # Copy the doc subdirectory, with mds.pdf and mds.html, to ToolboxDir.

            printf("\nExtracting doc files\n");

            InstallDir("doc", "");

        end if;

        #}}}
        #{{{ maple

        if maple then

            # Copy the maple subdirectory subdirectories to ToolboxDir/maple

            printf("\nExtracting maple source files\n");

            InstallDir("maple", "lib");

        end if;

        #}}}
        #{{{ emacs

        if emacs then

            printf("\nExtracting tar file\n");

            src := FT:-ListDirectory(Book, 'select' = "*.tar" );

            if src = [] then
                error "missing tar file";
            else
                src := src[1];
            end if;

            dst := FT:-JoinPath([ToolboxDir, src]);
            src := cat("this:///", src);
            Copy(src, dst, 'force', 'verbose');

        end if;

        #}}}
        #{{{ emacs_init

        if emacs_init then

            lisp := ("(use-package maplev\n"
                     "  :commands maplev-mode)"
                    );

            numlines := 1 + StringTools:-CountCharacterOccurrences(lisp, "\n");
            numchars := max(map(numelems, StringTools:-Split(lisp,"\n")));
            pixheight := 20 * numlines;
            pixwidth  := 10 * numchars;

            DocumentTools:-SetProperty("emacs_init", "value", lisp);
            DocumentTools:-SetProperty("emacs_init", "pixelheight", pixheight);
            DocumentTools:-SetProperty("emacs_init", "pixelwidth", pixwidth);
            DocumentTools:-SetProperty("emacs_init", "codelanguage", `text/plain`);

        end if;

        #}}}
        #{{{ rebuild

        if rebuild then

            local mla := FT:-JoinPath([ToolboxDir, "lib", "maplev.mla"]);

            if FT:-Exists(mla) then
                FT:-Remove(mla);
            end if;

            LibraryTools:-Create(mla, 100);
            LibraryTools:-Save(maplev, mla);

        end if;

        #}}}

        return NULL;

    end proc;

    #{{{ InstallDir

    # Install, recursively, files in srcdir into dstdir.
    # The srcdir is relative to the .maplev file (this:///).

local
    InstallDir := proc(srcdir :: string, dstdir :: string)
    local dir, dst, file, files, src;

        files := FT:-ListDirectory(cat("this:///", srcdir), 'recurse');
        files := map(substring, files, 9..-1);  # remove leading this:///

        for file in files do
            dst := FT:-JoinPath([ToolboxDir, dstdir, file]);
            dir := FT:-ParentDirectory(dst);
            if not FT:-Exists(dir) then
                FT:-MakeDirectory(dir, 'recurse');
            end if;
            src := cat("this:///", file);
            Copy(src, dst, 'force', 'verbose');
        end do;
    end proc

    #}}}


end module:

$undef EMACS_PKG
$undef TOOLBOX
