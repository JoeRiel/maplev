#LINK ../src/mdc.mpl


$define EMACS_PKG "maplev"
$define TOOLBOX maplev

Install := module()

local Copy;

$include <maple/Install/Copy.mm>

export
    ModuleApply := proc( { doc :: truefalse := false }
                         , { emacs :: truefalse := false }
                         , { emacs_init :: truefalse := false }
                         , { maple :: truefalse := false }
                       )

    local book, dir, dst, file, files, lisp, numchars, numlines, pixheight, pixwidth, src, tboxdir;

    uses  FT = FileTools
        , JoinPath = FileTools:-JoinPath
        ;

        tboxdir := kernelopts('toolboxdir' = 'TOOLBOX');

        book := JoinPath([tboxdir, "lib", sprintf("%a.maple", 'TOOLBOX')]);

        if not FT:-Exists(book) then
            error "Maple book %1 does not exist", book;
        end if;

        book := sprintf("maple://%s", book);

        #{{{ doc

        if doc then

            printf("\nextracting the doc files\n");

            dir := JoinPath([book, "doc"]);
            files := FT:-ListDirectory(dir);

            for file in files do
                dst := JoinPath([tboxdir, file]);
                src := JoinPath([book, file]);
                Copy(src, dst, 'force', 'verbose');
            end do;

        end if;

        #}}}
        #{{{ maple

        if maple then

            printf("\nextracting maple source files\n");

            dir := JoinPath([book, "maple"]);
            files := FT:-ListDirectory(dir, 'recurse');

            for file in files do
                dst := JoinPath([tboxdir, file]);
                src := JoinPath([book, file]);
                Copy(src, dst, 'force', 'verbose');
            end do;

        end if;

        #}}}
        #{{{ emacs

        if emacs then

            printf("\nextracting tar file\n");

            src := FT:-ListDirectory(book, 'returnonly' = "*.tar");
            if src = [] then
                error "missing tar file";
            else
                src := src[1];
            end if;

            dst := JoinPath([tboxdir, src]);
            src := JoinPath([book, src]);
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

        end if;

        #}}}


        return NULL;

    end proc;


end module:

$undef EMACS_PKG
$undef TOOLBOX
