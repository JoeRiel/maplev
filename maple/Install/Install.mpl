#LINK ../src/mdc.mpl


$define EMACS_PKG "maplev"
$define TOOLBOX maplev

Install := module()

local Copy;

$include <Install/Copy.mm>

export
    ModuleApply := proc( { data :: truefalse := false }
                         , { emacs :: truefalse := false }
                         , { emacs_init :: truefalse := false }
                       )

    local bindir, book, dir, dst, file, join, lisp, maple, mapledir, mint, numchars, numlines, pixheight, pixwidth, platform, pmaple, src, systype, tboxdir;

    uses FT = FileTools;

        join := proc()
            FileTools:-JoinPath([_passed]);
        end proc;

        tboxdir := kernelopts('toolboxdir' = 'TOOLBOX');

        book := join(tboxdir, "lib", sprintf("%a.maple", 'TOOLBOX'));

        if not FT:-Exists(book) then
            error "Maple book %1 does not exist", book;
        end if;

        book := sprintf("maple://%s", book);

        #{{{ data

        if data then

            printf("\nextracting data\n");

            src := FT:-ListDirectory(FT:-JoinPath([book, "data"]), 'returnonly' = "*.mpl");
            if src = [] then
                error "no data found";
            end if;
            dir := join(tboxdir, "data");
            if not FT:-Exists(dir) then
                FT:-MakeDirectory(dir);
            end if;
            for file in src do
                dst  := join(dir, file);
                file := join(book, "data", file);
                Copy(file, dst, 'force', 'verbose');
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

            dst := join(tboxdir, src);
            src := join(book, src);
            Copy(src, dst, 'force', 'verbose');

        end if;

        #}}}


        #{{{ emacs_init

        if emacs_init then

            (bindir,mapledir,platform) := kernelopts(':-bindir',':-mapledir',':-platform');

            systype := FileTools:-Filename(kernelopts('bindir'));

            maple  := join(bindir, "cmaple");
            mint   := join(bindir, "mint");
            pmaple := join(kernelopts('toolboxdir' = 'maplev'), systype, "pmaple");

            if platform = "windows" then
                maple  := cat(maple , ".exe");
                mint   := cat(mint   , ".exe");
                pmaple := cat(pmaple , ".exe");
            elif platform = "unix" then
                # use scripts so environment is properly set
                for file in ["maple", "smaple"] do
                    file := join(mapledir, "bin", file);
                    if FileTools:-Exists(file) then
                        maple := file;
                        break;
                    end if;
                end do;
            end if;

            if not FileTools:-Exists(maple)  then maple  := 'nil'; end if;
            if not FileTools:-Exists(pmaple) then pmaple := 'nil'; end if;
            if not FileTools:-Exists(mint)   then mint   := 'nil'; end if;

            lisp := sprintf(";; Open files with extension .mpl with maplev-mode\n"
                            "(add-to-list 'auto-mode-alist `(\"\\\\.mpl\\\\'\" . maplev-mode))\n"
                            "\n"
                            ";; Assign maplev-config-default; it can also be customized with M-x customize-group RET maplev\n"
                            "(eval-after-load 'maplev-config\n"
                            "  '(setq maplev-config-default\n"
                            "       (make-instance 'maplev-config-class\n"
                            "                      :bindir   %a\n"
                            "                      :mapledir %a\n"
                            "                      :maple    %a\n"
                            "                      :mint     %a\n"
                            "                      :pmaple   %a)))"
                            , bindir
                            , mapledir
                            , maple
                            , mint
                            , pmaple
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
