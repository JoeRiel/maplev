#LINK maplev.mpl

Emacs := proc(lisp :: string)

local cmd, result;

    if pmaple_buffer <> "unknown" then
        cmd := sprintf("emacsclient --eval '%s'", lisp);
        result := ssystem(cmd);
        if result[1] <> 0 then
            error "problem contacting emacs: %1", result[2];
        end if;
    end if;

    NULL;

end proc;

