MakeBook := proc()
local eqs, file, lispfiles;

uses FT = FileTools;

    lispfiles := FT:-ListDirectory("lisp", 'returnonly' = "*.el");
    eqs := seq(`=`(FT:-JoinPath(["lisp",file])$2), file = lispfiles);

    PackageTools:-Create(_rest, eqs);

end proc:
