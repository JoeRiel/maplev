# Example:
#
# MakeBook("maplev.maple", "maplev.mla", ..., "lisp" = "lisp/*.el", ... );

MakeBook := proc(book :: string)
local dest, eqs, files, i, spec, src;
    for i to _nrest do
        spec := _rest[i];
        if spec :: string then
            eqs[i] := GetFiles(spec);
        elif spec :: (string = {string,list}) then
            (dest,src) := op(spec);
            files := [GetFiles(src)];
            eqs[i] := op(map(proc(path)
                             local file := FileTools:-Filename(path);
                                 FileTools:-JoinPath([dest,file]) = path;
                             end proc, files));
        else
            error "unexpected spec: %1", spec;
        end if;
    end do;

    eqs := seq(eqs[i], i=1.._nrest);

    PackageTools:-Create(book, eqs);
    Cloud:-CopyInfo(book);

end proc:

# Expand any file specs that have wildcards.

GetFiles := proc(spec :: {string,list})
local dir, file;
    if spec :: list then
        seq(thisproc(file), file=spec);
    elif StringTools:-RegMatch("[*?[]", spec) then
        # handle wildcard
        dir := FileTools:-ParentDirectory(spec);
        file := FileTools:-Filename(spec);
        op(FileTools:-ListDirectory(dir, 'returnonly' = file, 'absolute'));
    else
        FileTools:-AbsolutePath(spec);
    end if;
end proc:


