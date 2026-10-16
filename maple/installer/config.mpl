# Optional configuration file used with maplev-installer.

VERSION := "3.2.4":

# To change the default locations where the installer
# installs files, rename this file to config.mpl
# and modify it accordingly.

# Assign a utility procedure for generating path names.
MakePath := () -> StringTools:-Join([_passed], kernelopts(':-dirsep')):

# Define the locations.  The values assigned here correspond to the
# default locations, but this file is not used unless it has been
# renamed as described above.

# MapleLib : directory where *.mla and *.help files are stored (~/maple/toolbox/maplev)
# LispDir  : directory where maple lisp files are stored (~/.emacs.d/elpa
# InfoDir  : directory where info files are stored (~/share/info/)
# DirFile  : file used to update the dir node for the info system (~/share/info/dir)

MapleLib := MakePath(kernelopts('homedir'), "maple", "toolbox", "maplev", "lib"):
LispDir  := MakePath(kernelopts('homedir'), ".emacs.d", "elpa", concat("maplev-",VERSION)):
InfoDir  := MakePath(kernelopts('homedir'), "share", "info"):
DirFile  := MakePath(InfoDir, "dir"):

# Emacs executable (used to byte-compile the lisp files)
Emacs := "emacs":

# Procedure to update dir/info entries.
# This (the default) uses the O/S command ginstall-info
# to do the actual work.  If your O/S does not provide
# a command to do this, modify this procedure to do nothing,
# then update the file manually.
UpdateDir := proc(dirfile::string, file::string)
local cmd;
    cmd := sprintf("ginstall-info --dir-file=%s %s"
                   , dirfile
                   , file
                  );
    ssystem(cmd);
end proc:
