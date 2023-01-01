#LINK maplev.mpl

Plot := module()

local opts := Record("height" = 400,
                     "width"  = 600,
                     "embed"  = false,
                     "viewer" = "display"
                    );

$ifdef MINTONLY
$ifndef MAIN
local pmaple_buffer := "";
$endif
$endif


export
    ModuleLoad := proc()
        kernelopts( ':-headlessawt' = false, ':-restartjvm' = false );
        NULL;
    end proc;

export
    ModuleUnload := proc()
        if pmaple_buffer <> "unknown" then
            # remove images from pmaple buffer
            local lisp := sprintf("(with-current-buffer %a (maplev-pmaple-remove-images))"
                                  , pmaple_buffer);
            Emacs(lisp);
        end if;

    end proc;


##INCLUDE ../include/mpldoc_macros.mpi
##PROCEDURE(label = "PlotOptions") maplev:-Plot:-Options
##HALFLINE set maplev plot options
##INDEXPAGE PKG[Exports],PROC,set maplev plot options
##CALLINGSEQUENCE
##- maplev:-Plot:-Options()
##RETURNS
##- equations
##OPTIONS
##opt(height,posint)
##  The default pixel height of the plot image.
##  This value is overridden by the same option to "Plot".
##  The default value is 400.
##opt(width,posint)
##  The default pixel width of the plot image.
##  This value is overridden by the same option to "Plot".
##  The default value is 400.
##opt(embed,truefalse)
##  True means that the image created by "Plot" is embedded in the buffer
##  unless overriddent by the the same option to "Plot".
##  The default is false.
##opt(viewer,string)
##  The utility used to view non-embedded images.
##  This is used when ~kernelopts(platform) = "unix"~.
##  The default is ~"display"~.
##DESCRIPTION
##- The `Options` command
##  sets options used by the "maplev:-Plot" command.
##  It returns an expression sequence of equations of the values of all options,
##  after applying any changes.
##EXAMPLE
##> maplev:-Plot:-Options();
##> maplev:-Plot:-Options('embed' = true);
##SEEALSO
##- "maplev"
##- "Plot"
##XREFMAP
##- "Plot" : Help:maplev,Plot
##TEST
## $include <include/test_macros.mi>
## AssignFUNC(Plot:-Options):
### mdc(FUNC):
##
## Try("1.1", FUNC(), height = 400, width = 600, embed = false, viewer = "display" );
## Try("1.2", FUNC(width = 700), height = 400, width = 700, embed = false, viewer = "display" );

export
    Options := proc({  height :: posint := opts:-height }
                       , { width :: posint := opts:-width }
                       , { embed :: truefalse := opts:-embed }
                       , { viewer :: string := opts:-viewer }
                       , $
                   )
    option threadlock;

        opts:-height  := height;
        opts:-width   := width;
        opts:-embed   := embed;
        opts:-viewer  := viewer;

        ( ':-height' = opts:-height
          , ':-width' = opts:-width
          , ':-embed' = opts:-embed
          , ':-viewer' = opts:-viewer

        );

    end proc;


##PROCEDURE(label="Plot") maplev:-Plot
##HALFLINE display a plot
##INDEXPAGE PKG[Exports],PROC,display a plot
##CALLINGSEQUENCE
##- maplev:-Plot('plt', 'opts')
##PARAMETERS
##- 'plt'  : plot
##opts
##RETURNS
##- ::string::; path to generated png
##OPTIONS
##opt(plotfile,string)
##opt(embed,truefalse)
##  True means embed the image into the buffer.
##  The default is the value set by "Options".
##opt(height,posint)
##  The pixel height of the image.
##  The default is the value set by "Options".
##opt(width,posint)
##  The pixel width of the image.
##  The default is the value set by "Options".
##DESCRIPTION
##- The `Plot` command
##  generates and displays an image of a plot.
##
##- The 'plt' parameter is the plot structure to display.
##  If 'plt' is not a plot structure,
##  the call is equivalent to ~Plot(plot(plt, _rest), _options)~.
##
##EXAMPLES(noexecute,notest)
##> with(maplev):
##- Create a plot and save the plot structure.
##> plt := plot(sin(x), x = 0..2*Pi):
##- Call `Plot` to display the plot.
##> Plot(plt):
##
##- The previous commands are equivalent to
##
##> Plot(sin(x), x = 0..2*Pi):
##
##- Embed a plot into the pmaple buffer.
##
##> Plot(cos, 0..3*Pi, 'embed'):
##
##SEEALSO
##- "maplev"
##- "plot"
##- "maplev:-Plot:-Options"
##
##ENDMPLDOC

export
    ModuleApply := proc(plt?
                        , { plotfile :: string := "DEFAULT" }
                        , { embed :: truefalse := opts:-embed }
                        , { height :: posint := opts:-height }
                        , { width  :: posint := opts:-width  }
                       )
    local plt, pltfile, tmpdir;
    option threadlock;

        if embed and pmaple_buffer = "unknown" then
            error "the 'embed' option can only be used in an Emacs pmaple buffer"
        end if;

        if plotfile = "DEFAULT" then
            # Assign temporary file $TMPDIR/MapleVPlots/*.png
            tmpdir  := FileTools:-JoinPath([FileTools:-TemporaryDirectory()
                                            , "MapleVPlots"]);
            if not FileTools:-Exists(tmpdir) then
                FileTools:-MakeDirectory(tmpdir);
            end if;
            pltfile := FileTools:-TemporaryFilename("",".png");
            pltfile := FileTools:-JoinPath([tmpdir, pltfile]);
        else
            pltfile := plotfile;
        end if;

        if plt? :: 'specfunc({INTERFACE_PLOT,INTERFACE_PLOT3D,PLOT,PLOT3D,_PLOTARRAY})' then
            plt := plt?;
        else
            plt := plot(plt?, _rest);
        end if;


        Export(pltfile, plt
               , ':-format' = "png"
               , _options['height','width']
               , ':-legacy' = false
              );

        if embed then
            local lisp := sprintf("(maplev-pmaple-insert-image %a %a)", pltfile, pmaple_buffer);
            Emacs(lisp);
        else
            if kernelopts('platform') = "unix" then
                system['launch'](opts:-viewer, pltfile);
            else # windows
                local cmd := sprintf("cmd /c %a", pltfile);
                ssystem(cmd);
            end if;
        end if;

        pltfile;

    end proc;

end module:
