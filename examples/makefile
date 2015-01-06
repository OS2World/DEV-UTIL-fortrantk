# Make FORTRAN/TK examples
# wmake [ico16=|ico=] [os=OS2|WIN] [calc=] [edit=] [example=]
# if "os" is omitted, they will be compiled for OS/2
# if "calc", "edit" or "example" is specified, just these examples will
# be compiled; if not, all 3 will be compiled
# if "ico" or "ico16" is defined, the FORTRAN/TK icon is included
# under OS/2: "ico" uses the rc resource compiler (Developers Toolkit)
#             "ico16" uses rc16 (OS/2 only)
# under Windows: "ico" uses the Watcom resource compiler (wrc)

!ifndef os
!       define os OS2
!endif

!ifndef calc
!ifndef edit
!ifndef example
!       define calc
!       define edit
!       define example
!endif
!endif
!endif

!ifeq os OS2
os_spec     = __OS2__
sys_spec    = OS2V2
syswin_spec = OS2V2_PM
bin_spec    = BINOS2

!ifdef ico16
!define ico
rc_bin      = rc16
!       else
rc_bin      = rc
!endif
rcfile      = ftkicon_os2.rc
resfile     = ftkicon_os2.res

!       else ifeq os WIN

os_spec     = __WIN__
sys_spec    = NT
syswin_spec = NT_WIN
bin_spec    = BINWIN

rc_bin      = wrc
rcfile      = ftkicon_win.rc
resfile     = ftkicon_win.res

!endif

all $(bin_spec)\calc.exe    &
    $(bin_spec)\edit.exe    &
    $(bin_spec)\example.exe &
    ..\$(resfile) : .SYMBOLIC

!ifdef calc
$(bin_spec)\calc.exe : calc.for fortrantk.lib
        wfl386 $[. -"f fortrantk.lib" -FE=$(bin_spec)\calc.exe &
               -BM -DEF=$(os_spec) -L=$(syswin_spec)
!endif

!ifdef edit
$(bin_spec)\edit.exe : edit.for fortrantk.lib
        wfl386 $[. -"f fortrantk.lib" -FE=$(bin_spec)\edit.exe &
               -BM -DEF=$(os_spec) -L=$(syswin_spec)
!endif

!ifdef example
$(bin_spec)\example.exe : example.for fortrantk.lib
        wfl386 $[. -"f fortrantk.lib" -FE=$(bin_spec)\example.exe &
               -BM -DEF=$(os_spec) -L=$(sys_spec)
!endif

!ifdef ico
..\$(resfile) : ..\$(rcfile)
        cd ..
        $(rc_bin) -r $(rcfile)
!endif

.AFTER
!ifdef ico
!       ifdef calc
         $(rc_bin) $(resfile) examples\$(bin_spec)\calc.exe
!       endif
!       ifdef edit
         $(rc_bin) $(resfile) examples\$(bin_spec)\edit.exe
!       endif
!       ifdef example
         $(rc_bin) $(resfile) examples\$(bin_spec)\example.exe
!       endif
        @if exist $(resfile) del $(resfile)
        cd examples
!endif

        @if exist *.obj del *.obj

