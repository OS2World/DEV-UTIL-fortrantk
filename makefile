# FORTRAN/TK make file - creates FORTRANTK.LIB
# compile with:
# wmake [os=OS2|WIN]
# if omitted, it will be compiled for OS/2

!ifndef os
os_spec = __OS2__
!else
os_spec = __$(os)__
!endif

all fortrantk.lib : .SYMBOLIC

fortrantk.lib : manargs.obj ftkcall.obj ftkapi.obj
                @if exist fortrantk.lib del fortrantk.lib
                wlib fortrantk +manargs +ftkcall +ftkapi

manargs.obj   : manargs.asm
                wasm -3 $[.

ftkcall.obj   : ftkcall.c
                wcc386 -D$(os_spec) -bm $[.

ftkapi.obj    : *.fap *.fi
                wfc386 -DEF=$(os_spec) -bm ftkapi.fap

.AFTER
                @if exist *.obj del *.obj

