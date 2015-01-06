FORTRAN/TK (v.0.6 beta):
~~~~~~~~~~~~~~~~~~~~~~~~

FORTRAN/TK is, an easy to learn, platform-independent (OS/2 and Windows 32-bit)
widget toolkit for the Open Watcom FORTRAN 77 compiler. The files "calc.for"
(Calculator), "edit.for"(Editor) and "example.for"(Simple example) are examples of
some features already implemented in FORTRAN/TK. They are located in the "examples"
subdirectory. FORTRAN/TK is based on Rexx/Tk by Roger O'Connor and Mark Hessling.
(technically it's a kind of wrapper to the Rexx/Tk library) and therefore uses the
TK-Toolkit from Tcl/Tk. All Rexx/Tk functions are already usable with FORTRAN/TK
including some which are not even listed in the Rexx/Tk help. However FORTRAN/TK
does not need a Rexx-Interpreter.
It also includes about 20 new functions which are neither part of Tcl/Tk nor Rexx/Tk
nor Open Watcom FORTRAN. They include functions for file management, file searching,
basic support for dynamic DLL handling and the clipboard which are commonly needed.

How to build FORTRAN/TK:
FORTRAN/TK is built as a library of different object files, which you can easily
include in your project.

FORTRAN/TK and its examples can be compiled using the provided makefiles.
To compile FOTRAN/TK, go to the commandline, set the environment variables of
Open Watcom and issue the following in the FORTRAN/TK directory:
wmake [os=OS2|WIN]
"os=WIN" compiles the library for Windows 32-bit. If "os=OS2" or "os" is
omitted, the library will be compiled for OS/2 32-bit.

A file called "fortrantk.lib" will be created.
Remember that the library includes platform dependent code and must be rebuilt to
work on another system as compiled for.

How to build the examples:
Copy the prievously built "fortrantk.lib" to the "examples" subdirectory.
Remember that the FORTRAN/TK directory should be somewhere in the FORTRAN include
path (e.g. defined with the FINCLUDE environment variable in your "setvars.cmd"/
"setvars.bat" or "config.sys"/"autoexec.bat") because FORTRAN/TK programs share
some include files with the core itself.

Then issue:
wmake [ico16=|ico=] [os=OS2|WIN] [calc=] [edit=] [example=]
If everything is omitted, all examples will be built for OS/2 without icons.
Specify "os=WIN" to compile the examples for Windows. If you set either
"calc=", "edit=" or "example=" then just these examples will be compiled.
If "ico=" or "ico16=" is set, the standard FORTRAN/TK icon will be included
in the file, whereas "ico16=" is for OS/2 only and says that the 16-bit
resource compiler should be used instead of the 32-bit one (I had problems with
it). Under OS/2, the resource compilers of the OS/2 Developers Toolkit are used,
so be sure that you have installed them.
So to compile all examples for OS/2 and include the icon, you would have to enter:
wmake ico=
or
wmake ico= os=WIN
for Windows.

Basically, a FORTRAN/TK program can be built by issuing the Open Watcom FORTRAN
Compile and Link Utility in the following way:

wfl386 [yourprogram.for] -"f fortrantk.lib" -FE=[yourprogram.exe] -BM -DEF=[OSDEF] -L=[SYSTEM]

where [yourprogram.for] is your program, [yourprogram.exe] is the executable name
of your program, [OSDEF] is a compiler definition - either __OS2__ or __WIN__ and
[SYSTEM] is the system identifier for the linker, which should be either OS2V2,
OS2V2_PM (without console, windowed application), NT or NT_WIN (without console).

Required libraries:
OS/2: "rexxtk.dll", "rexxtran.dll", "tcl80.dll", "tk80.dll", EMX libraries (for Tcl/Tk).
They must be somewhere in the PATH or in the directory defined by the TKINIT-function of your
program.
These files are already located in the "examples\binos2\runtime" directory of
FORTRAN/TK.

Windows: "Msvcrt.dll", "rexxtk.dll", "rexxtrans.dll", "tcl80.dll", "tk80.dll"
(should be in the directory defined by the TKINIT-function of your program).
You may also need the Tcl/Tk libraries - the "lib" directory must be
one level deeper than the Tcl/Tk-DLLs (../), but you may change this
by the TCL_LIBRARY and TK_LIBRARY environment variables.
These files are already located in the "examples\binwin\runtime" directory of
FORTRAN/TK.

Documentation:
Sadly, there is no real documentation yet except the example programs, the
source code (which needs some more documentation, too) and this readme file.
The FORTRAN/TK-Api is very similar to the Rexx/Tk library, so the Rexx/Tk help
(http://rexxtk.sourceforge.net/) will also be useful.
But the Rexx/Tk documentation isn't complete, too, so you should read the Tcl/Tk
help of your installed release anyway.

FORTRAN/TK is an Open Source project, so you are invited to participate in the
development, to study the sourcecode, to send bug fixes and to modify the
sourcecode in all ways allowed by the OSL v.2.1 license.
I am happy about any suggestion and help.

Copyright (C): 2005 Robin Haberkorn
License: Open Software License v.2.1
(look at "osl-2.1.txt" for a text copy)

FORTRAN/TK may still contain serious bugs.
It is distributed WITHOUT ANY WARRANTY!

Detailed documentation coming soon...

Contact:             None-Brain@web.de
FORTRAN/TK Homepage: http://qdlos.sourceforge.net/fortrantk

