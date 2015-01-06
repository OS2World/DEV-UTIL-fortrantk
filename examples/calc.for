* FORTRAN/TK (Simple) Calculator example
* for OpenWatcom FORTRAN/77
* by Robin Haberkorn
* --- Heavily commented ---

* Main include-file - pragmas:
c$include fortrantk.fap

* Main program:
      program FTK_CALC
       include   'fortrantk.fi' ! For "declarations", structures...
       character nl*2,          ! New line chars
     &           dum*256,       ! For return values
     &           savenum*28     ! Place to save DISNUM,
                                ! when ENTER was pressed
       real      calcnum        ! Contains number to calculate with,
                                ! when operation was choosed

       ! Function "declarations":
       character WONull*256
       logical   IsNumber

       ! Global variables:
       character     disnum*28  ! Buffer when entering number
       integer       op         ! Choosed operator
       logical       rval,      ! TRUE = right side of operation
     &               point      ! FALSE = "." was already entered
       common /CALC/ disnum,
     &               op,
     &               rval,
     &               point
       data          disnum/'0'c/,
     &               op/0/,
     &               rval/.false./,
     &               point/.true./

       save   /CALC/

       nl = char(13)//char(10)
       savenum = char(0)

       ! Init code - Exit if error:
       if(TkInit('runtime'c)) then
        call MessageBeep(1000, 100)
        stop
       end if

       ! Create the widgets:
       call CreateWindow()

       ! The (infinite) event loop:
       loop

        ! You shouldn't use a SELECT block for the eventhandling
        ! Instead use global CMPVAL and CMP function with an
        ! IF/ELSEIF construction
        cmpval = TkWait()                   ! Get "rexx" command
        if     (cmpval(:3) .EQ. 'num') then ! A number (0-9) button
         call AddToNum(cmpval(4:4))         ! was clicked
         savenum = char(0)                  ! Reset SAVENUM - if it was
                                            ! used after pressing ENTER
        else if(cmp('point_click'c)) then   ! "." was clicked
         if(point) call AddToNum('.')       ! Add it, if it was not
         point = .false.                    ! already; disable POINT
         savenum = char(0)

        else if(cmp('div_click'c)) then     ! "/" was clicked
         call PerformOp(calcnum)            ! Perform necessary steps
         op = 1                             ! Division = 1
        else if(cmp('mul_click'c)) then     ! "x" was clicked
         call PerformOp(calcnum)
         op = 2                             ! Multiplication = 2
        else if(cmp('minus_click'c)) then   ! "-" was clicked
         call PerformOp(calcnum)
         op = 3                             ! Minus = 3
        else if(cmp('plus_click'c)) then    ! "+" was clicked
         call PerformOp(calcnum)
         op = 4                             ! Plus = 4
        else if(cmp('enter_click'c)) then   ! "=" was clicked
         call PerformOp(calcnum)
         op      = 0                        ! No new operation
         savenum = disnum                   ! Save DISNUM,
         disnum  = '0'c                     ! and reset it
        else if(cmp('clear_click'c)) then   ! "C" was clicked
         rval    = .false.                  ! Back to left side of op.
         point   = .true.
         calcnum = op = 0                   ! Reset CALCNUM, OP,
         disnum  = '0'c                     ! DISNUM and
         savenum = char(0)                  ! SAVENUM
         execute RefreshLab                 ! Show new (clear) display

        else if(cmp('about_click'c)) then   ! "->About" was clicked
         ! Show ABOUT message box:
         call TkMessageBox('-message'c,
     &                     'FORTRAN/TK Calculator example'//nl//
     &                     'by Robin Haberkorn'//nl//
     &                     'FORTRAN/TK is based on Rexx/TK by '//
     &                     'Roger O''Connor and Mark Hessling.'c,
     &                     '-title'c, 'FORTRAN/TK Calculator'c,
     &                     '-icon'c,  'info'//dn)

        else if(cmp('cb_paste'c)) then      ! "->Clipboard->Paste"
         dum = GetClipboard()               ! Get clipboard content
         if((ntlen(dum) .GT. 0) .AND.             ! If long enough and
     &      (ntlen(dum) .LT. len(disnum)-1) .AND. ! a number...
     &      (IsNumber(dum))) then
          disnum = WONull(dum)              ! ...remove trailing "0"
          execute RefreshLab                ! Show new display
          if((op .NE. 0) .AND. .NOT. rval) rval = .true.

          ! Test whether there is already a ".":
          if(index(disnum, '.') .NE. 0) point = .false.
           else
          call MessageBeep(1000, 100)       ! Output error beep
         end if

        else if(cmp('cb_copy'c)) then       ! "->Clipboard->Copy"
         if((op .NE. 0) .OR.                ! If not a final value
     &      (ntlen(savenum) .EQ. 0)) then   ! (ENTER was not pressed)...
          call SetClipboard(WONull(disnum)) ! Copy number to clipboard
           else
          call SetClipboard(WONull(savenum)) ! If yes, use saved DISNUM
         end if

        else if(cmp('Quit'c)) then          ! "Quit" message (eg. Menu)
         exit                               ! Exit the event loop

        else                                ! Unknown event
         ! write(*,*) 'EVENT(unknown): ', cmpval(:ntlen(cmpval))
         ! output message beep
         call MessageBeep(1000, 100)
        end if

       end loop

       call TkDestroy('.'//dn)              ! Close/Destroy main window
                                            ! Avoid problem under Win2k
       ! Unload FORTRAN/TK:
       if(TkUnload()) call MessageBeep(1000, 100)

       remote block RefreshLab              ! REFRESHLAB
        call TkConfig('.labres'c,           ! Change text of .labres
     &                '-text'c, disnum//dn)
       end block
      end

* CreateWindow: Create all necessary widgets

      subroutine CreateWindow()
       implicit  character*256 (A-Z)
       include   'fortrantk.fi'
       integer   x                          ! dummy var for loops
       dimension num(9)                     ! array for numpad widget
                                            ! names

       ! Set window title:
       call TkWm('title'c, '.'c, 'Calculator'//dn)
       ! Set window size to 200x300 - not resizable
       ! Does not work the ordinary way under OS/2:
c$ifdef __OS2__
       call TkWm('maxsize'c, '.'c, '200'c, '300'//dn)
       call TkWm('minsize'c, '.'c, '200'c, '300'//dn)
c$else
       call TkWm('geometry'c, '.'c, '=200x300'//dn)
       call TkWm('resizable'c, '.'c, 'no'c, 'no'//dn)
c$endif
       call TkConfig('.'c,                      ! Set window-bg-color to
     &               '-bg'c,       'black'//dn) ! black

       ! Create menu:
       mainm = TkMenu('.mainm'//dn)
       filem = TkMenu('.filem'c,             ! "File" menu
     &                '-tearoff'c, 'no'//dn)
       call TkAdd(filem, 'command'c,         ! Add "File->Exit"
     &            '-label'c,       'Exit'c,
     &            '-underline'c,   '1'c,
     &            '-accelerator'c, 'Ctrl+X'c,
     &            '-rexx'c,        'Quit'//dn)
       call TkAdd(mainm, 'cascade'c,         ! Add "File" to the main
     &            '-label'c,     'File'c,    ! menu
     &            '-underline'c, '0'c,
     &            '-menu'c,      filem//dn)

       cbrd  = TkMenu('.cbrd'c,              ! "Clipboard" menu
     &                '-tearoff'c, 'no'//dn)
       call TkAdd(cbrd, 'command'c,          ! Add "Clipboard->Copy"
     &            '-label'c,       'Copy'c,
     &            '-underline'c,   '0'c,
     &            '-accelerator'c, 'Ctrl+C'c,
     &            '-rexx'c,        'cb_copy'//dn)
       call TkAdd(cbrd, 'command'c,          ! Add "Clipboard->Paste"
     &            '-label'c,       'Paste'c,
     &            '-underline'c,   '0'c,
     &            '-accelerator'c, 'Ctrl+P'c,
     &            '-rexx'c,        'cb_paste'//dn)
       call TkAdd(mainm, 'cascade'c,         ! Add "Clipboard" to the
     &            '-label'c, 'Clipboard'c,   ! main menu
     &            '-underline'c, '1'c,
     &            '-menu'c,  cbrd//dn)

       call TkAdd(mainm, 'command'c,         ! Add "About" to the main
     &            '-label'c,     'About'c,   ! menu
     &            '-underline'c, '0'c,
     &            '-rexx'c,      'about_click'//dn)
       call TkConfig('.'c,                   ! Set the main menu of
     &               '-menu'c, mainm//dn)    ! the main window

       ! Font:
       font1 = 'Courier 12 bold'c

       ! Create result-display:
       labres = TkLabel('.labres'c,
     &                  '-text'c, '0'c,
     &                  '-bg'c,   'black'c,
     &                  '-fg'c,   'white'c,
     &                  '-font'c, font1//dn)

       ! Numpad-Frame:
       numpad = TkFrame('.numpad'//dn)       ! Numpad frame

       ! Create number buttons (numpad):
       do x = 0, 9
        dum    = int2str(x)
        num(x) = TkButton('.numpad.num'//dum,
     &                    '-text'c, dum,
     &                    '-rexx'c, 'num'//dum//dn)
       end do

       ! Create "." button:
       point = TkButton('.numpad.point'c,
     &                  '-text'c,             '.'c,
     &                  '-font'c,             font1,
     &                  '-rexx'c,             'point_click'//dn)

       ! Create operator buttons:
       clear = TkButton('.numpad.clear'c,
     &                  '-text'c,             'C'c,
     &                  '-font'c,             font1,
     &                  '-activebackground'c, 'red'c,
     &                  '-bg'c,               'red'c,
     &                  '-rexx'c,             'clear_click'//dn)
       div   = TkButton('.numpad.div'c,
     &                  '-text'c,             '/'c,
     &                  '-font'c,             font1,
     &                  '-activebackground'c, 'yellow'c,
     &                  '-bg'c,               'yellow'c,
     &                  '-rexx'c,             'div_click'//dn)
       mul   = TkButton('.numpad.mul'c,
     &                  '-text'c,             'x'c,
     &                  '-font'c,             font1,
     &                  '-activebackground'c, 'yellow'c,
     &                  '-bg'c,               'yellow'c,
     &                  '-rexx'c,             'mul_click'//dn)
       minus = TkButton('.numpad.minus'c,
     &                  '-text'c,             '-'c,
     &                  '-font'c,             font1,
     &                  '-activebackground'c, 'yellow'c,
     &                  '-bg'c,               'yellow'c,
     &                  '-rexx'c,             'minus_click'//dn)

       plus  = TkButton('.numpad.plus'c,
     &                  '-text'c,             '+'c,
     &                  '-font'c,             font1,
     &                  '-activebackground'c, 'yellow'c,
     &                  '-bg'c,               'yellow'c,
     &                  '-rexx'c,             'plus_click'//dn)
       enter = TkButton('.numpad.enter'c,
     &                  '-text'c,             '='c,
     &                  '-font'c,             font1,
     &                  '-activebackground'c, 'lightblue'c,
     &                  '-bg'c,               'lightblue'c,
     &                  '-rexx'c,             'enter_click'//dn)

       ! Pack widgets into the grid:
       call TkGrid(clear, div, mul, minus,  ! 1. Row = operators
     &             '-row'c,    '0'c,
     &             '-sticky'c, 'nsew'//dn)
       call TkGrid(num(7), num(8), num(9), plus, ! 2. - 4. Row = "1":"9"
     &             '-row'c,    '1'c,             ! and "+", "="
     &             '-sticky'c, 'nsew'//dn)
       call TkGrid(num(4), num(5), num(6), '^'c, ! "+" = 2 rows
     &             '-row'c,    '2'c,
     &             '-sticky'c, 'nsew'//dn)
       call TkGrid(num(1), num(2), num(3), enter,
     &             '-row'c,    '3'c,
     &             '-sticky'c, 'nsew'//dn)
       call TkGrid(num(0), ' -'c, point, '^'c,   ! 5. Row = "0", "."
     &             '-row'c,        '4'c,         ! "0" = 2 columns,
     &             '-sticky'c,     'nsew'//dn)   ! "=" = 2 rows

       ! Let the columns take the whole space:
       do x = 0, 3
        call TkGridColumnConfig(numpad, int2str(x),
     &                          '-weight'c, '5'//dn)
       end do

       ! Let the rows take the whole space:
       do x = 0, 4
        call TkGridRowConfig(numpad, int2str(x),
     &                       '-weight'c, '5'//dn)
       end do

       ! Pack display and numpad in the window:
       call TkPack(labres,
     &             '-anchor'c, 'e'//dn)
       call TkPack(numpad,
     &             '-expand'c, 'yes'c,
     &             '-fill'c,   'both'//dn)

       ! Create standart key bindings (for buttons and
       ! menus):
       call TkEvent('add'c, '<<num0>>'c, '<Any-Insert>'//dn)
       call TkEvent('add'c, '<<num0>>'c, '<Key-0>'//dn)
       call TkEvent('add'c, '<<num1>>'c, '<Any-End>'//dn)
       call TkEvent('add'c, '<<num1>>'c, '<Key-1>'//dn)
       call TkEvent('add'c, '<<num2>>'c, '<Any-Down>'//dn)
       call TkEvent('add'c, '<<num2>>'c, '<Key-2>'//dn)
       call TkEvent('add'c, '<<num3>>'c, '<Any-Next>'//dn)
       call TkEvent('add'c, '<<num3>>'c, '<Key-3>'//dn)
       call TkEvent('add'c, '<<num4>>'c, '<Any-Left>'//dn)
       call TkEvent('add'c, '<<num4>>'c, '<Key-4>'//dn)
       call TkEvent('add'c, '<<num5>>'c, '<Any-Clear>'//dn)
       call TkEvent('add'c, '<<num5>>'c, '<Key-5>'//dn)
       call TkEvent('add'c, '<<num6>>'c, '<Any-Right>'//dn)
       call TkEvent('add'c, '<<num6>>'c, '<Key-6>'//dn)
       call TkEvent('add'c, '<<num7>>'c, '<Any-Home>'//dn)
       call TkEvent('add'c, '<<num7>>'c, '<Key-7>'//dn)
       call TkEvent('add'c, '<<num8>>'c, '<Any-Up>'//dn)
       call TkEvent('add'c, '<<num8>>'c, '<Key-8>'//dn)
       call TkEvent('add'c, '<<num9>>'c, '<Any-Prior>'//dn)
       call TkEvent('add'c, '<<num9>>'c, '<Key-9>'//dn)

       call TkBind('.'c, '<<num0>>'c, '*num0'//dn)
       call TkBind('.'c, '<<num1>>'c, '*num1'//dn)
       call TkBind('.'c, '<<num2>>'c, '*num2'//dn)
       call TkBind('.'c, '<<num3>>'c, '*num3'//dn)
       call TkBind('.'c, '<<num4>>'c, '*num4'//dn)
       call TkBind('.'c, '<<num5>>'c, '*num5'//dn)
       call TkBind('.'c, '<<num6>>'c, '*num6'//dn)
       call TkBind('.'c, '<<num7>>'c, '*num7'//dn)
       call TkBind('.'c, '<<num8>>'c, '*num8'//dn)
       call TkBind('.'c, '<<num9>>'c, '*num9'//dn)

       call TkEvent('add'c, '<<clear>>'c, '<c>'//dn)
       call TkEvent('add'c, '<<clear>>'c, '<C>'//dn)

       call TkBind('.'c, '<<clear>>'c,      '*clear_click'//dn)
       call TkBind('.'c, '<Any-slash>'c,    '*div_click'//dn)
       call TkBind('.'c, '<Any-asterisk>'c, '*mul_click'//dn)
       call TkBind('.'c, '<Any-minus>'c,    '*minus_click'//dn)
       call TkBind('.'c, '<Any-plus>'c,     '*plus_click'//dn)
       call TkBind('.'c, '<Any-Return>'c,   '*enter_click'//dn)
       call TkBind('.'c, '<Any-Delete>'c,   '*point_click'//dn)

       ! menus:
       call TkEvent('add'c, '<<exit>>'c,  '<Control-x>'//dn)
       call TkEvent('add'c, '<<exit>>'c,  '<Control-X>'//dn)
       call TkEvent('add'c, '<<copy>>'c,  '<Control-c>'//dn)
       call TkEvent('add'c, '<<copy>>'c,  '<Control-C>'//dn)
       call TkEvent('add'c, '<<paste>>'c, '<Control-p>'//dn)
       call TkEvent('add'c, '<<paste>>'c, '<Control-P>'//dn)
       call TkBind('.'c, '<<exit>>'c,  '*Quit'//dn)
       call TkBind('.'c, '<<copy>>'c,  '*cb_copy'//dn)
       call TkBind('.'c, '<<paste>>'c, '*cb_paste'//dn)

       ! Click -> Focus
       ! (necessary under OS/2)
       call TkBind('.'c, '<Button>'c, 'focus -force .'//dn)

       ! Set focus to main window
       ! (necessary under Windows)
       call TkFocus('-force'c, '.'//dn)
      end

* AddToNum: Add a digit to the end of the display/DISNUM
* NUM = digit to add (as character)

      subroutine AddToNum(num)
       include       'fortrantk.fi'
       character     num

       character     disnum*28
       integer       op
       logical       rval,
     &               point
       common /CALC/ disnum,
     &               op,
     &               rval,
     &               point

       save   /CALC/

       ! Switch to right side of operation if operator was set:
       if((op .NE. 0) .AND. .NOT. rval) then
        disnum = '0'c
        rval = .true.
       end if

       ! Set the buffer to the char,
       ! if there is just a Null in it yet...
       if((ntlen(disnum) .EQ. 1) .AND. (disnum(1:1) .EQ. '0')) then
        disnum = num//char(0)
         ! ...but if there is enough space, add the new character:
         else if(ntlen(disnum) .LT. len(disnum)-1) then
        disnum = disnum(:ntlen(disnum))//num//char(0)
         ! If digit can't be added, output error beep:
         else
        call MessageBeep(1000, 100)
       end if
       call TkConfig('.labres'c,           ! Change the text of
     &               '-text'c, disnum//dn) ! .labres (refresh)
      end

* PerformOp: Do necessary things to switch to the
* other side of the operation - set CALCNUM to the content of
* the buffer or perform the operation and fill DISNUM buffer again
* CALCNUM = number to actually calculate with

      subroutine PerformOp(calcnum)
       real          calcnum
       include       'fortrantk.fi'
       character*256 WONull

       character     disnum*28
       integer       op
       logical       rval,
     &               point
       common /CALC/ disnum,
     &               op,
     &               rval,
     &               point

       save   /CALC/

       ! If we are already at the right side of the operation...
       if(rval) then
        select(op)                             ! ...do the operation:
         case(1)                               ! DIVISION
          calcnum = calcnum / str2real(disnum)
         case(2)                               ! MULTIPLICATION
          calcnum = calcnum * str2real(disnum)
         case(3)                               ! MINUS
          calcnum = calcnum - str2real(disnum)
         case(4)                               ! PLUS
          calcnum = calcnum + str2real(disnum)
        end select

        disnum = WONull(real2str(calcnum))     ! Fill buffer with new
                                               ! value (as character)
        ! Set the text of the result-display again -
        ! with preceding '= '
        call TkConfig('.labres'c,
     &                '-text'c, '= '//disnum//dn)

        rval = .false.                      ! Switch to left side again
                                            ! Instead of user input,
                                            ! there is the calculated
                                            ! number, now
         else                               ! ...if we are on left side
        calcnum = str2real(disnum)          ! save the buffer in
       end if                               ! CALCNUM

       point = .true.                       ! "." can be entered anyway
      end

* WONull: Remove the trailing "0" of a NT string and return
* the result
* STR = NT string

      character*256 function WONull(str)
       character*(*) str
       integer       ntlen,
     &               x                      ! dummy var

       if(index(str, '.')) then             ! Just remove from fractions
        x = ntlen(str)                      ! Start at the last char
        while(str(x:x) .EQ. '0') x = x - 1  ! Search for the last digit
        if(str(x:x) .EQ. '.')    x = x - 1  ! Return all chars until
        WONull = str(:x)//char(0)           ! the counted position
         else
        WONull = str
       end if
      end

* IsNumber: Test a string whether it is a number (consists of
* digits, spaces and points) or not
* IsNumber = .TRUE. - it's a number
* STR = NT string

      logical function IsNumber(str)
       character*(*) str
       integer       ntlen,
     &               x                 ! ...dummy var...
       logical       point,            ! flag, whether there is a "."
     &               pm                ! flag: plus or minus

       point    = pm = .false.
       IsNumber = .true.
       do x = 1, ntlen(str) : ploop    ! Go through the whole string
        select(str(x:x))
         case(' ', '0' : '9')          ! Do nothing if a digit or space
         case('.')                     ! If it is a ".",
          if(.NOT. point) then         ! mark that there is one...
           point = .true.
            else                       ! ...or exit if this is already
           IsNumber = .false.          ! the second one
           quit : ploop
          end if
         case('+', '-')                ! If it is a minus or plus
          if(.NOT. pm) then            ! mark that there is one...
           pm = .true.
            else                       ! ...or exit if this is already
           IsNumber = .false.          ! the second one
           quit : ploop
          end if
         case default                  ! Exit if any other character
          IsNumber = .false.           ! is in the string
          quit : ploop
        end select
       end do
      end

