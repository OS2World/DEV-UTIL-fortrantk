* FORTRAN/TK (Simple) Editor example
* for OpenWatcom FORTRAN/77
* by Robin Haberkorn

c$include fortrantk.fap

* Main program:
      program FTK_PAD
       include       'fortrantk.fi'

       logical       wrap/.false./,
     &               new,
     &               saved
       logical       exists
       character*256 nl*2,
     &               filename,
     &               dum,
     &               dum2,
     &               word,
     &               curfont,
     &               cursize,
     &               curport

       record /RXSTRING/ memi
       character*(*)     buf,
     &                   buf2
       integer           x, y, z,
     &                   width,
     &                   height

       logical       SaveFile,
     &               OpenFile

       nl = char(13)//char(10)
       curfont = 'Courier'c
       cursize = '10'c

       if(TkInit('runtime'c)) then
        call MessageBeep(1000, 100)
        stop
       end if

       call CreateWindow()

       execute CreateNew
       loop
        cmpval = TkWait()
        if     (cmp('file_new'c))      then : evswitch
         if(saved) then
          execute CreateNew
           else
          execute MsgNotSaved
          if(dum .NE. 'cancel'c) then
           if(dum .EQ. 'yes'c) execute Save_Click
           execute CreateNew
          end if
         end if

        else if(cmp('file_save'c))     then
         execute Save_Click

        else if(cmp('file_saveas'c))   then
         if(saved) then
          execute SaveAs
           else
          execute MsgNotSaved
          if(dum .NE. 'cancel'c) then
           if(dum .EQ. 'yes'c) execute Save_Click
           execute SaveAs
          end if
         end if

        else if(cmp('file_open'c))     then
         if(saved) then
          execute OpenClick
           else
          execute MsgNotSaved
          if(dum .NE. 'cancel'c) then
           if(dum .EQ. 'yes'c) execute Save_Click
           execute OpenClick
          end if
         end if

        else if(cmp('file_print'c))    then
         call TkWm('deiconify'c, '.print'//dn)
         call TkFocus('.print'//dn)

        else if(cmp('export_ps'c))     then    ! experimental
         loop : askloop                        ! just works for one page
          dum = TkGetSaveFile('-defaultextension'c, '.ps'c,
     &                        '-filetypes'c,
     &                        '{{All files} *} '//
     &                        '{{Postscript files} {.ps}}'c,
     &                        '-initialfile'c,
     &                        filename(:index(filename, '.'))//'ps'c,
     &                        '-title'c, 'Export as Postscript...'//dn)
          if(dum(1:1) .NE. char(0)) then
           inquire(file=dum(:ntlen(dum)), exist=exists)
           if(exists) then
            dum2 = TkMessageBox('-message'c,
     &                          'This file already exists.'//nl//
     &                          'Do you want to overwrite it?'c,
     &                          '-title'c, 'FORTRAN/TK Editor'c,
     &                          '-icon'c,  'question'c,
     &                          '-type'c,  'yesnocancel'//dn)
            if(dum2 .EQ. 'yes'c) then
             quit : askloop
              else if(dum2 .EQ. 'cancel'c) then
             quit : evswitch
            end if
             else
            quit : askloop
           end if
            else
           quit : evswitch
          end if
         end loop

         call TkPack(TkCanvas('.prn'//dn),
     &               '-expand'c, 'yes'c,
     &               '-fill'c,   'both'//dn)

         memi = TkGetEx('.editbox'c, '0.0'c, 'end'//dn)
         allocate(buf*memi.strlength, location=memi.strptr)
         allocate(buf2*30000)

         ! dum = TkCget('.editbox'c, '-font'//dn)
         ! Has problems with fonts bigger or smaller than 10
         dum2  = curfont(:ntlen(curfont))//' 10'c
         word  = ' '
         width = height = 0
         y     = z = 1

         do x = 1, len(buf)
          word(z:z) = buf(x:x)
          z = z + 1

          if((buf(x:x) .EQ. ' ') .OR. (buf(x:x) .EQ. char(10))) then
           if(buf(x:x) .EQ. char(10)) height = height + 10
           word(z:z) = char(0)
           width = width + str2int(TkFontMeasure(dum2, word//dn))

           if(width .GE. 630) then
            buf2(y:y+1+ntlen(word)) = char(10)//word(:ntlen(word))
            y = y + 1 + ntlen(word)
            width  = 0
            height = height + 10
             else
            buf2(y:y+ntlen(word)) = word(:ntlen(word))
            y = y + ntlen(word)
            if(buf(x:x) .EQ. char(10)) width = 0
           end if
           if(height .GE. 610) then
            call TkMessageBox('-message'c,
     &                        'Exporting to postscript is an '//
     &                        'experimental function. Just one page '//
     &                        'can be exportet yet.'//nl//
     &                        'Therefore, your text was cut of at '//
     &                        'the end of the first page.'c,
     &                        '-title'c, 'FORTRAN/TK Editor'c,
     &                        '-icon'c,  'info'//dn)
            exit
           end if

           word = ' '
           z    = 1
          end if
         end do
         buf2(y:y+1) = dn

         call TkCanvasText('.prn'c, '2.5c'c, '2.5c'c,
     &                     '-anchor'c, 'nw'c,
     &                     '-font'c,   dum2,
     &                     '-fill'c,
     &                     TkCget('.editbox'c, '-fg'//dn),
     &                     '-text'c,   buf2)

         call TkCanvasPostscript('.prn'c,
     &                           '-file'c,       dum,
     &                           '-width'c,      '21c'c,
     &                           '-height'c,     '30c'c,
     &                           '-pagex'c,      '0c'c,
     &                           '-pagey'c,      '30c'c,
     &                           '-pageanchor'c, 'nw'//dn)

         deallocate(buf, buf2)
         call TkDestroy('.prn'//dn)

        else if(cmp('Quit'c))           then
         if(saved) then
          exit
           else
          execute MsgNotSaved
          if(dum .NE. 'cancel'c) then
           if(dum .EQ. 'yes'c) execute Save_Click
           exit
          end if
         end if

        else if(cmp('edit_copy'c) .OR.
     &          cmp('edit_cut'c))      then
         memi = TkGetEx('.editbox'c, 'sel.first'c, 'sel.last'//dn)
         allocate(buf*memi.strlength, location=memi.strptr)

         if(buf .EQ. '-1'c) then
          call MessageBeep(1000, 100)
           else if(SetClipboard(buf)) then
          call MessageBeep(1000, 100)
           else if(cmp('edit_cut'c))  then
          call TkDelete('.editbox'c, 'sel.first'c, 'sel.last'//dn)
          execute DelEvent
         end if
         deallocate(buf)

        else if(cmp('edit_paste'c))    then
         memi = GetClipboardEx(30000)
         if(memi.strptr) then
          allocate(buf*memi.strlength, location=memi.strptr)

          if(buf(1:1) .NE. char(0)) then
           allocate(buf2*memi.strlength+1)

           y = 1
           do x = 1, ntlen(buf)+1
            if(buf(x:x) .NE. char(13)) then
             buf2(y:y) = buf(x:x)
             y = y + 1
            end if
           end do
           buf2(y:y) = char(0)

           call TkDelete('.editbox'c, 'sel.first'c, 'sel.last'//dn)
           call TkInsert('.editbox'c, 'insert'c, buf2)
           execute DelEvent

           deallocate(buf2)
          end if
          call CloseClipbrd()

          deallocate(buf)
         end if

        else if(cmp('edit_selectall'c)) then
         ! There is no TkTextTagAdd yet:
         call TkTcl('.editbox tag add sel 0.0 end'//dn)

        else if(cmp('edit_wowrap'c))   then
         if(wrap) then
          wrap = .false.
          call TkConfig('.editbox'c,
     &                  '-wrap'c, 'none'//dn)
           else
          wrap = .true.
          call TkConfig('.editbox'c,
     &                  '-wrap'c, 'word'//dn)
         end if

        else if(cmp('choose_color'c))       then
         dum = TkChooseColor('-initialcolor'c,
     &                       TkCget('.editbox'c, '-fg'//dn),
     &                       '-title'c,
     &                       'Font Color...'//dn)
         if(dum(1:1) .NE. char(0))
     &    call TkConfig('.editbox'c, '-fg'c, dum//dn)


        else if(cmp('help_about'c))    then
         call TkMessageBox('-message'c,
     &                     'FORTRAN/TK Editor example'//nl//
     &                     'by Robin Haberkorn'//nl//
     &                     'FORTRAN/TK is based on Rexx/TK by '//
     &                     'Roger O''Connor and Mark Hessling.'c,
     &                     '-title'c, 'FORTRAN/TK Editor'c,
     &                     '-icon'c,  'info'//dn)

        else if(cmp('print_ok'c))       then
         open(10, file=curport(:ntlen(curport)),
     &        access='SEQUENTIAL', form='FORMATTED',
     &        recordtype='FIXED', action='WRITE',
     &        blocksize=256, err=10)
         memi = TkGetEx('.editbox'c, '0.0'c, 'end'//dn)
         allocate(buf*memi.strlength, location=memi.strptr)

          do x = 1, ntlen(buf)
           if(buf(x:x) .EQ. char(10)) write(10, '(A, $)') char(13)
           write(10, '(A, $)') buf(x:x)
          end do

         deallocate(buf)
         close(10, err=10)
         call TkWm('withdraw'c, '.print'//dn)

         quit
10       call MessageBeep(1000, 100)

        else if(cmp('key_press'c))     then
         execute DelEvent

        else if(evname(cmpval) .EQ. 'port'c)   then
         curport = evarg(cmpval)
         call TkConfig('.print.port'c,
     &                 '-text'c, 'Choosed: '//curport//dn)

        else if(evname(cmpval) .EQ. 'font'c)   then
         curfont = evarg(cmpval)
         call TkConfig('.editbox'c,
     &                 '-font'c, curfont(:ntlen(curfont))//' '//
     &                 cursize//dn)

        else if(evname(cmpval) .EQ. 'fsize'c)  then
         cursize = evarg(cmpval)
         call TkConfig('.editbox'c,
     &                 '-font'c, curfont(:ntlen(curfont))//' '//
     &                 cursize//dn)

        else
         ! write(*,*) 'EVENT(unknown): ', cmpval(:ntlen(cmpval))
         ! output message beep
         call MessageBeep(1000, 100)
        end if
       end loop

       call TkDestroy('.'//dn)

       if(TkUnload()) call MessageBeep(1000, 100)

       remote block DelEvent
        call TkBind('.editbox'c, '<KeyRelease>'c, dn)
        saved = .false.
        call SetTitle(filename, saved)
       end block

       remote block Save_Click
        if(SaveFile(filename, new)) then
         new   = .false.
         saved = .true.
         call TkBind('.editbox'c, '<KeyRelease>'c,
     &               '*key_press'//dn)
        end if
       end block

       remote block SaveAs
        if(SaveFile(filename, .true.)) then
         new   = .false.
         saved = .true.
         call TkBind('.editbox'c, '<KeyRelease>'c,
     &               '*key_press'//dn)
        end if
       end block

       remote block OpenClick
        if(OpenFile(filename)) then
         new   = .false.
         saved = .true.
        end if
       end block

       remote block MsgNotSaved
        dum = TkMessageBox('-message'c,
     &                     'The file was not saved.'//nl//
     &                     'Do you want to save it now?'c,
     &                     '-title'c, 'FORTRAN/TK Editor'c,
     &                     '-icon'c,  'question'c,
     &                     '-type'c,  'yesnocancel'//dn)
       end block

       remote block CreateNew
        filename = 'noname.txt'c
        saved    = new = .true.

        call SetTitle(filename, saved)
        call TkBind('.editbox'c, '<KeyRelease>'c,
     &              '*key_press'//dn)

        call TkDelete('.editbox'c, '0.0'c, 'end'//dn)
       end block
      end

* SaveFile:
      logical function SaveFile(filename, toask)
       character*(*) filename
       logical       toask
       include       'fortrantk.fi'
       character*256 nl*2,
     &               dum,
c$ifndef __WIN__
     &               dum2,
c$endif
     &               buffer*(*)

       logical       exists
       integer       i

       record /RXSTRING/ memi

       nl = char(13)//char(10)
       SaveFile = .false.

       if(toask) then
        loop : askloop
         dum = TkGetSaveFile('-defaultextension'c, '.txt'c,
     &                       '-filetypes'c,
     &                       '{{All files} *} '//
     &                       '{{Text files} {.txt}}'c,
     &                       '-initialfile'c, filename,
     &                       '-title'c,       'Save File As...'//dn)

        if(dum(1:1) .NE. char(0)) then

         inquire(file=dum(:ntlen(dum)), exist=exists)
c$ifndef __WIN__
         if(exists) then
          dum2 = TkMessageBox('-message'c,
     &                        'This file already exists.'//nl//
     &                        'Do you want to overwrite it?'c,
     &                        '-title'c, 'FORTRAN/TK Editor'c,
     &                        '-icon'c,  'question'c,
     &                        '-type'c,  'yesnocancel'//dn)
          if(dum2 .EQ. 'yes'c) then
           call DeleteFile(dum)
           quit : askloop
            else if(dum2 .EQ. 'cancel'c) then
           return
          end if
           else
          quit : askloop
         end if
c$else
         if(exists) call DeleteFile(dum)  ! Don't need MsgBox under Win
         quit : askloop
c$endif
          else
         return
        end if

        end loop
         else
        dum = filename
        call DeleteFile(dum)
       end if

       open(10, file=dum(:ntlen(dum)), status='NEW',
     &      access='SEQUENTIAL', form='UNFORMATTED',
     &      recordtype='FIXED', action='WRITE',
     &      blocksize=256, err=10)
       memi = TkGetEx('.editbox'c, '0.0'c, 'end'//dn)
       allocate(buffer*memi.strlength, location=memi.strptr)

        do i=1, ntlen(buffer)-1
         if(buffer(i:i) .EQ. char(10)) write(10) char(13)
         write(10) buffer(i:i)
        end do

       deallocate(buffer)
       close(10, err=10)

       filename = dum
       call SetTitle(filename, .true.)

       SaveFile = .true.

       return
10     call MessageBeep(1000, 100)
      end

* OpenFile:
      logical function OpenFile(filename)
       character*(*) filename
       include       'fortrantk.fi'
       include       'fsublib.fi'
       character*256 dum,
     &               blockbuf,
     &               buffer*(*)
       character     rchar
       integer       rstat,
     &               x, y
       logical       exists

       OpenFile = .false.
       rstat    = 0

       loop
        dum = TkGetOpenFile('-defaultextension'c, '.txt'c,
     &                      '-filetypes'c,
     &                      '{{All files} *} '//
     &                      '{{Text files} {.txt}}'c,
     &                      '-initialfile'c, filename,
     &                      '-title'c,       'Open File...'//dn)
       if(dum(1:1) .EQ. char(0)) return

       inquire(file=dum(:ntlen(dum)), exist=exists)
       if(.NOT. exists)
     &  call TkMessageBox('-message'c,
     &                    'This file does not exist!'c,
     &                    '-title'c, 'FORTRAN/TK Editor'c,
     &                    '-icon'c,  'error'//dn)
       until(exists)

       open(10, file=dum(:ntlen(dum)), status='OLD',
     &      access='SEQUENTIAL', form='UNFORMATTED',
     &      recordtype='FIXED', action='READ',
     &      blocksize=256, err=10)
       allocate(buffer*(filesize(10)*2+2))

        call TkDelete('.editbox'c, '0.0'c, 'end'//dn)
        x = 1
        loop
         read(10, iostat=rstat) rchar

         if(.NOT. ((rchar .EQ. char(13)) .OR. (rstat .LT. 0))) then
          select(rchar)
           case('"', '\', '$', '[')
            buffer(x:x) = '\'
            x = x + 1
          end select

          buffer(x:x) = rchar
          x = x + 1
         end if
        until(rstat .NE. 0)

        x = x - 1
        do y = 1, x, 256
         if(y+255 .LE. x) then
          blockbuf = buffer(y:y+255)
           else
          blockbuf = buffer(y:x)//char(0)
         end if

         call TkInsert('.editbox'c, 'end'c, blockbuf//dn)
        end do

       deallocate(buffer)
       close(10, err=10)

       if(rstat .GT. 0) goto 10

       filename = dum
       call SetTitle(filename, .true.)
       OpenFile = .true.

       return
10     call MessageBeep(1000, 100)
      end

* SetTitle:
      subroutine SetTitle(file, saved)
       character*(*) file
       logical       saved
       include       'fortrantk.fi'
       character*256 buffer

       buffer = file
       if(.NOT. saved) buffer = buffer(:ntlen(buffer))//'*'//char(0)
       call TkWm('title'c, '.'c,
     &           'FORTRAN/TK Editor - '//buffer//dn)
      end

* CreateWindow:
      subroutine CreateWindow()
       implicit character*256 (A-Z)
       include 'fortrantk.fi'

       ! Window size:
       call TkWm('geometry'c, '.'c, '=500x400'//dn)

       ! Menus:
       mainm = TkMenu('.mainm'//dn)
       filem = TkMenu('.filem'c,
     &                '-tearoff'c, 'no'//dn)
       call TkAdd(filem, 'command'c,
     &            '-label'c,       'New'c,
     &            '-underline'c,   '0'c,
     &            '-accelerator'c, 'Ctrl+N'c,
     &            '-rexx'c,        'file_new'//dn)
       call TkAdd(filem, 'command'c,
     &            '-label'c,       'Open...'c,
     &            '-underline'c,   '0'c,
     &            '-accelerator'c, 'Ctrl+O'c,
     &            '-rexx'c,        'file_open'//dn)
       call TkAdd(filem, 'command'c,
     &            '-label'c,       'Save'c,
     &            '-underline'c,   '0'c,
     &            '-accelerator'c, 'Ctrl+S'c,
     &            '-rexx'c,        'file_save'//dn)
       call TkAdd(filem, 'command'c,
     &            '-label'c,       'Save As...'c,
     &            '-underline'c,   '1'c,
     &            '-accelerator'c, 'Shift+Ctrl+S'c,
     &            '-rexx'c,        'file_saveas'//dn)
       call TkAdd(filem, 'separator'//dn)
       call TkAdd(filem, 'command'c,
     &            '-label'c,       'Raw ASCII Print...'c,
     &            '-underline'c,   '11'c,
     &            '-accelerator'c, 'Ctrl+R'c,
     &            '-rexx'c,        'file_print'//dn)
       call TkAdd(filem, 'command'c,
     &            '-label'c,       'Export as Postscript...'c,
     &            '-underline'c,   '0'c,
     &            '-accelerator'c, 'Ctrl+E'c,
     &            '-rexx'c,        'export_ps'//dn)
       call TkAdd(filem, 'separator'//dn)
       call TkAdd(filem, 'command'c,
     &            '-label'c,       'Exit'c,
     &            '-underline'c,   '1'c,
     &            '-accelerator'c, 'Ctrl+X'c,
     &            '-rexx'c,        'Quit'//dn)
       call TkAdd(mainm, 'cascade'c,
     &            '-label'c,     'File'c,
     &            '-underline'c, '0'c,
     &            '-menu'c,      filem//dn)

       oplist = TkMenu('.oplist'c,
     &                 '-tearoff'c, 'no'//dn)
       call TkAdd(oplist, 'radiobutton'c,
     &            '-label'c,    'Courier'c,
     &            '-variable'c, 'selfnt'c,
     &            '-rexx'c,     'font Courier'//dn)
       call TkAdd(oplist, 'radiobutton'c,
     &            '-label'c,    'Helvetica'c,
     &            '-variable'c, 'selfnt'c,
     &            '-rexx'c,     'font Helvetica'//dn)
       call TkAdd(oplist, 'radiobutton'c,
     &            '-label'c,    'Times'c,
     &            '-variable'c, 'selfnt'c,
     &            '-rexx'c,     'font Times'//dn)
       call TkAdd(oplist, 'separator'//dn)
       call TkAdd(oplist, 'radiobutton'c,
     &            '-label'c,    'Size 5'c,
     &            '-variable'c, 'selsize'c,
     &            '-rexx'c,     'fsize 5'//dn)
       call TkAdd(oplist, 'radiobutton'c,
     &            '-label'c,    'Size 10'c,
     &            '-variable'c, 'selsize'c,
     &            '-rexx'c,     'fsize 10'//dn)
       call TkAdd(oplist, 'radiobutton'c,
     &            '-label'c,    'Size 15'c,
     &            '-variable'c, 'selsize'c,
     &            '-rexx'c,     'fsize 15'//dn)
       call TkAdd(oplist, 'separator'//dn)
       call TkAdd(oplist, 'command'c,
     &            '-label'c,     'Font Color...'c,
     &            '-underline'c, '5'c,
     &            '-rexx'c,      'choose_color'//dn)

       editm = TkMenu('.editm'c,
     &                '-tearoff'c, 'no'//dn)
       call TkAdd(editm, 'command'c,
     &            '-label'c,       'Cut'c,
     &            '-underline'c,   '1'c,
     &            '-accelerator'c, 'Ctrl+U'c,
     &            '-rexx'c,        'edit_cut'//dn)
       call TkAdd(editm, 'command'c,
     &            '-label'c,       'Copy'c,
     &            '-underline'c,   '0'c,
     &            '-accelerator'c, 'Ctrl+C'c,
     &            '-rexx'c,        'edit_copy'//dn)
       call TkAdd(editm, 'command'c,
     &            '-label'c,       'Paste'c,
     &            '-underline'c,   '0'c,
     &            '-accelerator'c, 'Ctrl+P'c,
     &            '-rexx'c,        'edit_paste'//dn)
       call TkAdd(editm, 'command'c,
     &            '-label'c,       'Select All'c,
     &            '-underline'c,   '7'c,
     &            '-accelerator'c, 'Ctrl+A'c,
     &            '-rexx'c,        'edit_selectall'//dn)
       call TkAdd(editm, 'separator'//dn)
       call TkAdd(editm, 'cascade'c,
     &            '-label'c,     'Font Options'c,
     &            '-underline'c, '5'c,
     &            '-menu'c,      oplist//dn)
       call TkAdd(editm, 'checkbutton'c,
     &            '-label'c,       'Word Wrap'c,
     &            '-underline'c,   '0'c,
     &            '-accelerator'c, 'Ctrl+W'c,
     &            '-rexx'c,        'edit_wowrap'//dn)
       call TkAdd(mainm, 'cascade'c,
     &            '-label'c,     'Edit'c,
     &            '-underline'c, '0'c,
     &            '-menu'c,      editm//dn)

       helpm = TkMenu('.helpm'c,
     &                '-tearoff'c, 'no'//dn)
       call TkAdd(helpm, 'command'c,
     &            '-label'c,       'About...'c,
     &            '-underline'c,   '0'c,
     &            '-accelerator'c, 'F1'c,
     &            '-rexx'c,        'help_about'//dn)
       call TkAdd(mainm, 'cascade'c,
     &            '-label'c,     'Help'c,
     &            '-underline'c, '0'c,
     &            '-menu'c,      helpm//dn)

       call TkConfig('.'c,
     &               '-menu'c, mainm//dn)
       call TkMenuInvoke(editm, '6'//dn)
       call TkMenuInvoke(oplist, '0'//dn)
       call TkMenuInvoke(oplist, '5'//dn)

       ! Scrollbars:
       call TkPack(TkScrollbar('.yscroll'c,
     &                         '-orient'c,  'vertical'c,
     &                         '-command'c, '.editbox yview'//dn),
     &             '-side'c, 'right'c,
     &             '-fill'c, 'y'//dn)
       call TkPack(TkScrollbar('.xscroll'c,
     &                         '-orient'c,  'horizontal'c,
     &                         '-command'c, '.editbox xview'//dn),
     &             '-side'c, 'bottom'c,
     &             '-fill'c, 'x'//dn)

       ! Edit field:
       call TkPack(TkText('.editbox'c,
     &                    '-xscrollcommand'c,  '.xscroll set'c,
     &                    '-yscrollcommand'c,  '.yscroll set'//dn),
     &             '-expand'c, 'yes'c,
     &             '-fill'c,   'both'c,
     &             '-side'c,   'top'//dn)

       ! Menu bindings:
       call TkEvent('add'c, '<<new>>'c,    '<Control-n>'//dn)
       call TkEvent('add'c, '<<new>>'c,    '<Control-N>'//dn)
       call TkEvent('add'c, '<<open>>'c,   '<Control-o>'//dn)
       call TkEvent('add'c, '<<open>>'c,   '<Control-O>'//dn)
       call TkEvent('add'c, '<<save>>'c,   '<Control-s>'//dn)
       call TkEvent('add'c, '<<save>>'c,   '<Control-S>'//dn)
       call TkEvent('add'c, '<<saveas>>'c, '<Shift-Control-s>'//dn)
       call TkEvent('add'c, '<<saveas>>'c, '<Shift-Control-S>'//dn)
       call TkEvent('add'c, '<<print>>'c,  '<Control-r>'//dn)
       call TkEvent('add'c, '<<print>>'c,  '<Control-R>'//dn)
       call TkEvent('add'c, '<<ps>>'c,     '<Control-e>'//dn)
       call TkEvent('add'c, '<<ps>>'c,     '<Control-E>'//dn)
       call TkEvent('add'c, '<<exit>>'c,   '<Control-x>'//dn)
       call TkEvent('add'c, '<<exit>>'c,   '<Control-X>'//dn)

       call TkEvent('add'c, '<<cut>>'c,   '<Control-u>'//dn)
       call TkEvent('add'c, '<<cut>>'c,   '<Control-U>'//dn)
       call TkEvent('add'c, '<<copy>>'c,  '<Control-c>'//dn)
       call TkEvent('add'c, '<<copy>>'c,  '<Control-C>'//dn)
       call TkEvent('add'c, '<<paste>>'c, '<Control-p>'//dn)
       call TkEvent('add'c, '<<paste>>'c, '<Control-P>'//dn)
       call TkEvent('add'c, '<<sall>>'c,  '<Control-a>'//dn)
       call TkEvent('add'c, '<<sall>>'c,  '<Control-A>'//dn)
       call TkEvent('add'c, '<<wowa>>'c,  '<Control-w>'//dn)
       call TkEvent('add'c, '<<wowa>>'c,  '<Control-W>'//dn)

       call TkBind('.'c, '<<new>>'c,    '*file_new'//dn)
       call TkBind('.'c, '<<open>>'c,   '*file_open'//dn)
       call TkBind('.'c, '<<save>>'c,   '*file_save'//dn)
       call TkBind('.'c, '<<saveas>>'c, '*file_saveas'//dn)
       call TkBind('.'c, '<<print>>'c,  '*file_print'//dn)
       call TkBind('.'c, '<<ps>>'c,     '*export_ps'//dn)
       call TkBind('.'c, '<<exit>>'c,   '*Quit'//dn)
       call TkBind('.'c, '<<cut>>'c,    '*edit_cut'//dn)
       call TkBind('.'c, '<<copy>>'c,   '*edit_copy'//dn)
       call TkBind('.'c, '<<paste>>'c,  '*edit_paste'//dn)
       call TkBind('.'c, '<<sall>>'c,   '*edit_selectall'//dn)
       call TkBind('.'c, '<<wowa>>'c,   '*edit_wowrap'//dn)

       call TkBind('.'c, '<F1>'c,       '*help_about'//dn)

       ! Set event for right click (doesn't works with OS/2):
       call TkBind('.'c, '<Button-3>'c,
     &             'tk_popup .editm %X %Y'//dn)

       ! Click -> make it the foreground window
       ! (necessary under OS/2)
       call TkBind('.'c, '<Button>'c,
     &             'focus -force .editbox'//dn)

       ! Printing window:
       prnwin = TkTopLevel('.print'//dn)
       call TkWm('title'c, prnwin, 'Raw ASCII Print...'//dn)
       call TkWm('geometry'c, prnwin, '=300x150'//dn)
       call TkWm('resizable'c, prnwin, 'no'c, 'no'//dn)
       call TkWm('withdraw'c, prnwin//dn)

       ! Popup menu for choosing a port
       portm = TkMenu('.portm'c,
     &                '-tearoff'c, 'no'//dn)
       call TkAdd(portm, 'radiobutton'c,
     &            '-label'c,       'LPT1'c,
     &            '-variable'c,    'selport'c,
     &            '-rexx'c,        'port LPT1'//dn)
       call TkAdd(portm, 'radiobutton'c,
     &            '-label'c,       'LPT2'c,
     &            '-variable'c,    'selport'c,
     &            '-rexx'c,        'port LPT2'//dn)
       call TkAdd(portm, 'radiobutton'c,
     &            '-label'c,       'LPT3'c,
     &            '-variable'c,    'selport'c,
     &            '-rexx'c,        'port LPT3'//dn)
       call TkAdd(portm, 'separator'//dn)
       call TkAdd(portm, 'radiobutton'c,
     &            '-label'c,       'COM1'c,
     &            '-variable'c,    'selport'c,
     &            '-rexx'c,        'port COM1'//dn)
       call TkAdd(portm, 'radiobutton'c,
     &            '-label'c,       'COM2'c,
     &            '-variable'c,    'selport'c,
     &            '-rexx'c,        'port COM2'//dn)
       call TkMenuInvoke(portm, '0'//dn)

       ! Add widgets
       call TkGrid(TkLabel('.print.lab1'c,
     &                     '-text'c, 'Print to port:'//dn),
     &             TkButton('.print.port'//dn),
     &             '-padx'c,   '10'c,
     &             '-pady'c,   '10'c,
     &             '-row'c,    '0'c,
     &             '-sticky'c, 'nsew'//dn)
       call TkGrid(TkButton('.print.ok'c,
     &                      '-text'c,      'Print file'c,
     &                      '-underline'c, '0'c,
     &                      '-rexx'c,      'print_ok'//dn),
     &             TkButton('.print.cancel'c,
     &                      '-text'c,      'Cancel'c,
     &                      '-underline'c, '0'c,
     &                      '-command'c,   'wm withdraw .print'//dn)
     &                      //'A',
     &             '-padx'c,   '10'c,
     &             '-pady'c,   '10'c,
     &             '-row'c,    '1'c,
     &             '-sticky'c, 'nsew'//dn)
       call TkGridColumnConfig(prnwin, '0'c,
     &                         '-weight'c, '5'//dn)
       call TkGridColumnConfig(prnwin, '1'c,
     &                         '-weight'c, '5'//dn)
       call TkGridRowConfig(prnwin, '0'c,
     &                      '-weight'c, '5'//dn)
       call TkGridRowConfig(prnwin, '1'c,
     &                      '-weight'c, '5'//dn)


       ! Set standart key bindings
       call TkEvent('add'c, '<<print>>'c,  '<Alt-p>'//dn)
       call TkEvent('add'c, '<<print>>'c,  '<Alt-P>'//dn)
       call TkEvent('add'c, '<<cancel>>'c, '<Alt-c>'//dn)
       call TkEvent('add'c, '<<cancel>>'c, '<Alt-C>'//dn)
       call TkBind(prnwin, '<<print>>'c,   '*print_ok'//dn)
       call TkBind(prnwin, '<<cancel>>'c,  'wm withdraw .print'//dn)

       ! Key binding for popup menu
c$ifdef __OS2__
       call TkBind('.print.port'c, '<Button-1>'c,
     &             'tk_popup .portm %X %Y'//dn)
c$else
       call TkBind('.print.port'c, '<ButtonRelease-1>'c,
     &             'tk_popup .portm %X %Y'//dn)
c$endif

       ! Set focus to editbox/main window
       ! (necessary under Windows)
       call TkFocus('-force'c, '.editbox'//dn)
      end