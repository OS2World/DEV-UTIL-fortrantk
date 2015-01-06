* FORTRAN/TK example
* for OpenWatcom FORTRAN/77
* by Robin Haberkorn

c$include fortrantk.fap

      program FTK_EXAMPLE
       include 'fortrantk.fi'
       character*256 dum,
     &               dum2

* Init code:
       if(TkInit('runtime'c))
     &  stop 'FORTRAN/TK couldn''t get initialized!'

       call CreateWindow()

* The main event loop:
       loop

* You shouldn't use a SELECT block for the eventhandling
        cmpval = TkWait()
        if     (cmp('Quit'c))           then
         write(*,*) 'EVENT: Exiting...'
         exit

        else if(cmp('hellobtm_click'c)) then
         dum2 = TkCget('.hellobtm'c, '-text'//dn)
         write(*,*) 'EVENT: The "', dum2(:ntlen(dum2)),
     &              '" button was clicked!'
         dum = TkMessageBox('-message 'c,
     &                      'You pressed the "'//dum2(:ntlen(dum2))//
     &                      '" button!'//char(13)//char(10)//
     &                      'Please choose "Yes" or "No".'c,
     &                      '-title'c, 'Fortran/TK example'c,
     &                      '-type'c, 'yesno'//dn)
         write(*,*) 'You choosed "', dum(:ntlen(dum)), '".'

        else if(cmp('changebtm_click'c)) then
         write(*,*) 'EVENT: changebtm_click'
         dum = TkGet('.entry1'//dn)
         call TkConfig('.hellobtm'c, '-text'c, dum//dn)
         write(*,*) 'Text of .hellobtm changed to: "',
     &              dum(:ntlen(dum)), '"'

        else
         write(*,*) 'EVENT(unknown): ', cmpval(:ntlen(cmpval))
        end if

       end loop

* Exit code:
       call TkDestroy('.'//dn)
       if(TkUnload()) write(*,*) 'FORTRAN/TK couldn''t get unloaded!'
      end

      subroutine CreateWindow()
       include 'fortrantk.fi'

* Change the window title and its size:
       call TkWm('title'c, '.'c, 'FORTRAN/TK example'//dn)
       call TkWm('geometry'c, '.'c, '=300x200'//dn)

* Create the buttons and entry widgets...
* ...and put them in the window:

       call TkPack(TkButton('.hellobtm'c,
     &                      '-text'c,      'Hello world'c,
     &                      '-rexx'c,      'hellobtm_click'//dn),
     &             TkButton('.exitbtm'c,
     &                      '-text'c,      'EXIT'c,
     &                      '-underline'c, '0'c,
     &                      '-rexx'c,      'Quit'//dn)//'A',
     &             '-expand'c, 'yes'c,
     &             '-fill'c,   'both'//dn)
       call TkPack(TkEntry('.entry1'//dn),
     &             '-fill'c,   'x'//dn)
       call TkPack(TkButton('.changebtm'c,
     &                      '-text'c,      'Change caption'c,
     &                      '-underline'c, '0'c,
     &                      '-rexx'c,      'changebtm_click'//dn),
     &             '-expand'c, 'yes'c,
     &             '-fill'c,   'both'//dn)

* Set Entry widgets text:
       call TkInsert('.entry1'c, '1'c,
     &               TkCget('.hellobtm'c, '-text'//dn)//dn)

* Display Intro-MessageBox:
* (DOESN'T WORKS WITH WINDOWS YET - Rexx/Tk Problem)
c$ifndef __WIN__
       call TkMessageBox('-message'c,
     &                   'Welcome to the great FORTRAN/TK example '//
     &                   'program. *Stunning*'c,
     &                   '-title'c, 'FORTRAN/TK example'c,
     &                   '-icon'c, 'info'//dn)
c$endif
      end