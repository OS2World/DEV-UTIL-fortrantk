;###
;### FORTRAN/TK
;### Assembly part for getting a multiple number of arguments
;### and to manipulate the return value
;###

_TEXT     segment byte public 'CODE'
          assume  CS:_TEXT

          public  TKGETARGS
TKGETARGS proc    near
          push ebp
          mov  ebp,esp
          push esi

          push eax
          push ebx
          push ecx
          push edx

          mov  esi,ebp
          add  esi,ecx           ;add  esi,60d    ;44d  (+16d)
          mov word ptr [ebx],0d  ;numargs = 0

          inc  eax    ;add  eax,4d
          inc  eax
          inc  eax
          inc  eax
          mainl:

           mov  ecx,[esi]
           mov  edx,[ecx]

           mov  [eax],edx
           dec  eax   ;sub  eax,4d
           dec  eax
           dec  eax
           dec  eax

           push edx
           inc  ecx    ;add  ecx,4d
           inc  ecx
           inc  ecx
           inc  ecx
           mov  edx,[ecx]
           mov  ecx,edx
           pop  edx

           inc  byte ptr [ebx]
           push ebx

           mov  ebx,edx
           dec  ebx
           countlen:
            inc  ebx
           cmp  byte ptr [ebx],0d
           jne  countlen

           sub  ebx,edx

           add  edx,ecx
           mov  [eax],ebx
           add  eax,12d

           pop  ebx

           inc  esi    ;add  esi,4d
           inc  esi
           inc  esi
           inc  esi

          dec  edx
          dec  edx
          cmp  byte ptr [edx],0d
          jne  mainl
          inc  edx
          cmp  byte ptr [edx],0d
          jne  mainl

          pop  edx
          pop  ecx
          pop  ebx
          pop  eax

          pop  esi
          pop  ebp
          ret
TKGETARGS endp

;set a correct return value
;(set new return variable)
          public  RESETARG
RESETARG  proc    near
          push ebp
          mov ebp,esp
          push edx

           add ebp,60d          ;40d (+20d)
           push eax
           mov eax,4d
           mul ebx
           add ebp,eax
           pop eax

          mov ebp,[ebp]
          mov eax,[eax]
          mov [ebp],eax
          add ebp,4d
          mov word ptr [ebp],256d

          pop edx
          pop ebp
          ret
RESETARG  endp

_TEXT     ends
          end

