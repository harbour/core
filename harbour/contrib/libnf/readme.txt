This library has been ported to Harbour by Luiz Rafael Culik

//TODO
The follow functions must be rewrite in C
asm\ADAPTER.ASM               ;     FT_ADAPTER()
asm\DEFAULT.ASM               ;     FT_DEFAULT()
asm\IAMIDLE.ASM               ;     FT_IAmIdle()
asm\INP.ASM                   ;      FT_INP()
asm\ISPRINT.ASM               ;     FT_ISPRINT()
asm\OUTP.ASM                  ;      FT_OUTP()
asm\PUTKEY.ASM                ;     FT_PUTKEY()
asm\REBOOT.ASM                ;      FT_REBOOT()
asm\RESTATT.ASM               ;     FT_RESTATT()
asm\SAVEATT.ASM               ;     FT_SAVEATT()
asm\SETKEYS.ASM               ;     FT_SETKEYS()
asm\SETLASTK.ASM              ;     FT_LASTKEY()
asm\SHADOW.ASM                ;      FT_SHADOW()

//TOFIX

The follow functions need to be fixed.
CINT86.C                    ;     FT_INT86()

  This function is Called from many prg source code
fttext.c
   I never could compiler this file under BCC

The follow functions Need that FT_INT86() been fixed or writed a "C" Wrapper


SETDATE.PRG               FT_SETDATE()
SETTIME.PRG               FT_SETTIME()
SYSMEM.PRG                FT_SYSMEM()
SCANCODE.PRG              FT_SCANCODE()
NWSEM.PRG                 FT_NWSEMOPEN()
NWSEM.PRG                 FT_NWSEMEX()
NWSEM.PRG                 FT_NWSEMWAIT()
NWSEM.PRG                 FT_NWSEMSIG()
NWSEM.PRG                 FT_NWSEMCLOSE()
NWSEM.PRG                 FT_NWSEMLOCK()
NWSEM.PRG                 FT_NWSEMUNLOCK()
NWUID.PRG                 FT_NWUID()
FLOPTST.PRG               FT_FLOPTST()

