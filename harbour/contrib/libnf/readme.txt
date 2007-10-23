/*
 * $Id$
 */

This library has been ported to Harbour by Luiz Rafael Culik

//TODO
The follow functions must be rewrite in C
asm\ADAPTER.ASM               ;     FT_ADAPTER()
asm\DEFAULT.ASM               ;     FT_DEFAULT()
asm\INP.ASM                   ;     FT_INP()
asm\OUTP.ASM                  ;     FT_OUTP()
asm\REBOOT.ASM                ;     FT_REBOOT()
asm\RESTATT.ASM               ;     FT_RESTATT()
asm\SAVEATT.ASM               ;     FT_SAVEATT()

The follow functions need to be fixed.
CINT86.C                      ;     FT_INT86()

The following functions need FT_INT86() or to be rewritten in C:

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
