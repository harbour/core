/*
 * $Id$
 */

This library has been ported to Harbour by Luiz Rafael Culik

//TODO
The following functions must be rewritten in C:
asm\adapter.asm               ;     FT_ADAPTER()
asm\default.asm               ;     FT_DEFAULT()
asm\inp.asm                   ;     FT_INP()
asm\outp.asm                  ;     FT_OUTP()
asm\reboot.asm                ;     FT_REBOOT()
asm\restatt.asm               ;     FT_RESTATT()
asm\saveatt.asm               ;     FT_SAVEATT()

The following functions need to be fixed:
cint86.c                      ;     FT_INT86()

The following functions need FT_INT86() or to be rewritten in C:

setdate.prg               FT_SETDATE()
settime.prg               FT_SETTIME()
sysmem.prg                FT_SYSMEM()
scancode.prg              FT_SCANCODE()
nwsem.prg                 FT_NWSEMOPEN()
nwsem.prg                 FT_NWSEMEX()
nwsem.prg                 FT_NWSEMWAIT()
nwsem.prg                 FT_NWSEMSIG()
nwsem.prg                 FT_NWSEMCLOSE()
nwsem.prg                 FT_NWSEMLOCK()
nwsem.prg                 FT_NWSEMUNLOCK()
nwuid.prg                 FT_NWUID()
floptst.prg               FT_FLOPTST()
