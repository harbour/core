/*
 * $Id$
 */

Functions to be rewritten:

c\video1.c                FT_VIDSTR(), FT_WRTCHR(), FT_CLS(), FT_REVATTR(), FT_REVCHR()

Functions to be fixed:

cint86.c                  FT_INT86()

Functions using FT_INT86() to be rewritten:

floptst.prg               FT_FLOPTST()
nwsem.prg                 FT_NWSEMOPEN()
nwsem.prg                 FT_NWSEMEX()
nwsem.prg                 FT_NWSEMWAIT()
nwsem.prg                 FT_NWSEMSIG()
nwsem.prg                 FT_NWSEMCLOSE()
nwsem.prg                 FT_NWSEMLOCK()
nwsem.prg                 FT_NWSEMUNLOCK()
nwuid.prg                 FT_NWUID()
scancode.prg              FT_SCANCODE()
setdate.prg               FT_SETDATE()
settime.prg               FT_SETTIME()
sysmem.prg                FT_SYSMEM()
vidcur.prg                FT_GETVCUR(), FT_SETVCUR()
