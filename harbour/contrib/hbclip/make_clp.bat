@echo off
rem
rem $Id$
rem

clipper hbclip.prg   /n /w
clipper hbkeyput.prg /n /w
clipper hbshadow.prg /n /w
clipper hbvaltoc.prg /n /w

cl /c /AL /Zl /Oalt /Gs /W3 hbarg.c
cl /c /AL /Zl /Oalt /Gs /W3 hbcolind.c
cl /c /AL /Zl /Oalt /Gs /W3 hbstod.c
cl /c /AL /Zl /Oalt /Gs /W3 hbstodx.c

del hbclip.lib

lib hbclip -+ hbclip.obj   ,,
lib hbclip -+ hbkeyput.obj ,,
lib hbclip -+ hbshadow.obj ,,
lib hbclip -+ hbvaltoc.obj ,,

lib hbclip -+ hbarg.obj ,,
lib hbclip -+ hbcolind.obj ,,
lib hbclip -+ hbstod.obj   ,,
lib hbclip -+ hbstodx.obj  ,,

del hbclip.bak
del *.obj
