@echo off
rem
rem $Id$
rem

clipper hbclip.prg   /n
clipper hbkeyput.prg /n
clipper hbshadow.prg /n
clipper hbvaltoc.prg /n

cl /c /AL /Zl /Oalt /Gs /W3 hbcolind.c
cl /c /AL /Zl /Oalt /Gs /W3 hbstod.c
cl /c /AL /Zl /Oalt /Gs /W3 hbstodx.c

del hbclip.lib

lib hbclip -+ hbclip.obj   ,,
lib hbclip -+ hbkeyput.obj ,,
lib hbclip -+ hbshadow.obj ,,
lib hbclip -+ hbvaltoc.obj ,,

lib hbclip -+ hbcolind.obj ,,
lib hbclip -+ hbstod.obj   ,,
lib hbclip -+ hbstodx.obj  ,,

del hbclip.bak
del *.obj
