@rem
@rem $Id$
@rem

@echo off

clipper hbextern.prg /w /n /i..\..\include\
rtlink fi hbextern
del *.obj
