@echo off
rem
rem $Id$
rem

clipper hbextern.prg /w /n /i..\..\include\
rtlink fi hbextern
del *.obj
