@echo off
rem 
rem $Id$
rem 

..\bin\harbour %1 /n /i..\include /gh
..\bin\hbrun %1
del %1.hrb
