@echo off
rem 
rem $Id$
rem 

hbdoc -htm genwww.lnk genwww.rsp
cd htm
echo renaming Harbour.htm to index.htm
ren harbour.htm index.htm


