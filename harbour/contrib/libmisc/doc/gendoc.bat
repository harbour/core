if "%1" =="/OS2" goto OS2
if "%1" =="/NGI" goto NG
if "%1" =="/RTF" goto RTF
if "%1" =="/HTM" goto HTM
ECHO Assembling input files
:NG
hbdoc /ngi libmisc.lnk libmisc.rsp
REM Compile the sources
Echo Compiling the sources
Processing Input Files
Copy ngi\funcam.txt+ngi\funcn_.txt overview.ngi
Compiling Sources
ngxc overview.ngi
Linking the Guide
ngxl libmisc.lnk
del *.ngi
del *.ngo
del ngi\*.txt
del ngi\*.ngi
GOTO END
:OS2
   hbdoc /OS2 libmisc.lnk libmisc.rsp
GOTO END
:RTF
   hbdoc /RTF libmisc.lnk libmisc.rsp
   HCW HARBOUR.HPJ
GOTO END
:HTM
    hbdoc /HTM libmisc.lnk libmisc.rsp
   GOTO END
:END
del ass*.bat

