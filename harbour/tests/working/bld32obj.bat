..\..\bin\harbour %1 /fobj32 /n
tlink32 -L..\..\libs\b32;c:\bc5\lib c:\bc5\lib\c0x32.obj %1.obj hvm.obj,%1.exe,, harbour.lib terminal.lib import32.lib cw32mt.lib

