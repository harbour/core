//NOTEST
// AutoMatic Test Bank
// Patrick Mast
// For 32 bits Borland Compiler.

// Sorry, only clipper compile (I want this thing Compiled in Harbour!<g>, So give me Directory() Please)

Function Main()
LOCAL aDir,f,o,cRead

aDir:=Directory("*.PRG")
o=fCreate("Test_All.Bat")
FOR f=1 TO Len(aDir)
    IF TestIt(aDir[f][1])
       fWrite(o,;
              "..\..\bin\harbour "+aDir[f][1]+" /n /i..\..\include"+Chr(13)+Chr(10)+;
              "if errorlevel 1 goto end"+Chr(13)+Chr(10)+;
              "echo -O2 -e"+Left(aDir[f][1],Len(aDir[f][1])-4)+".EXE -I..\..\include ..\..\source\vm\hvm.c "+Left(aDir[f][1],Len(aDir[f][1])-4)+".C > b32.bc"+Chr(13)+Chr(10)+;
              "echo ..\..\libs\b32\harbour.lib  ..\..\libs\b32\terminal.lib >> b32.bc"+Chr(13)+Chr(10)+;
              "bcc32 @b32.bc"+Chr(13)+Chr(10)+;
              "if errorlevel 1 goto end"+Chr(13)+Chr(10)+;
              "del b32.bc"+Chr(13)+Chr(10)+Chr(13)+Chr(10))
    ENDIF
NEXT

fWrite(o,":END"+Chr(13)+Chr(10))
fWrite(o,"del b32.c"+Chr(13)+Chr(10))
fClose(o)

RETURN NIL



Function TestIt(cFile)
LOCAL nH1,lRetu,nH2

nH1=fOpen(cFile)
lRetu:=Upper(fReadStr(nH1,8))<>"//NOTEST"
fClose(nH1)

IF !lRetu
   IF !File("NotTestd.txt")
      nH2=fCreate("NotTestd.txt")
   ELSE
      nH2=fOpen("NotTestd.txt",1)
   ENDIF
   fSeek(nH2,0,2)
   fWrite(nH2,DtoC(Date())+"  "+Time()+"  "+cFile+Chr(13)+Chr(10))
   fClose(nH2)
ENDIF

RETURN lRetu
