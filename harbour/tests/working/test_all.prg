//NOTEST
// AutoMatic Test Bank
// Patrick Mast and David G. Holm
// Compiler independent, but not platform independent (creates a DOS style batch file).
// Specify the hbxxx batch file name to use to build with on the command line.
// Defaults to HB32.
// The TESTALL.BAT batch file has restart capability. For example, if there is an error
// in testgt.prg, find and fix the problem, then restart by running "TESTALL TESTGT".

Function Main( cOption, cCmd )
LOCAL aDir,f,n,o,p,cRead

   aDir:=Directory("*.PRG")
   o=fCreate("TestAll.Bat")
   IF Empty( cOption )
      cOption:="HB32"
   ELSEIF Upper( cOption ) == "HRB"
      fWrite(o,"del test_all.out"+chr(13)+chr(10))
   ENDIF
   IF Empty( cCmd )
      cCmd := "call "
   ELSE
      cCmd += " /c "
   ENDIF

   fWrite(o,"if not .%1==. goto %1" + Chr(13) + Chr(10))

   FOR f=1 TO Len(aDir)
      IF TestIt(aDir[f][1])
         p = At(".PRG",Upper(aDir[f][1]))
         IF p > 1
            n := Left(aDir[f][1],p-1)
            fWrite(o,":" + n + Chr(13) + Chr(10))
            IF !Empty( cOption ) .and. Upper( cOption ) == "HRB"
               fWrite(o,;
                  "..\..\bin\harbour "+aDir[f][1]+" /n /gHRB /i..\..\include >> test_all.out"+Chr(13)+Chr(10)+;
                  "if errorlevel 1 goto end"+Chr(13)+Chr(10)+;
                  "runner "+Left(aDir[f][1],Len(aDir[f][1])-4)+".hrb >> test_all.out"+Chr(13)+Chr(10) )
            ELSE
               fWrite(o,cCmd + cOption + " " + n + Chr(13) + Chr(10);
                + "if errorlevel 1 goto end" + Chr(13) + Chr(10) + Chr(13) + Chr(10))
            ENDIF
         ENDIF
      ENDIF
   NEXT

   fWrite(o,":END"+Chr(13)+Chr(10))
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
