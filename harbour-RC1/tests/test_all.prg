//NOTEST
//
// $Id$
//

// AutoMatic Test Bank
// Patrick Mast and David G. Holm
// Compiler independent, but not platform independent (creates a DOS style batch file).
// Specify the hbxxx batch file name to use to build with on the command line.
// Defaults to "run_prg".
// The TEST_ALL.BAT batch file has restart capability. For example, if there is an error
// in testgt.prg, find and fix the problem, then restart by running "TEST_ALL TESTGT".

#include "directry.ch"
#include "fileio.ch"

Function Main( cOption, cCmd )
   LOCAL aDir,f,n,o,p,cRead

   SET DATE ANSI
   SET CENTURY ON

   aDir := Directory("*.prg")
   o := fCreate("test_all.bat")
   IF Empty( cOption )
      cOption:="run_prg"
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
            fWrite(o,cCmd + cOption + " " + n + Chr(13) + Chr(10);
                + "if errorlevel 1 goto end" + Chr(13) + Chr(10) + Chr(13) + Chr(10))
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
         nH2=fCreate("nottestd.txt")
      ELSE
         nH2=fOpen("nottestd.txt", FO_WRITE)
      ENDIF
      fSeek(nH2, 0, FS_END)
      fWrite(nH2,DtoC(Date())+"  "+Time()+"  "+cFile+Chr(13)+Chr(10))
      fClose(nH2)
   ENDIF

RETURN lRetu
