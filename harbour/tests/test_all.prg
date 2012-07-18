//NOTEST
/*
 * $Id$
 */

/* AutoMatic Test Bank
   Patrick Mast and David G. Holm
   Compiler independent, but not platform independent (creates a DOS style batch file).
   Specify the hbxxx batch file name to use to build with on the command line.
   Defaults to "run_prg".
   The test_all.bat batch file has restart capability. For example, if there is an error
   in testgt.prg, find and fix the problem, then restart by running "TEST_ALL TESTGT".
 */

#include "directry.ch"
#include "fileio.ch"

PROCEDURE Main( cOption, cCmd )

   LOCAL aDir, f, n, o, p, cRead

   SET DATE ANSI
   SET CENTURY ON

   aDir := Directory( "*.prg" )
   o := FCreate( "test_all.bat" )
   IF Empty( cOption )
      cOption := "run_prg"
   ENDIF
   IF Empty( cCmd )
      cCmd := "call "
   ELSE
      cCmd += " /c "
   ENDIF

   FWrite( o, "if not .%1==. goto %1" + Chr( 13 ) + Chr( 10 ) )

   FOR f := 1 TO Len( aDir )
      IF TestIt( aDir[ f ][ F_NAME ] )
         p := At( ".prg", Lower( aDir[ f ][ F_NAME ] ) )
         IF p > 1
            n := Left( aDir[ f ][ F_NAME ], p - 1 )
            FWrite( o, ":" + n + Chr( 13 ) + Chr( 10 ) )
            FWrite( o, cCmd + cOption + " " + n + Chr( 13 ) + Chr( 10 ) +;
               "if errorlevel 1 goto end" + Chr( 13 ) + Chr( 10 ) + Chr( 13 ) + Chr( 10 ) )
         ENDIF
      ENDIF
   NEXT

   FWrite( o, ":END" + Chr( 13 ) + Chr( 10 ) )
   FClose( o )

   RETURN

FUNCTION TestIt( cFile )

   LOCAL nH1, lRetu, nH2

   nH1 := FOpen( cFile )
   lRetu := !( Upper( FReadStr( nH1, 8 ) ) == "//NOTEST" )
   FClose( nH1 )

   IF ! lRetu
      IF ! hb_FileExists( "NotTestd.txt" )
         nH2 := FCreate( "nottestd.txt" )
      ELSE
         nH2 := FOpen( "nottestd.txt", FO_WRITE )
      ENDIF
      FSeek( nH2, 0, FS_END )
      FWrite( nH2, DToC( Date() ) + "  " + Time() + "  " + cFile + Chr( 13 ) + Chr( 10 ) )
      FClose( nH2 )
   ENDIF

   RETURN lRetu
