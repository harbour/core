/*
 * $Id$
 */

PROCEDURE Main( cPath, cHide )

   LOCAL cFile, nHandle

   cFile := FT_TEMPFIL( cPath, ( cHide == "Y" ) )

   IF ! Empty( cFile )
      QOut( cFile )
      nHandle := FOpen( cFile, FO_WRITE )
      FWrite( nHandle, "This is a test!" )
      FClose( nHandle )
   ELSE
      QOut( "An error occurred" )
   ENDIF

   RETURN
