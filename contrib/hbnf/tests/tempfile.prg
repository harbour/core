/*
 * $Id$
 */

#require "hbnf"

#include "fileio.ch"

PROCEDURE Main( cPath, cHide )

   LOCAL cFile, nHandle

   cFile := ft_TempFil( cPath, cHide == "Y" )

   IF ! Empty( cFile )
      ? cFile
      nHandle := FOpen( cFile, FO_WRITE )
      FWrite( nHandle, "This is a test!" )
      FClose( nHandle )
   ELSE
      ? "An error occurred"
   ENDIF

   RETURN
