/*
 * Strips spaces at EOL text files.
 *
 * Copyright 2012 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 */

#pragma -w3
#pragma -km+
#pragma -ko+

#include "directry.ch"

PROCEDURE Main( cMask )

   LOCAL aFile

   hb_default( @cMask, "*.txt" )

   FOR EACH aFile IN Directory( hb_osFileMask() )
      hb_MemoWrit( aFile[ F_NAME ], __stripeol( MemoRead( aFile[ F_NAME ] ) ) )
   NEXT

   RETURN

FUNCTION __stripeol( cFile )

   LOCAL cOK := ""
   LOCAL cLine

   FOR EACH cLine IN hb_ATokens( StrTran( StrTran( cFile, Chr( 13 ) ), Chr( 9 ), " " ), Chr( 10 ) )
      cOK += RTrim( cLine )
      IF cLine:__enumIndex() < Len( cLine:__enumBase )
         cOK += hb_eol()
      ENDIF
   NEXT

   RETURN cOK
