/*
 * $Id$ 
 */

#define SCANCODE_ESCAPE   ( hb_BChar( 27 ) + hb_BChar( 1 ) )

PROCEDURE Main()

   LOCAL cKey

   CLS
   QOut( "Press any key, ESCape to exit:" )

   DO WHILE .T.
      cKey := FT_SCANCODE()
      QOut( Str( hb_BCode( hb_BSubStr( cKey, 1, 1 ) ), 3 ) + ", " + Str( hb_BCode( hb_BSubStr( cKey, 2, 1 ) ), 3 ) + hb_eol() )
      IF cKey == SCANCODE_ESCAPE
         EXIT
      ENDIF
   ENDDO

   RETURN

