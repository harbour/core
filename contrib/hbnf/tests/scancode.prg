
#require "hbnf"

#define SCANCODE_ESCAPE   ( hb_BChar( 27 ) + hb_BChar( 1 ) )

PROCEDURE Main()

   LOCAL cKey

   CLS
   ? "Press any key, ESCape to exit:"

   DO WHILE .T.
      cKey := ft_ScanCode()
      ? Str( hb_BCode( hb_BSubStr( cKey, 1, 1 ) ), 3 ) + ", " + Str( hb_BCode( hb_BSubStr( cKey, 2, 1 ) ), 3 )
      IF cKey == SCANCODE_ESCAPE
         EXIT
      ENDIF
   ENDDO

   RETURN
