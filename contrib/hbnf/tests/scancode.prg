#require "hbnf"

#define SCANCODE_ESCAPE  ( hb_BChar( 27 ) + hb_BChar( 1 ) )

PROCEDURE Main()

   LOCAL cKey

   ? "Press any key, ESCape to exit:"

   DO WHILE !( ( cKey := ft_ScanCode() ) == SCANCODE_ESCAPE )
      ? ;
         Str( hb_BPeek( cKey, 1 ), 3 ), ;
         Str( hb_BPeek( cKey, 2 ), 3 )
   ENDDO

   RETURN
