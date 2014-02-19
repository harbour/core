#include "inkey.ch"

PROCEDURE Main()

   LOCAL kX, k

   ? hb_gtVersion(), hb_gtVersion( 1 )

   Set( _SET_EVENTMASK, hb_bitOr( HB_INKEY_ALL, HB_INKEY_EXT ) )

   DO WHILE .T.

      kX := Inkey( 0 )
      k := hb_keyStd( kX )

      ? "", ;
        "key:", Str( k, 10 ), ;
        "ext: 0x" + hb_NumToHex( kX, 8 ), "->", ;
        hb_NumToHex( hb_keyMod( kX ), 2 ) + ":" + ;
        hb_NumToHex( hb_keyVal( kX ), 8 )
      IF ( k >= 32 .AND. k <= 126 ) .OR. ;
         ( k >= 160 .AND. k <= 255 ) .OR. ;
         Len( hb_keyChar( k ) ) > 0
         ?? "", ;
            "char:", iif( k > 256, ;
            "U+" + hb_NumToHex( hb_keyVal( k ), 4 ), ;
            Str( k, 6 ) ), "", hb_keyChar( k )
      ENDIF

      IF k == hb_keyCode( "@" ) .AND. NextKey() == 0
         EXIT
      ENDIF
   ENDDO

   RETURN
