#include "inkey.ch"

PROCEDURE Main()

   LOCAL kX, k

#if defined( __HBSCRIPT__HBSHELL )
   /* GTWIN doesn't support extended keycodes */
   #if defined( __PLATFORM__WINDOWS )
      hbshell_gtSelect( "GTWVT" )
   #endif
#endif

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
         ! HB_ISNULL( hb_keyChar( k ) )
         ?? "", ;
            "char:", iif( k > 256, ;
            "U+" + hb_NumToHex( hb_keyVal( k ), 4 ), ;
            Str( k, 6 ) ), "", hb_keyChar( k ), hb_keyChar( kX )
      ENDIF

      IF k == hb_keyCode( "@" ) .AND. NextKey() == 0
         EXIT
      ENDIF
   ENDDO

   RETURN

#if ! defined( __HBSCRIPT__HBSHELL ) .AND. defined( __PLATFORM__WINDOWS )

PROCEDURE hb_GTSYS()  /* must be a public function */

   REQUEST HB_GT_WVT_DEFAULT

   RETURN

#endif
