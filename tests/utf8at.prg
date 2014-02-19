/* hb_utf8At() / hb_utf8RAt() test
   UTF-8 aware hb_At()/hb_RAt() */

#include "simpleio.ch"

REQUEST HB_CODEPAGE_FR850
REQUEST HB_CODEPAGE_FRISO

PROCEDURE Main()

   #define _UTF8_E_ACUTE       hb_BChar( 0xC3 ) + hb_BChar( 0xA9 )
   #define _UTF8_E_CIRCUMFLEX  hb_BChar( 0xC3 ) + hb_BChar( 0xAA )

   LOCAL u := "Une r" + _UTF8_E_CIRCUMFLEX + "ve est la moiti" + _UTF8_E_ACUTE + " d'une r" + _UTF8_E_ACUTE + "alit" + _UTF8_E_ACUTE + "."
   LOCAL i := hb_Translate( u, "UTF8", "FRISO" )
   LOCAL d := hb_Translate( u, "UTF8", "FR850" )

   LOCAL uu := _UTF8_E_ACUTE
   LOCAL ii := hb_Translate( uu, "UTF8", "FRISO" )
   LOCAL dd := hb_Translate( uu, "UTF8", "FR850" )

   LOCAL l := Len( uu ) + 2

   ? PadR( hb_cdpUniID( "FRISO" ), 10 ), "-", PadR( ii, l ), "=>", i
   ?
   ? PadR( hb_cdpUniID( "FR850" ), 10 ), "-", PadR( dd, l ), "=>", d
   ?
   ? PadR( "UTF-8", 10 )               , "-", PadR( uu, l ), "=>", u
   ?
   ?        At( ii, i ),        RAt( ii, i ), "-", PadR( hb_cdpUniID( "FRISO" ), 10 ), "at/rat"
   ?     hb_At( ii, i ),     hb_RAt( ii, i ), "-", PadR( hb_cdpUniID( "FRISO" ), 10 ), "hb_at/rat"
   ? hb_utf8At( ii, i ), hb_utf8RAt( ii, i ), "-", PadR( hb_cdpUniID( "FRISO" ), 10 ), "hb_utf8at/rat"
   ?        At( dd, d ),        RAt( dd, d ), "-", PadR( hb_cdpUniID( "FR850" ), 10 ), "at/rat"
   ?     hb_At( dd, d ),     hb_RAt( dd, d ), "-", PadR( hb_cdpUniID( "FR850" ), 10 ), "hb_at/rat"
   ? hb_utf8At( dd, d ), hb_utf8RAt( dd, d ), "-", PadR( hb_cdpUniID( "FR850" ), 10 ), "hb_utf8at/rat"
   ?        At( uu, u ),        RAt( uu, u ), "-", PadR( "UTF-8", 10 )               , "at/rat"
   ?     hb_At( uu, u ),     hb_RAt( uu, u ), "-", PadR( "UTF-8", 10 )               , "hb_at/rat"
   ? hb_utf8At( uu, u ), hb_utf8RAt( uu, u ), "-", PadR( "UTF-8", 10 )               , "hb_utf8at/rat"
   ?
   ?     hb_At( ii, i, 33 ),     hb_RAt( ii, i, 33 ),     hb_At( ii, i, , 33 ),     hb_RAt( ii, i, , 33 ), "-", PadR( hb_cdpUniID( "FRISO" ), 10 ), "hb_at/rat"
   ? hb_utf8At( ii, i, 33 ), hb_utf8RAt( ii, i, 33 ), hb_utf8At( ii, i, , 33 ), hb_utf8RAt( ii, i, , 33 ), "-", PadR( hb_cdpUniID( "FRISO" ), 10 ), "hb_utf8at/rat"
   ?     hb_At( dd, d, 33 ),     hb_RAt( dd, d, 33 ),     hb_At( dd, d, , 33 ),     hb_RAt( dd, d, , 33 ), "-", PadR( hb_cdpUniID( "FR850" ), 10 ), "hb_at/rat"
   ? hb_utf8At( dd, d, 33 ), hb_utf8RAt( dd, d, 33 ), hb_utf8At( dd, d, , 33 ), hb_utf8RAt( dd, d, , 33 ), "-", PadR( hb_cdpUniID( "FR850" ), 10 ), "hb_utf8at/rat"
   ?     hb_At( uu, u, 33 ),     hb_RAt( uu, u, 33 ),     hb_At( uu, u, , 33 ),     hb_RAt( uu, u, , 33 ), "-", PadR( "UTF-8", 10 )               , "hb_at/rat"
   ? hb_utf8At( uu, u, 33 ), hb_utf8RAt( uu, u, 33 ), hb_utf8At( uu, u, , 33 ), hb_utf8RAt( uu, u, , 33 ), "-", PadR( "UTF-8", 10 )               , "hb_utf8at/rat"

   RETURN
