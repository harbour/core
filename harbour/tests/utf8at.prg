/*
 * $Id$
 */

/* hb_utf8At() / hb_utf8RAt() test
   UTF8-aware hb_At()/hb_rAt() */

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

   ? "ISO-8859-1 -", PadR( ii, l ), "=>", i
   ?
   ? "CP850      -", PadR( dd, l ), "=>", d
   ?
   ? "UTF-8      -", PadR( uu, l ), "=>", u
   ?
   ?        At( ii, i ),        rAt( ii, i ), "- ISO-8859-1 at/rat"
   ?     hb_At( ii, i ),     hb_rAt( ii, i ), "- ISO-8859-1 hb_at/rat"
   ? hb_utf8At( ii, i ), hb_utf8RAt( ii, i ), "- ISO-8859-1 hb_utf8at/rat"
   ?        At( dd, d ),        rAt( dd, d ), "- CP850      at/rat"
   ?     hb_At( dd, d ),     hb_rAt( dd, d ), "- CP850      hb_at/rat"
   ? hb_utf8At( dd, d ), hb_utf8RAt( dd, d ), "- CP850      hb_utf8at/rat"
   ?        At( uu, u ),        rAt( uu, u ), "- UTF-8      at/rat"
   ?     hb_At( uu, u ),     hb_rAt( uu, u ), "- UTF-8      hb_at/rat"
   ? hb_utf8At( uu, u ), hb_utf8RAt( uu, u ), "- UTF-8      hb_utf8at/rat"
   ?
   ?     hb_At( ii, i, 33 ),     hb_rAt( ii, i, 33 ),     hb_At( ii, i, , 33 ),     hb_rAt( ii, i, , 33 ), "- ISO-8859-1 hb_at/rat"
   ? hb_utf8At( ii, i, 33 ), hb_utf8RAt( ii, i, 33 ), hb_utf8At( ii, i, , 33 ), hb_utf8RAt( ii, i, , 33 ), "- ISO-8859-1 hb_utf8at/rat"
   ?     hb_At( dd, d, 33 ),     hb_rAt( dd, d, 33 ),     hb_At( dd, d, , 33 ),     hb_rAt( dd, d, , 33 ), "- CP850      hb_at/rat"
   ? hb_utf8At( dd, d, 33 ), hb_utf8RAt( dd, d, 33 ), hb_utf8At( dd, d, , 33 ), hb_utf8RAt( dd, d, , 33 ), "- CP850      hb_utf8at/rat"
   ?     hb_At( uu, u, 33 ),     hb_rAt( uu, u, 33 ),     hb_At( uu, u, , 33 ),     hb_rAt( uu, u, , 33 ), "- UTF-8      hb_at/rat"
   ? hb_utf8At( uu, u, 33 ), hb_utf8RAt( uu, u, 33 ), hb_utf8At( uu, u, , 33 ), hb_utf8RAt( uu, u, , 33 ), "- UTF-8      hb_utf8at/rat"

   RETURN
