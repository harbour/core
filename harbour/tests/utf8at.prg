/*
 * $Id$
 */

/* hb_utf8at() / hb_utf8rat() test
   UTF8-aware hb_at()/hb_rat() */

#include "simpleio.ch"

REQUEST HB_CODEPAGE_FR850
REQUEST HB_CODEPAGE_FRISO

PROCEDURE Main()

   #define _UTF8_E_ACUTE       hb_BChar( 0xC3 ) + hb_BChar( 0xA9 )
   #define _UTF8_E_CIRCUMFLEX  hb_BChar( 0xC3 ) + hb_BChar( 0xAA )

   LOCAL u := "Une r" + _UTF8_E_CIRCUMFLEX + "ve est la moiti" + _UTF8_E_ACUTE + " d'une r" + _UTF8_E_ACUTE + "alit" + _UTF8_E_ACUTE + "."
   LOCAL i := hb_translate( u, "UTF8", "FRISO" )
   LOCAL d := hb_translate( u, "UTF8", "FR850" )

   LOCAL uu := _UTF8_E_ACUTE
   LOCAL ii := hb_translate( uu, "UTF8", "FRISO" )
   LOCAL dd := hb_translate( uu, "UTF8", "FR850" )

   LOCAL l := Len( uu ) + 2

   ? "ISO-8859-1 -", PadR( ii, l ), "=>", i
   ?
   ? "CP850      -", PadR( dd, l ), "=>", d
   ?
   ? "UTF-8      -", PadR( uu, l ), "=>", u
   ?
   ?        at( ii, i ),        rat( ii, i ), "- ISO-8859-1 at/rat"
   ?     hb_at( ii, i ),     hb_rat( ii, i ), "- ISO-8859-1 hb_at/rat"
   ? hb_utf8at( ii, i ), hb_utf8rat( ii, i ), "- ISO-8859-1 hb_utf8at/rat"
   ?        at( dd, d ),        rat( dd, d ), "- CP850      at/rat"
   ?     hb_at( dd, d ),     hb_rat( dd, d ), "- CP850      hb_at/rat"
   ? hb_utf8at( dd, d ), hb_utf8rat( dd, d ), "- CP850      hb_utf8at/rat"
   ?        at( uu, u ),        rat( uu, u ), "- UTF-8      at/rat"
   ?     hb_at( uu, u ),     hb_rat( uu, u ), "- UTF-8      hb_at/rat"
   ? hb_utf8at( uu, u ), hb_utf8rat( uu, u ), "- UTF-8      hb_utf8at/rat"
   ?
   ?     hb_at( ii, i, 33 ),     hb_rat( ii, i, 33 ),     hb_at( ii, i, , 33 ),     hb_rat( ii, i, , 33 ), "- ISO-8859-1 hb_at/rat"
   ? hb_utf8at( ii, i, 33 ), hb_utf8rat( ii, i, 33 ), hb_utf8at( ii, i, , 33 ), hb_utf8rat( ii, i, , 33 ), "- ISO-8859-1 hb_utf8at/rat"
   ?     hb_at( dd, d, 33 ),     hb_rat( dd, d, 33 ),     hb_at( dd, d, , 33 ),     hb_rat( dd, d, , 33 ), "- CP850      hb_at/rat"
   ? hb_utf8at( dd, d, 33 ), hb_utf8rat( dd, d, 33 ), hb_utf8at( dd, d, , 33 ), hb_utf8rat( dd, d, , 33 ), "- CP850      hb_utf8at/rat"
   ?     hb_at( uu, u, 33 ),     hb_rat( uu, u, 33 ),     hb_at( uu, u, , 33 ),     hb_rat( uu, u, , 33 ), "- UTF-8      hb_at/rat"
   ? hb_utf8at( uu, u, 33 ), hb_utf8rat( uu, u, 33 ), hb_utf8at( uu, u, , 33 ), hb_utf8rat( uu, u, , 33 ), "- UTF-8      hb_utf8at/rat"

   RETURN
