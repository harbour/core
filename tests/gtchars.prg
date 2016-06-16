/*
 * Demonstration/test code for terminal character output and codepage
 * translations
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
 */

#include "hbgtinfo.ch"

#define NAT_STR  "ĄĆĘŁŃÓŚŹŻąćęłńóśźż"
#define NAT_CP   { "PLISO", "PLMAZ", "PLWIN", "PL852" }

REQUEST HB_CODEPAGE_PLMAZ
REQUEST HB_CODEPAGE_PLISO
REQUEST HB_CODEPAGE_PL852
REQUEST HB_CODEPAGE_PLWIN

PROCEDURE Main( cTermCP, cHostCP, lBoxChar )

   LOCAL i, j, x, cp

   hb_gtInfo( HB_GTI_FONTNAME, "fixed" )
   hb_gtInfo( HB_GTI_FONTWIDTH, 9 )
   hb_gtInfo( HB_GTI_FONTSIZE, 20 )

   cTermCP := Upper( hb_defaultValue( cTermCP, "PLISO" ) )
   cHostCP := Upper( hb_defaultValue( cHostCP, "PLMAZ" ) )

   hb_cdpSelect( cHostCP )
   hb_SetTermCP( cTermCP,, ! Empty( lBoxChar ) )

   ? OS(), Version(), Date(), Time()
   ? "GT" + hb_gtVersion(), hb_gtVersion( 1 )
   ? "Host codpage:", hb_cdpSelect() + ", terminal codepage:", cTermCP
   ?

#if 0
   hb_gtInfo( HB_GTI_COMPATBUFFER, .F. )
#endif
   ?
   FOR i := 0 TO 15
      FOR j := 0 TO 15
         x := i * 16 + j
         DispOut( "  " + Chr( x ) )
      NEXT
      ?
   NEXT
   Inkey( 0 )
   ?; dspboxch( hb_UTF8ToStrBox( "┌─┬─┐  ╔═╦═╗  ╒═╤═╕  ╓─╥─╖  ▄▄▄ ░▒▓█  ░ ▒ ▓ █" ) )
   ?; dspboxch( hb_UTF8ToStrBox( "│ │ │  ║ ║ ║  ├─┼─┤  ╟─╫─╢  ▌■▐" ) )
   ?; dspboxch( hb_UTF8ToStrBox( "├─┼─┤  ╠═╬═╣  │ │ │  ║ ║ ║  ▌█▐ █■█" ) )
   ?; dspboxch( hb_UTF8ToStrBox( "│ │ │  ║ ║ ║  ╞═╪═╡  ╠═╬═╣  ▌■▐" ) )
   ?; dspboxch( hb_UTF8ToStrBox( "└─┴─┘  ╚═╩═╝  ╘═╧═╛  ╙─╨─╜  ▀▀▀ √ « »" ) )
   ?
   FOR EACH cp IN NAT_CP
      ? PadL( hb_cdpUniID( cp ), 10 ) + ": say[ " + hb_Translate( NAT_STR, "UTF8", cp ) + " ]"
      dspboxch( ", box[ " + hb_Translate( NAT_STR, "UTF8", cp ) + " ]" )
   NEXT
   ? Chr( 4 ) + Chr( 16 ) + Chr( 17 ) + Chr( 24 ) + Chr( 25 ) + Chr( 26 ) + Chr( 27 ) + Chr( 30 ) + Chr( 31 )
   hb_gtInfo( HB_GTI_ESCDELAY, 5 )
   Inkey( 0 )
   Alert( "ALERT BOX" )

   RETURN

STATIC PROCEDURE dspboxch( cStr )

   LOCAL i, r, c

   FOR i := 1 TO Len( cStr )
      r := Row()
      c := Col()
      hb_DispBox( r, c, r, c, SubStr( cStr, i, 1 ) )
      SetPos( r, c + 1 )
   NEXT

   RETURN
