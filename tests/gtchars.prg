/*
 * demonstration/test code for terminal character output and codepage
 *    translations
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
 */

/* UTF-8 */

#include "hbgtinfo.ch"

#define POL_MAZ   hb_Translate( "ĄĆĘŁŃÓŚŹŻąćęłńóśźż", "UTF8", "PLMAZ" )
#define POL_ISO   hb_Translate( "ĄĆĘŁŃÓŚŹŻąćęłńóśźż", "UTF8", "PLISO" )
#define POL_852   hb_Translate( "ĄĆĘŁŃÓŚŹŻąćęłńóśźż", "UTF8", "PL852" )
#define POL_WIN   hb_Translate( "ĄĆĘŁŃÓŚŹŻąćęłńóśźż", "UTF8", "PLWIN" )

REQUEST HB_CODEPAGE_UTF8

REQUEST HB_CODEPAGE_PLMAZ
REQUEST HB_CODEPAGE_PLISO
REQUEST HB_CODEPAGE_PL852
REQUEST HB_CODEPAGE_PLWIN

PROCEDURE Main( cTermCP, cHostCP, lBoxChar )

   LOCAL i, j, x

   IF Empty( cTermCP )
      cTermCP := "PLISO"
   ELSE
      cTermCP := Upper( cTermCP )
   ENDIF
   IF Empty( cHostCP )
      cHostCP := "PLMAZ"
   ELSE
      cHostCP := Upper( cHostCP )
   ENDIF
   lBoxChar := ! Empty( lBoxChar )

   hb_gtInfo( HB_GTI_FONTNAME, "fixed" )
   hb_gtInfo( HB_GTI_FONTWIDTH, 9 )
   hb_gtInfo( HB_GTI_FONTSIZE, 20 )

   hb_cdpSelect( cHostCP )

   hb_SetTermCP( cTermCP,, lBoxChar )

   ? OS(), Version(), Date(), Time()
   ? "GT" + hb_gtVersion(), hb_gtVersion( 1 )
   ? "Host codpage: " + cHostCP + ", terminal codepage: " + cTermCP
   ?

// hb_gtInfo( HB_GTI_COMPATBUFFER, .F. )
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
   ? "ISO-8859-2: say[ " + POL_ISO + " ]"; dspboxch( ", box[ " + POL_ISO + " ]" )
   ? "   Mazovia: say[ " + POL_MAZ + " ]"; dspboxch( ", box[ " + POL_MAZ + " ]" )
   ? "   CP-1250: say[ " + POL_WIN + " ]"; dspboxch( ", box[ " + POL_WIN + " ]" )
   ? "    CP-852: say[ " + POL_852 + " ]"; dspboxch( ", box[ " + POL_852 + " ]" )
   ? Chr( 4 ) + Chr( 16 ) + Chr( 17 ) + Chr( 24 ) + Chr( 25 ) + Chr( 26 ) + Chr( 27 ) + Chr( 30 ) + Chr( 31 )
   hb_gtInfo( HB_GTI_ESCDELAY, 5 )
   Inkey( 0 )
   Alert( "ALERT BOX" )

   RETURN

FUNCTION dspboxch( cStr )

   LOCAL i, r, c

   FOR i := 1 TO Len( cStr )
      r := Row(); c := Col()
      DispBox( r, c, r, c, SubStr( cStr, i, 1 ) )
      SetPos( r, c + 1 )
   NEXT

   RETURN NIL
