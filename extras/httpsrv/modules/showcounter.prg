/*
 * Harbour Project source code:
 *    simple image counter
 *
 * Copyright 2009 Francesco Saverio Giudice <info / at / fsgiudice.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#if defined( HBMK_HAS_HBGD )

MEMVAR _SERVER  // defined in uHTTPD
MEMVAR _REQUEST // defined in uHTTPD

#define IMAGES_IN  hb_DirSepToOS( "../../../contrib/hbgd/tests/digits/" )
#define IMAGES_OUT ( _SERVER[ "DOCUMENT_ROOT" ] + hb_ps() + "counter" + hb_ps() )

#define DISPLAY_NUM  10

FUNCTION HRBMAIN()

   LOCAL cHtml

   IF hb_HHasKey( _REQUEST, "w" )

      cHtml := CreateCounter( hb_ntos( Val( _REQUEST[ "w" ] ) ) )
      IF ! Empty( cHtml )
         uhttpd_SetHeader( "Content-Type", "image/gif" )
         uhttpd_SetHeader( "Pragma", "no-cache" )
         uhttpd_SetHeader( "Content-Disposition", "inline; filename=counter" + hb_ntos( hb_RandomInt( 100 ) ) + ".gif" )
         uhttpd_Write( cHtml )
      ELSE
         uhttpd_SetHeader( "Content-Type", "text/html" )
         uhttpd_Write( "<h1>Error: No image created</h1>" )
      ENDIF
   ELSE
      uhttpd_SetHeader( "Content-Type", "text/html" )
      uhttpd_Write( "<h1>Error: no parameters passed</h1>" )
   ENDIF

   RETURN .T.

STATIC FUNCTION CreateCounter( cValue, cBaseImage )

   LOCAL oI, oIDigits, nWidth, nHeight, nDigits, nNumWidth, oTemp

#if 0
   LOCAL black, white, blue, red, green, cyan, gray
   LOCAL white
#endif
   LOCAL aNumberImages := {}
   LOCAL n, nValue

   // A value if not passed
   hb_default( @cValue, Str( hb_RandomInt( 1, 10 ^ DISPLAY_NUM ), DISPLAY_NUM ) )
   hb_default( @cBaseImage, "57chevy.gif" )

   IF ! hb_FileExists( IMAGES_IN + cBaseImage )
      RETURN NIL
   ENDIF

   nValue := Val( cValue )

   // Fix num length
   IF nValue > 10 ^ DISPLAY_NUM
      nValue := 10 ^ DISPLAY_NUM
   ENDIF

   cValue := StrZero( nValue, DISPLAY_NUM )

#if 0
   ? "Value = ", cValue

   // Check output directory
   IF ! hb_DirExists( IMAGES_OUT )
      DirMake( IMAGES_OUT )
   ENDIF
#endif

   /* Load a digits image in memory from file */
   oIDigits := GDImage():LoadFromGif( IMAGES_IN + cBaseImage )

   /* Get single number images */

   // Get dimensions
   nWidth  := oIDigits:Width()
   nHeight := oIDigits:Height()

   // Check base digits image
   DO CASE
   CASE nWidth % 10 == 0   // 0..9 digits
      nDigits := 10
   CASE nWidth % 11 == 0   // 0..9 :
      nDigits := 11
   CASE nWidth % 13 == 0   // 0..9 : am pm
      nDigits := 13
   OTHERWISE
      uhttpd_Write( "Error on digits image" )
   ENDCASE
   nNumWidth := nWidth / nDigits

#if 0
   ? "nNumWidth, nWidth, nHeight, nDigits = ", nNumWidth, nWidth, nHeight, nDigits
#endif

   /* extracts single digits */
   FOR n := 1 TO nDigits
      oTemp := oIDigits:Copy( ( n - 1 ) * nNumWidth, 0, nNumWidth, nHeight )
      // Here I have to clone the image, otherwise on var destruction I loose
      // the image in memory
      AAdd( aNumberImages, oTemp:Clone() )
   NEXT

   /* Create counter image in memory */
   oI := GDImage():New( nNumWidth * DISPLAY_NUM, nHeight )  // the counter
#if 0
   ? "Image dimensions: ", oI:Width(), oI:Height()

   /* Allocate background */
   white := oI:SetColor( 255, 255, 255 )

   /* Allocate drawing color */
   black := oI:SetColor( 0, 0, 0 )
   blue  := oI:SetColor( 0, 0, 255 )
   red   := oI:SetColor( 255, 0, 0 )
   green := oI:SetColor( 0, 255, 0 )
   cyan  := oI:SetColor( 0, 255, 255 )

   /* Draw rectangle */
   oI:Rectangle( 0, 0, 200, 30,, blue )
#endif

   /* Draw Digits */
   FOR n := 1 TO Len( cValue )
      // Retrieve the number from array in memory
      oTemp := aNumberImages[ Val( SubStr( cValue, n, 1 ) ) + 1 ]:Clone()
      // Save it to show the number for a position
      // oTemp:SaveGif( IMAGES_OUT + "Pos_" + StrZero( n, 2 ) + ".gif" )
      // Set the digit as tile that I have to use to fill position in counter
      oI:SetTile( oTemp )
      // Fill the position with the image digit
      oI:Rectangle( ( n - 1 ) * nNumWidth, 0, ( n - 1 ) * nNumWidth + nNumWidth, nHeight, .T., gdTiled )
   NEXT

#if 0
   /* Write Final Counter Image */
   oI:SaveGif( IMAGES_OUT + "counter" + StrZero( hb_RandomInt( 1, 99 ), 2 ) + ".gif" )

   ?
   ? "Look at " + IMAGES_OUT + " folder for output images"
   ?
#endif

   RETURN oI:ToStringGif()

#endif
