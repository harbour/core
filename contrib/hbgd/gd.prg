/*
 * GD graphic library prg level (client api) interface code.
 *
 * Copyright 2004 Francesco Saverio Giudice <info@fsgiudice.com>
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

FUNCTION gdImageCircle( im, cx, cy, w, color )
   RETURN gdImageArc( im, cx, cy, w, w, 0, 360, color )

FUNCTION gdImageFilledCircle( im, cx, cy, w, color )
   RETURN gdImageFilledEllipse( im, cx, cy, w, w, color )

FUNCTION gdImageEllipse( im, cx, cy, w, h, color )
   RETURN gdImageArc( im, cx, cy, w, h, 0, 360, color )

FUNCTION gdImageFTWidth( fontname, ptsize, angle )

   LOCAL aRect := Array( 8 )

   IF gdImageStringFTEx( , @aRect, 0, ;
      hb_defaultValue( fontname, "Arial" ), ;
      hb_defaultValue( ptsize, 8 ), ;
      hb_defaultValue( angle, 0 ), 0, 0, "M" ) == ""

      RETURN aRect[ 3 ] - aRect[ 1 ]
   ENDIF

   RETURN 0

FUNCTION gdImageFTHeight( fontname, ptsize, angle )

   LOCAL aRect := Array( 8 )

   IF gdImageStringFTEx( , @aRect, 0, ;
      hb_defaultValue( fontname, "Arial" ), ;
      hb_defaultValue( ptsize, 8 ), ;
      hb_defaultValue( angle, 0 ), 0, 0, "M" ) == ""

      RETURN aRect[ 2 ] - aRect[ 8 ]
   ENDIF

   RETURN 0

FUNCTION gdImageFTSize( string, fontname, ptsize, angle )

   LOCAL nWidth
   LOCAL nHeight
   LOCAL nX, nY
   LOCAL aRect := Array( 8 )

   IF gdImageStringFTEx( , @aRect, 0, ;
      hb_defaultValue( fontname, "Arial" ), ;
      hb_defaultValue( ptsize, 8 ), ;
      hb_defaultValue( angle, 0 ), 0, 0, string ) == ""

      nWidth  := aRect[ 3 ] - aRect[ 1 ]
      nHeight := aRect[ 2 ] - aRect[ 8 ]
      nX      := aRect[ 1 ]
      nY      := aRect[ 2 ]
   ELSE
      nWidth  := 0
      nHeight := 0
      nX      := 0
      nY      := 0
   ENDIF

   RETURN { nWidth, nHeight, nX, nY }

FUNCTION gdImageStringFT( im, fg, fontname, ptsize, angle, x, y, string, ;
      linespacing, charmap, resolution )

   LOCAL cErr
   LOCAL aRect := Array( 8 )

   IF ( cErr := gdImageStringFTEx( , @aRect, fg, fontname, ptsize, angle, x, y, string, linespacing, charmap, resolution ) ) == ""
      cErr := gdImageStringFTEx( im, aRect, fg, fontname, ptsize, angle, x, y, string, linespacing, charmap, resolution )
   ENDIF

   RETURN cErr

FUNCTION gdImageFromFile( cFile )

   LOCAL cPath, cName, cExt, cDrive
   LOCAL cType, cMime
   LOCAL hFile := { => }
   LOCAL oImage

   IF hb_vfExists( cFile )
      hb_FNameSplit( cFile, @cPath, @cName, @cExt, @cDrive )

      SWITCH Lower( cExt )
      CASE ".jpg"
      CASE ".jpeg"
         hFile[ "file"  ] := cFile
         hFile[ "path"  ] := cPath
         hFile[ "name"  ] := cName
         hFile[ "ext"   ] := cExt
         hFile[ "drive" ] := cDrive
         cType  := "jpeg"
         cMime  := "image/jpeg"
         oImage := GDImage():LoadFromJpeg( cFile )
         EXIT
      CASE ".gif"
         hFile[ "file"  ] := cFile
         hFile[ "path"  ] := cPath
         hFile[ "name"  ] := cName
         hFile[ "ext"   ] := cExt
         hFile[ "drive" ] := cDrive
         cType  := "gif"
         cMime  := "image/gif"
         oImage := GDImage():LoadFromGif( cFile )
         EXIT
      CASE ".png"
         hFile[ "file"  ] := cFile
         hFile[ "path"  ] := cPath
         hFile[ "name"  ] := cName
         hFile[ "ext"   ] := cExt
         hFile[ "drive" ] := cDrive
         cType  := "png"
         cMime  := "image/png"
         oImage := GDImage():LoadFromPng( cFile )
         EXIT
      ENDSWITCH
   ENDIF

   RETURN { oImage, hFile, cType, cMime }

FUNCTION gdImageToString( oImage )

   IF HB_ISOBJECT( oImage ) .AND. ( oImage:className() == "GDIMAGE" .OR. oImage:IsDerivedFrom( "GDIMAGE" ) ) .AND. oImage:cType != NIL

      SWITCH oImage:cType
      CASE "jpeg"
         RETURN oImage:ToStringJpeg()
      CASE "gif"
         RETURN oImage:ToStringGif()
      CASE "png"
         RETURN oImage:ToStringPng()
      ENDSWITCH
   ENDIF

   RETURN NIL

PROCEDURE gdImageToFile( oImage, cFile )

   LOCAL cString, cExt

   IF HB_ISOBJECT( oImage ) .AND. ( oImage:className() == "GDIMAGE" .OR. oImage:IsDerivedFrom( "GDIMAGE" ) ) .AND. oImage:cType != NIL

      SWITCH oImage:cType
      CASE "jpeg"
         cString := oImage:ToStringJpeg()
         cExt := ".jpg"
         EXIT
      CASE "gif"
         cString := oImage:ToStringGif()
         cExt := ".gif"
         EXIT
      CASE "png"
         cString := oImage:ToStringPng()
         cExt := ".png"
         EXIT
      ENDSWITCH

      IF cString != NIL
         hb_MemoWrit( hb_defaultValue( cFile, "image" ) + cExt, cString )
      ENDIF
   ENDIF

   RETURN

PROCEDURE gdImageToHandle( oImage, nHandle )

   IF HB_ISOBJECT( oImage ) .AND. ( oImage:className() == "GDIMAGE" .OR. oImage:IsDerivedFrom( "GDIMAGE" ) ) .AND. oImage:cType != NIL

      SWITCH oImage:cType
      CASE "jpeg"
         oImage:OutputJpeg( nHandle )
         RETURN
      CASE "gif"
         oImage:OutputGif( nHandle )
         RETURN
      CASE "png"
         oImage:OutputPng( nHandle )
         RETURN
      ENDSWITCH
   ENDIF

   RETURN
