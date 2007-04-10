/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * GD graphic library prg level (client api) interface code.
 *
 * Copyright 2004 Francesco Saverio Giudice <info@fsgiudice.com>
 * www - http://www.xharbour.org http://www.harbour-project.org
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
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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

/*
 *
 * See doc/license files for licensing terms.
 *
 */

#include "common.ch"

FUNCTION gdImageChar( im, font, x, y, char, color )
RETURN gdImageString( im, font, x, y, char, color )

FUNCTION gdImageCharUp( im, font, x, y, char, color )
RETURN gdImageStringUp( im, font, x, y, char, color )

FUNCTION gdImageCircle( im, cx, cy, w, color )
RETURN gdImageArc( im, cx, cy, w, w, 0, 360, color )

FUNCTION gdImageFilledCircle( im, cx, cy, w, color )
RETURN gdImageFilledEllipse( im, cx, cy, w, w, color )

FUNCTION gdImageEllipse( im, cx, cy, w, h, color )
RETURN gdImageArc( im, cx, cy, w, h, 0, 360, color )

FUNCTION gdImageFTWidth( fontname, ptsize, angle )
  LOCAL nWidth := 0
  LOCAL cErr
  LOCAL aRect := Array(8)
  DEFAULT fontname TO "Arial"
  DEFAULT ptsize   TO 8
  DEFAULT angle    TO 0
  cErr := gdImageStringFTEx( , @aRect, 0, fontname, ptsize, angle, 0, 0, "M" )
  //__OutDebug( "ptsize", ptsize, aRect )
  IF cErr == ""
     nWidth := aRect[3] - aRect[1]
  ENDIF

RETURN nWidth

FUNCTION gdImageFTHeight( fontname, ptsize, angle )
  LOCAL nWidth := 0
  LOCAL cErr
  LOCAL aRect := Array(8)
  DEFAULT fontname TO "Arial"
  DEFAULT ptsize   TO 8
  DEFAULT angle    TO 0

  cErr := gdImageStringFTEx( , @aRect, 0, fontname, ptsize, angle, 0, 0, "M" )
  IF cErr == ""
     nWidth := aRect[2] - aRect[8]
  ENDIF

RETURN nWidth

FUNCTION gdImageFTSize( string, fontname, ptsize, angle )
  LOCAL nWidth  := 0
  LOCAL nHeight := 0
  LOCAL nX, nY
  LOCAL cErr
  LOCAL aRect := Array(8)
  DEFAULT fontname TO "Arial"
  DEFAULT ptsize   TO 8
  DEFAULT angle    TO 0
  cErr := gdImageStringFTEx( , @aRect, 0, fontname, ptsize, angle, 0, 0, string )
  //__OutDebug( "ptsize", ptsize, aRect )
  IF cErr == ""
     nWidth  := aRect[3] - aRect[1]
     nHeight := aRect[2] - aRect[8]
     nX      := aRect[1]
     nY      := aRect[2]
  ENDIF

RETURN { nWidth, nHeight, nX, nY }

FUNCTION gdImageStringFT( im, fg, fontname, ptsize, angle, x, y, string, ;
                          linespacing, charmap, resolution )
  LOCAL cErr
  LOCAL aRect := Array(8)
  cErr := gdImageStringFTEx( , @aRect, fg, fontname, ptsize, angle, x, y, string, linespacing, charmap, resolution )
  IF cErr == ""
     cErr := gdImageStringFTEx( im, aRect, fg, fontname, ptsize, angle, x, y, string, linespacing, charmap, resolution )
  ENDIF
RETURN cErr

FUNCTION gdImageFromFile( cFile )
  LOCAL cPath, cName, cExt, cDrive
  LOCAL cType, cMime
  LOCAL hFile := {=>}
  LOCAL oImage

  IF File( cFile )
     HB_FNameSplit( cFile, @cPath, @cName, @cExt, @cDrive )
     //TraceLog( cFile, cPath, cName, cExt, cDrive )
     cExt := Lower( cExt )
     DO CASE
        CASE cExt == ".jpg" .OR. cExt == ".jpeg"
             hFile[ "file"  ] := cFile
             hFile[ "path"  ] := cPath
             hFile[ "name"  ] := cName
             hFile[ "ext"   ] := cExt
             hFile[ "drive" ] := cDrive
             cType  := "jpeg"
             cMime  := "image/jpeg"
             oImage := GDImage():LoadFromJpeg( cFile )


        CASE cExt == ".gif"
             hFile[ "file"  ] := cFile
             hFile[ "path"  ] := cPath
             hFile[ "name"  ] := cName
             hFile[ "ext"   ] := cExt
             hFile[ "drive" ] := cDrive
             cType  := "gif"
             cMime  := "image/gif"
             oImage := GDImage():LoadFromGif( cFile )


        CASE cExt == ".png"
             hFile[ "file"  ] := cFile
             hFile[ "path"  ] := cPath
             hFile[ "name"  ] := cName
             hFile[ "ext"   ] := cExt
             hFile[ "drive" ] := cDrive
             cType  := "png"
             cMime  := "image/png"
             oImage := GDImage():LoadFromPng( cFile )
     ENDCASE

  ENDIF

RETURN { oImage, hFile, cType, cMime }

FUNCTION gdImageToString( oImage )
  LOCAL cString

  //Tracelog( "oImage, oImage:ClassName, oImage:IsDerivedFrom( 'GDIMAGE' )", ;
  //          oImage, oImage:ClassName, oImage:IsDerivedFrom( 'GDIMAGE' ) )

  IF ValType( oImage ) == "O" .AND. ( oImage:ClassName == "GDIMAGE" .OR. oImage:IsDerivedFrom( "GDIMAGE" ) )
     WITH OBJECT oImage
        IF :cType <> NIL
           DO CASE
              CASE :cType == "jpeg"
                   cString := :ToStringJpeg()
              CASE :cType == "gif"
                   cString := :ToStringGif()
              CASE :cType == "png"
                   cString := :ToStringPng()
           ENDCASE
        ENDIF
     END
  ENDIF
RETURN cString

PROCEDURE gdImageToFile( oImage, cFile )
  LOCAL cString, cExt

  DEFAULT cFile TO "image"

  //Tracelog( "oImage, oImage:ClassName, oImage:IsDerivedFrom( 'GDIMAGE' )", ;
  //          oImage, oImage:ClassName, oImage:IsDerivedFrom( 'GDIMAGE' ) )

  IF ValType( oImage ) == "O" .AND. ( oImage:ClassName == "GDIMAGE" .OR. oImage:IsDerivedFrom( "GDIMAGE" ) )
     WITH OBJECT oImage
        IF :cType <> NIL
           DO CASE
              CASE :cType == "jpeg"
                   cString := :ToStringJpeg()
                   cExt    := "jpg"
              CASE :cType == "gif"
                   cString := :ToStringGif()
                   cExt    := "gif"
              CASE :cType == "png"
                   cString := :ToStringPng()
                   cExt    := "png"
           ENDCASE
           IF cString <> NIL
              MemoWrit( cFile + "." + cExt, cString )
           ENDIF
        ENDIF
     END
  ENDIF

RETURN

/*

   ////aRect := { 10, 40, 100, 40, 100, 20, 10, 20 } //Array(8)
   //aRect := Array(8)
   ////TraceLog( "aRect = " + hb_dumpVar( aRect ) )
   //gdImageStringFtEx( , @aRect, blue, "arial", 20, 30, 20, 90, 'Test')
   ////TraceLog( "aRect = " + hb_dumpVar( aRect ) )
   //? "aRect = " + hb_dumpVar( aRect )
   //gdImageStringFtEx(im, aRect, blue, "arial", 20, 30, 20, 90, 'Test')
   ////TraceLog( "aRect = " + hb_dumpVar( aRect ) )

*/
