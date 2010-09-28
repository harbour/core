/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * http://harbour-project.org
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                   Xbase++ xbpBitmap compatible Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               04Jul2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "gra.ch"
#include "appevent.ch"

/*----------------------------------------------------------------------*/

CLASS XbpBitmap

   DATA     bufferOffset                          INIT    0
   DATA     transparentColor                      INIT    GRA_CLR_INVALID

   DATA     oWidget
   DATA     oPS
   DATA     cImageFileName

   ACCESS   bits                                  INLINE  ::oWidget:depth()
   ACCESS   planes                                INLINE  0
   ACCESS   xSize                                 INLINE  ::oWidget:width()
   ACCESS   ySize                                 INLINE  ::oWidget:height()

   METHOD   new()                                 INLINE  Self
   METHOD   create( oPS )
   METHOD   configure()                           VIRTUAL
   METHOD   destroy()                             VIRTUAL

   METHOD   draw()
   METHOD   getColorTable( nNumColors )
   METHOD   getDefaultBGColor()
   METHOD   load( cDLLName, cID )
   METHOD   loadFile( cImageFileName )
   METHOD   make( nXsize, nYsize, nPlanes, nBits )
   METHOD   presSpace( oPS )
   METHOD   saveFile( cImageFileName, nFormat, nCompression )
   METHOD   setBuffer( cBuffer, nFormat, nCompression )
   METHOD   getIPicture()
   METHOD   setIPicture( oPicture )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpBitmap:create( oPS )

   DEFAULT oPS TO ::oPS
   ::oPS := oPS

   ::oWidget := QImage()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBitmap:draw()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBitmap:getColorTable( nNumColors )

   HB_SYMBOL_UNUSED( nNumColors )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBitmap:getDefaultBGColor()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBitmap:load( cDLLName, cID )
   LOCAL lSuccess := .f.

   HB_SYMBOL_UNUSED( cDLLName )
   HB_SYMBOL_UNUSED( cID )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

METHOD XbpBitmap:loadFile( cImageFileName )
   LOCAL lSuccess

   // BMP, GIF, JPEG, PNG

   ::cImageFileName := cImageFileName

   lSuccess := ::oWidget:load( cImageFileName )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

METHOD XbpBitmap:make( nXsize, nYsize, nPlanes, nBits )

   HB_SYMBOL_UNUSED( nXsize  )
   HB_SYMBOL_UNUSED( nYsize  )
   HB_SYMBOL_UNUSED( nPlanes )
   HB_SYMBOL_UNUSED( nBits   )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBitmap:presSpace( oPS )
   LOCAL oPresSpace := NIL

   HB_SYMBOL_UNUSED( oPS )

   RETURN oPresSpace

/*----------------------------------------------------------------------*/

METHOD XbpBitmap:saveFile( cImageFileName, nFormat, nCompression )
   LOCAL lSuccess

   DEFAULT nCompression TO XBPBMP_DEF_COMPRESSION

   DO CASE
   CASE nFormat == XBPBMP_FORMAT_GIF
      lSuccess := ::oWidget:save( cImageFileName, "GIF", nCompression )  /* QT Does not support writing to GIF */
   CASE nFormat == XBPBMP_FORMAT_JPG
      lSuccess := ::oWidget:save( cImageFileName, "JPG", nCompression )
   CASE nFormat == XBPBMP_FORMAT_PNG
      lSuccess := ::oWidget:save( cImageFileName, "PNG", nCompression )
   CASE nFormat == XBPBMP_FORMAT_WIN2X
      lSuccess := ::oWidget:save( cImageFileName, "BMP", nCompression )
   CASE nFormat == XBPBMP_FORMAT_WIN3X
      lSuccess := ::oWidget:save( cImageFileName, "BMP", nCompression )
   OTHERWISE
      lSuccess := ::oWidget:save( cImageFileName )
   ENDCASE

   RETURN lSuccess

/*----------------------------------------------------------------------*/

METHOD XbpBitmap:setBuffer( cBuffer, nFormat, nCompression )

   HB_SYMBOL_UNUSED( cBuffer )
   HB_SYMBOL_UNUSED( nFormat )
   HB_SYMBOL_UNUSED( nCompression )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBitmap:getIPicture()
   LOCAL oPicture := NIL

   RETURN oPicture

/*----------------------------------------------------------------------*/

METHOD XbpBitmap:setIPicture( oPicture )
   LOCAL lSuccess := .f.

   HB_SYMBOL_UNUSED( oPicture )

   RETURN lSuccess

/*----------------------------------------------------------------------*/
