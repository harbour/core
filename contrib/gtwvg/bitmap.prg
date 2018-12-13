/*
 * Xbase++ xbpBitmap compatible Class
 *
 * Copyright 2008-2012 Pritpal Bedi <bedipritpal@hotmail.com>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

/*                                EkOnkar
 *                          ( The LORD is ONE )
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

CREATE CLASS WvgBitmap

   VAR    bits                                  INIT 0 READONLY
   VAR    bufferOffset                          INIT 0 READONLY
   VAR    planes                                INIT 0 READONLY
   VAR    transparentClr                        INIT 0
   VAR    xSize                                 INIT 0 READONLY
   VAR    ySize                                 INIT 0 READONLY

   VAR    hBitmap
   VAR    hDCcompat
   VAR    lDCToDestroy                          INIT .F.

   METHOD new()
   METHOD create( oPScompat )
   METHOD configure()                           VIRTUAL
   METHOD destroy()

   METHOD draw()                                VIRTUAL
   METHOD getColorTable()                       VIRTUAL
   METHOD getDefaultBGColor()                   VIRTUAL
   METHOD load()                                VIRTUAL
   METHOD loadFile()                            VIRTUAL
   METHOD make()                                VIRTUAL
   METHOD presSpace()                           VIRTUAL
   METHOD saveFile()                            VIRTUAL
   METHOD setBuffer()                           VIRTUAL
   METHOD getPicture()                          VIRTUAL
   METHOD setPicture()                          VIRTUAL

ENDCLASS

METHOD WvgBitmap:new()
   RETURN Self

METHOD WvgBitmap:create( oPScompat )

   IF oPScompat == NIL
      ::hDCComp := wvg_GetDC()
      ::lDCToDestroy := .T.
   ELSE
      ::hDCComp := oPScompat:hDC
   ENDIF

   RETURN Self

METHOD WvgBitmap:destroy()

   IF ::hBitmap != NIL
      wvg_DeleteObject( ::hBitmap )
   ENDIF
   IF ::lDCtoDestroy
      wvg_ReleaseDC( ::hDCcompat )
   ENDIF

   RETURN Self
