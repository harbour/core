/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                 Xbase++ Compatible xbpClipBoard Class
 *
 *                            Pritpal Bedi
 *                              13Mar2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "appevent.ch"

/*----------------------------------------------------------------------*/

CLASS XbpClipBoard

   DATA   oWidget
   DATA   oOldXbp

   METHOD new()
   METHOD create()
   METHOD destroy()

   METHOD clear()                               //  lSuccess     Deletes the data currently in the clipboard.
   METHOD close()                               //  self         Closes the clipboard.
   METHOD getBuffer( nFormat )                  //  xBuffer      Retrieves the data from the clipboard.
   METHOD getFormatName( nFormatID )            //  cName        Retrieves the name of a clipboard data format.
   METHOD open()                                //  lSuccess     Opens the clipboard.
   METHOD queryFormats()                        //  aFormats     Returns the data formats for which there is data currently available in the clipboard.
   METHOD registerChangeHandler( oXbp )         //  oOldXbp|NIL  Registers an Xbase Part as recipient for "change" messages
   METHOD registerFormat( cFormatName )         //  nFormatID    Registers a user-defined data format
   METHOD setBuffer( xBuffer, nFormatId )       //  lSuccess     Writes data to the clipboard.

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpClipBoard:new()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpClipBoard:create()

   ::oWidget := QClipBoard()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpClipBoard:destroy()

   ::oWidget:clear()
   ::oWidget := NIL

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpClipBoard:clear()
   LOCAL lSuccess := .t.

   ::oWidget:clear()

   RETURN lSuccess

/*----------------------------------------------------------------------*/

METHOD XbpClipBoard:close()
   // Always open
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpClipBoard:getBuffer( nFormat )
   LOCAL xBuffer

   IF nFormat == XBPCLPBRD_TEXT
      xBuffer := ::oWidget:text()

   ELSEIF nFormat == XBPCLPBRD_BITMAP
      xBuffer := XbpBitmap():new()
      xBuffer:oWiget := ::oWidget:image()

   ENDIF

   RETURN xBuffer

/*----------------------------------------------------------------------*/

METHOD XbpClipBoard:getFormatName( nFormatID )
   LOCAL cName := ""
   HB_SYMBOL_UNUSED( nFormatID )
   RETURN cName

/*----------------------------------------------------------------------*/

METHOD XbpClipBoard:open()
   LOCAL lSuccess := .t.
   // Always open
   RETURN lSuccess

/*----------------------------------------------------------------------*/

METHOD XbpClipBoard:queryFormats()
   LOCAL aFormats := {}
   LOCAL cText  := ::oWidget:text()
   LOCAL qImage := ::oWidget:image()

   IF !empty( cText )
      aadd( aFormats, XBPCLPBRD_TEXT )
   ENDIF
   IF !qImage:isNull()
      aadd( aFormats, XBPCLPBRD_BITMAP )
   ENDIF

   RETURN aFormats

/*----------------------------------------------------------------------*/

METHOD XbpClipBoard:registerChangeHandler( oXbp )
   LOCAL oOld := ::oOldXbp

   ::oOldXbp := oXbp

   RETURN oOld

/*----------------------------------------------------------------------*/

METHOD XbpClipBoard:registerFormat( cFormatName )
   LOCAL nFormatID := 0
   HB_SYMBOL_UNUSED( cFormatName )
   RETURN nFormatID

/*----------------------------------------------------------------------*/

METHOD XbpClipBoard:setBuffer( xBuffer, nFormatId )
   LOCAL lSuccess := .t.

   IF nFormatId == XBPCLPBRD_TEXT
      ::oWidget:setText( xBuffer )

   ELSEIF nFormatId == XBPCLPBRD_BITMAP
      ::oWidget:setImage( xBuffer:oWidget )   /* XbpBitmap */

   ENDIF

   RETURN lSuccess

/*----------------------------------------------------------------------*/



