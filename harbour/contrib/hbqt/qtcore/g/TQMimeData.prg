/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://harbour-project.org
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


#include "hbclass.ch"


FUNCTION QMimeData( ... )
   RETURN HB_QMimeData():new( ... )


CREATE CLASS QMimeData INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QMimeData

   METHOD  new( ... )

   METHOD  clear()
   METHOD  colorData()
   METHOD  data( cMimeType )
   METHOD  formats()
   METHOD  hasColor()
   METHOD  hasFormat( cMimeType )
   METHOD  hasHtml()
   METHOD  hasImage()
   METHOD  hasText()
   METHOD  hasUrls()
   METHOD  html()
   METHOD  imageData()
   METHOD  removeFormat( cMimeType )
   METHOD  setColorData( pColor )
   METHOD  setData( cMimeType, pData )
   METHOD  setHtml( cHtml )
   METHOD  setImageData( pImage )
   METHOD  setText( cText )
   METHOD  text()
   METHOD  urls()
   METHOD  hbUrlList()

   ENDCLASS


METHOD QMimeData:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMimeData( ... )
   RETURN Self


METHOD QMimeData:clear()
   RETURN Qt_QMimeData_clear( ::pPtr )


METHOD QMimeData:colorData()
   RETURN Qt_QMimeData_colorData( ::pPtr )


METHOD QMimeData:data( cMimeType )
   RETURN Qt_QMimeData_data( ::pPtr, cMimeType )


METHOD QMimeData:formats()
   RETURN Qt_QMimeData_formats( ::pPtr )


METHOD QMimeData:hasColor()
   RETURN Qt_QMimeData_hasColor( ::pPtr )


METHOD QMimeData:hasFormat( cMimeType )
   RETURN Qt_QMimeData_hasFormat( ::pPtr, cMimeType )


METHOD QMimeData:hasHtml()
   RETURN Qt_QMimeData_hasHtml( ::pPtr )


METHOD QMimeData:hasImage()
   RETURN Qt_QMimeData_hasImage( ::pPtr )


METHOD QMimeData:hasText()
   RETURN Qt_QMimeData_hasText( ::pPtr )


METHOD QMimeData:hasUrls()
   RETURN Qt_QMimeData_hasUrls( ::pPtr )


METHOD QMimeData:html()
   RETURN Qt_QMimeData_html( ::pPtr )


METHOD QMimeData:imageData()
   RETURN Qt_QMimeData_imageData( ::pPtr )


METHOD QMimeData:removeFormat( cMimeType )
   RETURN Qt_QMimeData_removeFormat( ::pPtr, cMimeType )


METHOD QMimeData:setColorData( pColor )
   RETURN Qt_QMimeData_setColorData( ::pPtr, hbqt_ptr( pColor ) )


METHOD QMimeData:setData( cMimeType, pData )
   RETURN Qt_QMimeData_setData( ::pPtr, cMimeType, hbqt_ptr( pData ) )


METHOD QMimeData:setHtml( cHtml )
   RETURN Qt_QMimeData_setHtml( ::pPtr, cHtml )


METHOD QMimeData:setImageData( pImage )
   RETURN Qt_QMimeData_setImageData( ::pPtr, hbqt_ptr( pImage ) )


METHOD QMimeData:setText( cText )
   RETURN Qt_QMimeData_setText( ::pPtr, cText )


METHOD QMimeData:text()
   RETURN Qt_QMimeData_text( ::pPtr )


METHOD QMimeData:urls()
   RETURN Qt_QMimeData_urls( ::pPtr )


METHOD QMimeData:hbUrlList()
   RETURN Qt_QMimeData_hbUrlList( ::pPtr )

