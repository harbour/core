/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


CREATE CLASS QTextFormat

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QTextFormat_destroy( ::pPtr )

   METHOD  background()                        INLINE  Qt_QTextFormat_background( ::pPtr )
   METHOD  boolProperty( nPropertyId )         INLINE  Qt_QTextFormat_boolProperty( ::pPtr, nPropertyId )
   METHOD  brushProperty( nPropertyId )        INLINE  Qt_QTextFormat_brushProperty( ::pPtr, nPropertyId )
   METHOD  clearBackground()                   INLINE  Qt_QTextFormat_clearBackground( ::pPtr )
   METHOD  clearForeground()                   INLINE  Qt_QTextFormat_clearForeground( ::pPtr )
   METHOD  clearProperty( nPropertyId )        INLINE  Qt_QTextFormat_clearProperty( ::pPtr, nPropertyId )
   METHOD  colorProperty( nPropertyId )        INLINE  Qt_QTextFormat_colorProperty( ::pPtr, nPropertyId )
   METHOD  doubleProperty( nPropertyId )       INLINE  Qt_QTextFormat_doubleProperty( ::pPtr, nPropertyId )
   METHOD  foreground()                        INLINE  Qt_QTextFormat_foreground( ::pPtr )
   METHOD  hasProperty( nPropertyId )          INLINE  Qt_QTextFormat_hasProperty( ::pPtr, nPropertyId )
   METHOD  intProperty( nPropertyId )          INLINE  Qt_QTextFormat_intProperty( ::pPtr, nPropertyId )
   METHOD  isBlockFormat()                     INLINE  Qt_QTextFormat_isBlockFormat( ::pPtr )
   METHOD  isCharFormat()                      INLINE  Qt_QTextFormat_isCharFormat( ::pPtr )
   METHOD  isFrameFormat()                     INLINE  Qt_QTextFormat_isFrameFormat( ::pPtr )
   METHOD  isImageFormat()                     INLINE  Qt_QTextFormat_isImageFormat( ::pPtr )
   METHOD  isListFormat()                      INLINE  Qt_QTextFormat_isListFormat( ::pPtr )
   METHOD  isTableCellFormat()                 INLINE  Qt_QTextFormat_isTableCellFormat( ::pPtr )
   METHOD  isTableFormat()                     INLINE  Qt_QTextFormat_isTableFormat( ::pPtr )
   METHOD  isValid()                           INLINE  Qt_QTextFormat_isValid( ::pPtr )
   METHOD  layoutDirection()                   INLINE  Qt_QTextFormat_layoutDirection( ::pPtr )
   METHOD  lengthProperty( nPropertyId )       INLINE  Qt_QTextFormat_lengthProperty( ::pPtr, nPropertyId )
   METHOD  merge( pOther )                     INLINE  Qt_QTextFormat_merge( ::pPtr, pOther )
   METHOD  objectIndex()                       INLINE  Qt_QTextFormat_objectIndex( ::pPtr )
   METHOD  objectType()                        INLINE  Qt_QTextFormat_objectType( ::pPtr )
   METHOD  penProperty( nPropertyId )          INLINE  Qt_QTextFormat_penProperty( ::pPtr, nPropertyId )
   METHOD  property( nPropertyId )             INLINE  Qt_QTextFormat_property( ::pPtr, nPropertyId )
   METHOD  propertyCount()                     INLINE  Qt_QTextFormat_propertyCount( ::pPtr )
   METHOD  setBackground( pBrush )             INLINE  Qt_QTextFormat_setBackground( ::pPtr, pBrush )
   METHOD  setForeground( pBrush )             INLINE  Qt_QTextFormat_setForeground( ::pPtr, pBrush )
   METHOD  setLayoutDirection( nDirection )    INLINE  Qt_QTextFormat_setLayoutDirection( ::pPtr, nDirection )
   METHOD  setObjectIndex( nIndex )            INLINE  Qt_QTextFormat_setObjectIndex( ::pPtr, nIndex )
   METHOD  setObjectType( nType )              INLINE  Qt_QTextFormat_setObjectType( ::pPtr, nType )
   METHOD  setProperty( nPropertyId, pValue )  INLINE  Qt_QTextFormat_setProperty( ::pPtr, nPropertyId, pValue )
   METHOD  stringProperty( nPropertyId )       INLINE  Qt_QTextFormat_stringProperty( ::pPtr, nPropertyId )
   METHOD  toBlockFormat()                     INLINE  Qt_QTextFormat_toBlockFormat( ::pPtr )
   METHOD  toCharFormat()                      INLINE  Qt_QTextFormat_toCharFormat( ::pPtr )
   METHOD  toFrameFormat()                     INLINE  Qt_QTextFormat_toFrameFormat( ::pPtr )
   METHOD  toImageFormat()                     INLINE  Qt_QTextFormat_toImageFormat( ::pPtr )
   METHOD  toListFormat()                      INLINE  Qt_QTextFormat_toListFormat( ::pPtr )
   METHOD  toTableCellFormat()                 INLINE  Qt_QTextFormat_toTableCellFormat( ::pPtr )
   METHOD  toTableFormat()                     INLINE  Qt_QTextFormat_toTableFormat( ::pPtr )
   METHOD  type()                              INLINE  Qt_QTextFormat_type( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QTextFormat

   ::pParent := pParent

   ::pPtr := Qt_QTextFormat( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QTextFormat

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
