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


CREATE CLASS QFontDatabase

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  bold( cFamily, cStyle )             INLINE  Qt_QFontDatabase_bold( ::pPtr, cFamily, cStyle )
   METHOD  families( nWritingSystem )          INLINE  Qt_QFontDatabase_families( ::pPtr, nWritingSystem )
   METHOD  font( cFamily, cStyle, nPointSize )  INLINE  Qt_QFontDatabase_font( ::pPtr, cFamily, cStyle, nPointSize )
   METHOD  isBitmapScalable( cFamily, cStyle )  INLINE  Qt_QFontDatabase_isBitmapScalable( ::pPtr, cFamily, cStyle )
   METHOD  isFixedPitch( cFamily, cStyle )     INLINE  Qt_QFontDatabase_isFixedPitch( ::pPtr, cFamily, cStyle )
   METHOD  isScalable( cFamily, cStyle )       INLINE  Qt_QFontDatabase_isScalable( ::pPtr, cFamily, cStyle )
   METHOD  isSmoothlyScalable( cFamily, cStyle )  INLINE  Qt_QFontDatabase_isSmoothlyScalable( ::pPtr, cFamily, cStyle )
   METHOD  italic( cFamily, cStyle )           INLINE  Qt_QFontDatabase_italic( ::pPtr, cFamily, cStyle )
   METHOD  styleString( pFont )                INLINE  Qt_QFontDatabase_styleString( ::pPtr, pFont )
   METHOD  styleString_1( pFontInfo )          INLINE  Qt_QFontDatabase_styleString_1( ::pPtr, pFontInfo )
   METHOD  styles( cFamily )                   INLINE  Qt_QFontDatabase_styles( ::pPtr, cFamily )
   METHOD  weight( cFamily, cStyle )           INLINE  Qt_QFontDatabase_weight( ::pPtr, cFamily, cStyle )
   METHOD  addApplicationFont( cFileName )     INLINE  Qt_QFontDatabase_addApplicationFont( ::pPtr, cFileName )
   METHOD  addApplicationFontFromData( pFontData )  INLINE  Qt_QFontDatabase_addApplicationFontFromData( ::pPtr, pFontData )
   METHOD  applicationFontFamilies( nId )      INLINE  Qt_QFontDatabase_applicationFontFamilies( ::pPtr, nId )
   METHOD  removeAllApplicationFonts()         INLINE  Qt_QFontDatabase_removeAllApplicationFonts( ::pPtr )
   METHOD  removeApplicationFont( nId )        INLINE  Qt_QFontDatabase_removeApplicationFont( ::pPtr, nId )
   METHOD  supportsThreadedFontRendering()     INLINE  Qt_QFontDatabase_supportsThreadedFontRendering( ::pPtr )
   METHOD  writingSystemName( nWritingSystem )  INLINE  Qt_QFontDatabase_writingSystemName( ::pPtr, nWritingSystem )
   METHOD  writingSystemSample( nWritingSystem )  INLINE  Qt_QFontDatabase_writingSystemSample( ::pPtr, nWritingSystem )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QFontDatabase

   ::pParent := pParent

   ::pPtr := Qt_QFontDatabase( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QFontDatabase

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
