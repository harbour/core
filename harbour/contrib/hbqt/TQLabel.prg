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


CREATE CLASS QLabel INHERIT QFrame

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QLabel_destroy( ::pPtr )

   METHOD  alignment()                         INLINE  Qt_QLabel_alignment( ::pPtr )
   METHOD  buddy()                             INLINE  Qt_QLabel_buddy( ::pPtr )
   METHOD  hasScaledContents()                 INLINE  Qt_QLabel_hasScaledContents( ::pPtr )
   METHOD  indent()                            INLINE  Qt_QLabel_indent( ::pPtr )
   METHOD  margin()                            INLINE  Qt_QLabel_margin( ::pPtr )
   METHOD  movie()                             INLINE  Qt_QLabel_movie( ::pPtr )
   METHOD  openExternalLinks()                 INLINE  Qt_QLabel_openExternalLinks( ::pPtr )
   METHOD  picture()                           INLINE  Qt_QLabel_picture( ::pPtr )
   METHOD  pixmap()                            INLINE  Qt_QLabel_pixmap( ::pPtr )
   METHOD  setAlignment( nQt_Alignment )       INLINE  Qt_QLabel_setAlignment( ::pPtr, nQt_Alignment )
   METHOD  setBuddy( pBuddy )                  INLINE  Qt_QLabel_setBuddy( ::pPtr, pBuddy )
   METHOD  setIndent( nInt )                   INLINE  Qt_QLabel_setIndent( ::pPtr, nInt )
   METHOD  setMargin( nInt )                   INLINE  Qt_QLabel_setMargin( ::pPtr, nInt )
   METHOD  setOpenExternalLinks( lOpen )       INLINE  Qt_QLabel_setOpenExternalLinks( ::pPtr, lOpen )
   METHOD  setScaledContents( lBool )          INLINE  Qt_QLabel_setScaledContents( ::pPtr, lBool )
   METHOD  setTextFormat( nQt_TextFormat )     INLINE  Qt_QLabel_setTextFormat( ::pPtr, nQt_TextFormat )
   METHOD  setTextInteractionFlags( nFlags )   INLINE  Qt_QLabel_setTextInteractionFlags( ::pPtr, nFlags )
   METHOD  setWordWrap( lOn )                  INLINE  Qt_QLabel_setWordWrap( ::pPtr, lOn )
   METHOD  text()                              INLINE  Qt_QLabel_text( ::pPtr )
   METHOD  textFormat()                        INLINE  Qt_QLabel_textFormat( ::pPtr )
   METHOD  textInteractionFlags()              INLINE  Qt_QLabel_textInteractionFlags( ::pPtr )
   METHOD  wordWrap()                          INLINE  Qt_QLabel_wordWrap( ::pPtr )
   METHOD  clear()                             INLINE  Qt_QLabel_clear( ::pPtr )
   METHOD  setMovie( pMovie )                  INLINE  Qt_QLabel_setMovie( ::pPtr, pMovie )
   METHOD  setNum( nNum )                      INLINE  Qt_QLabel_setNum( ::pPtr, nNum )
   METHOD  setNum_1( nNum )                    INLINE  Qt_QLabel_setNum_1( ::pPtr, nNum )
   METHOD  setPicture( pPicture )              INLINE  Qt_QLabel_setPicture( ::pPtr, pPicture )
   METHOD  setPixmap( pQPixmap )               INLINE  Qt_QLabel_setPixmap( ::pPtr, pQPixmap )
   METHOD  setText( cQString )                 INLINE  Qt_QLabel_setText( ::pPtr, cQString )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QLabel

   ::pParent := pParent

   ::pPtr := Qt_QLabel( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QLabel

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

