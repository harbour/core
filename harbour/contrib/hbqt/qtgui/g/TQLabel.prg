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


FUNCTION QLabel( ... )
   RETURN HB_QLabel():new( ... )


CREATE CLASS QLabel INHERIT HbQtObjectHandler, HB_QFrame FUNCTION HB_QLabel

   METHOD  new( ... )

   METHOD  alignment()
   METHOD  buddy()
   METHOD  hasScaledContents()
   METHOD  indent()
   METHOD  margin()
   METHOD  movie()
   METHOD  openExternalLinks()
   METHOD  picture()
   METHOD  pixmap()
   METHOD  setAlignment( nQt_Alignment )
   METHOD  setBuddy( pBuddy )
   METHOD  setIndent( nInt )
   METHOD  setMargin( nInt )
   METHOD  setOpenExternalLinks( lOpen )
   METHOD  setScaledContents( lBool )
   METHOD  setTextFormat( nQt_TextFormat )
   METHOD  setTextInteractionFlags( nFlags )
   METHOD  setWordWrap( lOn )
   METHOD  text()
   METHOD  textFormat()
   METHOD  textInteractionFlags()
   METHOD  wordWrap()
   METHOD  clear()
   METHOD  setMovie( pMovie )
   METHOD  setNum( nNum )
   METHOD  setNum_1( nNum )
   METHOD  setPicture( pPicture )
   METHOD  setPixmap( pQPixmap )
   METHOD  setText( cQString )

   ENDCLASS


METHOD QLabel:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QLabel( ... )
   RETURN Self


METHOD QLabel:alignment()
   RETURN Qt_QLabel_alignment( ::pPtr )


METHOD QLabel:buddy()
   RETURN Qt_QLabel_buddy( ::pPtr )


METHOD QLabel:hasScaledContents()
   RETURN Qt_QLabel_hasScaledContents( ::pPtr )


METHOD QLabel:indent()
   RETURN Qt_QLabel_indent( ::pPtr )


METHOD QLabel:margin()
   RETURN Qt_QLabel_margin( ::pPtr )


METHOD QLabel:movie()
   RETURN Qt_QLabel_movie( ::pPtr )


METHOD QLabel:openExternalLinks()
   RETURN Qt_QLabel_openExternalLinks( ::pPtr )


METHOD QLabel:picture()
   RETURN Qt_QLabel_picture( ::pPtr )


METHOD QLabel:pixmap()
   RETURN Qt_QLabel_pixmap( ::pPtr )


METHOD QLabel:setAlignment( nQt_Alignment )
   RETURN Qt_QLabel_setAlignment( ::pPtr, nQt_Alignment )


METHOD QLabel:setBuddy( pBuddy )
   RETURN Qt_QLabel_setBuddy( ::pPtr, hbqt_ptr( pBuddy ) )


METHOD QLabel:setIndent( nInt )
   RETURN Qt_QLabel_setIndent( ::pPtr, nInt )


METHOD QLabel:setMargin( nInt )
   RETURN Qt_QLabel_setMargin( ::pPtr, nInt )


METHOD QLabel:setOpenExternalLinks( lOpen )
   RETURN Qt_QLabel_setOpenExternalLinks( ::pPtr, lOpen )


METHOD QLabel:setScaledContents( lBool )
   RETURN Qt_QLabel_setScaledContents( ::pPtr, lBool )


METHOD QLabel:setTextFormat( nQt_TextFormat )
   RETURN Qt_QLabel_setTextFormat( ::pPtr, nQt_TextFormat )


METHOD QLabel:setTextInteractionFlags( nFlags )
   RETURN Qt_QLabel_setTextInteractionFlags( ::pPtr, nFlags )


METHOD QLabel:setWordWrap( lOn )
   RETURN Qt_QLabel_setWordWrap( ::pPtr, lOn )


METHOD QLabel:text()
   RETURN Qt_QLabel_text( ::pPtr )


METHOD QLabel:textFormat()
   RETURN Qt_QLabel_textFormat( ::pPtr )


METHOD QLabel:textInteractionFlags()
   RETURN Qt_QLabel_textInteractionFlags( ::pPtr )


METHOD QLabel:wordWrap()
   RETURN Qt_QLabel_wordWrap( ::pPtr )


METHOD QLabel:clear()
   RETURN Qt_QLabel_clear( ::pPtr )


METHOD QLabel:setMovie( pMovie )
   RETURN Qt_QLabel_setMovie( ::pPtr, hbqt_ptr( pMovie ) )


METHOD QLabel:setNum( nNum )
   RETURN Qt_QLabel_setNum( ::pPtr, nNum )


METHOD QLabel:setNum_1( nNum )
   RETURN Qt_QLabel_setNum_1( ::pPtr, nNum )


METHOD QLabel:setPicture( pPicture )
   RETURN Qt_QLabel_setPicture( ::pPtr, hbqt_ptr( pPicture ) )


METHOD QLabel:setPixmap( pQPixmap )
   RETURN Qt_QLabel_setPixmap( ::pPtr, hbqt_ptr( pQPixmap ) )


METHOD QLabel:setText( cQString )
   RETURN Qt_QLabel_setText( ::pPtr, cQString )

