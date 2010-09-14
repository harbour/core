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


FUNCTION QTextBlock( ... )
   RETURN HB_QTextBlock():new( ... )


CREATE CLASS QTextBlock INHERIT HbQtObjectHandler FUNCTION HB_QTextBlock

   METHOD  new( ... )

   METHOD  blockFormat()
   METHOD  blockFormatIndex()
   METHOD  blockNumber()
   METHOD  charFormat()
   METHOD  charFormatIndex()
   METHOD  clearLayout()
   METHOD  contains( nPosition )
   METHOD  document()
   METHOD  firstLineNumber()
   METHOD  isValid()
   METHOD  isVisible()
   METHOD  layout()
   METHOD  length()
   METHOD  lineCount()
   METHOD  next()
   METHOD  position()
   METHOD  previous()
   METHOD  revision()
   METHOD  setLineCount( nCount )
   METHOD  setRevision( nRev )
   METHOD  setUserData( pData )
   METHOD  setUserState( nState )
   METHOD  setVisible( lVisible )
   METHOD  text()
   METHOD  textList()
   METHOD  userData()
   METHOD  userState()

   ENDCLASS


METHOD QTextBlock:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextBlock( ... )
   RETURN Self


METHOD QTextBlock:blockFormat()
   RETURN Qt_QTextBlock_blockFormat( ::pPtr )


METHOD QTextBlock:blockFormatIndex()
   RETURN Qt_QTextBlock_blockFormatIndex( ::pPtr )


METHOD QTextBlock:blockNumber()
   RETURN Qt_QTextBlock_blockNumber( ::pPtr )


METHOD QTextBlock:charFormat()
   RETURN Qt_QTextBlock_charFormat( ::pPtr )


METHOD QTextBlock:charFormatIndex()
   RETURN Qt_QTextBlock_charFormatIndex( ::pPtr )


METHOD QTextBlock:clearLayout()
   RETURN Qt_QTextBlock_clearLayout( ::pPtr )


METHOD QTextBlock:contains( nPosition )
   RETURN Qt_QTextBlock_contains( ::pPtr, nPosition )


METHOD QTextBlock:document()
   RETURN Qt_QTextBlock_document( ::pPtr )


METHOD QTextBlock:firstLineNumber()
   RETURN Qt_QTextBlock_firstLineNumber( ::pPtr )


METHOD QTextBlock:isValid()
   RETURN Qt_QTextBlock_isValid( ::pPtr )


METHOD QTextBlock:isVisible()
   RETURN Qt_QTextBlock_isVisible( ::pPtr )


METHOD QTextBlock:layout()
   RETURN Qt_QTextBlock_layout( ::pPtr )


METHOD QTextBlock:length()
   RETURN Qt_QTextBlock_length( ::pPtr )


METHOD QTextBlock:lineCount()
   RETURN Qt_QTextBlock_lineCount( ::pPtr )


METHOD QTextBlock:next()
   RETURN Qt_QTextBlock_next( ::pPtr )


METHOD QTextBlock:position()
   RETURN Qt_QTextBlock_position( ::pPtr )


METHOD QTextBlock:previous()
   RETURN Qt_QTextBlock_previous( ::pPtr )


METHOD QTextBlock:revision()
   RETURN Qt_QTextBlock_revision( ::pPtr )


METHOD QTextBlock:setLineCount( nCount )
   RETURN Qt_QTextBlock_setLineCount( ::pPtr, nCount )


METHOD QTextBlock:setRevision( nRev )
   RETURN Qt_QTextBlock_setRevision( ::pPtr, nRev )


METHOD QTextBlock:setUserData( pData )
   RETURN Qt_QTextBlock_setUserData( ::pPtr, hbqt_ptr( pData ) )


METHOD QTextBlock:setUserState( nState )
   RETURN Qt_QTextBlock_setUserState( ::pPtr, nState )


METHOD QTextBlock:setVisible( lVisible )
   RETURN Qt_QTextBlock_setVisible( ::pPtr, lVisible )


METHOD QTextBlock:text()
   RETURN Qt_QTextBlock_text( ::pPtr )


METHOD QTextBlock:textList()
   RETURN Qt_QTextBlock_textList( ::pPtr )


METHOD QTextBlock:userData()
   RETURN Qt_QTextBlock_userData( ::pPtr )


METHOD QTextBlock:userState()
   RETURN Qt_QTextBlock_userState( ::pPtr )

