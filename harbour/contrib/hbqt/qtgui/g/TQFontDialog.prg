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


FUNCTION QFontDialog( ... )
   RETURN HB_QFontDialog():new( ... )


CREATE CLASS QFontDialog INHERIT HbQtObjectHandler, HB_QDialog FUNCTION HB_QFontDialog

   METHOD  new( ... )

   METHOD  currentFont()
   METHOD  options()
   METHOD  selectedFont()
   METHOD  setCurrentFont( pFont )
   METHOD  setOption( nOption, lOn )
   METHOD  setOptions( nOptions )
   METHOD  testOption( nOption )
   METHOD  getFont( ... )

   ENDCLASS


METHOD QFontDialog:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFontDialog( ... )
   RETURN Self


METHOD QFontDialog:currentFont()
   RETURN Qt_QFontDialog_currentFont( ::pPtr )


METHOD QFontDialog:options()
   RETURN Qt_QFontDialog_options( ::pPtr )


METHOD QFontDialog:selectedFont()
   RETURN Qt_QFontDialog_selectedFont( ::pPtr )


METHOD QFontDialog:setCurrentFont( pFont )
   RETURN Qt_QFontDialog_setCurrentFont( ::pPtr, hbqt_ptr( pFont ) )


METHOD QFontDialog:setOption( nOption, lOn )
   RETURN Qt_QFontDialog_setOption( ::pPtr, nOption, lOn )


METHOD QFontDialog:setOptions( nOptions )
   RETURN Qt_QFontDialog_setOptions( ::pPtr, nOptions )


METHOD QFontDialog:testOption( nOption )
   RETURN Qt_QFontDialog_testOption( ::pPtr, nOption )


METHOD QFontDialog:getFont( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 5
      DO CASE
      CASE aV[ 1 ] $ "L" .AND. aV[ 2 ] $ "PO" .AND. aV[ 3 ] $ "PO" .AND. aV[ 4 ] $ "C" .AND. aV[ 5 ] $ "N"
                // QFont getFont ( bool * ok, const QFont & initial, QWidget * parent, const QString & title, FontDialogOptions options )
                // L @ bool, PO p QFont, PO p QWidget, C c QString, N n QFontDialog::FontDialogOptions
         RETURN QFont():from( Qt_QFontDialog_getFont( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 4
      DO CASE
      CASE aV[ 1 ] $ "L" .AND. aV[ 2 ] $ "PO" .AND. aV[ 3 ] $ "PO" .AND. aV[ 4 ] $ "C"
                // QFont getFont ( bool * ok, const QFont & initial, QWidget * parent, const QString & title )
                // L @ bool, PO p QFont, PO p QWidget, C c QString
         RETURN QFont():from( Qt_QFontDialog_getFont_2( ::pPtr, ... ) )
      CASE aV[ 1 ] $ "L" .AND. aV[ 2 ] $ "PO" .AND. aV[ 3 ] $ "PO" .AND. aV[ 4 ] $ "PO"
                // QFont getFont ( bool * ok, const QFont & initial, QWidget * parent, const char * name )
                // L @ bool, PO p QFont, PO p QWidget, PO p char
         RETURN QFont():from( Qt_QFontDialog_getFont_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "L" .AND. aV[ 2 ] $ "PO" .AND. aV[ 3 ] $ "PO"
                // QFont getFont ( bool * ok, const QFont & initial, QWidget * parent = 0 )
                // L @ bool, PO p QFont, PO p QWidget
         RETURN QFont():from( Qt_QFontDialog_getFont_3( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "L" .AND. aV[ 2 ] $ "PO"
                // QFont getFont ( bool * ok, QWidget * parent = 0 )
                // L @ bool, PO p QWidget
         RETURN QFont():from( Qt_QFontDialog_getFont_4( ::pPtr, ... ) )
                // QFont getFont ( bool * ok, const QFont & initial, QWidget * parent = 0 )
                // L @ bool, PO p QFont, PO p QWidget
         // RETURN QFont():from( Qt_QFontDialog_getFont_3( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "L"
                // QFont getFont ( bool * ok, QWidget * parent = 0 )
                // L @ bool, PO p QWidget
         RETURN QFont():from( Qt_QFontDialog_getFont_4( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL

