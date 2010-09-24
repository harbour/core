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


FUNCTION QColorDialog( ... )
   RETURN HB_QColorDialog():new( ... )


CREATE CLASS QColorDialog INHERIT HbQtObjectHandler, HB_QDialog FUNCTION HB_QColorDialog

   METHOD  new( ... )

   METHOD  currentColor()
   METHOD  open()
   METHOD  options()
   METHOD  selectedColor()
   METHOD  setCurrentColor( pColor )
   METHOD  setOption( nOption, lOn )
   METHOD  setOptions( nOptions )
   METHOD  setVisible( lVisible )
   METHOD  testOption( nOption )
   METHOD  customColor( nIndex )
   METHOD  customCount()
   METHOD  getColor( ... )
   METHOD  setCustomColor( nIndex, nColor )
   METHOD  setStandardColor( nIndex, nColor )

   ENDCLASS


METHOD QColorDialog:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QColorDialog( ... )
   RETURN Self


METHOD QColorDialog:currentColor()
   RETURN Qt_QColorDialog_currentColor( ::pPtr )


METHOD QColorDialog:open()
   RETURN Qt_QColorDialog_open( ::pPtr )


METHOD QColorDialog:options()
   RETURN Qt_QColorDialog_options( ::pPtr )


METHOD QColorDialog:selectedColor()
   RETURN Qt_QColorDialog_selectedColor( ::pPtr )


METHOD QColorDialog:setCurrentColor( pColor )
   RETURN Qt_QColorDialog_setCurrentColor( ::pPtr, hbqt_ptr( pColor ) )


METHOD QColorDialog:setOption( nOption, lOn )
   RETURN Qt_QColorDialog_setOption( ::pPtr, nOption, lOn )


METHOD QColorDialog:setOptions( nOptions )
   RETURN Qt_QColorDialog_setOptions( ::pPtr, nOptions )


METHOD QColorDialog:setVisible( lVisible )
   RETURN Qt_QColorDialog_setVisible( ::pPtr, lVisible )


METHOD QColorDialog:testOption( nOption )
   RETURN Qt_QColorDialog_testOption( ::pPtr, nOption )


METHOD QColorDialog:customColor( nIndex )
   RETURN Qt_QColorDialog_customColor( ::pPtr, nIndex )


METHOD QColorDialog:customCount()
   RETURN Qt_QColorDialog_customCount( ::pPtr )


METHOD QColorDialog:getColor( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 4
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO" .AND. aV[ 3 ] $ "C" .AND. aV[ 4 ] $ "N"
                // QColor getColor ( const QColor & initial, QWidget * parent, const QString & title, ColorDialogOptions options = 0 )
                // PO p QColor, PO p QWidget, C c QString, N n QColorDialog::ColorDialogOptions
         RETURN QColor():from( Qt_QColorDialog_getColor( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO" .AND. aV[ 3 ] $ "C"
                // QColor getColor ( const QColor & initial, QWidget * parent, const QString & title, ColorDialogOptions options = 0 )
                // PO p QColor, PO p QWidget, C c QString, N n QColorDialog::ColorDialogOptions
         RETURN QColor():from( Qt_QColorDialog_getColor( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO"
                // QColor getColor ( const QColor & initial = Qt::white, QWidget * parent = 0 )
                // PO p QColor, PO p QWidget
         RETURN QColor():from( Qt_QColorDialog_getColor_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 0
             // QColor getColor ( const QColor & initial = Qt::white, QWidget * parent = 0 )
             // PO p QColor, PO p QWidget
      RETURN QColor():from( Qt_QColorDialog_getColor_1( ::pPtr, ... ) )
   ENDCASE
   RETURN NIL


METHOD QColorDialog:setCustomColor( nIndex, nColor )
   RETURN Qt_QColorDialog_setCustomColor( ::pPtr, nIndex, nColor )


METHOD QColorDialog:setStandardColor( nIndex, nColor )
   RETURN Qt_QColorDialog_setStandardColor( ::pPtr, nIndex, nColor )

