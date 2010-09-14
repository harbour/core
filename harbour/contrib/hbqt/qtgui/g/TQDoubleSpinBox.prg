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


FUNCTION QDoubleSpinBox( ... )
   RETURN HB_QDoubleSpinBox():new( ... )


CREATE CLASS QDoubleSpinBox INHERIT HbQtObjectHandler, HB_QAbstractSpinBox FUNCTION HB_QDoubleSpinBox

   METHOD  new( ... )

   METHOD  cleanText()
   METHOD  decimals()
   METHOD  maximum()
   METHOD  minimum()
   METHOD  prefix()
   METHOD  setDecimals( nPrec )
   METHOD  setMaximum( nMax )
   METHOD  setMinimum( nMin )
   METHOD  setPrefix( cPrefix )
   METHOD  setRange( nMinimum, nMaximum )
   METHOD  setSingleStep( nVal )
   METHOD  setSuffix( cSuffix )
   METHOD  singleStep()
   METHOD  suffix()
   METHOD  textFromValue( nValue )
   METHOD  value()
   METHOD  valueFromText( cText )
   METHOD  setValue( nVal )

   ENDCLASS


METHOD QDoubleSpinBox:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDoubleSpinBox( ... )
   RETURN Self


METHOD QDoubleSpinBox:cleanText()
   RETURN Qt_QDoubleSpinBox_cleanText( ::pPtr )


METHOD QDoubleSpinBox:decimals()
   RETURN Qt_QDoubleSpinBox_decimals( ::pPtr )


METHOD QDoubleSpinBox:maximum()
   RETURN Qt_QDoubleSpinBox_maximum( ::pPtr )


METHOD QDoubleSpinBox:minimum()
   RETURN Qt_QDoubleSpinBox_minimum( ::pPtr )


METHOD QDoubleSpinBox:prefix()
   RETURN Qt_QDoubleSpinBox_prefix( ::pPtr )


METHOD QDoubleSpinBox:setDecimals( nPrec )
   RETURN Qt_QDoubleSpinBox_setDecimals( ::pPtr, nPrec )


METHOD QDoubleSpinBox:setMaximum( nMax )
   RETURN Qt_QDoubleSpinBox_setMaximum( ::pPtr, nMax )


METHOD QDoubleSpinBox:setMinimum( nMin )
   RETURN Qt_QDoubleSpinBox_setMinimum( ::pPtr, nMin )


METHOD QDoubleSpinBox:setPrefix( cPrefix )
   RETURN Qt_QDoubleSpinBox_setPrefix( ::pPtr, cPrefix )


METHOD QDoubleSpinBox:setRange( nMinimum, nMaximum )
   RETURN Qt_QDoubleSpinBox_setRange( ::pPtr, nMinimum, nMaximum )


METHOD QDoubleSpinBox:setSingleStep( nVal )
   RETURN Qt_QDoubleSpinBox_setSingleStep( ::pPtr, nVal )


METHOD QDoubleSpinBox:setSuffix( cSuffix )
   RETURN Qt_QDoubleSpinBox_setSuffix( ::pPtr, cSuffix )


METHOD QDoubleSpinBox:singleStep()
   RETURN Qt_QDoubleSpinBox_singleStep( ::pPtr )


METHOD QDoubleSpinBox:suffix()
   RETURN Qt_QDoubleSpinBox_suffix( ::pPtr )


METHOD QDoubleSpinBox:textFromValue( nValue )
   RETURN Qt_QDoubleSpinBox_textFromValue( ::pPtr, nValue )


METHOD QDoubleSpinBox:value()
   RETURN Qt_QDoubleSpinBox_value( ::pPtr )


METHOD QDoubleSpinBox:valueFromText( cText )
   RETURN Qt_QDoubleSpinBox_valueFromText( ::pPtr, cText )


METHOD QDoubleSpinBox:setValue( nVal )
   RETURN Qt_QDoubleSpinBox_setValue( ::pPtr, nVal )

