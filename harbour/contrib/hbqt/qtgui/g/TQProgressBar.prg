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


FUNCTION QProgressBar( ... )
   RETURN HB_QProgressBar():new( ... )


CREATE CLASS QProgressBar INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QProgressBar

   METHOD  new( ... )

   METHOD  alignment()
   METHOD  format()
   METHOD  invertedAppearance()
   METHOD  isTextVisible()
   METHOD  maximum()
   METHOD  minimum()
   METHOD  orientation()
   METHOD  setAlignment( nAlignment )
   METHOD  setFormat( cFormat )
   METHOD  setInvertedAppearance( lInvert )
   METHOD  setTextDirection( nTextDirection )
   METHOD  setTextVisible( lVisible )
   METHOD  text()
   METHOD  textDirection()
   METHOD  value()
   METHOD  reset()
   METHOD  setMaximum( nMaximum )
   METHOD  setMinimum( nMinimum )
   METHOD  setOrientation( nQt_Orientation )
   METHOD  setRange( nMinimum, nMaximum )
   METHOD  setValue( nValue )

   ENDCLASS


METHOD QProgressBar:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QProgressBar( ... )
   RETURN Self


METHOD QProgressBar:alignment()
   RETURN Qt_QProgressBar_alignment( ::pPtr )


METHOD QProgressBar:format()
   RETURN Qt_QProgressBar_format( ::pPtr )


METHOD QProgressBar:invertedAppearance()
   RETURN Qt_QProgressBar_invertedAppearance( ::pPtr )


METHOD QProgressBar:isTextVisible()
   RETURN Qt_QProgressBar_isTextVisible( ::pPtr )


METHOD QProgressBar:maximum()
   RETURN Qt_QProgressBar_maximum( ::pPtr )


METHOD QProgressBar:minimum()
   RETURN Qt_QProgressBar_minimum( ::pPtr )


METHOD QProgressBar:orientation()
   RETURN Qt_QProgressBar_orientation( ::pPtr )


METHOD QProgressBar:setAlignment( nAlignment )
   RETURN Qt_QProgressBar_setAlignment( ::pPtr, nAlignment )


METHOD QProgressBar:setFormat( cFormat )
   RETURN Qt_QProgressBar_setFormat( ::pPtr, cFormat )


METHOD QProgressBar:setInvertedAppearance( lInvert )
   RETURN Qt_QProgressBar_setInvertedAppearance( ::pPtr, lInvert )


METHOD QProgressBar:setTextDirection( nTextDirection )
   RETURN Qt_QProgressBar_setTextDirection( ::pPtr, nTextDirection )


METHOD QProgressBar:setTextVisible( lVisible )
   RETURN Qt_QProgressBar_setTextVisible( ::pPtr, lVisible )


METHOD QProgressBar:text()
   RETURN Qt_QProgressBar_text( ::pPtr )


METHOD QProgressBar:textDirection()
   RETURN Qt_QProgressBar_textDirection( ::pPtr )


METHOD QProgressBar:value()
   RETURN Qt_QProgressBar_value( ::pPtr )


METHOD QProgressBar:reset()
   RETURN Qt_QProgressBar_reset( ::pPtr )


METHOD QProgressBar:setMaximum( nMaximum )
   RETURN Qt_QProgressBar_setMaximum( ::pPtr, nMaximum )


METHOD QProgressBar:setMinimum( nMinimum )
   RETURN Qt_QProgressBar_setMinimum( ::pPtr, nMinimum )


METHOD QProgressBar:setOrientation( nQt_Orientation )
   RETURN Qt_QProgressBar_setOrientation( ::pPtr, nQt_Orientation )


METHOD QProgressBar:setRange( nMinimum, nMaximum )
   RETURN Qt_QProgressBar_setRange( ::pPtr, nMinimum, nMaximum )


METHOD QProgressBar:setValue( nValue )
   RETURN Qt_QProgressBar_setValue( ::pPtr, nValue )

