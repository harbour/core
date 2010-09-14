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


FUNCTION QFontInfo( ... )
   RETURN HB_QFontInfo():new( ... )


CREATE CLASS QFontInfo INHERIT HbQtObjectHandler FUNCTION HB_QFontInfo

   METHOD  new( ... )

   METHOD  bold()
   METHOD  exactMatch()
   METHOD  family()
   METHOD  fixedPitch()
   METHOD  italic()
   METHOD  pixelSize()
   METHOD  pointSize()
   METHOD  pointSizeF()
   METHOD  rawMode()
   METHOD  style()
   METHOD  styleHint()
   METHOD  weight()

   ENDCLASS


METHOD QFontInfo:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFontInfo( ... )
   RETURN Self


METHOD QFontInfo:bold()
   RETURN Qt_QFontInfo_bold( ::pPtr )


METHOD QFontInfo:exactMatch()
   RETURN Qt_QFontInfo_exactMatch( ::pPtr )


METHOD QFontInfo:family()
   RETURN Qt_QFontInfo_family( ::pPtr )


METHOD QFontInfo:fixedPitch()
   RETURN Qt_QFontInfo_fixedPitch( ::pPtr )


METHOD QFontInfo:italic()
   RETURN Qt_QFontInfo_italic( ::pPtr )


METHOD QFontInfo:pixelSize()
   RETURN Qt_QFontInfo_pixelSize( ::pPtr )


METHOD QFontInfo:pointSize()
   RETURN Qt_QFontInfo_pointSize( ::pPtr )


METHOD QFontInfo:pointSizeF()
   RETURN Qt_QFontInfo_pointSizeF( ::pPtr )


METHOD QFontInfo:rawMode()
   RETURN Qt_QFontInfo_rawMode( ::pPtr )


METHOD QFontInfo:style()
   RETURN Qt_QFontInfo_style( ::pPtr )


METHOD QFontInfo:styleHint()
   RETURN Qt_QFontInfo_styleHint( ::pPtr )


METHOD QFontInfo:weight()
   RETURN Qt_QFontInfo_weight( ::pPtr )

