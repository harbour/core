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


FUNCTION QsciStyle( ... )
   RETURN HB_QsciStyle():new( ... )


CREATE CLASS QsciStyle INHERIT HbQtObjectHandler FUNCTION HB_QsciStyle

   METHOD  new( ... )

   METHOD  style()
   METHOD  setDescription( cDescription )
   METHOD  description()
   METHOD  setColor( pColor )
   METHOD  color()
   METHOD  setPaper( pPaper )
   METHOD  paper()
   METHOD  setFont( pFont )
   METHOD  font()
   METHOD  setEolFill( lFill )
   METHOD  eolFill()
   METHOD  setTextCase( nText_case )
   METHOD  textCase()
   METHOD  setVisible( lVisible )
   METHOD  visible()
   METHOD  setChangeable( lChangeable )
   METHOD  changeable()
   METHOD  setHotspot( lHotspot )
   METHOD  hotspot()
   METHOD  refresh()

   ENDCLASS


METHOD QsciStyle:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QsciStyle( ... )
   RETURN Self


METHOD QsciStyle:style()
   RETURN Qt_QsciStyle_style( ::pPtr )


METHOD QsciStyle:setDescription( cDescription )
   RETURN Qt_QsciStyle_setDescription( ::pPtr, cDescription )


METHOD QsciStyle:description()
   RETURN Qt_QsciStyle_description( ::pPtr )


METHOD QsciStyle:setColor( pColor )
   RETURN Qt_QsciStyle_setColor( ::pPtr, hbqt_ptr( pColor ) )


METHOD QsciStyle:color()
   RETURN Qt_QsciStyle_color( ::pPtr )


METHOD QsciStyle:setPaper( pPaper )
   RETURN Qt_QsciStyle_setPaper( ::pPtr, hbqt_ptr( pPaper ) )


METHOD QsciStyle:paper()
   RETURN Qt_QsciStyle_paper( ::pPtr )


METHOD QsciStyle:setFont( pFont )
   RETURN Qt_QsciStyle_setFont( ::pPtr, hbqt_ptr( pFont ) )


METHOD QsciStyle:font()
   RETURN Qt_QsciStyle_font( ::pPtr )


METHOD QsciStyle:setEolFill( lFill )
   RETURN Qt_QsciStyle_setEolFill( ::pPtr, lFill )


METHOD QsciStyle:eolFill()
   RETURN Qt_QsciStyle_eolFill( ::pPtr )


METHOD QsciStyle:setTextCase( nText_case )
   RETURN Qt_QsciStyle_setTextCase( ::pPtr, nText_case )


METHOD QsciStyle:textCase()
   RETURN Qt_QsciStyle_textCase( ::pPtr )


METHOD QsciStyle:setVisible( lVisible )
   RETURN Qt_QsciStyle_setVisible( ::pPtr, lVisible )


METHOD QsciStyle:visible()
   RETURN Qt_QsciStyle_visible( ::pPtr )


METHOD QsciStyle:setChangeable( lChangeable )
   RETURN Qt_QsciStyle_setChangeable( ::pPtr, lChangeable )


METHOD QsciStyle:changeable()
   RETURN Qt_QsciStyle_changeable( ::pPtr )


METHOD QsciStyle:setHotspot( lHotspot )
   RETURN Qt_QsciStyle_setHotspot( ::pPtr, lHotspot )


METHOD QsciStyle:hotspot()
   RETURN Qt_QsciStyle_hotspot( ::pPtr )


METHOD QsciStyle:refresh()
   RETURN Qt_QsciStyle_refresh( ::pPtr )

