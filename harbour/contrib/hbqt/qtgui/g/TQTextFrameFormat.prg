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


FUNCTION QTextFrameFormat( ... )
   RETURN HB_QTextFrameFormat():new( ... )


CREATE CLASS QTextFrameFormat INHERIT HbQtObjectHandler, HB_QTextFormat FUNCTION HB_QTextFrameFormat

   METHOD  new( ... )

   METHOD  border()
   METHOD  borderBrush()
   METHOD  borderStyle()
   METHOD  bottomMargin()
   METHOD  height()
   METHOD  isValid()
   METHOD  leftMargin()
   METHOD  margin()
   METHOD  padding()
   METHOD  pageBreakPolicy()
   METHOD  position()
   METHOD  rightMargin()
   METHOD  setBorder( nWidth )
   METHOD  setBorderBrush( pBrush )
   METHOD  setBorderStyle( nStyle )
   METHOD  setBottomMargin( nMargin )
   METHOD  setHeight( pHeight )
   METHOD  setHeight_1( nHeight )
   METHOD  setLeftMargin( nMargin )
   METHOD  setMargin( nMargin )
   METHOD  setPadding( nWidth )
   METHOD  setPageBreakPolicy( nPolicy )
   METHOD  setPosition( nPolicy )
   METHOD  setRightMargin( nMargin )
   METHOD  setTopMargin( nMargin )
   METHOD  setWidth( pWidth )
   METHOD  setWidth_1( nWidth )
   METHOD  topMargin()
   METHOD  width()

   ENDCLASS


METHOD QTextFrameFormat:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextFrameFormat( ... )
   RETURN Self


METHOD QTextFrameFormat:border()
   RETURN Qt_QTextFrameFormat_border( ::pPtr )


METHOD QTextFrameFormat:borderBrush()
   RETURN Qt_QTextFrameFormat_borderBrush( ::pPtr )


METHOD QTextFrameFormat:borderStyle()
   RETURN Qt_QTextFrameFormat_borderStyle( ::pPtr )


METHOD QTextFrameFormat:bottomMargin()
   RETURN Qt_QTextFrameFormat_bottomMargin( ::pPtr )


METHOD QTextFrameFormat:height()
   RETURN Qt_QTextFrameFormat_height( ::pPtr )


METHOD QTextFrameFormat:isValid()
   RETURN Qt_QTextFrameFormat_isValid( ::pPtr )


METHOD QTextFrameFormat:leftMargin()
   RETURN Qt_QTextFrameFormat_leftMargin( ::pPtr )


METHOD QTextFrameFormat:margin()
   RETURN Qt_QTextFrameFormat_margin( ::pPtr )


METHOD QTextFrameFormat:padding()
   RETURN Qt_QTextFrameFormat_padding( ::pPtr )


METHOD QTextFrameFormat:pageBreakPolicy()
   RETURN Qt_QTextFrameFormat_pageBreakPolicy( ::pPtr )


METHOD QTextFrameFormat:position()
   RETURN Qt_QTextFrameFormat_position( ::pPtr )


METHOD QTextFrameFormat:rightMargin()
   RETURN Qt_QTextFrameFormat_rightMargin( ::pPtr )


METHOD QTextFrameFormat:setBorder( nWidth )
   RETURN Qt_QTextFrameFormat_setBorder( ::pPtr, nWidth )


METHOD QTextFrameFormat:setBorderBrush( pBrush )
   RETURN Qt_QTextFrameFormat_setBorderBrush( ::pPtr, hbqt_ptr( pBrush ) )


METHOD QTextFrameFormat:setBorderStyle( nStyle )
   RETURN Qt_QTextFrameFormat_setBorderStyle( ::pPtr, nStyle )


METHOD QTextFrameFormat:setBottomMargin( nMargin )
   RETURN Qt_QTextFrameFormat_setBottomMargin( ::pPtr, nMargin )


METHOD QTextFrameFormat:setHeight( pHeight )
   RETURN Qt_QTextFrameFormat_setHeight( ::pPtr, hbqt_ptr( pHeight ) )


METHOD QTextFrameFormat:setHeight_1( nHeight )
   RETURN Qt_QTextFrameFormat_setHeight_1( ::pPtr, nHeight )


METHOD QTextFrameFormat:setLeftMargin( nMargin )
   RETURN Qt_QTextFrameFormat_setLeftMargin( ::pPtr, nMargin )


METHOD QTextFrameFormat:setMargin( nMargin )
   RETURN Qt_QTextFrameFormat_setMargin( ::pPtr, nMargin )


METHOD QTextFrameFormat:setPadding( nWidth )
   RETURN Qt_QTextFrameFormat_setPadding( ::pPtr, nWidth )


METHOD QTextFrameFormat:setPageBreakPolicy( nPolicy )
   RETURN Qt_QTextFrameFormat_setPageBreakPolicy( ::pPtr, nPolicy )


METHOD QTextFrameFormat:setPosition( nPolicy )
   RETURN Qt_QTextFrameFormat_setPosition( ::pPtr, nPolicy )


METHOD QTextFrameFormat:setRightMargin( nMargin )
   RETURN Qt_QTextFrameFormat_setRightMargin( ::pPtr, nMargin )


METHOD QTextFrameFormat:setTopMargin( nMargin )
   RETURN Qt_QTextFrameFormat_setTopMargin( ::pPtr, nMargin )


METHOD QTextFrameFormat:setWidth( pWidth )
   RETURN Qt_QTextFrameFormat_setWidth( ::pPtr, hbqt_ptr( pWidth ) )


METHOD QTextFrameFormat:setWidth_1( nWidth )
   RETURN Qt_QTextFrameFormat_setWidth_1( ::pPtr, nWidth )


METHOD QTextFrameFormat:topMargin()
   RETURN Qt_QTextFrameFormat_topMargin( ::pPtr )


METHOD QTextFrameFormat:width()
   RETURN Qt_QTextFrameFormat_width( ::pPtr )

