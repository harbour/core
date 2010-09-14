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


FUNCTION QFrame( ... )
   RETURN HB_QFrame():new( ... )


CREATE CLASS QFrame INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QFrame

   METHOD  new( ... )

   METHOD  frameRect()
   METHOD  frameShadow()
   METHOD  frameShape()
   METHOD  frameStyle()
   METHOD  frameWidth()
   METHOD  lineWidth()
   METHOD  midLineWidth()
   METHOD  setFrameRect( pQRect )
   METHOD  setFrameShadow( nShadow )
   METHOD  setFrameShape( nShape )
   METHOD  setFrameStyle( nStyle )
   METHOD  setLineWidth( nInt )
   METHOD  setMidLineWidth( nInt )

   ENDCLASS


METHOD QFrame:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFrame( ... )
   RETURN Self


METHOD QFrame:frameRect()
   RETURN Qt_QFrame_frameRect( ::pPtr )


METHOD QFrame:frameShadow()
   RETURN Qt_QFrame_frameShadow( ::pPtr )


METHOD QFrame:frameShape()
   RETURN Qt_QFrame_frameShape( ::pPtr )


METHOD QFrame:frameStyle()
   RETURN Qt_QFrame_frameStyle( ::pPtr )


METHOD QFrame:frameWidth()
   RETURN Qt_QFrame_frameWidth( ::pPtr )


METHOD QFrame:lineWidth()
   RETURN Qt_QFrame_lineWidth( ::pPtr )


METHOD QFrame:midLineWidth()
   RETURN Qt_QFrame_midLineWidth( ::pPtr )


METHOD QFrame:setFrameRect( pQRect )
   RETURN Qt_QFrame_setFrameRect( ::pPtr, hbqt_ptr( pQRect ) )


METHOD QFrame:setFrameShadow( nShadow )
   RETURN Qt_QFrame_setFrameShadow( ::pPtr, nShadow )


METHOD QFrame:setFrameShape( nShape )
   RETURN Qt_QFrame_setFrameShape( ::pPtr, nShape )


METHOD QFrame:setFrameStyle( nStyle )
   RETURN Qt_QFrame_setFrameStyle( ::pPtr, nStyle )


METHOD QFrame:setLineWidth( nInt )
   RETURN Qt_QFrame_setLineWidth( ::pPtr, nInt )


METHOD QFrame:setMidLineWidth( nInt )
   RETURN Qt_QFrame_setMidLineWidth( ::pPtr, nInt )

