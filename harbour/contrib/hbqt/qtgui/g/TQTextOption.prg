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


FUNCTION QTextOption( ... )
   RETURN HB_QTextOption():new( ... )


CREATE CLASS QTextOption INHERIT HbQtObjectHandler FUNCTION HB_QTextOption

   METHOD  new( ... )

   METHOD  alignment()
   METHOD  flags()
   METHOD  setAlignment( nAlignment )
   METHOD  setFlags( nFlags )
   METHOD  setTabStop( nTabStop )
   METHOD  setTextDirection( nDirection )
   METHOD  setUseDesignMetrics( lEnable )
   METHOD  setWrapMode( nMode )
   METHOD  tabArray()
   METHOD  tabStop()
   METHOD  textDirection()
   METHOD  useDesignMetrics()
   METHOD  wrapMode()

   ENDCLASS


METHOD QTextOption:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextOption( ... )
   RETURN Self


METHOD QTextOption:alignment()
   RETURN Qt_QTextOption_alignment( ::pPtr )


METHOD QTextOption:flags()
   RETURN Qt_QTextOption_flags( ::pPtr )


METHOD QTextOption:setAlignment( nAlignment )
   RETURN Qt_QTextOption_setAlignment( ::pPtr, nAlignment )


METHOD QTextOption:setFlags( nFlags )
   RETURN Qt_QTextOption_setFlags( ::pPtr, nFlags )


METHOD QTextOption:setTabStop( nTabStop )
   RETURN Qt_QTextOption_setTabStop( ::pPtr, nTabStop )


METHOD QTextOption:setTextDirection( nDirection )
   RETURN Qt_QTextOption_setTextDirection( ::pPtr, nDirection )


METHOD QTextOption:setUseDesignMetrics( lEnable )
   RETURN Qt_QTextOption_setUseDesignMetrics( ::pPtr, lEnable )


METHOD QTextOption:setWrapMode( nMode )
   RETURN Qt_QTextOption_setWrapMode( ::pPtr, nMode )


METHOD QTextOption:tabArray()
   RETURN Qt_QTextOption_tabArray( ::pPtr )


METHOD QTextOption:tabStop()
   RETURN Qt_QTextOption_tabStop( ::pPtr )


METHOD QTextOption:textDirection()
   RETURN Qt_QTextOption_textDirection( ::pPtr )


METHOD QTextOption:useDesignMetrics()
   RETURN Qt_QTextOption_useDesignMetrics( ::pPtr )


METHOD QTextOption:wrapMode()
   RETURN Qt_QTextOption_wrapMode( ::pPtr )

