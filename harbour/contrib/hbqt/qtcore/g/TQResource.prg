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


FUNCTION QResource( ... )
   RETURN HB_QResource():new( ... )


CREATE CLASS QResource INHERIT HbQtObjectHandler FUNCTION HB_QResource

   METHOD  new( ... )

   METHOD  absoluteFilePath()
   METHOD  data()
   METHOD  fileName()
   METHOD  isCompressed()
   METHOD  isValid()
   METHOD  locale()
   METHOD  setFileName( cFile )
   METHOD  setLocale( pLocale )
   METHOD  size()
   METHOD  registerResource( cRccFileName, cMapRoot )
   METHOD  registerResource_1( pRccData, cMapRoot )
   METHOD  searchPaths()
   METHOD  unregisterResource( cRccFileName, cMapRoot )
   METHOD  unregisterResource_1( pRccData, cMapRoot )

   ENDCLASS


METHOD QResource:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QResource( ... )
   RETURN Self


METHOD QResource:absoluteFilePath()
   RETURN Qt_QResource_absoluteFilePath( ::pPtr )


METHOD QResource:data()
   RETURN Qt_QResource_data( ::pPtr )


METHOD QResource:fileName()
   RETURN Qt_QResource_fileName( ::pPtr )


METHOD QResource:isCompressed()
   RETURN Qt_QResource_isCompressed( ::pPtr )


METHOD QResource:isValid()
   RETURN Qt_QResource_isValid( ::pPtr )


METHOD QResource:locale()
   RETURN Qt_QResource_locale( ::pPtr )


METHOD QResource:setFileName( cFile )
   RETURN Qt_QResource_setFileName( ::pPtr, cFile )


METHOD QResource:setLocale( pLocale )
   RETURN Qt_QResource_setLocale( ::pPtr, hbqt_ptr( pLocale ) )


METHOD QResource:size()
   RETURN Qt_QResource_size( ::pPtr )


METHOD QResource:registerResource( cRccFileName, cMapRoot )
   RETURN Qt_QResource_registerResource( ::pPtr, cRccFileName, cMapRoot )


METHOD QResource:registerResource_1( pRccData, cMapRoot )
   RETURN Qt_QResource_registerResource_1( ::pPtr, hbqt_ptr( pRccData ), cMapRoot )


METHOD QResource:searchPaths()
   RETURN Qt_QResource_searchPaths( ::pPtr )


METHOD QResource:unregisterResource( cRccFileName, cMapRoot )
   RETURN Qt_QResource_unregisterResource( ::pPtr, cRccFileName, cMapRoot )


METHOD QResource:unregisterResource_1( pRccData, cMapRoot )
   RETURN Qt_QResource_unregisterResource_1( ::pPtr, hbqt_ptr( pRccData ), cMapRoot )

