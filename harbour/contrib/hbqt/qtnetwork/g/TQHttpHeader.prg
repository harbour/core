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


FUNCTION QHttpHeader( ... )
   RETURN HB_QHttpHeader():new( ... )


CREATE CLASS QHttpHeader INHERIT HbQtObjectHandler FUNCTION HB_QHttpHeader

   METHOD  new( ... )

   METHOD  addValue( cKey, cValue )
   METHOD  allValues( cKey )
   METHOD  contentLength()
   METHOD  contentType()
   METHOD  hasContentLength()
   METHOD  hasContentType()
   METHOD  hasKey( cKey )
   METHOD  isValid()
   METHOD  keys()
   METHOD  majorVersion()
   METHOD  minorVersion()
   METHOD  removeAllValues( cKey )
   METHOD  removeValue( cKey )
   METHOD  setContentLength( nLen )
   METHOD  setContentType( cType )
   METHOD  setValue( cKey, cValue )
   METHOD  toString()
   METHOD  value( cKey )

   ENDCLASS


METHOD QHttpHeader:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QHttpHeader( ... )
   RETURN Self


METHOD QHttpHeader:addValue( cKey, cValue )
   RETURN Qt_QHttpHeader_addValue( ::pPtr, cKey, cValue )


METHOD QHttpHeader:allValues( cKey )
   RETURN Qt_QHttpHeader_allValues( ::pPtr, cKey )


METHOD QHttpHeader:contentLength()
   RETURN Qt_QHttpHeader_contentLength( ::pPtr )


METHOD QHttpHeader:contentType()
   RETURN Qt_QHttpHeader_contentType( ::pPtr )


METHOD QHttpHeader:hasContentLength()
   RETURN Qt_QHttpHeader_hasContentLength( ::pPtr )


METHOD QHttpHeader:hasContentType()
   RETURN Qt_QHttpHeader_hasContentType( ::pPtr )


METHOD QHttpHeader:hasKey( cKey )
   RETURN Qt_QHttpHeader_hasKey( ::pPtr, cKey )


METHOD QHttpHeader:isValid()
   RETURN Qt_QHttpHeader_isValid( ::pPtr )


METHOD QHttpHeader:keys()
   RETURN Qt_QHttpHeader_keys( ::pPtr )


METHOD QHttpHeader:majorVersion()
   RETURN Qt_QHttpHeader_majorVersion( ::pPtr )


METHOD QHttpHeader:minorVersion()
   RETURN Qt_QHttpHeader_minorVersion( ::pPtr )


METHOD QHttpHeader:removeAllValues( cKey )
   RETURN Qt_QHttpHeader_removeAllValues( ::pPtr, cKey )


METHOD QHttpHeader:removeValue( cKey )
   RETURN Qt_QHttpHeader_removeValue( ::pPtr, cKey )


METHOD QHttpHeader:setContentLength( nLen )
   RETURN Qt_QHttpHeader_setContentLength( ::pPtr, nLen )


METHOD QHttpHeader:setContentType( cType )
   RETURN Qt_QHttpHeader_setContentType( ::pPtr, cType )


METHOD QHttpHeader:setValue( cKey, cValue )
   RETURN Qt_QHttpHeader_setValue( ::pPtr, cKey, cValue )


METHOD QHttpHeader:toString()
   RETURN Qt_QHttpHeader_toString( ::pPtr )


METHOD QHttpHeader:value( cKey )
   RETURN Qt_QHttpHeader_value( ::pPtr, cKey )

