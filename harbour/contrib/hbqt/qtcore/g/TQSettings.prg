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


FUNCTION QSettings( ... )
   RETURN HB_QSettings():new( ... )


CREATE CLASS QSettings INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QSettings

   METHOD  new( ... )

   METHOD  allKeys()
   METHOD  applicationName()
   METHOD  beginGroup( cPrefix )
   METHOD  beginReadArray( cPrefix )
   METHOD  beginWriteArray( cPrefix, nSize )
   METHOD  childGroups()
   METHOD  childKeys()
   METHOD  clear()
   METHOD  contains( cKey )
   METHOD  endArray()
   METHOD  endGroup()
   METHOD  fallbacksEnabled()
   METHOD  fileName()
   METHOD  format()
   METHOD  group()
   METHOD  iniCodec()
   METHOD  isWritable()
   METHOD  organizationName()
   METHOD  remove( cKey )
   METHOD  scope()
   METHOD  setArrayIndex( nI )
   METHOD  setFallbacksEnabled( lB )
   METHOD  setIniCodec( pCodec )
   METHOD  setIniCodec_1( pCodecName )
   METHOD  setValue( cKey, pValue )
   METHOD  status()
   METHOD  sync()
   METHOD  value( cKey, pDefaultValue )
   METHOD  defaultFormat()
   METHOD  setDefaultFormat( nFormat )
   METHOD  setPath( nFormat, nScope, cPath )

   ENDCLASS


METHOD QSettings:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QSettings( ... )
   RETURN Self


METHOD QSettings:allKeys()
   RETURN Qt_QSettings_allKeys( ::pPtr )


METHOD QSettings:applicationName()
   RETURN Qt_QSettings_applicationName( ::pPtr )


METHOD QSettings:beginGroup( cPrefix )
   RETURN Qt_QSettings_beginGroup( ::pPtr, cPrefix )


METHOD QSettings:beginReadArray( cPrefix )
   RETURN Qt_QSettings_beginReadArray( ::pPtr, cPrefix )


METHOD QSettings:beginWriteArray( cPrefix, nSize )
   RETURN Qt_QSettings_beginWriteArray( ::pPtr, cPrefix, nSize )


METHOD QSettings:childGroups()
   RETURN Qt_QSettings_childGroups( ::pPtr )


METHOD QSettings:childKeys()
   RETURN Qt_QSettings_childKeys( ::pPtr )


METHOD QSettings:clear()
   RETURN Qt_QSettings_clear( ::pPtr )


METHOD QSettings:contains( cKey )
   RETURN Qt_QSettings_contains( ::pPtr, cKey )


METHOD QSettings:endArray()
   RETURN Qt_QSettings_endArray( ::pPtr )


METHOD QSettings:endGroup()
   RETURN Qt_QSettings_endGroup( ::pPtr )


METHOD QSettings:fallbacksEnabled()
   RETURN Qt_QSettings_fallbacksEnabled( ::pPtr )


METHOD QSettings:fileName()
   RETURN Qt_QSettings_fileName( ::pPtr )


METHOD QSettings:format()
   RETURN Qt_QSettings_format( ::pPtr )


METHOD QSettings:group()
   RETURN Qt_QSettings_group( ::pPtr )


METHOD QSettings:iniCodec()
   RETURN Qt_QSettings_iniCodec( ::pPtr )


METHOD QSettings:isWritable()
   RETURN Qt_QSettings_isWritable( ::pPtr )


METHOD QSettings:organizationName()
   RETURN Qt_QSettings_organizationName( ::pPtr )


METHOD QSettings:remove( cKey )
   RETURN Qt_QSettings_remove( ::pPtr, cKey )


METHOD QSettings:scope()
   RETURN Qt_QSettings_scope( ::pPtr )


METHOD QSettings:setArrayIndex( nI )
   RETURN Qt_QSettings_setArrayIndex( ::pPtr, nI )


METHOD QSettings:setFallbacksEnabled( lB )
   RETURN Qt_QSettings_setFallbacksEnabled( ::pPtr, lB )


METHOD QSettings:setIniCodec( pCodec )
   RETURN Qt_QSettings_setIniCodec( ::pPtr, hbqt_ptr( pCodec ) )


METHOD QSettings:setIniCodec_1( pCodecName )
   RETURN Qt_QSettings_setIniCodec_1( ::pPtr, hbqt_ptr( pCodecName ) )


METHOD QSettings:setValue( cKey, pValue )
   RETURN Qt_QSettings_setValue( ::pPtr, cKey, hbqt_ptr( pValue ) )


METHOD QSettings:status()
   RETURN Qt_QSettings_status( ::pPtr )


METHOD QSettings:sync()
   RETURN Qt_QSettings_sync( ::pPtr )


METHOD QSettings:value( cKey, pDefaultValue )
   RETURN Qt_QSettings_value( ::pPtr, cKey, hbqt_ptr( pDefaultValue ) )


METHOD QSettings:defaultFormat()
   RETURN Qt_QSettings_defaultFormat( ::pPtr )


METHOD QSettings:setDefaultFormat( nFormat )
   RETURN Qt_QSettings_setDefaultFormat( ::pPtr, nFormat )


METHOD QSettings:setPath( nFormat, nScope, cPath )
   RETURN Qt_QSettings_setPath( ::pPtr, nFormat, nScope, cPath )

