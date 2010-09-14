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


FUNCTION QWebSettings( ... )
   RETURN HB_QWebSettings():new( ... )


CREATE CLASS QWebSettings INHERIT HbQtObjectHandler FUNCTION HB_QWebSettings

   METHOD  new( ... )

   METHOD  fontFamily( nWhich )
   METHOD  fontSize( nType )
   METHOD  resetAttribute( nAttribute )
   METHOD  resetFontFamily( nWhich )
   METHOD  resetFontSize( nType )
   METHOD  setAttribute( nAttribute, lOn )
   METHOD  setFontFamily( nWhich, cFamily )
   METHOD  setFontSize( nType, nSize )
   METHOD  setUserStyleSheetUrl( pLocation )
   METHOD  testAttribute( nAttribute )
   METHOD  userStyleSheetUrl()
   METHOD  clearIconDatabase()
   METHOD  globalSettings()
   METHOD  iconDatabasePath()
   METHOD  iconForUrl( pUrl )
   METHOD  maximumPagesInCache()
   METHOD  offlineStorageDefaultQuota()
   METHOD  offlineStoragePath()
   METHOD  setIconDatabasePath( cPath )
   METHOD  setMaximumPagesInCache( nPages )
   METHOD  setObjectCacheCapacities( nCacheMinDeadCapacity, nCacheMaxDead, nTotalCapacity )
   METHOD  setOfflineStorageDefaultQuota( nMaximumSize )
   METHOD  setOfflineStoragePath( cPath )
   METHOD  setWebGraphic( nType, pGraphic )
   METHOD  webGraphic( nType )

   ENDCLASS


METHOD QWebSettings:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWebSettings( ... )
   RETURN Self


METHOD QWebSettings:fontFamily( nWhich )
   RETURN Qt_QWebSettings_fontFamily( ::pPtr, nWhich )


METHOD QWebSettings:fontSize( nType )
   RETURN Qt_QWebSettings_fontSize( ::pPtr, nType )


METHOD QWebSettings:resetAttribute( nAttribute )
   RETURN Qt_QWebSettings_resetAttribute( ::pPtr, nAttribute )


METHOD QWebSettings:resetFontFamily( nWhich )
   RETURN Qt_QWebSettings_resetFontFamily( ::pPtr, nWhich )


METHOD QWebSettings:resetFontSize( nType )
   RETURN Qt_QWebSettings_resetFontSize( ::pPtr, nType )


METHOD QWebSettings:setAttribute( nAttribute, lOn )
   RETURN Qt_QWebSettings_setAttribute( ::pPtr, nAttribute, lOn )


METHOD QWebSettings:setFontFamily( nWhich, cFamily )
   RETURN Qt_QWebSettings_setFontFamily( ::pPtr, nWhich, cFamily )


METHOD QWebSettings:setFontSize( nType, nSize )
   RETURN Qt_QWebSettings_setFontSize( ::pPtr, nType, nSize )


METHOD QWebSettings:setUserStyleSheetUrl( pLocation )
   RETURN Qt_QWebSettings_setUserStyleSheetUrl( ::pPtr, hbqt_ptr( pLocation ) )


METHOD QWebSettings:testAttribute( nAttribute )
   RETURN Qt_QWebSettings_testAttribute( ::pPtr, nAttribute )


METHOD QWebSettings:userStyleSheetUrl()
   RETURN Qt_QWebSettings_userStyleSheetUrl( ::pPtr )


METHOD QWebSettings:clearIconDatabase()
   RETURN Qt_QWebSettings_clearIconDatabase( ::pPtr )


METHOD QWebSettings:globalSettings()
   RETURN Qt_QWebSettings_globalSettings( ::pPtr )


METHOD QWebSettings:iconDatabasePath()
   RETURN Qt_QWebSettings_iconDatabasePath( ::pPtr )


METHOD QWebSettings:iconForUrl( pUrl )
   RETURN Qt_QWebSettings_iconForUrl( ::pPtr, hbqt_ptr( pUrl ) )


METHOD QWebSettings:maximumPagesInCache()
   RETURN Qt_QWebSettings_maximumPagesInCache( ::pPtr )


METHOD QWebSettings:offlineStorageDefaultQuota()
   RETURN Qt_QWebSettings_offlineStorageDefaultQuota( ::pPtr )


METHOD QWebSettings:offlineStoragePath()
   RETURN Qt_QWebSettings_offlineStoragePath( ::pPtr )


METHOD QWebSettings:setIconDatabasePath( cPath )
   RETURN Qt_QWebSettings_setIconDatabasePath( ::pPtr, cPath )


METHOD QWebSettings:setMaximumPagesInCache( nPages )
   RETURN Qt_QWebSettings_setMaximumPagesInCache( ::pPtr, nPages )


METHOD QWebSettings:setObjectCacheCapacities( nCacheMinDeadCapacity, nCacheMaxDead, nTotalCapacity )
   RETURN Qt_QWebSettings_setObjectCacheCapacities( ::pPtr, nCacheMinDeadCapacity, nCacheMaxDead, nTotalCapacity )


METHOD QWebSettings:setOfflineStorageDefaultQuota( nMaximumSize )
   RETURN Qt_QWebSettings_setOfflineStorageDefaultQuota( ::pPtr, nMaximumSize )


METHOD QWebSettings:setOfflineStoragePath( cPath )
   RETURN Qt_QWebSettings_setOfflineStoragePath( ::pPtr, cPath )


METHOD QWebSettings:setWebGraphic( nType, pGraphic )
   RETURN Qt_QWebSettings_setWebGraphic( ::pPtr, nType, hbqt_ptr( pGraphic ) )


METHOD QWebSettings:webGraphic( nType )
   RETURN Qt_QWebSettings_webGraphic( ::pPtr, nType )

