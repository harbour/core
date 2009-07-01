/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * www - http://www.harbour-project.org
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


CREATE CLASS QWebSettings

   VAR     pParent
   VAR     pPtr

   METHOD  New()

   METHOD  fontFamily( nWhich )                INLINE  Qt_QWebSettings_fontFamily( ::pPtr, nWhich )
   METHOD  fontSize( nType )                   INLINE  Qt_QWebSettings_fontSize( ::pPtr, nType )
   METHOD  resetAttribute( nAttribute )        INLINE  Qt_QWebSettings_resetAttribute( ::pPtr, nAttribute )
   METHOD  resetFontFamily( nWhich )           INLINE  Qt_QWebSettings_resetFontFamily( ::pPtr, nWhich )
   METHOD  resetFontSize( nType )              INLINE  Qt_QWebSettings_resetFontSize( ::pPtr, nType )
   METHOD  setAttribute( nAttribute, lOn )     INLINE  Qt_QWebSettings_setAttribute( ::pPtr, nAttribute, lOn )
   METHOD  setFontFamily( nWhich, cFamily )    INLINE  Qt_QWebSettings_setFontFamily( ::pPtr, nWhich, cFamily )
   METHOD  setFontSize( nType, nSize )         INLINE  Qt_QWebSettings_setFontSize( ::pPtr, nType, nSize )
   METHOD  setUserStyleSheetUrl( pLocation )   INLINE  Qt_QWebSettings_setUserStyleSheetUrl( ::pPtr, pLocation )
   METHOD  testAttribute( nAttribute )         INLINE  Qt_QWebSettings_testAttribute( ::pPtr, nAttribute )
   METHOD  userStyleSheetUrl()                 INLINE  Qt_QWebSettings_userStyleSheetUrl( ::pPtr )
   METHOD  clearIconDatabase()                 INLINE  Qt_QWebSettings_clearIconDatabase( ::pPtr )
   METHOD  globalSettings()                    INLINE  Qt_QWebSettings_globalSettings( ::pPtr )
   METHOD  iconDatabasePath()                  INLINE  Qt_QWebSettings_iconDatabasePath( ::pPtr )
   METHOD  iconForUrl( pUrl )                  INLINE  Qt_QWebSettings_iconForUrl( ::pPtr, pUrl )
   METHOD  maximumPagesInCache()               INLINE  Qt_QWebSettings_maximumPagesInCache( ::pPtr )
   METHOD  offlineStorageDefaultQuota()        INLINE  Qt_QWebSettings_offlineStorageDefaultQuota( ::pPtr )
   METHOD  offlineStoragePath()                INLINE  Qt_QWebSettings_offlineStoragePath( ::pPtr )
   METHOD  setIconDatabasePath( cPath )        INLINE  Qt_QWebSettings_setIconDatabasePath( ::pPtr, cPath )
   METHOD  setMaximumPagesInCache( nPages )    INLINE  Qt_QWebSettings_setMaximumPagesInCache( ::pPtr, nPages )
   METHOD  setObjectCacheCapacities( nCacheMinDeadCapacity, nCacheMaxDead, nTotalCapacity )  INLINE  Qt_QWebSettings_setObjectCacheCapacities( ::pPtr, nCacheMinDeadCapacity, nCacheMaxDead, nTotalCapacity )
   METHOD  setOfflineStorageDefaultQuota( nMaximumSize )  INLINE  Qt_QWebSettings_setOfflineStorageDefaultQuota( ::pPtr, nMaximumSize )
   METHOD  setOfflineStoragePath( cPath )      INLINE  Qt_QWebSettings_setOfflineStoragePath( ::pPtr, cPath )
   METHOD  setWebGraphic( nType, pGraphic )    INLINE  Qt_QWebSettings_setWebGraphic( ::pPtr, nType, pGraphic )
   METHOD  webGraphic( nType )                 INLINE  Qt_QWebSettings_webGraphic( ::pPtr, nType )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QWebSettings

   ::pParent := pParent

   ::pPtr := Qt_QWebSettings( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

