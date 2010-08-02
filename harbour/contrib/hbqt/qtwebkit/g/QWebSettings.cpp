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

#include "hbqt.h"
#include "hbqtwebkit_garbage.h"
#include "hbqtwebkit.h"
#include "hbqtcore_garbage.h"
#include "hbqtcore.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum FontFamily { StandardFont, FixedFont, SerifFont, SansSerifFont, CursiveFont, FantasyFont }
 *  enum FontSize { MinimumFontSize, MinimumLogicalFontSize, DefaultFontSize, DefaultFixedFontSize }
 *  enum WebAttribute { AutoLoadImages, JavascriptEnabled, JavaEnabled, PluginsEnabled, ..., LocalStorageDatabaseEnabled }
 *  enum WebGraphic { MissingImageGraphic, MissingPluginGraphic, DefaultFrameIconGraphic, TextAreaSizeGripCornerGraphic }
 */

#include <QtCore/QPointer>

#include <QtWebKit/QWebSettings>
#include <QtCore/QUrl>


/*
 *
 */

typedef struct
{
   QWebSettings * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QWebSettings;

QT_G_FUNC( hbqt_gcRelease_QWebSettings )
{
   HB_SYMBOL_UNUSED( Cargo );
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QWebSettings( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QWebSettings * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWebSettings;
   p->type = HBQT_TYPE_QWebSettings;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QWebSettings", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QWebSettings", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QWEBSETTINGS )
{
   //hb_retptr( ( QWebSettings* ) new QWebSettings() );
}

/*
 * QString fontFamily ( FontFamily which ) const
 */
HB_FUNC( QT_QWEBSETTINGS_FONTFAMILY )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      hb_retc( ( p )->fontFamily( ( QWebSettings::FontFamily ) hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_FONTFAMILY FP=hb_retc( ( p )->fontFamily( ( QWebSettings::FontFamily ) hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * int fontSize ( FontSize type ) const
 */
HB_FUNC( QT_QWEBSETTINGS_FONTSIZE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      hb_retni( ( p )->fontSize( ( QWebSettings::FontSize ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_FONTSIZE FP=hb_retni( ( p )->fontSize( ( QWebSettings::FontSize ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void resetAttribute ( WebAttribute attribute )
 */
HB_FUNC( QT_QWEBSETTINGS_RESETATTRIBUTE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->resetAttribute( ( QWebSettings::WebAttribute ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_RESETATTRIBUTE FP=( p )->resetAttribute( ( QWebSettings::WebAttribute ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void resetFontFamily ( FontFamily which )
 */
HB_FUNC( QT_QWEBSETTINGS_RESETFONTFAMILY )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->resetFontFamily( ( QWebSettings::FontFamily ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_RESETFONTFAMILY FP=( p )->resetFontFamily( ( QWebSettings::FontFamily ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void resetFontSize ( FontSize type )
 */
HB_FUNC( QT_QWEBSETTINGS_RESETFONTSIZE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->resetFontSize( ( QWebSettings::FontSize ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_RESETFONTSIZE FP=( p )->resetFontSize( ( QWebSettings::FontSize ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setAttribute ( WebAttribute attribute, bool on )
 */
HB_FUNC( QT_QWEBSETTINGS_SETATTRIBUTE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->setAttribute( ( QWebSettings::WebAttribute ) hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_SETATTRIBUTE FP=( p )->setAttribute( ( QWebSettings::WebAttribute ) hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setFontFamily ( FontFamily which, const QString & family )
 */
HB_FUNC( QT_QWEBSETTINGS_SETFONTFAMILY )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->setFontFamily( ( QWebSettings::FontFamily ) hb_parni( 2 ), hbqt_par_QString( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_SETFONTFAMILY FP=( p )->setFontFamily( ( QWebSettings::FontFamily ) hb_parni( 2 ), hbqt_par_QString( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setFontSize ( FontSize type, int size )
 */
HB_FUNC( QT_QWEBSETTINGS_SETFONTSIZE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->setFontSize( ( QWebSettings::FontSize ) hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_SETFONTSIZE FP=( p )->setFontSize( ( QWebSettings::FontSize ) hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setUserStyleSheetUrl ( const QUrl & location )
 */
HB_FUNC( QT_QWEBSETTINGS_SETUSERSTYLESHEETURL )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->setUserStyleSheetUrl( *hbqt_par_QUrl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_SETUSERSTYLESHEETURL FP=( p )->setUserStyleSheetUrl( *hbqt_par_QUrl( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool testAttribute ( WebAttribute attribute ) const
 */
HB_FUNC( QT_QWEBSETTINGS_TESTATTRIBUTE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      hb_retl( ( p )->testAttribute( ( QWebSettings::WebAttribute ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_TESTATTRIBUTE FP=hb_retl( ( p )->testAttribute( ( QWebSettings::WebAttribute ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QUrl userStyleSheetUrl () const
 */
HB_FUNC( QT_QWEBSETTINGS_USERSTYLESHEETURL )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->userStyleSheetUrl() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_USERSTYLESHEETURL FP=hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->userStyleSheetUrl() ), true ) ); p is NULL" ) );
   }
}

/*
 * void clearIconDatabase ()
 */
HB_FUNC( QT_QWEBSETTINGS_CLEARICONDATABASE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->clearIconDatabase();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_CLEARICONDATABASE FP=( p )->clearIconDatabase(); p is NULL" ) );
   }
}

/*
 * QWebSettings * globalSettings ()
 */
HB_FUNC( QT_QWEBSETTINGS_GLOBALSETTINGS )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebSettings( ( p )->globalSettings(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_GLOBALSETTINGS FP=hb_retptrGC( hbqt_gcAllocate_QWebSettings( ( p )->globalSettings(), false ) ); p is NULL" ) );
   }
}

/*
 * QString iconDatabasePath ()
 */
HB_FUNC( QT_QWEBSETTINGS_ICONDATABASEPATH )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      hb_retc( ( p )->iconDatabasePath().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_ICONDATABASEPATH FP=hb_retc( ( p )->iconDatabasePath().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QIcon iconForUrl ( const QUrl & url )
 */
HB_FUNC( QT_QWEBSETTINGS_ICONFORURL )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->iconForUrl( *hbqt_par_QUrl( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_ICONFORURL FP=hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->iconForUrl( *hbqt_par_QUrl( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * int maximumPagesInCache ()
 */
HB_FUNC( QT_QWEBSETTINGS_MAXIMUMPAGESINCACHE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      hb_retni( ( p )->maximumPagesInCache() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_MAXIMUMPAGESINCACHE FP=hb_retni( ( p )->maximumPagesInCache() ); p is NULL" ) );
   }
}

/*
 * qint64 offlineStorageDefaultQuota ()
 */
HB_FUNC( QT_QWEBSETTINGS_OFFLINESTORAGEDEFAULTQUOTA )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      hb_retnint( ( p )->offlineStorageDefaultQuota() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_OFFLINESTORAGEDEFAULTQUOTA FP=hb_retnint( ( p )->offlineStorageDefaultQuota() ); p is NULL" ) );
   }
}

/*
 * QString offlineStoragePath ()
 */
HB_FUNC( QT_QWEBSETTINGS_OFFLINESTORAGEPATH )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      hb_retc( ( p )->offlineStoragePath().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_OFFLINESTORAGEPATH FP=hb_retc( ( p )->offlineStoragePath().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void setIconDatabasePath ( const QString & path )
 */
HB_FUNC( QT_QWEBSETTINGS_SETICONDATABASEPATH )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->setIconDatabasePath( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_SETICONDATABASEPATH FP=( p )->setIconDatabasePath( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMaximumPagesInCache ( int pages )
 */
HB_FUNC( QT_QWEBSETTINGS_SETMAXIMUMPAGESINCACHE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->setMaximumPagesInCache( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_SETMAXIMUMPAGESINCACHE FP=( p )->setMaximumPagesInCache( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setObjectCacheCapacities ( int cacheMinDeadCapacity, int cacheMaxDead, int totalCapacity )
 */
HB_FUNC( QT_QWEBSETTINGS_SETOBJECTCACHECAPACITIES )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->setObjectCacheCapacities( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_SETOBJECTCACHECAPACITIES FP=( p )->setObjectCacheCapacities( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) ); p is NULL" ) );
   }
}

/*
 * void setOfflineStorageDefaultQuota ( qint64 maximumSize )
 */
HB_FUNC( QT_QWEBSETTINGS_SETOFFLINESTORAGEDEFAULTQUOTA )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->setOfflineStorageDefaultQuota( hb_parnint( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_SETOFFLINESTORAGEDEFAULTQUOTA FP=( p )->setOfflineStorageDefaultQuota( hb_parnint( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setOfflineStoragePath ( const QString & path )
 */
HB_FUNC( QT_QWEBSETTINGS_SETOFFLINESTORAGEPATH )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->setOfflineStoragePath( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_SETOFFLINESTORAGEPATH FP=( p )->setOfflineStoragePath( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWebGraphic ( WebGraphic type, const QPixmap & graphic )
 */
HB_FUNC( QT_QWEBSETTINGS_SETWEBGRAPHIC )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->setWebGraphic( ( QWebSettings::WebGraphic ) hb_parni( 2 ), *hbqt_par_QPixmap( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_SETWEBGRAPHIC FP=( p )->setWebGraphic( ( QWebSettings::WebGraphic ) hb_parni( 2 ), *hbqt_par_QPixmap( 3 ) ); p is NULL" ) );
   }
}

/*
 * QPixmap webGraphic ( WebGraphic type )
 */
HB_FUNC( QT_QWEBSETTINGS_WEBGRAPHIC )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->webGraphic( ( QWebSettings::WebGraphic ) hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSETTINGS_WEBGRAPHIC FP=hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->webGraphic( ( QWebSettings::WebGraphic ) hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
