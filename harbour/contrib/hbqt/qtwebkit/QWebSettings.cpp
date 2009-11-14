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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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

#include "hbapi.h"
#include "../hbqt.h"

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

QT_G_FUNC( release_QWebSettings )
{
   HB_SYMBOL_UNUSED( Cargo );
}

HB_FUNC( QT_QWEBSETTINGS )
{
}
/*
 * QString fontFamily ( FontFamily which ) const
 */
HB_FUNC( QT_QWEBSETTINGS_FONTFAMILY )
{
   hb_retc( hbqt_par_QWebSettings( 1 )->fontFamily( ( QWebSettings::FontFamily ) hb_parni( 2 ) ).toAscii().data() );
}

/*
 * int fontSize ( FontSize type ) const
 */
HB_FUNC( QT_QWEBSETTINGS_FONTSIZE )
{
   hb_retni( hbqt_par_QWebSettings( 1 )->fontSize( ( QWebSettings::FontSize ) hb_parni( 2 ) ) );
}

/*
 * void resetAttribute ( WebAttribute attribute )
 */
HB_FUNC( QT_QWEBSETTINGS_RESETATTRIBUTE )
{
   hbqt_par_QWebSettings( 1 )->resetAttribute( ( QWebSettings::WebAttribute ) hb_parni( 2 ) );
}

/*
 * void resetFontFamily ( FontFamily which )
 */
HB_FUNC( QT_QWEBSETTINGS_RESETFONTFAMILY )
{
   hbqt_par_QWebSettings( 1 )->resetFontFamily( ( QWebSettings::FontFamily ) hb_parni( 2 ) );
}

/*
 * void resetFontSize ( FontSize type )
 */
HB_FUNC( QT_QWEBSETTINGS_RESETFONTSIZE )
{
   hbqt_par_QWebSettings( 1 )->resetFontSize( ( QWebSettings::FontSize ) hb_parni( 2 ) );
}

/*
 * void setAttribute ( WebAttribute attribute, bool on )
 */
HB_FUNC( QT_QWEBSETTINGS_SETATTRIBUTE )
{
   hbqt_par_QWebSettings( 1 )->setAttribute( ( QWebSettings::WebAttribute ) hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setFontFamily ( FontFamily which, const QString & family )
 */
HB_FUNC( QT_QWEBSETTINGS_SETFONTFAMILY )
{
   hbqt_par_QWebSettings( 1 )->setFontFamily( ( QWebSettings::FontFamily ) hb_parni( 2 ), hbqt_par_QString( 3 ) );
}

/*
 * void setFontSize ( FontSize type, int size )
 */
HB_FUNC( QT_QWEBSETTINGS_SETFONTSIZE )
{
   hbqt_par_QWebSettings( 1 )->setFontSize( ( QWebSettings::FontSize ) hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setUserStyleSheetUrl ( const QUrl & location )
 */
HB_FUNC( QT_QWEBSETTINGS_SETUSERSTYLESHEETURL )
{
   hbqt_par_QWebSettings( 1 )->setUserStyleSheetUrl( *hbqt_par_QUrl( 2 ) );
}

/*
 * bool testAttribute ( WebAttribute attribute ) const
 */
HB_FUNC( QT_QWEBSETTINGS_TESTATTRIBUTE )
{
   hb_retl( hbqt_par_QWebSettings( 1 )->testAttribute( ( QWebSettings::WebAttribute ) hb_parni( 2 ) ) );
}

/*
 * QUrl userStyleSheetUrl () const
 */
HB_FUNC( QT_QWEBSETTINGS_USERSTYLESHEETURL )
{
   hb_retptrGC( gcAllocate_QUrl( new QUrl( hbqt_par_QWebSettings( 1 )->userStyleSheetUrl() ) ) );
}

/*
 * void clearIconDatabase ()
 */
HB_FUNC( QT_QWEBSETTINGS_CLEARICONDATABASE )
{
   hbqt_par_QWebSettings( 1 )->clearIconDatabase();
}

/*
 * QWebSettings * globalSettings ()
 */
HB_FUNC( QT_QWEBSETTINGS_GLOBALSETTINGS )
{
   hb_retptr( ( QWebSettings* ) hbqt_par_QWebSettings( 1 )->globalSettings() );
}

/*
 * QString iconDatabasePath ()
 */
HB_FUNC( QT_QWEBSETTINGS_ICONDATABASEPATH )
{
   hb_retc( hbqt_par_QWebSettings( 1 )->iconDatabasePath().toAscii().data() );
}

/*
 * QIcon iconForUrl ( const QUrl & url )
 */
HB_FUNC( QT_QWEBSETTINGS_ICONFORURL )
{
   hb_retptrGC( gcAllocate_QIcon( new QIcon( hbqt_par_QWebSettings( 1 )->iconForUrl( *hbqt_par_QUrl( 2 ) ) ) ) );
}

/*
 * int maximumPagesInCache ()
 */
HB_FUNC( QT_QWEBSETTINGS_MAXIMUMPAGESINCACHE )
{
   hb_retni( hbqt_par_QWebSettings( 1 )->maximumPagesInCache() );
}

/*
 * qint64 offlineStorageDefaultQuota ()
 */
HB_FUNC( QT_QWEBSETTINGS_OFFLINESTORAGEDEFAULTQUOTA )
{
   hb_retnint( hbqt_par_QWebSettings( 1 )->offlineStorageDefaultQuota() );
}

/*
 * QString offlineStoragePath ()
 */
HB_FUNC( QT_QWEBSETTINGS_OFFLINESTORAGEPATH )
{
   hb_retc( hbqt_par_QWebSettings( 1 )->offlineStoragePath().toAscii().data() );
}

/*
 * void setIconDatabasePath ( const QString & path )
 */
HB_FUNC( QT_QWEBSETTINGS_SETICONDATABASEPATH )
{
   hbqt_par_QWebSettings( 1 )->setIconDatabasePath( hbqt_par_QString( 2 ) );
}

/*
 * void setMaximumPagesInCache ( int pages )
 */
HB_FUNC( QT_QWEBSETTINGS_SETMAXIMUMPAGESINCACHE )
{
   hbqt_par_QWebSettings( 1 )->setMaximumPagesInCache( hb_parni( 2 ) );
}

/*
 * void setObjectCacheCapacities ( int cacheMinDeadCapacity, int cacheMaxDead, int totalCapacity )
 */
HB_FUNC( QT_QWEBSETTINGS_SETOBJECTCACHECAPACITIES )
{
   hbqt_par_QWebSettings( 1 )->setObjectCacheCapacities( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/*
 * void setOfflineStorageDefaultQuota ( qint64 maximumSize )
 */
HB_FUNC( QT_QWEBSETTINGS_SETOFFLINESTORAGEDEFAULTQUOTA )
{
   hbqt_par_QWebSettings( 1 )->setOfflineStorageDefaultQuota( hb_parnint( 2 ) );
}

/*
 * void setOfflineStoragePath ( const QString & path )
 */
HB_FUNC( QT_QWEBSETTINGS_SETOFFLINESTORAGEPATH )
{
   hbqt_par_QWebSettings( 1 )->setOfflineStoragePath( hbqt_par_QString( 2 ) );
}

/*
 * void setWebGraphic ( WebGraphic type, const QPixmap & graphic )
 */
HB_FUNC( QT_QWEBSETTINGS_SETWEBGRAPHIC )
{
   hbqt_par_QWebSettings( 1 )->setWebGraphic( ( QWebSettings::WebGraphic ) hb_parni( 2 ), *hbqt_par_QPixmap( 3 ) );
}

/*
 * QPixmap webGraphic ( WebGraphic type )
 */
HB_FUNC( QT_QWEBSETTINGS_WEBGRAPHIC )
{
   hb_retptrGC( gcAllocate_QPixmap( new QPixmap( hbqt_par_QWebSettings( 1 )->webGraphic( ( QWebSettings::WebGraphic ) hb_parni( 2 ) ) ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
