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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"
#include "hbqtwebkit.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum FontFamily { StandardFont, FixedFont, SerifFont, SansSerifFont, CursiveFont, FantasyFont }
 *  enum FontSize { MinimumFontSize, MinimumLogicalFontSize, DefaultFontSize, DefaultFixedFontSize }
 *  enum WebAttribute { AutoLoadImages, JavascriptEnabled, JavaEnabled, PluginsEnabled, ..., LocalStorageDatabaseEnabled }
 *  enum WebGraphic { MissingImageGraphic, MissingPluginGraphic, DefaultFrameIconGraphic, TextAreaSizeGripCornerGraphic }
 */

/*
 *  Constructed[ 25/25 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include "hbqtgui.h"

#include <QtWebKit/QWebSettings>
#include <QtCore/QUrl>


/*
 * QWebSettings ()
 */

typedef struct
{
   QWebSettings * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QWebSettings;

HBQT_GC_FUNC( hbqt_gcRelease_QWebSettings )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QWebSettings( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

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
   //__HB_RETPTRGC__( new QWebSettings() );
}

/*
 * QString fontFamily ( FontFamily which ) const
 */
HB_FUNC( QT_QWEBSETTINGS_FONTFAMILY )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->fontFamily( ( QWebSettings::FontFamily ) hb_parni( 2 ) ).toUtf8().data() );
   }
}

/*
 * int fontSize ( FontSize type ) const
 */
HB_FUNC( QT_QWEBSETTINGS_FONTSIZE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      hb_retni( ( p )->fontSize( ( QWebSettings::FontSize ) hb_parni( 2 ) ) );
   }
}

/*
 * void resetAttribute ( WebAttribute attribute )
 */
HB_FUNC( QT_QWEBSETTINGS_RESETATTRIBUTE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      ( p )->resetAttribute( ( QWebSettings::WebAttribute ) hb_parni( 2 ) );
   }
}

/*
 * void resetFontFamily ( FontFamily which )
 */
HB_FUNC( QT_QWEBSETTINGS_RESETFONTFAMILY )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      ( p )->resetFontFamily( ( QWebSettings::FontFamily ) hb_parni( 2 ) );
   }
}

/*
 * void resetFontSize ( FontSize type )
 */
HB_FUNC( QT_QWEBSETTINGS_RESETFONTSIZE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      ( p )->resetFontSize( ( QWebSettings::FontSize ) hb_parni( 2 ) );
   }
}

/*
 * void setAttribute ( WebAttribute attribute, bool on )
 */
HB_FUNC( QT_QWEBSETTINGS_SETATTRIBUTE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      ( p )->setAttribute( ( QWebSettings::WebAttribute ) hb_parni( 2 ), hb_parl( 3 ) );
   }
}

/*
 * void setFontFamily ( FontFamily which, const QString & family )
 */
HB_FUNC( QT_QWEBSETTINGS_SETFONTFAMILY )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      void * pText;
      ( p )->setFontFamily( ( QWebSettings::FontFamily ) hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setFontSize ( FontSize type, int size )
 */
HB_FUNC( QT_QWEBSETTINGS_SETFONTSIZE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      ( p )->setFontSize( ( QWebSettings::FontSize ) hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void setUserStyleSheetUrl ( const QUrl & location )
 */
HB_FUNC( QT_QWEBSETTINGS_SETUSERSTYLESHEETURL )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      ( p )->setUserStyleSheetUrl( *hbqt_par_QUrl( 2 ) );
   }
}

/*
 * bool testAttribute ( WebAttribute attribute ) const
 */
HB_FUNC( QT_QWEBSETTINGS_TESTATTRIBUTE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      hb_retl( ( p )->testAttribute( ( QWebSettings::WebAttribute ) hb_parni( 2 ) ) );
   }
}

/*
 * QUrl userStyleSheetUrl () const
 */
HB_FUNC( QT_QWEBSETTINGS_USERSTYLESHEETURL )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->userStyleSheetUrl() ), true ) );
   }
}

/*
 * void clearIconDatabase ()
 */
HB_FUNC( QT_QWEBSETTINGS_CLEARICONDATABASE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      ( p )->clearIconDatabase();
   }
}

/*
 * QWebSettings * globalSettings ()
 */
HB_FUNC( QT_QWEBSETTINGS_GLOBALSETTINGS )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWebSettings( ( p )->globalSettings(), false ) );
   }
}

/*
 * QString iconDatabasePath ()
 */
HB_FUNC( QT_QWEBSETTINGS_ICONDATABASEPATH )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->iconDatabasePath().toUtf8().data() );
   }
}

/*
 * QIcon iconForUrl ( const QUrl & url )
 */
HB_FUNC( QT_QWEBSETTINGS_ICONFORURL )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->iconForUrl( *hbqt_par_QUrl( 2 ) ) ), true ) );
   }
}

/*
 * int maximumPagesInCache ()
 */
HB_FUNC( QT_QWEBSETTINGS_MAXIMUMPAGESINCACHE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      hb_retni( ( p )->maximumPagesInCache() );
   }
}

/*
 * qint64 offlineStorageDefaultQuota ()
 */
HB_FUNC( QT_QWEBSETTINGS_OFFLINESTORAGEDEFAULTQUOTA )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      hb_retnint( ( p )->offlineStorageDefaultQuota() );
   }
}

/*
 * QString offlineStoragePath ()
 */
HB_FUNC( QT_QWEBSETTINGS_OFFLINESTORAGEPATH )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->offlineStoragePath().toUtf8().data() );
   }
}

/*
 * void setIconDatabasePath ( const QString & path )
 */
HB_FUNC( QT_QWEBSETTINGS_SETICONDATABASEPATH )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      void * pText;
      ( p )->setIconDatabasePath( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setMaximumPagesInCache ( int pages )
 */
HB_FUNC( QT_QWEBSETTINGS_SETMAXIMUMPAGESINCACHE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      ( p )->setMaximumPagesInCache( hb_parni( 2 ) );
   }
}

/*
 * void setObjectCacheCapacities ( int cacheMinDeadCapacity, int cacheMaxDead, int totalCapacity )
 */
HB_FUNC( QT_QWEBSETTINGS_SETOBJECTCACHECAPACITIES )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      ( p )->setObjectCacheCapacities( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
   }
}

/*
 * void setOfflineStorageDefaultQuota ( qint64 maximumSize )
 */
HB_FUNC( QT_QWEBSETTINGS_SETOFFLINESTORAGEDEFAULTQUOTA )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      ( p )->setOfflineStorageDefaultQuota( hb_parnint( 2 ) );
   }
}

/*
 * void setOfflineStoragePath ( const QString & path )
 */
HB_FUNC( QT_QWEBSETTINGS_SETOFFLINESTORAGEPATH )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      void * pText;
      ( p )->setOfflineStoragePath( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setWebGraphic ( WebGraphic type, const QPixmap & graphic )
 */
HB_FUNC( QT_QWEBSETTINGS_SETWEBGRAPHIC )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      ( p )->setWebGraphic( ( QWebSettings::WebGraphic ) hb_parni( 2 ), *hbqt_par_QPixmap( 3 ) );
   }
}

/*
 * QPixmap webGraphic ( WebGraphic type )
 */
HB_FUNC( QT_QWEBSETTINGS_WEBGRAPHIC )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->webGraphic( ( QWebSettings::WebGraphic ) hb_parni( 2 ) ) ), true ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
