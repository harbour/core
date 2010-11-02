/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */

#include "hbqtcore.h"
#include "hbqtwebkit.h"

#if QT_VERSION >= 0x040500

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
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QWebSettings( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QWebSettings * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWebSettings;
   p->type = HBQT_TYPE_QWebSettings;

   return p;
}

HB_FUNC( QT_QWEBSETTINGS )
{
   //__HB_RETPTRGC__( new QWebSettings() );
}

/* QString fontFamily ( FontFamily which ) const */
HB_FUNC( QT_QWEBSETTINGS_FONTFAMILY )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      hb_retstr_utf8( ( p )->fontFamily( ( QWebSettings::FontFamily ) hb_parni( 2 ) ).toUtf8().data() );
}

/* int fontSize ( FontSize type ) const */
HB_FUNC( QT_QWEBSETTINGS_FONTSIZE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      hb_retni( ( p )->fontSize( ( QWebSettings::FontSize ) hb_parni( 2 ) ) );
}

/* void resetAttribute ( WebAttribute attribute ) */
HB_FUNC( QT_QWEBSETTINGS_RESETATTRIBUTE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->resetAttribute( ( QWebSettings::WebAttribute ) hb_parni( 2 ) );
}

/* void resetFontFamily ( FontFamily which ) */
HB_FUNC( QT_QWEBSETTINGS_RESETFONTFAMILY )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->resetFontFamily( ( QWebSettings::FontFamily ) hb_parni( 2 ) );
}

/* void resetFontSize ( FontSize type ) */
HB_FUNC( QT_QWEBSETTINGS_RESETFONTSIZE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->resetFontSize( ( QWebSettings::FontSize ) hb_parni( 2 ) );
}

/* void setAttribute ( WebAttribute attribute, bool on ) */
HB_FUNC( QT_QWEBSETTINGS_SETATTRIBUTE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->setAttribute( ( QWebSettings::WebAttribute ) hb_parni( 2 ), hb_parl( 3 ) );
}

/* void setFontFamily ( FontFamily which, const QString & family ) */
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

/* void setFontSize ( FontSize type, int size ) */
HB_FUNC( QT_QWEBSETTINGS_SETFONTSIZE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->setFontSize( ( QWebSettings::FontSize ) hb_parni( 2 ), hb_parni( 3 ) );
}

/* void setUserStyleSheetUrl ( const QUrl & location ) */
HB_FUNC( QT_QWEBSETTINGS_SETUSERSTYLESHEETURL )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->setUserStyleSheetUrl( *hbqt_par_QUrl( 2 ) );
}

/* bool testAttribute ( WebAttribute attribute ) const */
HB_FUNC( QT_QWEBSETTINGS_TESTATTRIBUTE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      hb_retl( ( p )->testAttribute( ( QWebSettings::WebAttribute ) hb_parni( 2 ) ) );
}

/* QUrl userStyleSheetUrl () const */
HB_FUNC( QT_QWEBSETTINGS_USERSTYLESHEETURL )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->userStyleSheetUrl() ), true ) );
}

/* void clearIconDatabase () */
HB_FUNC( QT_QWEBSETTINGS_CLEARICONDATABASE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->clearIconDatabase();
}

/* QWebSettings * globalSettings () */
HB_FUNC( QT_QWEBSETTINGS_GLOBALSETTINGS )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebSettings( ( p )->globalSettings(), false ) );
}

/* QString iconDatabasePath () */
HB_FUNC( QT_QWEBSETTINGS_ICONDATABASEPATH )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      hb_retstr_utf8( ( p )->iconDatabasePath().toUtf8().data() );
}

/* QIcon iconForUrl ( const QUrl & url ) */
HB_FUNC( QT_QWEBSETTINGS_ICONFORURL )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->iconForUrl( *hbqt_par_QUrl( 2 ) ) ), true ) );
}

/* int maximumPagesInCache () */
HB_FUNC( QT_QWEBSETTINGS_MAXIMUMPAGESINCACHE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      hb_retni( ( p )->maximumPagesInCache() );
}

/* qint64 offlineStorageDefaultQuota () */
HB_FUNC( QT_QWEBSETTINGS_OFFLINESTORAGEDEFAULTQUOTA )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      hb_retnint( ( p )->offlineStorageDefaultQuota() );
}

/* QString offlineStoragePath () */
HB_FUNC( QT_QWEBSETTINGS_OFFLINESTORAGEPATH )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      hb_retstr_utf8( ( p )->offlineStoragePath().toUtf8().data() );
}

/* void setIconDatabasePath ( const QString & path ) */
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

/* void setMaximumPagesInCache ( int pages ) */
HB_FUNC( QT_QWEBSETTINGS_SETMAXIMUMPAGESINCACHE )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->setMaximumPagesInCache( hb_parni( 2 ) );
}

/* void setObjectCacheCapacities ( int cacheMinDeadCapacity, int cacheMaxDead, int totalCapacity ) */
HB_FUNC( QT_QWEBSETTINGS_SETOBJECTCACHECAPACITIES )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->setObjectCacheCapacities( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/* void setOfflineStorageDefaultQuota ( qint64 maximumSize ) */
HB_FUNC( QT_QWEBSETTINGS_SETOFFLINESTORAGEDEFAULTQUOTA )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->setOfflineStorageDefaultQuota( hb_parnint( 2 ) );
}

/* void setOfflineStoragePath ( const QString & path ) */
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

/* void setWebGraphic ( WebGraphic type, const QPixmap & graphic ) */
HB_FUNC( QT_QWEBSETTINGS_SETWEBGRAPHIC )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      ( p )->setWebGraphic( ( QWebSettings::WebGraphic ) hb_parni( 2 ), *hbqt_par_QPixmap( 3 ) );
}

/* QPixmap webGraphic ( WebGraphic type ) */
HB_FUNC( QT_QWEBSETTINGS_WEBGRAPHIC )
{
   QWebSettings * p = hbqt_par_QWebSettings( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->webGraphic( ( QWebSettings::WebGraphic ) hb_parni( 2 ) ) ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
