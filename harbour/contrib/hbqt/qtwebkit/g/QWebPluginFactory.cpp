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
 *  class ExtensionOption
 *  enum Extension { }
 */

/*
 *  Constructed[ 3/3 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // virtual bool extension ( Extension extension, const ExtensionOption * option = 0, ExtensionReturn * output = 0 )
 *  // virtual QList<Plugin> plugins () const = 0
 */

#include <QtCore/QPointer>

#include "hbqtgui.h"

#include <QtWebKit/QWebPluginFactory>


/*
 * QWebPluginFactory ( QObject * parent = 0 )
 * virtual ~QWebPluginFactory ()
 */

typedef struct
{
   QPointer< QWebPluginFactory > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QWebPluginFactory;

HBQT_GC_FUNC( hbqt_gcRelease_QWebPluginFactory )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QWebPluginFactory( void * pObj, bool bNew )
{
   HBQT_GC_T_QWebPluginFactory * p = ( HBQT_GC_T_QWebPluginFactory * ) hb_gcAllocate( sizeof( HBQT_GC_T_QWebPluginFactory ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QWebPluginFactory >( ( QWebPluginFactory * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWebPluginFactory;
   p->type = HBQT_TYPE_QWebPluginFactory;

   return p;
}

HB_FUNC( QT_QWEBPLUGINFACTORY )
{
   //__HB_RETPTRGC__( new QWebPluginFactory() );
}

/* virtual QObject * create ( const QString & mimeType, const QUrl & url, const QStringList & argumentNames, const QStringList & argumentValues ) const = 0 */
HB_FUNC( QT_QWEBPLUGINFACTORY_CREATE )
{
   QWebPluginFactory * p = hbqt_par_QWebPluginFactory( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QObject( ( p )->create( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QUrl( 3 ), *hbqt_par_QStringList( 4 ), *hbqt_par_QStringList( 5 ) ), false ) );
      hb_strfree( pText );
   }
}

/* virtual void refreshPlugins () */
HB_FUNC( QT_QWEBPLUGINFACTORY_REFRESHPLUGINS )
{
   QWebPluginFactory * p = hbqt_par_QWebPluginFactory( 1 );
   if( p )
      ( p )->refreshPlugins();
}

/* virtual bool supportsExtension ( Extension extension ) const */
HB_FUNC( QT_QWEBPLUGINFACTORY_SUPPORTSEXTENSION )
{
   QWebPluginFactory * p = hbqt_par_QWebPluginFactory( 1 );
   if( p )
      hb_retl( ( p )->supportsExtension( ( QWebPluginFactory::Extension ) hb_parni( 2 ) ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
