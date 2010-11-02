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
#include "hbqtuitools.h"

#if QT_VERSION >= 0x040500

/*
 *  Constructed[ 14/14 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include "hbqtgui.h"

#include <QtUiTools/QUiLoader>
#include <QtCore/QDir>
#include <QtCore/QStringList>

/*
 * QUiLoader ( QObject * parent = 0 )
 * virtual ~QUiLoader ()
 */

typedef struct
{
   QPointer< QUiLoader > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QUiLoader;

HBQT_GC_FUNC( hbqt_gcRelease_QUiLoader )
{
   HBQT_GC_T_QUiLoader * p = ( HBQT_GC_T_QUiLoader * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QUiLoader * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QUiLoader( void * pObj, bool bNew )
{
   HBQT_GC_T_QUiLoader * p = ( HBQT_GC_T_QUiLoader * ) hb_gcAllocate( sizeof( HBQT_GC_T_QUiLoader ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QUiLoader >( ( QUiLoader * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QUiLoader;
   p->type = HBQT_TYPE_QUiLoader;

   return p;
}

HB_FUNC( QT_QUILOADER )
{
   QUiLoader * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QUiLoader( hbqt_par_QObject( 1 ) ) ;
   }
   else
   {
      pObj = new QUiLoader() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QUiLoader( ( void * ) pObj, true ) );
}

/* void addPluginPath ( const QString & path ) */
HB_FUNC( QT_QUILOADER_ADDPLUGINPATH )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
   {
      void * pText;
      ( p )->addPluginPath( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* QStringList availableLayouts () const */
HB_FUNC( QT_QUILOADER_AVAILABLELAYOUTS )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->availableLayouts() ), true ) );
}

/* QStringList availableWidgets () const */
HB_FUNC( QT_QUILOADER_AVAILABLEWIDGETS )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->availableWidgets() ), true ) );
}

/* void clearPluginPaths () */
HB_FUNC( QT_QUILOADER_CLEARPLUGINPATHS )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
      ( p )->clearPluginPaths();
}

/* virtual QAction * createAction ( QObject * parent = 0, const QString & name = QString() ) */
HB_FUNC( QT_QUILOADER_CREATEACTION )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->createAction( hbqt_par_QObject( 2 ), hb_parstr_utf8( 3, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/* virtual QActionGroup * createActionGroup ( QObject * parent = 0, const QString & name = QString() ) */
HB_FUNC( QT_QUILOADER_CREATEACTIONGROUP )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QActionGroup( ( p )->createActionGroup( hbqt_par_QObject( 2 ), hb_parstr_utf8( 3, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/* virtual QLayout * createLayout ( const QString & className, QObject * parent = 0, const QString & name = QString() ) */
HB_FUNC( QT_QUILOADER_CREATELAYOUT )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QLayout( ( p )->createLayout( hb_parstr_utf8( 2, &pText, NULL ), hbqt_par_QObject( 3 ), hb_parstr_utf8( 4, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/* virtual QWidget * createWidget ( const QString & className, QWidget * parent = 0, const QString & name = QString() ) */
HB_FUNC( QT_QUILOADER_CREATEWIDGET )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->createWidget( hb_parstr_utf8( 2, &pText, NULL ), hbqt_par_QWidget( 3 ), hb_parstr_utf8( 4, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/* bool isLanguageChangeEnabled () const */
HB_FUNC( QT_QUILOADER_ISLANGUAGECHANGEENABLED )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
      hb_retl( ( p )->isLanguageChangeEnabled() );
}

/* QWidget * load ( QIODevice * device, QWidget * parentWidget = 0 ) */
HB_FUNC( QT_QUILOADER_LOAD )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->load( hbqt_par_QIODevice( 2 ), hbqt_par_QWidget( 3 ) ), false ) );
}

/* QStringList pluginPaths () const */
HB_FUNC( QT_QUILOADER_PLUGINPATHS )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->pluginPaths() ), true ) );
}

/* void setLanguageChangeEnabled ( bool enabled ) */
HB_FUNC( QT_QUILOADER_SETLANGUAGECHANGEENABLED )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
      ( p )->setLanguageChangeEnabled( hb_parl( 2 ) );
}

/* void setWorkingDirectory ( const QDir & dir ) */
HB_FUNC( QT_QUILOADER_SETWORKINGDIRECTORY )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
      ( p )->setWorkingDirectory( *hbqt_par_QDir( 2 ) );
}

/* QDir workingDirectory () const */
HB_FUNC( QT_QUILOADER_WORKINGDIRECTORY )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->workingDirectory() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
