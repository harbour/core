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

#if QT_VERSION >= 0x040500

/*
 *  Constructed[ 24/25 [ 96.00% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  const QMetaObject staticMetaObject
 *
 *  *** Commented out protostypes ***
 *
 *  // const QObjectList & children () const
 *  // bool connect ( const QObject * sender, const char * signal, const char * method, Qt::ConnectionType type = Qt::AutoConnection ) const
 *  // bool disconnect ( const char * signal = 0, const QObject * receiver = 0, const char * method = 0 )
 *  // bool disconnect ( const QObject * receiver, const char * method = 0 )
 *  //T findChild ( const QString & name = QString() ) const
 *  //QList<T> findChildren ( const QString & name = QString() ) const
 *  //QList<T> findChildren ( const QRegExp & regExp ) const
 *  //virtual const QMetaObject * metaObject () const
 *  // bool connect ( const QObject * sender, const char * signal, const QObject * receiver, const char * method, Qt::ConnectionType type = Qt::AutoConnection )
 *  // bool disconnect ( const QObject * sender, const char * signal, const QObject * receiver, const char * method )
 */

#include <QtCore/QPointer>

#include <QtCore/QObject>
#include <QtCore/QVariant>

/*
 * QObject ( QObject * parent = 0 )
 * ~QObject ()
 */

typedef struct
{
   QPointer< QObject > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QObject;

HBQT_GC_FUNC( hbqt_gcRelease_QObject )
{
   HBQT_GC_T_QObject * p = ( HBQT_GC_T_QObject * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QObject * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QObject( void * pObj, bool bNew )
{
   HBQT_GC_T_QObject * p = ( HBQT_GC_T_QObject * ) hb_gcAllocate( sizeof( HBQT_GC_T_QObject ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QObject >( ( QObject * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QObject;
   p->type = HBQT_TYPE_QObject;

   return p;
}

HB_FUNC( QT_QOBJECT )
{
   QObject * pObj = NULL;

   pObj = new QObject( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QObject( ( void * ) pObj, true ) );
}

/* bool blockSignals ( bool block ) */
HB_FUNC( QT_QOBJECT_BLOCKSIGNALS )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retl( ( p )->blockSignals( hb_parl( 2 ) ) );
}

/* void dumpObjectInfo () */
HB_FUNC( QT_QOBJECT_DUMPOBJECTINFO )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      ( p )->dumpObjectInfo();
}

/* void dumpObjectTree () */
HB_FUNC( QT_QOBJECT_DUMPOBJECTTREE )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      ( p )->dumpObjectTree();
}

/* QList<QByteArray> dynamicPropertyNames () const */
HB_FUNC( QT_QOBJECT_DYNAMICPROPERTYNAMES )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QByteArray>( ( p )->dynamicPropertyNames() ), true ) );
}

/* virtual bool event ( QEvent * e ) */
HB_FUNC( QT_QOBJECT_EVENT )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retl( ( p )->event( hbqt_par_QEvent( 2 ) ) );
}

/* virtual bool eventFilter ( QObject * watched, QEvent * event ) */
HB_FUNC( QT_QOBJECT_EVENTFILTER )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retl( ( p )->eventFilter( hbqt_par_QObject( 2 ), hbqt_par_QEvent( 3 ) ) );
}

/* bool inherits ( const char * className ) const */
HB_FUNC( QT_QOBJECT_INHERITS )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retl( ( p )->inherits( ( const char * ) hb_parc( 2 ) ) );
}

/* void installEventFilter ( QObject * filterObj ) */
HB_FUNC( QT_QOBJECT_INSTALLEVENTFILTER )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      ( p )->installEventFilter( hbqt_par_QObject( 2 ) );
}

/* bool isWidgetType () const */
HB_FUNC( QT_QOBJECT_ISWIDGETTYPE )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retl( ( p )->isWidgetType() );
}

/* void killTimer ( int id ) */
HB_FUNC( QT_QOBJECT_KILLTIMER )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      ( p )->killTimer( hb_parni( 2 ) );
}

/* void moveToThread ( QThread * targetThread ) */
HB_FUNC( QT_QOBJECT_MOVETOTHREAD )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      ( p )->moveToThread( hbqt_par_QThread( 2 ) );
}

/* QString objectName () const */
HB_FUNC( QT_QOBJECT_OBJECTNAME )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retstr_utf8( ( p )->objectName().toUtf8().data() );
}

/* QObject * parent () const */
HB_FUNC( QT_QOBJECT_PARENT )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QObject( ( p )->parent(), false ) );
}

/* QVariant property ( const char * name ) const */
HB_FUNC( QT_QOBJECT_PROPERTY )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->property( ( const char * ) hb_parc( 2 ) ) ), true ) );
}

/* void removeEventFilter ( QObject * obj ) */
HB_FUNC( QT_QOBJECT_REMOVEEVENTFILTER )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      ( p )->removeEventFilter( hbqt_par_QObject( 2 ) );
}

/* void setObjectName ( const QString & name ) */
HB_FUNC( QT_QOBJECT_SETOBJECTNAME )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
   {
      void * pText;
      ( p )->setObjectName( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setParent ( QObject * parent ) */
HB_FUNC( QT_QOBJECT_SETPARENT )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      ( p )->setParent( hbqt_par_QObject( 2 ) );
}

/* bool setProperty ( const char * name, const QVariant & value ) */
HB_FUNC( QT_QOBJECT_SETPROPERTY )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retl( ( p )->setProperty( ( const char * ) hb_parc( 2 ), *hbqt_par_QVariant( 3 ) ) );
}

/* bool signalsBlocked () const */
HB_FUNC( QT_QOBJECT_SIGNALSBLOCKED )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retl( ( p )->signalsBlocked() );
}

/* int startTimer ( int interval ) */
HB_FUNC( QT_QOBJECT_STARTTIMER )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retni( ( p )->startTimer( hb_parni( 2 ) ) );
}

/* QThread * thread () const */
HB_FUNC( QT_QOBJECT_THREAD )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QThread( ( p )->thread(), false ) );
}

/* QString tr ( const char * sourceText, const char * disambiguation = 0, int n = -1 ) */
HB_FUNC( QT_QOBJECT_TR )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retstr_utf8( ( p )->tr( ( const char * ) hb_parc( 2 ), ( const char * ) hb_parc( 3 ), hb_parnidef( 4, -1 ) ).toUtf8().data() );
}

/* QString trUtf8 ( const char * sourceText, const char * disambiguation = 0, int n = -1 ) */
HB_FUNC( QT_QOBJECT_TRUTF8 )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retstr_utf8( ( p )->trUtf8( ( const char * ) hb_parc( 2 ), ( const char * ) hb_parc( 3 ), hb_parnidef( 4, -1 ) ).toUtf8().data() );
}

/* void deleteLater () */
HB_FUNC( QT_QOBJECT_DELETELATER )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      ( p )->deleteLater();
}


#endif /* #if QT_VERSION >= 0x040500 */
