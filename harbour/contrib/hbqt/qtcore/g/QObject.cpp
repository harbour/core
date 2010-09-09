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

#include "hbqtcore.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 24/35 [ 68.57% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  const QMetaObject staticMetaObject
 *
 *  *** Commented out protos which construct fine but do not compile ***
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
 * Q_INVOKABLE QObject ( QObject * parent = 0 )
 * virtual ~QObject ()
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
   QObject  * ph = NULL ;
   HBQT_GC_T_QObject * p = ( HBQT_GC_T_QObject * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QObject   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QObject   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QObject          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QObject    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QObject    :    Object not created with new=true", ph ) );
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

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QObject  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QObject", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QOBJECT )
{
   QObject * pObj = NULL;

   pObj =  new QObject( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QObject( ( void * ) pObj, true ) );
}

/*
 * bool blockSignals ( bool block )
 */
HB_FUNC( QT_QOBJECT_BLOCKSIGNALS )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retl( ( p )->blockSignals( hb_parl( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_BLOCKSIGNALS FP=hb_retl( ( p )->blockSignals( hb_parl( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void dumpObjectInfo ()
 */
HB_FUNC( QT_QOBJECT_DUMPOBJECTINFO )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      ( p )->dumpObjectInfo();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_DUMPOBJECTINFO FP=( p )->dumpObjectInfo(); p is NULL" ) );
   }
}

/*
 * void dumpObjectTree ()
 */
HB_FUNC( QT_QOBJECT_DUMPOBJECTTREE )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      ( p )->dumpObjectTree();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_DUMPOBJECTTREE FP=( p )->dumpObjectTree(); p is NULL" ) );
   }
}

/*
 * QList<QByteArray> dynamicPropertyNames () const
 */
HB_FUNC( QT_QOBJECT_DYNAMICPROPERTYNAMES )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QByteArray>( ( p )->dynamicPropertyNames() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_DYNAMICPROPERTYNAMES FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QByteArray>( ( p )->dynamicPropertyNames() ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual bool event ( QEvent * e )
 */
HB_FUNC( QT_QOBJECT_EVENT )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retl( ( p )->event( hbqt_par_QEvent( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_EVENT FP=hb_retl( ( p )->event( hbqt_par_QEvent( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual bool eventFilter ( QObject * watched, QEvent * event )
 */
HB_FUNC( QT_QOBJECT_EVENTFILTER )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retl( ( p )->eventFilter( hbqt_par_QObject( 2 ), hbqt_par_QEvent( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_EVENTFILTER FP=hb_retl( ( p )->eventFilter( hbqt_par_QObject( 2 ), hbqt_par_QEvent( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * bool inherits ( const char * className ) const
 */
HB_FUNC( QT_QOBJECT_INHERITS )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retl( ( p )->inherits( hbqt_par_char( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_INHERITS FP=hb_retl( ( p )->inherits( hbqt_par_char( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void installEventFilter ( QObject * filterObj )
 */
HB_FUNC( QT_QOBJECT_INSTALLEVENTFILTER )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      ( p )->installEventFilter( hbqt_par_QObject( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_INSTALLEVENTFILTER FP=( p )->installEventFilter( hbqt_par_QObject( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool isWidgetType () const
 */
HB_FUNC( QT_QOBJECT_ISWIDGETTYPE )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retl( ( p )->isWidgetType() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_ISWIDGETTYPE FP=hb_retl( ( p )->isWidgetType() ); p is NULL" ) );
   }
}

/*
 * void killTimer ( int id )
 */
HB_FUNC( QT_QOBJECT_KILLTIMER )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      ( p )->killTimer( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_KILLTIMER FP=( p )->killTimer( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void moveToThread ( QThread * targetThread )
 */
HB_FUNC( QT_QOBJECT_MOVETOTHREAD )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      ( p )->moveToThread( hbqt_par_QThread( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_MOVETOTHREAD FP=( p )->moveToThread( hbqt_par_QThread( 2 ) ); p is NULL" ) );
   }
}

/*
 * QString objectName () const
 */
HB_FUNC( QT_QOBJECT_OBJECTNAME )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retc( ( p )->objectName().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_OBJECTNAME FP=hb_retc( ( p )->objectName().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QObject * parent () const
 */
HB_FUNC( QT_QOBJECT_PARENT )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QObject( ( p )->parent(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_PARENT FP=hb_retptrGC( hbqt_gcAllocate_QObject( ( p )->parent(), false ) ); p is NULL" ) );
   }
}

/*
 * QVariant property ( const char * name ) const
 */
HB_FUNC( QT_QOBJECT_PROPERTY )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->property( hbqt_par_char( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_PROPERTY FP=hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->property( hbqt_par_char( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void removeEventFilter ( QObject * obj )
 */
HB_FUNC( QT_QOBJECT_REMOVEEVENTFILTER )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      ( p )->removeEventFilter( hbqt_par_QObject( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_REMOVEEVENTFILTER FP=( p )->removeEventFilter( hbqt_par_QObject( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setObjectName ( const QString & name )
 */
HB_FUNC( QT_QOBJECT_SETOBJECTNAME )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      ( p )->setObjectName( QObject::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_SETOBJECTNAME FP=( p )->setObjectName( QObject::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setParent ( QObject * parent )
 */
HB_FUNC( QT_QOBJECT_SETPARENT )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      ( p )->setParent( hbqt_par_QObject( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_SETPARENT FP=( p )->setParent( hbqt_par_QObject( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool setProperty ( const char * name, const QVariant & value )
 */
HB_FUNC( QT_QOBJECT_SETPROPERTY )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retl( ( p )->setProperty( hbqt_par_char( 2 ), *hbqt_par_QVariant( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_SETPROPERTY FP=hb_retl( ( p )->setProperty( hbqt_par_char( 2 ), *hbqt_par_QVariant( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * bool signalsBlocked () const
 */
HB_FUNC( QT_QOBJECT_SIGNALSBLOCKED )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retl( ( p )->signalsBlocked() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_SIGNALSBLOCKED FP=hb_retl( ( p )->signalsBlocked() ); p is NULL" ) );
   }
}

/*
 * int startTimer ( int interval )
 */
HB_FUNC( QT_QOBJECT_STARTTIMER )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retni( ( p )->startTimer( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_STARTTIMER FP=hb_retni( ( p )->startTimer( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QThread * thread () const
 */
HB_FUNC( QT_QOBJECT_THREAD )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QThread( ( p )->thread(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_THREAD FP=hb_retptrGC( hbqt_gcAllocate_QThread( ( p )->thread(), false ) ); p is NULL" ) );
   }
}

/*
 * QString tr ( const char * sourceText, const char * disambiguation = 0, int n = -1 )
 */
HB_FUNC( QT_QOBJECT_TR )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retc( ( p )->tr( hbqt_par_char( 2 ), hbqt_par_char( 3 ), hb_parnidef( 4, -1 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_TR FP=hb_retc( ( p )->tr( hbqt_par_char( 2 ), hbqt_par_char( 3 ), hb_parnidef( 4, -1 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString trUtf8 ( const char * sourceText, const char * disambiguation = 0, int n = -1 )
 */
HB_FUNC( QT_QOBJECT_TRUTF8 )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      hb_retc( ( p )->trUtf8( hbqt_par_char( 2 ), hbqt_par_char( 3 ), hb_parnidef( 4, -1 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_TRUTF8 FP=hb_retc( ( p )->trUtf8( hbqt_par_char( 2 ), hbqt_par_char( 3 ), hb_parnidef( 4, -1 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void deleteLater ()
 */
HB_FUNC( QT_QOBJECT_DELETELATER )
{
   QObject * p = hbqt_par_QObject( 1 );
   if( p )
      ( p )->deleteLater();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QOBJECT_DELETELATER FP=( p )->deleteLater(); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
