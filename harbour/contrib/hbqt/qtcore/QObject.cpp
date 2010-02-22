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

#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 29/35 [ 82.86% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<QByteArray> dynamicPropertyNames () const
 *  QList<T> findChildren ( const QString & name = QString() ) const
 *  QList<T> findChildren ( const QRegExp & regExp ) const
 *  const QMetaObject staticMetaObject
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  // const QObjectList & children () const
 *  //T findChild ( const QString & name = QString() ) const
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
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   QPointer< QObject > pq;
} QGC_POINTER_QObject;

QT_G_FUNC( hbqt_gcRelease_QObject )
{
   QGC_POINTER_QObject * p = ( QGC_POINTER_QObject * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QObject   /.\\   ph=%p pq=%p", p->ph, (void *)(p->pq) ) );
            delete ( ( QObject * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QObject   \\./   ph=%p pq=%p", p->ph, (void *)(p->pq) ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "NO__rel_QObjectph=%p pq=%p", p->ph, (void *)(p->pq) ) );
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QObject    :     Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QObject    :    Object not created with new()" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QObject( void * pObj, bool bNew )
{
   QGC_POINTER_QObject * p = ( QGC_POINTER_QObject * ) hb_gcAllocate( sizeof( QGC_POINTER_QObject ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QObject;

   if( bNew )
   {
      new( & p->pq ) QPointer< QObject >( ( QObject * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QObject                    ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QOBJECT )
{
   void * pObj = NULL;

   pObj = ( QObject* ) new QObject( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QObject( pObj, true ) );
}

/*
 * bool blockSignals ( bool block )
 */
HB_FUNC( QT_QOBJECT_BLOCKSIGNALS )
{
   hb_retl( hbqt_par_QObject( 1 )->blockSignals( hb_parl( 2 ) ) );
}

/*
 * bool connect ( const QObject * sender, const char * signal, const char * method, Qt::ConnectionType type = Qt::AutoConnection ) const
 */
HB_FUNC( QT_QOBJECT_CONNECT )
{
   hb_retl( hbqt_par_QObject( 1 )->connect( hbqt_par_QObject( 2 ), hbqt_par_char( 3 ), hbqt_par_char( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::ConnectionType ) hb_parni( 5 ) : ( Qt::ConnectionType ) Qt::AutoConnection ) ) );
}

/*
 * bool disconnect ( const char * signal = 0, const QObject * receiver = 0, const char * method = 0 )
 */
HB_FUNC( QT_QOBJECT_DISCONNECT )
{
   hb_retl( hbqt_par_QObject( 1 )->disconnect( hbqt_par_char( 2 ), hbqt_par_QObject( 3 ), hbqt_par_char( 4 ) ) );
}

/*
 * bool disconnect ( const QObject * receiver, const char * method = 0 )
 */
HB_FUNC( QT_QOBJECT_DISCONNECT_1 )
{
   hb_retl( hbqt_par_QObject( 1 )->disconnect( hbqt_par_QObject( 2 ), hbqt_par_char( 3 ) ) );
}

/*
 * void dumpObjectInfo ()
 */
HB_FUNC( QT_QOBJECT_DUMPOBJECTINFO )
{
   hbqt_par_QObject( 1 )->dumpObjectInfo();
}

/*
 * void dumpObjectTree ()
 */
HB_FUNC( QT_QOBJECT_DUMPOBJECTTREE )
{
   hbqt_par_QObject( 1 )->dumpObjectTree();
}

/*
 * virtual bool event ( QEvent * e )
 */
HB_FUNC( QT_QOBJECT_EVENT )
{
   hb_retl( hbqt_par_QObject( 1 )->event( hbqt_par_QEvent( 2 ) ) );
}

/*
 * virtual bool eventFilter ( QObject * watched, QEvent * event )
 */
HB_FUNC( QT_QOBJECT_EVENTFILTER )
{
   hb_retl( hbqt_par_QObject( 1 )->eventFilter( hbqt_par_QObject( 2 ), hbqt_par_QEvent( 3 ) ) );
}

/*
 * bool inherits ( const char * className ) const
 */
HB_FUNC( QT_QOBJECT_INHERITS )
{
   hb_retl( hbqt_par_QObject( 1 )->inherits( hbqt_par_char( 2 ) ) );
}

/*
 * void installEventFilter ( QObject * filterObj )
 */
HB_FUNC( QT_QOBJECT_INSTALLEVENTFILTER )
{
   hbqt_par_QObject( 1 )->installEventFilter( hbqt_par_QObject( 2 ) );
}

/*
 * bool isWidgetType () const
 */
HB_FUNC( QT_QOBJECT_ISWIDGETTYPE )
{
   hb_retl( hbqt_par_QObject( 1 )->isWidgetType() );
}

/*
 * void killTimer ( int id )
 */
HB_FUNC( QT_QOBJECT_KILLTIMER )
{
   hbqt_par_QObject( 1 )->killTimer( hb_parni( 2 ) );
}

/*
 * virtual const QMetaObject * metaObject () const
 */
HB_FUNC( QT_QOBJECT_METAOBJECT )
{
   hb_retptr( ( QMetaObject* ) hbqt_par_QObject( 1 )->metaObject() );
}

/*
 * void moveToThread ( QThread * targetThread )
 */
HB_FUNC( QT_QOBJECT_MOVETOTHREAD )
{
   hbqt_par_QObject( 1 )->moveToThread( hbqt_par_QThread( 2 ) );
}

/*
 * QString objectName () const
 */
HB_FUNC( QT_QOBJECT_OBJECTNAME )
{
   hb_retc( hbqt_par_QObject( 1 )->objectName().toAscii().data() );
}

/*
 * QObject * parent () const
 */
HB_FUNC( QT_QOBJECT_PARENT )
{
   hb_retptrGC( hbqt_gcAllocate_QObject( hbqt_par_QObject( 1 )->parent(), false ) );
}

/*
 * QVariant property ( const char * name ) const
 */
HB_FUNC( QT_QOBJECT_PROPERTY )
{
   hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( hbqt_par_QObject( 1 )->property( hbqt_par_char( 2 ) ) ), true ) );
}

/*
 * void removeEventFilter ( QObject * obj )
 */
HB_FUNC( QT_QOBJECT_REMOVEEVENTFILTER )
{
   hbqt_par_QObject( 1 )->removeEventFilter( hbqt_par_QObject( 2 ) );
}

/*
 * void setObjectName ( const QString & name )
 */
HB_FUNC( QT_QOBJECT_SETOBJECTNAME )
{
   hbqt_par_QObject( 1 )->setObjectName( QObject::tr( hb_parc( 2 ) ) );
}

/*
 * void setParent ( QObject * parent )
 */
HB_FUNC( QT_QOBJECT_SETPARENT )
{
   hbqt_par_QObject( 1 )->setParent( hbqt_par_QObject( 2 ) );
}

/*
 * bool setProperty ( const char * name, const QVariant & value )
 */
HB_FUNC( QT_QOBJECT_SETPROPERTY )
{
   hb_retl( hbqt_par_QObject( 1 )->setProperty( hbqt_par_char( 2 ), *hbqt_par_QVariant( 3 ) ) );
}

/*
 * bool signalsBlocked () const
 */
HB_FUNC( QT_QOBJECT_SIGNALSBLOCKED )
{
   hb_retl( hbqt_par_QObject( 1 )->signalsBlocked() );
}

/*
 * int startTimer ( int interval )
 */
HB_FUNC( QT_QOBJECT_STARTTIMER )
{
   hb_retni( hbqt_par_QObject( 1 )->startTimer( hb_parni( 2 ) ) );
}

/*
 * QThread * thread () const
 */
HB_FUNC( QT_QOBJECT_THREAD )
{
   hb_retptrGC( hbqt_gcAllocate_QThread( hbqt_par_QObject( 1 )->thread(), false ) );
}

/*
 * bool connect ( const QObject * sender, const char * signal, const QObject * receiver, const char * method, Qt::ConnectionType type = Qt::AutoConnection )
 */
HB_FUNC( QT_QOBJECT_CONNECT_1 )
{
   hb_retl( hbqt_par_QObject( 1 )->connect( hbqt_par_QObject( 2 ), hbqt_par_char( 3 ), hbqt_par_QObject( 4 ), hbqt_par_char( 5 ), ( HB_ISNUM( 6 ) ? ( Qt::ConnectionType ) hb_parni( 6 ) : ( Qt::ConnectionType ) Qt::AutoConnection ) ) );
}

/*
 * bool disconnect ( const QObject * sender, const char * signal, const QObject * receiver, const char * method )
 */
HB_FUNC( QT_QOBJECT_DISCONNECT_2 )
{
   hb_retl( hbqt_par_QObject( 1 )->disconnect( hbqt_par_QObject( 2 ), hbqt_par_char( 3 ), hbqt_par_QObject( 4 ), hbqt_par_char( 5 ) ) );
}

/*
 * QString tr ( const char * sourceText, const char * disambiguation = 0, int n = -1 )
 */
HB_FUNC( QT_QOBJECT_TR )
{
   hb_retc( hbqt_par_QObject( 1 )->tr( hbqt_par_char( 2 ), hbqt_par_char( 3 ), ( HB_ISNUM( 4 ) ? hb_parni( 4 ) : -1 ) ).toAscii().data() );
}

/*
 * QString trUtf8 ( const char * sourceText, const char * disambiguation = 0, int n = -1 )
 */
HB_FUNC( QT_QOBJECT_TRUTF8 )
{
   hb_retc( hbqt_par_QObject( 1 )->trUtf8( hbqt_par_char( 2 ), hbqt_par_char( 3 ), ( HB_ISNUM( 4 ) ? hb_parni( 4 ) : -1 ) ).toAscii().data() );
}

/*
 * void deleteLater ()
 */
HB_FUNC( QT_QOBJECT_DELETELATER )
{
   hbqt_par_QObject( 1 )->deleteLater();
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
