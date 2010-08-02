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
#include "hbqtcore_garbage.h"
#include "hbqtcore.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtCore/QTime>


/* QTime ()
 * QTime ( int h, int m, int s = 0, int ms = 0 )
 */

typedef struct
{
   QTime * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QTime;

QT_G_FUNC( hbqt_gcRelease_QTime )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QTime   /.\\", p->ph ) );
         delete ( ( QTime * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QTime   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTime    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTime    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTime( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QTime * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTime;
   p->type = HBQT_TYPE_QTime;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTime", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTime", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTIME )
{
   QTime * pObj = NULL;

   pObj = new QTime() ;

   hb_retptrGC( hbqt_gcAllocate_QTime( ( void * ) pObj, true ) );
}

/*
 * QTime addMSecs ( int ms ) const
 */
HB_FUNC( QT_QTIME_ADDMSECS )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->addMSecs( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTIME_ADDMSECS FP=hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->addMSecs( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QTime addSecs ( int s ) const
 */
HB_FUNC( QT_QTIME_ADDSECS )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->addSecs( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTIME_ADDSECS FP=hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->addSecs( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * int elapsed () const
 */
HB_FUNC( QT_QTIME_ELAPSED )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retni( ( p )->elapsed() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTIME_ELAPSED FP=hb_retni( ( p )->elapsed() ); p is NULL" ) );
   }
}

/*
 * int hour () const
 */
HB_FUNC( QT_QTIME_HOUR )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retni( ( p )->hour() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTIME_HOUR FP=hb_retni( ( p )->hour() ); p is NULL" ) );
   }
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QTIME_ISNULL )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTIME_ISNULL FP=hb_retl( ( p )->isNull() ); p is NULL" ) );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QTIME_ISVALID )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTIME_ISVALID FP=hb_retl( ( p )->isValid() ); p is NULL" ) );
   }
}

/*
 * int minute () const
 */
HB_FUNC( QT_QTIME_MINUTE )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retni( ( p )->minute() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTIME_MINUTE FP=hb_retni( ( p )->minute() ); p is NULL" ) );
   }
}

/*
 * int msec () const
 */
HB_FUNC( QT_QTIME_MSEC )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retni( ( p )->msec() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTIME_MSEC FP=hb_retni( ( p )->msec() ); p is NULL" ) );
   }
}

/*
 * int msecsTo ( const QTime & t ) const
 */
HB_FUNC( QT_QTIME_MSECSTO )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retni( ( p )->msecsTo( *hbqt_par_QTime( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTIME_MSECSTO FP=hb_retni( ( p )->msecsTo( *hbqt_par_QTime( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int restart ()
 */
HB_FUNC( QT_QTIME_RESTART )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retni( ( p )->restart() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTIME_RESTART FP=hb_retni( ( p )->restart() ); p is NULL" ) );
   }
}

/*
 * int second () const
 */
HB_FUNC( QT_QTIME_SECOND )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retni( ( p )->second() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTIME_SECOND FP=hb_retni( ( p )->second() ); p is NULL" ) );
   }
}

/*
 * int secsTo ( const QTime & t ) const
 */
HB_FUNC( QT_QTIME_SECSTO )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retni( ( p )->secsTo( *hbqt_par_QTime( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTIME_SECSTO FP=hb_retni( ( p )->secsTo( *hbqt_par_QTime( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool setHMS ( int h, int m, int s, int ms = 0 )
 */
HB_FUNC( QT_QTIME_SETHMS )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retl( ( p )->setHMS( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTIME_SETHMS FP=hb_retl( ( p )->setHMS( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ) ); p is NULL" ) );
   }
}

/*
 * void start ()
 */
HB_FUNC( QT_QTIME_START )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      ( p )->start();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTIME_START FP=( p )->start(); p is NULL" ) );
   }
}

/*
 * QString toString ( const QString & format ) const
 */
HB_FUNC( QT_QTIME_TOSTRING )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retc( ( p )->toString( hbqt_par_QString( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTIME_TOSTRING FP=hb_retc( ( p )->toString( hbqt_par_QString( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString toString ( Qt::DateFormat format = Qt::TextDate ) const
 */
HB_FUNC( QT_QTIME_TOSTRING_1 )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retc( ( p )->toString( ( HB_ISNUM( 2 ) ? ( Qt::DateFormat ) hb_parni( 2 ) : ( Qt::DateFormat ) Qt::TextDate ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTIME_TOSTRING_1 FP=hb_retc( ( p )->toString( ( HB_ISNUM( 2 ) ? ( Qt::DateFormat ) hb_parni( 2 ) : ( Qt::DateFormat ) Qt::TextDate ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QTime currentTime ()
 */
HB_FUNC( QT_QTIME_CURRENTTIME )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->currentTime() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTIME_CURRENTTIME FP=hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->currentTime() ), true ) ); p is NULL" ) );
   }
}

/*
 * QTime fromString ( const QString & string, Qt::DateFormat format = Qt::TextDate )
 */
HB_FUNC( QT_QTIME_FROMSTRING )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->fromString( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::DateFormat ) hb_parni( 3 ) : ( Qt::DateFormat ) Qt::TextDate ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTIME_FROMSTRING FP=hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->fromString( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::DateFormat ) hb_parni( 3 ) : ( Qt::DateFormat ) Qt::TextDate ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QTime fromString ( const QString & string, const QString & format )
 */
HB_FUNC( QT_QTIME_FROMSTRING_1 )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->fromString( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTIME_FROMSTRING_1 FP=hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->fromString( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * bool isValid ( int h, int m, int s, int ms = 0 )
 */
HB_FUNC( QT_QTIME_ISVALID_1 )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
      hb_retl( ( p )->isValid( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTIME_ISVALID_1 FP=hb_retl( ( p )->isValid( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
