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
 *  Constructed[ 20/20 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QTime>


/* QTime ()
 * QTime ( int h, int m, int s = 0, int ms = 0 )
 */

typedef struct
{
   QTime * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTime;

HBQT_GC_FUNC( hbqt_gcRelease_QTime )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

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
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

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
   {
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->addMSecs( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * QTime addSecs ( int s ) const
 */
HB_FUNC( QT_QTIME_ADDSECS )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->addSecs( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * int elapsed () const
 */
HB_FUNC( QT_QTIME_ELAPSED )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
   {
      hb_retni( ( p )->elapsed() );
   }
}

/*
 * int hour () const
 */
HB_FUNC( QT_QTIME_HOUR )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
   {
      hb_retni( ( p )->hour() );
   }
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QTIME_ISNULL )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
   {
      hb_retl( ( p )->isNull() );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QTIME_ISVALID )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
   {
      hb_retl( ( p )->isValid() );
   }
}

/*
 * int minute () const
 */
HB_FUNC( QT_QTIME_MINUTE )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
   {
      hb_retni( ( p )->minute() );
   }
}

/*
 * int msec () const
 */
HB_FUNC( QT_QTIME_MSEC )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
   {
      hb_retni( ( p )->msec() );
   }
}

/*
 * int msecsTo ( const QTime & t ) const
 */
HB_FUNC( QT_QTIME_MSECSTO )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
   {
      hb_retni( ( p )->msecsTo( *hbqt_par_QTime( 2 ) ) );
   }
}

/*
 * int restart ()
 */
HB_FUNC( QT_QTIME_RESTART )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
   {
      hb_retni( ( p )->restart() );
   }
}

/*
 * int second () const
 */
HB_FUNC( QT_QTIME_SECOND )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
   {
      hb_retni( ( p )->second() );
   }
}

/*
 * int secsTo ( const QTime & t ) const
 */
HB_FUNC( QT_QTIME_SECSTO )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
   {
      hb_retni( ( p )->secsTo( *hbqt_par_QTime( 2 ) ) );
   }
}

/*
 * bool setHMS ( int h, int m, int s, int ms = 0 )
 */
HB_FUNC( QT_QTIME_SETHMS )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
   {
      hb_retl( ( p )->setHMS( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ) );
   }
}

/*
 * void start ()
 */
HB_FUNC( QT_QTIME_START )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
   {
      ( p )->start();
   }
}

/*
 * QString toString ( const QString & format ) const
 */
HB_FUNC( QT_QTIME_TOSTRING )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->toString( hb_parstr_utf8( 2, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QString toString ( Qt::DateFormat format = Qt::TextDate ) const
 */
HB_FUNC( QT_QTIME_TOSTRING_1 )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->toString( ( HB_ISNUM( 2 ) ? ( Qt::DateFormat ) hb_parni( 2 ) : ( Qt::DateFormat ) Qt::TextDate ) ).toUtf8().data() );
   }
}

/*
 * QTime currentTime ()
 */
HB_FUNC( QT_QTIME_CURRENTTIME )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->currentTime() ), true ) );
   }
}

/*
 * QTime fromString ( const QString & string, Qt::DateFormat format = Qt::TextDate )
 */
HB_FUNC( QT_QTIME_FROMSTRING )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->fromString( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISNUM( 3 ) ? ( Qt::DateFormat ) hb_parni( 3 ) : ( Qt::DateFormat ) Qt::TextDate ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * QTime fromString ( const QString & string, const QString & format )
 */
HB_FUNC( QT_QTIME_FROMSTRING_1 )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->fromString( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * bool isValid ( int h, int m, int s, int ms = 0 )
 */
HB_FUNC( QT_QTIME_ISVALID_1 )
{
   QTime * p = hbqt_par_QTime( 1 );
   if( p )
   {
      hb_retl( ( p )->isValid( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
