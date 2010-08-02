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

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum ByteOrder { BigEndian, LittleEndian }
 *  enum Status { Ok, ReadPastEnd, ReadCorruptData }
 *  enum Version { Qt_1_0, Qt_2_0, Qt_2_1, Qt_3_0, ..., Qt_4_5 }
 */

#include <QtCore/QPointer>

#include <QtCore/QDataStream>


/* QDataStream ()
 * QDataStream ( QIODevice * d )
 * QDataStream ( QByteArray * a, QIODevice::OpenMode mode )
 * QDataStream ( const QByteArray & a )
 * virtual ~QDataStream ()
 */

typedef struct
{
   QDataStream * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QDataStream;

QT_G_FUNC( hbqt_gcRelease_QDataStream )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QDataStream   /.\\", p->ph ) );
         delete ( ( QDataStream * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QDataStream   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QDataStream    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QDataStream    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QDataStream( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QDataStream * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDataStream;
   p->type = HBQT_TYPE_QDataStream;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QDataStream", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QDataStream", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QDATASTREAM )
{
   QDataStream * pObj = NULL;

   pObj = new QDataStream() ;

   hb_retptrGC( hbqt_gcAllocate_QDataStream( ( void * ) pObj, true ) );
}

/*
 * bool atEnd () const
 */
HB_FUNC( QT_QDATASTREAM_ATEND )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      hb_retl( ( p )->atEnd() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATASTREAM_ATEND FP=hb_retl( ( p )->atEnd() ); p is NULL" ) );
   }
}

/*
 * ByteOrder byteOrder () const
 */
HB_FUNC( QT_QDATASTREAM_BYTEORDER )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      hb_retni( ( QDataStream::ByteOrder ) ( p )->byteOrder() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATASTREAM_BYTEORDER FP=hb_retni( ( QDataStream::ByteOrder ) ( p )->byteOrder() ); p is NULL" ) );
   }
}

/*
 * QIODevice * device () const
 */
HB_FUNC( QT_QDATASTREAM_DEVICE )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIODevice( ( p )->device(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATASTREAM_DEVICE FP=hb_retptrGC( hbqt_gcAllocate_QIODevice( ( p )->device(), false ) ); p is NULL" ) );
   }
}

/*
 * int readRawData ( char * s, int len )
 */
HB_FUNC( QT_QDATASTREAM_READRAWDATA )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      hb_retni( ( p )->readRawData( ( char * ) hb_parc( 2 ), hb_parni( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATASTREAM_READRAWDATA FP=hb_retni( ( p )->readRawData( ( char * ) hb_parc( 2 ), hb_parni( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * void resetStatus ()
 */
HB_FUNC( QT_QDATASTREAM_RESETSTATUS )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      ( p )->resetStatus();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATASTREAM_RESETSTATUS FP=( p )->resetStatus(); p is NULL" ) );
   }
}

/*
 * void setByteOrder ( ByteOrder bo )
 */
HB_FUNC( QT_QDATASTREAM_SETBYTEORDER )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      ( p )->setByteOrder( ( QDataStream::ByteOrder ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATASTREAM_SETBYTEORDER FP=( p )->setByteOrder( ( QDataStream::ByteOrder ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDevice ( QIODevice * d )
 */
HB_FUNC( QT_QDATASTREAM_SETDEVICE )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      ( p )->setDevice( hbqt_par_QIODevice( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATASTREAM_SETDEVICE FP=( p )->setDevice( hbqt_par_QIODevice( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setStatus ( Status status )
 */
HB_FUNC( QT_QDATASTREAM_SETSTATUS )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      ( p )->setStatus( ( QDataStream::Status ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATASTREAM_SETSTATUS FP=( p )->setStatus( ( QDataStream::Status ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setVersion ( int v )
 */
HB_FUNC( QT_QDATASTREAM_SETVERSION )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      ( p )->setVersion( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATASTREAM_SETVERSION FP=( p )->setVersion( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * int skipRawData ( int len )
 */
HB_FUNC( QT_QDATASTREAM_SKIPRAWDATA )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      hb_retni( ( p )->skipRawData( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATASTREAM_SKIPRAWDATA FP=hb_retni( ( p )->skipRawData( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * Status status () const
 */
HB_FUNC( QT_QDATASTREAM_STATUS )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      hb_retni( ( QDataStream::Status ) ( p )->status() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATASTREAM_STATUS FP=hb_retni( ( QDataStream::Status ) ( p )->status() ); p is NULL" ) );
   }
}

/*
 * int version () const
 */
HB_FUNC( QT_QDATASTREAM_VERSION )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      hb_retni( ( p )->version() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATASTREAM_VERSION FP=hb_retni( ( p )->version() ); p is NULL" ) );
   }
}

/*
 * int writeRawData ( const char * s, int len )
 */
HB_FUNC( QT_QDATASTREAM_WRITERAWDATA )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      hb_retni( ( p )->writeRawData( hbqt_par_char( 2 ), hb_parni( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATASTREAM_WRITERAWDATA FP=hb_retni( ( p )->writeRawData( hbqt_par_char( 2 ), hb_parni( 3 ) ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
