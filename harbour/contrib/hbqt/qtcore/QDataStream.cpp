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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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

#include "hbapi.h"
#include "../hbqt.h"

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

QT_G_FUNC( hbqt_gcRelease_QDataStream )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QDataStream                  p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QDataStream                 ph=%p", p->ph ) );

   if( p && p->ph )
   {
      delete ( ( QDataStream * ) p->ph );
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "YES hbqt_gcRelease_QDataStream                 Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL hbqt_gcRelease_QDataStream                 Object Already deleted!" ) );
   }
}

void * hbqt_gcAllocate_QDataStream( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->func = hbqt_gcRelease_QDataStream;
   HB_TRACE( HB_TR_DEBUG, ( "          new_QDataStream                 %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return p;
}

HB_FUNC( QT_QDATASTREAM )
{
   void * pObj = NULL;

   pObj = new QDataStream() ;

   hb_retptrGC( hbqt_gcAllocate_QDataStream( pObj ) );
}
/*
 * bool atEnd () const
 */
HB_FUNC( QT_QDATASTREAM_ATEND )
{
   hb_retl( hbqt_par_QDataStream( 1 )->atEnd() );
}

/*
 * ByteOrder byteOrder () const
 */
HB_FUNC( QT_QDATASTREAM_BYTEORDER )
{
   hb_retni( ( QDataStream::ByteOrder ) hbqt_par_QDataStream( 1 )->byteOrder() );
}

/*
 * QIODevice * device () const
 */
HB_FUNC( QT_QDATASTREAM_DEVICE )
{
   hb_retptr( ( QIODevice* ) hbqt_par_QDataStream( 1 )->device() );
}

/*
 * int readRawData ( char * s, int len )
 */
HB_FUNC( QT_QDATASTREAM_READRAWDATA )
{
   hb_retni( hbqt_par_QDataStream( 1 )->readRawData( ( char * ) hb_parc( 2 ), hb_parni( 3 ) ) );
}

/*
 * void resetStatus ()
 */
HB_FUNC( QT_QDATASTREAM_RESETSTATUS )
{
   hbqt_par_QDataStream( 1 )->resetStatus();
}

/*
 * void setByteOrder ( ByteOrder bo )
 */
HB_FUNC( QT_QDATASTREAM_SETBYTEORDER )
{
   hbqt_par_QDataStream( 1 )->setByteOrder( ( QDataStream::ByteOrder ) hb_parni( 2 ) );
}

/*
 * void setDevice ( QIODevice * d )
 */
HB_FUNC( QT_QDATASTREAM_SETDEVICE )
{
   hbqt_par_QDataStream( 1 )->setDevice( hbqt_par_QIODevice( 2 ) );
}

/*
 * void setStatus ( Status status )
 */
HB_FUNC( QT_QDATASTREAM_SETSTATUS )
{
   hbqt_par_QDataStream( 1 )->setStatus( ( QDataStream::Status ) hb_parni( 2 ) );
}

/*
 * void setVersion ( int v )
 */
HB_FUNC( QT_QDATASTREAM_SETVERSION )
{
   hbqt_par_QDataStream( 1 )->setVersion( hb_parni( 2 ) );
}

/*
 * int skipRawData ( int len )
 */
HB_FUNC( QT_QDATASTREAM_SKIPRAWDATA )
{
   hb_retni( hbqt_par_QDataStream( 1 )->skipRawData( hb_parni( 2 ) ) );
}

/*
 * Status status () const
 */
HB_FUNC( QT_QDATASTREAM_STATUS )
{
   hb_retni( ( QDataStream::Status ) hbqt_par_QDataStream( 1 )->status() );
}

/*
 * int version () const
 */
HB_FUNC( QT_QDATASTREAM_VERSION )
{
   hb_retni( hbqt_par_QDataStream( 1 )->version() );
}

/*
 * int writeRawData ( const char * s, int len )
 */
HB_FUNC( QT_QDATASTREAM_WRITERAWDATA )
{
   hb_retni( hbqt_par_QDataStream( 1 )->writeRawData( hbqt_par_char( 2 ), hb_parni( 3 ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
