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

/*
 *  flags OpenMode
 *  enum OpenModeFlag { NotOpen, ReadOnly, WriteOnly, ReadWrite, ..., Unbuffered }
 */

#include <QtCore/QPointer>

#include <QtCore/QIODevice>


/*
 * QIODevice ()
 * QIODevice ( QObject * parent )
 * virtual ~QIODevice ()
 */

typedef struct
{
   QPointer< QIODevice > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QIODevice;

QT_G_FUNC( hbqt_gcRelease_QIODevice )
{
   HB_SYMBOL_UNUSED( Cargo );
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QIODevice( void * pObj, bool bNew )
{
   QGC_POINTER_QIODevice * p = ( QGC_POINTER_QIODevice * ) hb_gcAllocate( sizeof( QGC_POINTER_QIODevice ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QIODevice >( ( QIODevice * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QIODevice;
   p->type = HBQT_TYPE_QIODevice;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QIODevice  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QIODevice", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QIODEVICE )
{
   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      // hb_retptr( new QIODevice( hbqt_par_QObject( 1 ) ) );
   }
}

/*
 * virtual bool atEnd () const
 */
HB_FUNC( QT_QIODEVICE_ATEND )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->atEnd() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_ATEND FP=hb_retl( ( p )->atEnd() ); p is NULL" ) );
   }
}

/*
 * virtual qint64 bytesAvailable () const
 */
HB_FUNC( QT_QIODEVICE_BYTESAVAILABLE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retnint( ( p )->bytesAvailable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_BYTESAVAILABLE FP=hb_retnint( ( p )->bytesAvailable() ); p is NULL" ) );
   }
}

/*
 * virtual qint64 bytesToWrite () const
 */
HB_FUNC( QT_QIODEVICE_BYTESTOWRITE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retnint( ( p )->bytesToWrite() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_BYTESTOWRITE FP=hb_retnint( ( p )->bytesToWrite() ); p is NULL" ) );
   }
}

/*
 * virtual bool canReadLine () const
 */
HB_FUNC( QT_QIODEVICE_CANREADLINE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->canReadLine() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_CANREADLINE FP=hb_retl( ( p )->canReadLine() ); p is NULL" ) );
   }
}

/*
 * virtual void close ()
 */
HB_FUNC( QT_QIODEVICE_CLOSE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      ( p )->close();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_CLOSE FP=( p )->close(); p is NULL" ) );
   }
}

/*
 * QString errorString () const
 */
HB_FUNC( QT_QIODEVICE_ERRORSTRING )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retc( ( p )->errorString().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_ERRORSTRING FP=hb_retc( ( p )->errorString().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool getChar ( char * c )
 */
HB_FUNC( QT_QIODEVICE_GETCHAR )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->getChar( ( char * ) hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_GETCHAR FP=hb_retl( ( p )->getChar( ( char * ) hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isOpen () const
 */
HB_FUNC( QT_QIODEVICE_ISOPEN )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->isOpen() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_ISOPEN FP=hb_retl( ( p )->isOpen() ); p is NULL" ) );
   }
}

/*
 * bool isReadable () const
 */
HB_FUNC( QT_QIODEVICE_ISREADABLE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->isReadable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_ISREADABLE FP=hb_retl( ( p )->isReadable() ); p is NULL" ) );
   }
}

/*
 * virtual bool isSequential () const
 */
HB_FUNC( QT_QIODEVICE_ISSEQUENTIAL )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->isSequential() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_ISSEQUENTIAL FP=hb_retl( ( p )->isSequential() ); p is NULL" ) );
   }
}

/*
 * bool isTextModeEnabled () const
 */
HB_FUNC( QT_QIODEVICE_ISTEXTMODEENABLED )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->isTextModeEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_ISTEXTMODEENABLED FP=hb_retl( ( p )->isTextModeEnabled() ); p is NULL" ) );
   }
}

/*
 * bool isWritable () const
 */
HB_FUNC( QT_QIODEVICE_ISWRITABLE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->isWritable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_ISWRITABLE FP=hb_retl( ( p )->isWritable() ); p is NULL" ) );
   }
}

/*
 * virtual bool open ( OpenMode mode )
 */
HB_FUNC( QT_QIODEVICE_OPEN )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->open( ( QIODevice::OpenMode ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_OPEN FP=hb_retl( ( p )->open( ( QIODevice::OpenMode ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * OpenMode openMode () const
 */
HB_FUNC( QT_QIODEVICE_OPENMODE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retni( ( QIODevice::OpenMode ) ( p )->openMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_OPENMODE FP=hb_retni( ( QIODevice::OpenMode ) ( p )->openMode() ); p is NULL" ) );
   }
}

/*
 * qint64 peek ( char * data, qint64 maxSize )
 */
HB_FUNC( QT_QIODEVICE_PEEK )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retnint( ( p )->peek( ( char * ) hb_parc( 2 ), hb_parnint( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_PEEK FP=hb_retnint( ( p )->peek( ( char * ) hb_parc( 2 ), hb_parnint( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * QByteArray peek ( qint64 maxSize )
 */
HB_FUNC( QT_QIODEVICE_PEEK_1 )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->peek( hb_parnint( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_PEEK_1 FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->peek( hb_parnint( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual qint64 pos () const
 */
HB_FUNC( QT_QIODEVICE_POS )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retnint( ( p )->pos() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_POS FP=hb_retnint( ( p )->pos() ); p is NULL" ) );
   }
}

/*
 * bool putChar ( char c )
 */
HB_FUNC( QT_QIODEVICE_PUTCHAR )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->putChar( ( char ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_PUTCHAR FP=hb_retl( ( p )->putChar( ( char ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * qint64 read ( char * data, qint64 maxSize )
 */
HB_FUNC( QT_QIODEVICE_READ )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retnint( ( p )->read( ( char * ) hb_parc( 2 ), hb_parnint( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_READ FP=hb_retnint( ( p )->read( ( char * ) hb_parc( 2 ), hb_parnint( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * QByteArray read ( qint64 maxSize )
 */
HB_FUNC( QT_QIODEVICE_READ_1 )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->read( hb_parnint( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_READ_1 FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->read( hb_parnint( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QByteArray readAll ()
 */
HB_FUNC( QT_QIODEVICE_READALL )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->readAll() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_READALL FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->readAll() ), true ) ); p is NULL" ) );
   }
}

/*
 * qint64 readLine ( char * data, qint64 maxSize )
 */
HB_FUNC( QT_QIODEVICE_READLINE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retnint( ( p )->readLine( ( char * ) hb_parc( 2 ), hb_parnint( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_READLINE FP=hb_retnint( ( p )->readLine( ( char * ) hb_parc( 2 ), hb_parnint( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * QByteArray readLine ( qint64 maxSize = 0 )
 */
HB_FUNC( QT_QIODEVICE_READLINE_1 )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->readLine( hb_parnint( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_READLINE_1 FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->readLine( hb_parnint( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual bool reset ()
 */
HB_FUNC( QT_QIODEVICE_RESET )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->reset() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_RESET FP=hb_retl( ( p )->reset() ); p is NULL" ) );
   }
}

/*
 * virtual bool seek ( qint64 pos )
 */
HB_FUNC( QT_QIODEVICE_SEEK )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->seek( hb_parnint( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_SEEK FP=hb_retl( ( p )->seek( hb_parnint( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setTextModeEnabled ( bool enabled )
 */
HB_FUNC( QT_QIODEVICE_SETTEXTMODEENABLED )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      ( p )->setTextModeEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_SETTEXTMODEENABLED FP=( p )->setTextModeEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual qint64 size () const
 */
HB_FUNC( QT_QIODEVICE_SIZE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retnint( ( p )->size() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_SIZE FP=hb_retnint( ( p )->size() ); p is NULL" ) );
   }
}

/*
 * void ungetChar ( char c )
 */
HB_FUNC( QT_QIODEVICE_UNGETCHAR )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      ( p )->ungetChar( ( char ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_UNGETCHAR FP=( p )->ungetChar( ( char ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual bool waitForBytesWritten ( int msecs )
 */
HB_FUNC( QT_QIODEVICE_WAITFORBYTESWRITTEN )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->waitForBytesWritten( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_WAITFORBYTESWRITTEN FP=hb_retl( ( p )->waitForBytesWritten( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual bool waitForReadyRead ( int msecs )
 */
HB_FUNC( QT_QIODEVICE_WAITFORREADYREAD )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->waitForReadyRead( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_WAITFORREADYREAD FP=hb_retl( ( p )->waitForReadyRead( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * qint64 write ( const char * data, qint64 maxSize )
 */
HB_FUNC( QT_QIODEVICE_WRITE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retnint( ( p )->write( hbqt_par_char( 2 ), hb_parnint( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_WRITE FP=hb_retnint( ( p )->write( hbqt_par_char( 2 ), hb_parnint( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * qint64 write ( const char * data )
 */
HB_FUNC( QT_QIODEVICE_WRITE_1 )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retnint( ( p )->write( hbqt_par_char( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_WRITE_1 FP=hb_retnint( ( p )->write( hbqt_par_char( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * qint64 write ( const QByteArray & byteArray )
 */
HB_FUNC( QT_QIODEVICE_WRITE_2 )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retnint( ( p )->write( *hbqt_par_QByteArray( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIODEVICE_WRITE_2 FP=hb_retnint( ( p )->write( *hbqt_par_QByteArray( 2 ) ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
