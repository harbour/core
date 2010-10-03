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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  flags OpenMode
 *  enum OpenModeFlag { NotOpen, ReadOnly, WriteOnly, ReadWrite, ..., Unbuffered }
 */

/*
 *  Constructed[ 33/33 [ 100.00% ] ]
 *
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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QIODevice;

HBQT_GC_FUNC( hbqt_gcRelease_QIODevice )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QIODevice( void * pObj, bool bNew )
{
   HBQT_GC_T_QIODevice * p = ( HBQT_GC_T_QIODevice * ) hb_gcAllocate( sizeof( HBQT_GC_T_QIODevice ), hbqt_gcFuncs() );

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
   {
      hb_retl( ( p )->atEnd() );
   }
}

/*
 * virtual qint64 bytesAvailable () const
 */
HB_FUNC( QT_QIODEVICE_BYTESAVAILABLE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retnint( ( p )->bytesAvailable() );
   }
}

/*
 * virtual qint64 bytesToWrite () const
 */
HB_FUNC( QT_QIODEVICE_BYTESTOWRITE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retnint( ( p )->bytesToWrite() );
   }
}

/*
 * virtual bool canReadLine () const
 */
HB_FUNC( QT_QIODEVICE_CANREADLINE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retl( ( p )->canReadLine() );
   }
}

/*
 * virtual void close ()
 */
HB_FUNC( QT_QIODEVICE_CLOSE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      ( p )->close();
   }
}

/*
 * QString errorString () const
 */
HB_FUNC( QT_QIODEVICE_ERRORSTRING )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->errorString().toUtf8().data() );
   }
}

/*
 * bool getChar ( char * c )
 */
HB_FUNC( QT_QIODEVICE_GETCHAR )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retl( ( p )->getChar( ( char * ) hb_parc( 2 ) ) );
   }
}

/*
 * bool isOpen () const
 */
HB_FUNC( QT_QIODEVICE_ISOPEN )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retl( ( p )->isOpen() );
   }
}

/*
 * bool isReadable () const
 */
HB_FUNC( QT_QIODEVICE_ISREADABLE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retl( ( p )->isReadable() );
   }
}

/*
 * virtual bool isSequential () const
 */
HB_FUNC( QT_QIODEVICE_ISSEQUENTIAL )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retl( ( p )->isSequential() );
   }
}

/*
 * bool isTextModeEnabled () const
 */
HB_FUNC( QT_QIODEVICE_ISTEXTMODEENABLED )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retl( ( p )->isTextModeEnabled() );
   }
}

/*
 * bool isWritable () const
 */
HB_FUNC( QT_QIODEVICE_ISWRITABLE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retl( ( p )->isWritable() );
   }
}

/*
 * virtual bool open ( OpenMode mode )
 */
HB_FUNC( QT_QIODEVICE_OPEN )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retl( ( p )->open( ( QIODevice::OpenMode ) hb_parni( 2 ) ) );
   }
}

/*
 * OpenMode openMode () const
 */
HB_FUNC( QT_QIODEVICE_OPENMODE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retni( ( QIODevice::OpenMode ) ( p )->openMode() );
   }
}

/*
 * qint64 peek ( char * data, qint64 maxSize )
 */
HB_FUNC( QT_QIODEVICE_PEEK )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retnint( ( p )->peek( ( char * ) hb_parc( 2 ), hb_parnint( 3 ) ) );
   }
}

/*
 * QByteArray peek ( qint64 maxSize )
 */
HB_FUNC( QT_QIODEVICE_PEEK_1 )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->peek( hb_parnint( 2 ) ) ), true ) );
   }
}

/*
 * virtual qint64 pos () const
 */
HB_FUNC( QT_QIODEVICE_POS )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retnint( ( p )->pos() );
   }
}

/*
 * bool putChar ( char c )
 */
HB_FUNC( QT_QIODEVICE_PUTCHAR )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retl( ( p )->putChar( ( char ) hb_parni( 2 ) ) );
   }
}

/*
 * qint64 read ( char * data, qint64 maxSize )
 */
HB_FUNC( QT_QIODEVICE_READ )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retnint( ( p )->read( ( char * ) hb_parc( 2 ), hb_parnint( 3 ) ) );
   }
}

/*
 * QByteArray read ( qint64 maxSize )
 */
HB_FUNC( QT_QIODEVICE_READ_1 )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->read( hb_parnint( 2 ) ) ), true ) );
   }
}

/*
 * QByteArray readAll ()
 */
HB_FUNC( QT_QIODEVICE_READALL )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->readAll() ), true ) );
   }
}

/*
 * qint64 readLine ( char * data, qint64 maxSize )
 */
HB_FUNC( QT_QIODEVICE_READLINE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retnint( ( p )->readLine( ( char * ) hb_parc( 2 ), hb_parnint( 3 ) ) );
   }
}

/*
 * QByteArray readLine ( qint64 maxSize = 0 )
 */
HB_FUNC( QT_QIODEVICE_READLINE_1 )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->readLine( hb_parnint( 2 ) ) ), true ) );
   }
}

/*
 * virtual bool reset ()
 */
HB_FUNC( QT_QIODEVICE_RESET )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retl( ( p )->reset() );
   }
}

/*
 * virtual bool seek ( qint64 pos )
 */
HB_FUNC( QT_QIODEVICE_SEEK )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retl( ( p )->seek( hb_parnint( 2 ) ) );
   }
}

/*
 * void setTextModeEnabled ( bool enabled )
 */
HB_FUNC( QT_QIODEVICE_SETTEXTMODEENABLED )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      ( p )->setTextModeEnabled( hb_parl( 2 ) );
   }
}

/*
 * virtual qint64 size () const
 */
HB_FUNC( QT_QIODEVICE_SIZE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retnint( ( p )->size() );
   }
}

/*
 * void ungetChar ( char c )
 */
HB_FUNC( QT_QIODEVICE_UNGETCHAR )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      ( p )->ungetChar( ( char ) hb_parni( 2 ) );
   }
}

/*
 * virtual bool waitForBytesWritten ( int msecs )
 */
HB_FUNC( QT_QIODEVICE_WAITFORBYTESWRITTEN )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retl( ( p )->waitForBytesWritten( hb_parni( 2 ) ) );
   }
}

/*
 * virtual bool waitForReadyRead ( int msecs )
 */
HB_FUNC( QT_QIODEVICE_WAITFORREADYREAD )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retl( ( p )->waitForReadyRead( hb_parni( 2 ) ) );
   }
}

/*
 * qint64 write ( const char * data, qint64 maxSize )
 */
HB_FUNC( QT_QIODEVICE_WRITE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retnint( ( p )->write( ( const char * ) hb_parc( 2 ), hb_parnint( 3 ) ) );
   }
}

/*
 * qint64 write ( const char * data )
 */
HB_FUNC( QT_QIODEVICE_WRITE_1 )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retnint( ( p )->write( ( const char * ) hb_parc( 2 ) ) );
   }
}

/*
 * qint64 write ( const QByteArray & byteArray )
 */
HB_FUNC( QT_QIODEVICE_WRITE_2 )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
   {
      hb_retnint( ( p )->write( *hbqt_par_QByteArray( 2 ) ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
