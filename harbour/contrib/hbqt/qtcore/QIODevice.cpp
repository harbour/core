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

QT_G_FUNC( release_QIODevice )
{
   HB_SYMBOL_UNUSED( Cargo );
}

HB_FUNC( QT_QIODEVICE )
{
}
/*
 * virtual bool atEnd () const
 */
HB_FUNC( QT_QIODEVICE_ATEND )
{
   hb_retl( hbqt_par_QIODevice( 1 )->atEnd() );
}

/*
 * virtual qint64 bytesAvailable () const
 */
HB_FUNC( QT_QIODEVICE_BYTESAVAILABLE )
{
   hb_retnint( hbqt_par_QIODevice( 1 )->bytesAvailable() );
}

/*
 * virtual qint64 bytesToWrite () const
 */
HB_FUNC( QT_QIODEVICE_BYTESTOWRITE )
{
   hb_retnint( hbqt_par_QIODevice( 1 )->bytesToWrite() );
}

/*
 * virtual bool canReadLine () const
 */
HB_FUNC( QT_QIODEVICE_CANREADLINE )
{
   hb_retl( hbqt_par_QIODevice( 1 )->canReadLine() );
}

/*
 * virtual void close ()
 */
HB_FUNC( QT_QIODEVICE_CLOSE )
{
   hbqt_par_QIODevice( 1 )->close();
}

/*
 * QString errorString () const
 */
HB_FUNC( QT_QIODEVICE_ERRORSTRING )
{
   hb_retc( hbqt_par_QIODevice( 1 )->errorString().toAscii().data() );
}

/*
 * bool getChar ( char * c )
 */
HB_FUNC( QT_QIODEVICE_GETCHAR )
{
   hb_retl( hbqt_par_QIODevice( 1 )->getChar( ( char * ) hb_parc( 2 ) ) );
}

/*
 * bool isOpen () const
 */
HB_FUNC( QT_QIODEVICE_ISOPEN )
{
   hb_retl( hbqt_par_QIODevice( 1 )->isOpen() );
}

/*
 * bool isReadable () const
 */
HB_FUNC( QT_QIODEVICE_ISREADABLE )
{
   hb_retl( hbqt_par_QIODevice( 1 )->isReadable() );
}

/*
 * virtual bool isSequential () const
 */
HB_FUNC( QT_QIODEVICE_ISSEQUENTIAL )
{
   hb_retl( hbqt_par_QIODevice( 1 )->isSequential() );
}

/*
 * bool isTextModeEnabled () const
 */
HB_FUNC( QT_QIODEVICE_ISTEXTMODEENABLED )
{
   hb_retl( hbqt_par_QIODevice( 1 )->isTextModeEnabled() );
}

/*
 * bool isWritable () const
 */
HB_FUNC( QT_QIODEVICE_ISWRITABLE )
{
   hb_retl( hbqt_par_QIODevice( 1 )->isWritable() );
}

/*
 * virtual bool open ( OpenMode mode )
 */
HB_FUNC( QT_QIODEVICE_OPEN )
{
   hb_retl( hbqt_par_QIODevice( 1 )->open( ( QIODevice::OpenMode ) hb_parni( 2 ) ) );
}

/*
 * OpenMode openMode () const
 */
HB_FUNC( QT_QIODEVICE_OPENMODE )
{
   hb_retni( ( QIODevice::OpenMode ) hbqt_par_QIODevice( 1 )->openMode() );
}

/*
 * qint64 peek ( char * data, qint64 maxSize )
 */
HB_FUNC( QT_QIODEVICE_PEEK )
{
   hb_retnint( hbqt_par_QIODevice( 1 )->peek( ( char * ) hb_parc( 2 ), hb_parnint( 3 ) ) );
}

/*
 * QByteArray peek ( qint64 maxSize )
 */
HB_FUNC( QT_QIODEVICE_PEEK_1 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QIODevice( 1 )->peek( hb_parnint( 2 ) ) ) ) );
}

/*
 * virtual qint64 pos () const
 */
HB_FUNC( QT_QIODEVICE_POS )
{
   hb_retnint( hbqt_par_QIODevice( 1 )->pos() );
}

/*
 * bool putChar ( char c )
 */
HB_FUNC( QT_QIODEVICE_PUTCHAR )
{
   hb_retl( hbqt_par_QIODevice( 1 )->putChar( ( char ) hb_parni( 2 ) ) );
}

/*
 * qint64 read ( char * data, qint64 maxSize )
 */
HB_FUNC( QT_QIODEVICE_READ )
{
   hb_retnint( hbqt_par_QIODevice( 1 )->read( ( char * ) hb_parc( 2 ), hb_parnint( 3 ) ) );
}

/*
 * QByteArray read ( qint64 maxSize )
 */
HB_FUNC( QT_QIODEVICE_READ_1 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QIODevice( 1 )->read( hb_parnint( 2 ) ) ) ) );
}

/*
 * QByteArray readAll ()
 */
HB_FUNC( QT_QIODEVICE_READALL )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QIODevice( 1 )->readAll() ) ) );
}

/*
 * qint64 readLine ( char * data, qint64 maxSize )
 */
HB_FUNC( QT_QIODEVICE_READLINE )
{
   hb_retnint( hbqt_par_QIODevice( 1 )->readLine( ( char * ) hb_parc( 2 ), hb_parnint( 3 ) ) );
}

/*
 * QByteArray readLine ( qint64 maxSize = 0 )
 */
HB_FUNC( QT_QIODEVICE_READLINE_1 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QIODevice( 1 )->readLine( hb_parnint( 2 ) ) ) ) );
}

/*
 * virtual bool reset ()
 */
HB_FUNC( QT_QIODEVICE_RESET )
{
   hb_retl( hbqt_par_QIODevice( 1 )->reset() );
}

/*
 * virtual bool seek ( qint64 pos )
 */
HB_FUNC( QT_QIODEVICE_SEEK )
{
   hb_retl( hbqt_par_QIODevice( 1 )->seek( hb_parnint( 2 ) ) );
}

/*
 * void setTextModeEnabled ( bool enabled )
 */
HB_FUNC( QT_QIODEVICE_SETTEXTMODEENABLED )
{
   hbqt_par_QIODevice( 1 )->setTextModeEnabled( hb_parl( 2 ) );
}

/*
 * virtual qint64 size () const
 */
HB_FUNC( QT_QIODEVICE_SIZE )
{
   hb_retnint( hbqt_par_QIODevice( 1 )->size() );
}

/*
 * void ungetChar ( char c )
 */
HB_FUNC( QT_QIODEVICE_UNGETCHAR )
{
   hbqt_par_QIODevice( 1 )->ungetChar( ( char ) hb_parni( 2 ) );
}

/*
 * virtual bool waitForBytesWritten ( int msecs )
 */
HB_FUNC( QT_QIODEVICE_WAITFORBYTESWRITTEN )
{
   hb_retl( hbqt_par_QIODevice( 1 )->waitForBytesWritten( hb_parni( 2 ) ) );
}

/*
 * virtual bool waitForReadyRead ( int msecs )
 */
HB_FUNC( QT_QIODEVICE_WAITFORREADYREAD )
{
   hb_retl( hbqt_par_QIODevice( 1 )->waitForReadyRead( hb_parni( 2 ) ) );
}

/*
 * qint64 write ( const char * data, qint64 maxSize )
 */
HB_FUNC( QT_QIODEVICE_WRITE )
{
   hb_retnint( hbqt_par_QIODevice( 1 )->write( hbqt_par_char( 2 ), hb_parnint( 3 ) ) );
}

/*
 * qint64 write ( const char * data )
 */
HB_FUNC( QT_QIODEVICE_WRITE_1 )
{
   hb_retnint( hbqt_par_QIODevice( 1 )->write( hbqt_par_char( 2 ) ) );
}

/*
 * qint64 write ( const QByteArray & byteArray )
 */
HB_FUNC( QT_QIODEVICE_WRITE_2 )
{
   hb_retnint( hbqt_par_QIODevice( 1 )->write( *hbqt_par_QByteArray( 2 ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
