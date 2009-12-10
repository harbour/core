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
 *  flags NumberFlags
 *  enum FieldAlignment { AlignLeft, AlignRight, AlignCenter, AlignAccountingStyle }
 *  enum NumberFlag { ShowBase, ForcePoint, ForceSign, UppercaseBase, UppercaseDigits }
 *  enum RealNumberNotation { ScientificNotation, FixedNotation, SmartNotation }
 *  enum Status { Ok, ReadPastEnd, ReadCorruptData }
 */

#include <QtCore/QPointer>

#include <QtCore/QTextStream>


/*
 * QTextStream ()
 * QTextStream ( QIODevice * device )
 * QTextStream ( FILE * fileHandle, QIODevice::OpenMode openMode = QIODevice::ReadWrite )
 * QTextStream ( QString * string, QIODevice::OpenMode openMode = QIODevice::ReadWrite )
 * QTextStream ( QByteArray * array, QIODevice::OpenMode openMode = QIODevice::ReadWrite )
 * QTextStream ( const QByteArray & array, QIODevice::OpenMode openMode = QIODevice::ReadOnly )
 * virtual ~QTextStream ()
 */
/*
 * QChar padChar () const
 */
HB_FUNC( QT_QTEXTSTREAM_PADCHAR )
{

}

QT_G_FUNC( release_QTextStream )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QTextStream                  p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "release_QTextStream                 ph=%p", p->ph ) );

   if( p && p->ph )
   {
      delete ( ( QTextStream * ) p->ph );
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "YES release_QTextStream                 Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_QTextStream                 Object Already deleted!" ) );
   }
}

void * gcAllocate_QTextStream( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QTextStream;
   HB_TRACE( HB_TR_DEBUG, ( "          new_QTextStream                 %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QTEXTSTREAM )
{
   void * pObj = NULL;

   pObj = new QTextStream( hb_parcx( 1 ), ( QIODevice::OpenMode ) ( HB_ISNUM( 2 ) ?  hb_parni( 2 ) : QIODevice::ReadWrite ) ) ;

   hb_retptrGC( gcAllocate_QTextStream( pObj ) );
}
/*
 * bool atEnd () const
 */
HB_FUNC( QT_QTEXTSTREAM_ATEND )
{
   hb_retl( hbqt_par_QTextStream( 1 )->atEnd() );
}

/*
 * bool autoDetectUnicode () const
 */
HB_FUNC( QT_QTEXTSTREAM_AUTODETECTUNICODE )
{
   hb_retl( hbqt_par_QTextStream( 1 )->autoDetectUnicode() );
}

/*
 * QTextCodec * codec () const
 */
HB_FUNC( QT_QTEXTSTREAM_CODEC )
{
   hb_retptr( ( QTextCodec* ) hbqt_par_QTextStream( 1 )->codec() );
}

/*
 * QIODevice * device () const
 */
HB_FUNC( QT_QTEXTSTREAM_DEVICE )
{
   hb_retptr( ( QIODevice* ) hbqt_par_QTextStream( 1 )->device() );
}

/*
 * FieldAlignment fieldAlignment () const
 */
HB_FUNC( QT_QTEXTSTREAM_FIELDALIGNMENT )
{
   hb_retni( ( QTextStream::FieldAlignment ) hbqt_par_QTextStream( 1 )->fieldAlignment() );
}

/*
 * int fieldWidth () const
 */
HB_FUNC( QT_QTEXTSTREAM_FIELDWIDTH )
{
   hb_retni( hbqt_par_QTextStream( 1 )->fieldWidth() );
}

/*
 * void flush ()
 */
HB_FUNC( QT_QTEXTSTREAM_FLUSH )
{
   hbqt_par_QTextStream( 1 )->flush();
}

/*
 * bool generateByteOrderMark () const
 */
HB_FUNC( QT_QTEXTSTREAM_GENERATEBYTEORDERMARK )
{
   hb_retl( hbqt_par_QTextStream( 1 )->generateByteOrderMark() );
}

/*
 * int integerBase () const
 */
HB_FUNC( QT_QTEXTSTREAM_INTEGERBASE )
{
   hb_retni( hbqt_par_QTextStream( 1 )->integerBase() );
}

/*
 * QLocale locale () const
 */
HB_FUNC( QT_QTEXTSTREAM_LOCALE )
{
   hb_retptrGC( gcAllocate_QLocale( new QLocale( hbqt_par_QTextStream( 1 )->locale() ) ) );
}

/*
 * NumberFlags numberFlags () const
 */
HB_FUNC( QT_QTEXTSTREAM_NUMBERFLAGS )
{
   hb_retni( ( QTextStream::NumberFlags ) hbqt_par_QTextStream( 1 )->numberFlags() );
}

/*
 * qint64 pos () const
 */
HB_FUNC( QT_QTEXTSTREAM_POS )
{
   hb_retnint( hbqt_par_QTextStream( 1 )->pos() );
}

/*
 * QString read ( qint64 maxlen )
 */
HB_FUNC( QT_QTEXTSTREAM_READ )
{
   hb_retc( hbqt_par_QTextStream( 1 )->read( hb_parnint( 2 ) ).toAscii().data() );
}

/*
 * QString readAll ()
 */
HB_FUNC( QT_QTEXTSTREAM_READALL )
{
   hb_retc( hbqt_par_QTextStream( 1 )->readAll().toAscii().data() );
}

/*
 * QString readLine ( qint64 maxlen = 0 )
 */
HB_FUNC( QT_QTEXTSTREAM_READLINE )
{
   hb_retc( hbqt_par_QTextStream( 1 )->readLine( hb_parnint( 2 ) ).toAscii().data() );
}

/*
 * RealNumberNotation realNumberNotation () const
 */
HB_FUNC( QT_QTEXTSTREAM_REALNUMBERNOTATION )
{
   hb_retni( ( QTextStream::RealNumberNotation ) hbqt_par_QTextStream( 1 )->realNumberNotation() );
}

/*
 * int realNumberPrecision () const
 */
HB_FUNC( QT_QTEXTSTREAM_REALNUMBERPRECISION )
{
   hb_retni( hbqt_par_QTextStream( 1 )->realNumberPrecision() );
}

/*
 * void reset ()
 */
HB_FUNC( QT_QTEXTSTREAM_RESET )
{
   hbqt_par_QTextStream( 1 )->reset();
}

/*
 * void resetStatus ()
 */
HB_FUNC( QT_QTEXTSTREAM_RESETSTATUS )
{
   hbqt_par_QTextStream( 1 )->resetStatus();
}

/*
 * bool seek ( qint64 pos )
 */
HB_FUNC( QT_QTEXTSTREAM_SEEK )
{
   hb_retl( hbqt_par_QTextStream( 1 )->seek( hb_parnint( 2 ) ) );
}

/*
 * void setAutoDetectUnicode ( bool enabled )
 */
HB_FUNC( QT_QTEXTSTREAM_SETAUTODETECTUNICODE )
{
   hbqt_par_QTextStream( 1 )->setAutoDetectUnicode( hb_parl( 2 ) );
}

/*
 * void setCodec ( QTextCodec * codec )
 */
HB_FUNC( QT_QTEXTSTREAM_SETCODEC )
{
   hbqt_par_QTextStream( 1 )->setCodec( hbqt_par_QTextCodec( 2 ) );
}

/*
 * void setCodec ( const char * codecName )
 */
HB_FUNC( QT_QTEXTSTREAM_SETCODEC_1 )
{
   hbqt_par_QTextStream( 1 )->setCodec( hbqt_par_char( 2 ) );
}

/*
 * void setDevice ( QIODevice * device )
 */
HB_FUNC( QT_QTEXTSTREAM_SETDEVICE )
{
   hbqt_par_QTextStream( 1 )->setDevice( hbqt_par_QIODevice( 2 ) );
}

/*
 * void setFieldAlignment ( FieldAlignment mode )
 */
HB_FUNC( QT_QTEXTSTREAM_SETFIELDALIGNMENT )
{
   hbqt_par_QTextStream( 1 )->setFieldAlignment( ( QTextStream::FieldAlignment ) hb_parni( 2 ) );
}

/*
 * void setFieldWidth ( int width )
 */
HB_FUNC( QT_QTEXTSTREAM_SETFIELDWIDTH )
{
   hbqt_par_QTextStream( 1 )->setFieldWidth( hb_parni( 2 ) );
}

/*
 * void setGenerateByteOrderMark ( bool generate )
 */
HB_FUNC( QT_QTEXTSTREAM_SETGENERATEBYTEORDERMARK )
{
   hbqt_par_QTextStream( 1 )->setGenerateByteOrderMark( hb_parl( 2 ) );
}

/*
 * void setIntegerBase ( int base )
 */
HB_FUNC( QT_QTEXTSTREAM_SETINTEGERBASE )
{
   hbqt_par_QTextStream( 1 )->setIntegerBase( hb_parni( 2 ) );
}

/*
 * void setLocale ( const QLocale & locale )
 */
HB_FUNC( QT_QTEXTSTREAM_SETLOCALE )
{
   hbqt_par_QTextStream( 1 )->setLocale( *hbqt_par_QLocale( 2 ) );
}

/*
 * void setNumberFlags ( NumberFlags flags )
 */
HB_FUNC( QT_QTEXTSTREAM_SETNUMBERFLAGS )
{
   hbqt_par_QTextStream( 1 )->setNumberFlags( ( QTextStream::NumberFlags ) hb_parni( 2 ) );
}

/*
 * void setPadChar ( QChar ch )
 */
HB_FUNC( QT_QTEXTSTREAM_SETPADCHAR )
{
   hbqt_par_QTextStream( 1 )->setPadChar( hb_parni( 2 ) );
}

/*
 * void setRealNumberNotation ( RealNumberNotation notation )
 */
HB_FUNC( QT_QTEXTSTREAM_SETREALNUMBERNOTATION )
{
   hbqt_par_QTextStream( 1 )->setRealNumberNotation( ( QTextStream::RealNumberNotation ) hb_parni( 2 ) );
}

/*
 * void setRealNumberPrecision ( int precision )
 */
HB_FUNC( QT_QTEXTSTREAM_SETREALNUMBERPRECISION )
{
   hbqt_par_QTextStream( 1 )->setRealNumberPrecision( hb_parni( 2 ) );
}

/*
 * void setStatus ( Status status )
 */
HB_FUNC( QT_QTEXTSTREAM_SETSTATUS )
{
   hbqt_par_QTextStream( 1 )->setStatus( ( QTextStream::Status ) hb_parni( 2 ) );
}

/*
 * void skipWhiteSpace ()
 */
HB_FUNC( QT_QTEXTSTREAM_SKIPWHITESPACE )
{
   hbqt_par_QTextStream( 1 )->skipWhiteSpace();
}

/*
 * Status status () const
 */
HB_FUNC( QT_QTEXTSTREAM_STATUS )
{
   hb_retni( ( QTextStream::Status ) hbqt_par_QTextStream( 1 )->status() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
