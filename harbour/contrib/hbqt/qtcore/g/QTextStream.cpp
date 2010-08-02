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

typedef struct
{
   QTextStream * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QTextStream;

QT_G_FUNC( hbqt_gcRelease_QTextStream )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QTextStream   /.\\", p->ph ) );
         delete ( ( QTextStream * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QTextStream   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTextStream    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTextStream    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextStream( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QTextStream * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextStream;
   p->type = HBQT_TYPE_QTextStream;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTextStream", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTextStream", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTSTREAM )
{
   QTextStream * pObj = NULL;

   pObj = new QTextStream( hb_parcx( 1 ), ( QIODevice::OpenMode ) ( HB_ISNUM( 2 ) ?  hb_parni( 2 ) : QIODevice::ReadWrite ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTextStream( ( void * ) pObj, true ) );
}

/*
 * QChar padChar () const
 */
HB_FUNC( QT_QTEXTSTREAM_PADCHAR )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->padChar() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_PADCHAR FP=hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->padChar() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool atEnd () const
 */
HB_FUNC( QT_QTEXTSTREAM_ATEND )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retl( ( p )->atEnd() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_ATEND FP=hb_retl( ( p )->atEnd() ); p is NULL" ) );
   }
}

/*
 * bool autoDetectUnicode () const
 */
HB_FUNC( QT_QTEXTSTREAM_AUTODETECTUNICODE )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retl( ( p )->autoDetectUnicode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_AUTODETECTUNICODE FP=hb_retl( ( p )->autoDetectUnicode() ); p is NULL" ) );
   }
}

/*
 * QTextCodec * codec () const
 */
HB_FUNC( QT_QTEXTSTREAM_CODEC )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codec(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_CODEC FP=hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codec(), false ) ); p is NULL" ) );
   }
}

/*
 * QIODevice * device () const
 */
HB_FUNC( QT_QTEXTSTREAM_DEVICE )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIODevice( ( p )->device(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_DEVICE FP=hb_retptrGC( hbqt_gcAllocate_QIODevice( ( p )->device(), false ) ); p is NULL" ) );
   }
}

/*
 * FieldAlignment fieldAlignment () const
 */
HB_FUNC( QT_QTEXTSTREAM_FIELDALIGNMENT )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retni( ( QTextStream::FieldAlignment ) ( p )->fieldAlignment() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_FIELDALIGNMENT FP=hb_retni( ( QTextStream::FieldAlignment ) ( p )->fieldAlignment() ); p is NULL" ) );
   }
}

/*
 * int fieldWidth () const
 */
HB_FUNC( QT_QTEXTSTREAM_FIELDWIDTH )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retni( ( p )->fieldWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_FIELDWIDTH FP=hb_retni( ( p )->fieldWidth() ); p is NULL" ) );
   }
}

/*
 * void flush ()
 */
HB_FUNC( QT_QTEXTSTREAM_FLUSH )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->flush();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_FLUSH FP=( p )->flush(); p is NULL" ) );
   }
}

/*
 * bool generateByteOrderMark () const
 */
HB_FUNC( QT_QTEXTSTREAM_GENERATEBYTEORDERMARK )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retl( ( p )->generateByteOrderMark() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_GENERATEBYTEORDERMARK FP=hb_retl( ( p )->generateByteOrderMark() ); p is NULL" ) );
   }
}

/*
 * int integerBase () const
 */
HB_FUNC( QT_QTEXTSTREAM_INTEGERBASE )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retni( ( p )->integerBase() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_INTEGERBASE FP=hb_retni( ( p )->integerBase() ); p is NULL" ) );
   }
}

/*
 * QLocale locale () const
 */
HB_FUNC( QT_QTEXTSTREAM_LOCALE )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( ( p )->locale() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_LOCALE FP=hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( ( p )->locale() ), true ) ); p is NULL" ) );
   }
}

/*
 * NumberFlags numberFlags () const
 */
HB_FUNC( QT_QTEXTSTREAM_NUMBERFLAGS )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retni( ( QTextStream::NumberFlags ) ( p )->numberFlags() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_NUMBERFLAGS FP=hb_retni( ( QTextStream::NumberFlags ) ( p )->numberFlags() ); p is NULL" ) );
   }
}

/*
 * qint64 pos () const
 */
HB_FUNC( QT_QTEXTSTREAM_POS )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retnint( ( p )->pos() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_POS FP=hb_retnint( ( p )->pos() ); p is NULL" ) );
   }
}

/*
 * QString read ( qint64 maxlen )
 */
HB_FUNC( QT_QTEXTSTREAM_READ )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retc( ( p )->read( hb_parnint( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_READ FP=hb_retc( ( p )->read( hb_parnint( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString readAll ()
 */
HB_FUNC( QT_QTEXTSTREAM_READALL )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retc( ( p )->readAll().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_READALL FP=hb_retc( ( p )->readAll().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString readLine ( qint64 maxlen = 0 )
 */
HB_FUNC( QT_QTEXTSTREAM_READLINE )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retc( ( p )->readLine( hb_parnint( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_READLINE FP=hb_retc( ( p )->readLine( hb_parnint( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * RealNumberNotation realNumberNotation () const
 */
HB_FUNC( QT_QTEXTSTREAM_REALNUMBERNOTATION )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retni( ( QTextStream::RealNumberNotation ) ( p )->realNumberNotation() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_REALNUMBERNOTATION FP=hb_retni( ( QTextStream::RealNumberNotation ) ( p )->realNumberNotation() ); p is NULL" ) );
   }
}

/*
 * int realNumberPrecision () const
 */
HB_FUNC( QT_QTEXTSTREAM_REALNUMBERPRECISION )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retni( ( p )->realNumberPrecision() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_REALNUMBERPRECISION FP=hb_retni( ( p )->realNumberPrecision() ); p is NULL" ) );
   }
}

/*
 * void reset ()
 */
HB_FUNC( QT_QTEXTSTREAM_RESET )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->reset();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_RESET FP=( p )->reset(); p is NULL" ) );
   }
}

/*
 * void resetStatus ()
 */
HB_FUNC( QT_QTEXTSTREAM_RESETSTATUS )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->resetStatus();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_RESETSTATUS FP=( p )->resetStatus(); p is NULL" ) );
   }
}

/*
 * bool seek ( qint64 pos )
 */
HB_FUNC( QT_QTEXTSTREAM_SEEK )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retl( ( p )->seek( hb_parnint( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_SEEK FP=hb_retl( ( p )->seek( hb_parnint( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setAutoDetectUnicode ( bool enabled )
 */
HB_FUNC( QT_QTEXTSTREAM_SETAUTODETECTUNICODE )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setAutoDetectUnicode( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_SETAUTODETECTUNICODE FP=( p )->setAutoDetectUnicode( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCodec ( QTextCodec * codec )
 */
HB_FUNC( QT_QTEXTSTREAM_SETCODEC )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setCodec( hbqt_par_QTextCodec( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_SETCODEC FP=( p )->setCodec( hbqt_par_QTextCodec( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCodec ( const char * codecName )
 */
HB_FUNC( QT_QTEXTSTREAM_SETCODEC_1 )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setCodec( hbqt_par_char( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_SETCODEC_1 FP=( p )->setCodec( hbqt_par_char( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDevice ( QIODevice * device )
 */
HB_FUNC( QT_QTEXTSTREAM_SETDEVICE )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setDevice( hbqt_par_QIODevice( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_SETDEVICE FP=( p )->setDevice( hbqt_par_QIODevice( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFieldAlignment ( FieldAlignment mode )
 */
HB_FUNC( QT_QTEXTSTREAM_SETFIELDALIGNMENT )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setFieldAlignment( ( QTextStream::FieldAlignment ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_SETFIELDALIGNMENT FP=( p )->setFieldAlignment( ( QTextStream::FieldAlignment ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFieldWidth ( int width )
 */
HB_FUNC( QT_QTEXTSTREAM_SETFIELDWIDTH )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setFieldWidth( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_SETFIELDWIDTH FP=( p )->setFieldWidth( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setGenerateByteOrderMark ( bool generate )
 */
HB_FUNC( QT_QTEXTSTREAM_SETGENERATEBYTEORDERMARK )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setGenerateByteOrderMark( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_SETGENERATEBYTEORDERMARK FP=( p )->setGenerateByteOrderMark( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setIntegerBase ( int base )
 */
HB_FUNC( QT_QTEXTSTREAM_SETINTEGERBASE )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setIntegerBase( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_SETINTEGERBASE FP=( p )->setIntegerBase( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setLocale ( const QLocale & locale )
 */
HB_FUNC( QT_QTEXTSTREAM_SETLOCALE )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setLocale( *hbqt_par_QLocale( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_SETLOCALE FP=( p )->setLocale( *hbqt_par_QLocale( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setNumberFlags ( NumberFlags flags )
 */
HB_FUNC( QT_QTEXTSTREAM_SETNUMBERFLAGS )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setNumberFlags( ( QTextStream::NumberFlags ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_SETNUMBERFLAGS FP=( p )->setNumberFlags( ( QTextStream::NumberFlags ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPadChar ( QChar ch )
 */
HB_FUNC( QT_QTEXTSTREAM_SETPADCHAR )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setPadChar( *hbqt_par_QChar( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_SETPADCHAR FP=( p )->setPadChar( *hbqt_par_QChar( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setRealNumberNotation ( RealNumberNotation notation )
 */
HB_FUNC( QT_QTEXTSTREAM_SETREALNUMBERNOTATION )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setRealNumberNotation( ( QTextStream::RealNumberNotation ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_SETREALNUMBERNOTATION FP=( p )->setRealNumberNotation( ( QTextStream::RealNumberNotation ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setRealNumberPrecision ( int precision )
 */
HB_FUNC( QT_QTEXTSTREAM_SETREALNUMBERPRECISION )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setRealNumberPrecision( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_SETREALNUMBERPRECISION FP=( p )->setRealNumberPrecision( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setStatus ( Status status )
 */
HB_FUNC( QT_QTEXTSTREAM_SETSTATUS )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->setStatus( ( QTextStream::Status ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_SETSTATUS FP=( p )->setStatus( ( QTextStream::Status ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void skipWhiteSpace ()
 */
HB_FUNC( QT_QTEXTSTREAM_SKIPWHITESPACE )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      ( p )->skipWhiteSpace();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_SKIPWHITESPACE FP=( p )->skipWhiteSpace(); p is NULL" ) );
   }
}

/*
 * Status status () const
 */
HB_FUNC( QT_QTEXTSTREAM_STATUS )
{
   QTextStream * p = hbqt_par_QTextStream( 1 );
   if( p )
      hb_retni( ( QTextStream::Status ) ( p )->status() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTSTREAM_STATUS FP=hb_retni( ( QTextStream::Status ) ( p )->status() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
