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
 *  enum ConversionFlag { DefaultConversion, ConvertInvalidToNull, IgnoreHeader }
 *  flags ConversionFlags
 */

/*
 *  Constructed[ 21/21 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //QByteArray fromUnicode ( const QChar * input, int number, ConverterState * state = 0 ) const
 *  // QString toUnicode ( const char * input, int size, ConverterState * state = 0 ) const
 */

#include <QtCore/QPointer>

#include <QtCore/QTextCodec>


/*
 *
 *
 */

typedef struct
{
   QTextCodec * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextCodec;

HBQT_GC_FUNC( hbqt_gcRelease_QTextCodec )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextCodec( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextCodec * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextCodec;
   p->type = HBQT_TYPE_QTextCodec;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTextCodec", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTextCodec", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTCODEC )
{
   //hb_retptr( ( QTextCodec* ) new QTextCodec() );
}

/*
 * virtual QList<QByteArray> aliases () const
 */
HB_FUNC( QT_QTEXTCODEC_ALIASES )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QByteArray>( ( p )->aliases() ), true ) );
   }
}

/*
 * bool canEncode ( QChar ch ) const
 */
HB_FUNC( QT_QTEXTCODEC_CANENCODE )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
   {
      hb_retl( ( p )->canEncode( *hbqt_par_QChar( 2 ) ) );
   }
}

/*
 * bool canEncode ( const QString & s ) const
 */
HB_FUNC( QT_QTEXTCODEC_CANENCODE_1 )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->canEncode( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * QByteArray fromUnicode ( const QString & str ) const
 */
HB_FUNC( QT_QTEXTCODEC_FROMUNICODE )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->fromUnicode( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * QTextDecoder * makeDecoder () const
 */
HB_FUNC( QT_QTEXTCODEC_MAKEDECODER )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextDecoder( ( p )->makeDecoder(), false ) );
   }
}

/*
 * QTextEncoder * makeEncoder () const
 */
HB_FUNC( QT_QTEXTCODEC_MAKEENCODER )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextEncoder( ( p )->makeEncoder(), false ) );
   }
}

/*
 * virtual int mibEnum () const = 0
 */
HB_FUNC( QT_QTEXTCODEC_MIBENUM )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
   {
      hb_retni( ( p )->mibEnum() );
   }
}

/*
 * virtual QByteArray name () const = 0
 */
HB_FUNC( QT_QTEXTCODEC_NAME )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->name() ), true ) );
   }
}

/*
 * QString toUnicode ( const QByteArray & a ) const
 */
HB_FUNC( QT_QTEXTCODEC_TOUNICODE )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->toUnicode( *hbqt_par_QByteArray( 2 ) ).toUtf8().data() );
   }
}

/*
 * QString toUnicode ( const char * chars ) const
 */
HB_FUNC( QT_QTEXTCODEC_TOUNICODE_1 )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->toUnicode( hbqt_par_char( 2 ) ).toUtf8().data() );
   }
}

/*
 * QTextCodec * codecForCStrings ()
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORCSTRINGS )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForCStrings(), false ) );
   }
}

/*
 * QTextCodec * codecForHtml ( const QByteArray & ba, QTextCodec * defaultCodec )
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORHTML )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForHtml( *hbqt_par_QByteArray( 2 ), hbqt_par_QTextCodec( 3 ) ), false ) );
   }
}

/*
 * QTextCodec * codecForHtml ( const QByteArray & ba )
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORHTML_1 )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForHtml( *hbqt_par_QByteArray( 2 ) ), false ) );
   }
}

/*
 * QTextCodec * codecForLocale ()
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORLOCALE )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForLocale(), false ) );
   }
}

/*
 * QTextCodec * codecForMib ( int mib )
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORMIB )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForMib( hb_parni( 2 ) ), false ) );
   }
}

/*
 * QTextCodec * codecForName ( const QByteArray & name )
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORNAME )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForName( *hbqt_par_QByteArray( 2 ) ), false ) );
   }
}

/*
 * QTextCodec * codecForName ( const char * name )
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORNAME_1 )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForName( hbqt_par_char( 2 ) ), false ) );
   }
}

/*
 * QTextCodec * codecForTr ()
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORTR )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForTr(), false ) );
   }
}

/*
 * void setCodecForCStrings ( QTextCodec * codec )
 */
HB_FUNC( QT_QTEXTCODEC_SETCODECFORCSTRINGS )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
   {
      ( p )->setCodecForCStrings( hbqt_par_QTextCodec( 2 ) );
   }
}

/*
 * void setCodecForLocale ( QTextCodec * c )
 */
HB_FUNC( QT_QTEXTCODEC_SETCODECFORLOCALE )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
   {
      ( p )->setCodecForLocale( hbqt_par_QTextCodec( 2 ) );
   }
}

/*
 * void setCodecForTr ( QTextCodec * c )
 */
HB_FUNC( QT_QTEXTCODEC_SETCODECFORTR )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
   {
      ( p )->setCodecForTr( hbqt_par_QTextCodec( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
