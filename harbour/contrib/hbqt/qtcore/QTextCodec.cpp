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

#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum ConversionFlag { DefaultConversion, ConvertInvalidToNull, IgnoreHeader }
 *  flags ConversionFlags
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
   QT_G_FUNC_PTR func;
} QGC_POINTER_QTextCodec;

QT_G_FUNC( hbqt_gcRelease_QTextCodec )
{
   HB_SYMBOL_UNUSED( Cargo );
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextCodec( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QTextCodec * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextCodec;

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
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QByteArray>( ( p )->aliases() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCODEC_ALIASES FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QByteArray>( ( p )->aliases() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool canEncode ( QChar ch ) const
 */
HB_FUNC( QT_QTEXTCODEC_CANENCODE )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retl( ( p )->canEncode( *hbqt_par_QChar( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCODEC_CANENCODE FP=hb_retl( ( p )->canEncode( *hbqt_par_QChar( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool canEncode ( const QString & s ) const
 */
HB_FUNC( QT_QTEXTCODEC_CANENCODE_1 )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retl( ( p )->canEncode( hbqt_par_QString( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCODEC_CANENCODE_1 FP=hb_retl( ( p )->canEncode( hbqt_par_QString( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QByteArray fromUnicode ( const QString & str ) const
 */
HB_FUNC( QT_QTEXTCODEC_FROMUNICODE )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->fromUnicode( hbqt_par_QString( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCODEC_FROMUNICODE FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->fromUnicode( hbqt_par_QString( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QTextDecoder * makeDecoder () const
 */
HB_FUNC( QT_QTEXTCODEC_MAKEDECODER )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextDecoder( ( p )->makeDecoder(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCODEC_MAKEDECODER FP=hb_retptrGC( hbqt_gcAllocate_QTextDecoder( ( p )->makeDecoder(), false ) ); p is NULL" ) );
   }
}

/*
 * QTextEncoder * makeEncoder () const
 */
HB_FUNC( QT_QTEXTCODEC_MAKEENCODER )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextEncoder( ( p )->makeEncoder(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCODEC_MAKEENCODER FP=hb_retptrGC( hbqt_gcAllocate_QTextEncoder( ( p )->makeEncoder(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual int mibEnum () const = 0
 */
HB_FUNC( QT_QTEXTCODEC_MIBENUM )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retni( ( p )->mibEnum() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCODEC_MIBENUM FP=hb_retni( ( p )->mibEnum() ); p is NULL" ) );
   }
}

/*
 * virtual QByteArray name () const = 0
 */
HB_FUNC( QT_QTEXTCODEC_NAME )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->name() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCODEC_NAME FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->name() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString toUnicode ( const QByteArray & a ) const
 */
HB_FUNC( QT_QTEXTCODEC_TOUNICODE )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retc( ( p )->toUnicode( *hbqt_par_QByteArray( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCODEC_TOUNICODE FP=hb_retc( ( p )->toUnicode( *hbqt_par_QByteArray( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString toUnicode ( const char * chars ) const
 */
HB_FUNC( QT_QTEXTCODEC_TOUNICODE_1 )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retc( ( p )->toUnicode( hbqt_par_char( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCODEC_TOUNICODE_1 FP=hb_retc( ( p )->toUnicode( hbqt_par_char( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QTextCodec * codecForCStrings ()
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORCSTRINGS )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForCStrings(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCODEC_CODECFORCSTRINGS FP=hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForCStrings(), false ) ); p is NULL" ) );
   }
}

/*
 * QTextCodec * codecForHtml ( const QByteArray & ba, QTextCodec * defaultCodec )
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORHTML )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForHtml( *hbqt_par_QByteArray( 2 ), hbqt_par_QTextCodec( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCODEC_CODECFORHTML FP=hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForHtml( *hbqt_par_QByteArray( 2 ), hbqt_par_QTextCodec( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QTextCodec * codecForHtml ( const QByteArray & ba )
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORHTML_1 )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForHtml( *hbqt_par_QByteArray( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCODEC_CODECFORHTML_1 FP=hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForHtml( *hbqt_par_QByteArray( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QTextCodec * codecForLocale ()
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORLOCALE )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForLocale(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCODEC_CODECFORLOCALE FP=hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForLocale(), false ) ); p is NULL" ) );
   }
}

/*
 * QTextCodec * codecForMib ( int mib )
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORMIB )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForMib( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCODEC_CODECFORMIB FP=hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForMib( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QTextCodec * codecForName ( const QByteArray & name )
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORNAME )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForName( *hbqt_par_QByteArray( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCODEC_CODECFORNAME FP=hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForName( *hbqt_par_QByteArray( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QTextCodec * codecForName ( const char * name )
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORNAME_1 )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForName( hbqt_par_char( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCODEC_CODECFORNAME_1 FP=hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForName( hbqt_par_char( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QTextCodec * codecForTr ()
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORTR )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForTr(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCODEC_CODECFORTR FP=hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codecForTr(), false ) ); p is NULL" ) );
   }
}

/*
 * void setCodecForCStrings ( QTextCodec * codec )
 */
HB_FUNC( QT_QTEXTCODEC_SETCODECFORCSTRINGS )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      ( p )->setCodecForCStrings( hbqt_par_QTextCodec( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCODEC_SETCODECFORCSTRINGS FP=( p )->setCodecForCStrings( hbqt_par_QTextCodec( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCodecForLocale ( QTextCodec * c )
 */
HB_FUNC( QT_QTEXTCODEC_SETCODECFORLOCALE )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      ( p )->setCodecForLocale( hbqt_par_QTextCodec( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCODEC_SETCODECFORLOCALE FP=( p )->setCodecForLocale( hbqt_par_QTextCodec( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCodecForTr ( QTextCodec * c )
 */
HB_FUNC( QT_QTEXTCODEC_SETCODECFORTR )
{
   QTextCodec * p = hbqt_par_QTextCodec( 1 );
   if( p )
      ( p )->setCodecForTr( hbqt_par_QTextCodec( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCODEC_SETCODECFORTR FP=( p )->setCodecForTr( hbqt_par_QTextCodec( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
