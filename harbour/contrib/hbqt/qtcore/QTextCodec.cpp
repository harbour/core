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
 *  enum ConversionFlag { DefaultConversion, ConvertInvalidToNull, IgnoreHeader }
 *  flags ConversionFlags
 */

/*
 *  Constructed[ 20/23 [ 86.96% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  virtual QList<QByteArray> aliases () const
 *
 *  *** Commented out protos which construct fine but do not compile ***
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
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
} QGC_POINTER_QTextCodec;

QT_G_FUNC( hbqt_gcRelease_QTextCodec )
{
   HB_SYMBOL_UNUSED( Cargo );
}

void * hbqt_gcAllocate_QTextCodec( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextCodec;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QTextCodec                 ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTCODEC )
{
}

/*
 * bool canEncode ( QChar ch ) const
 */
HB_FUNC( QT_QTEXTCODEC_CANENCODE )
{
   hb_retl( hbqt_par_QTextCodec( 1 )->canEncode( *hbqt_par_QChar( 2 ) ) );
}

/*
 * bool canEncode ( const QString & s ) const
 */
HB_FUNC( QT_QTEXTCODEC_CANENCODE_1 )
{
   hb_retl( hbqt_par_QTextCodec( 1 )->canEncode( hbqt_par_QString( 2 ) ) );
}

/*
 * QByteArray fromUnicode ( const QString & str ) const
 */
HB_FUNC( QT_QTEXTCODEC_FROMUNICODE )
{
   hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( hbqt_par_QTextCodec( 1 )->fromUnicode( hbqt_par_QString( 2 ) ) ), true ) );
}

/*
 * QTextDecoder * makeDecoder () const
 */
HB_FUNC( QT_QTEXTCODEC_MAKEDECODER )
{
   hb_retptrGC( hbqt_gcAllocate_QTextDecoder( hbqt_par_QTextCodec( 1 )->makeDecoder(), false ) );
}

/*
 * QTextEncoder * makeEncoder () const
 */
HB_FUNC( QT_QTEXTCODEC_MAKEENCODER )
{
   hb_retptrGC( hbqt_gcAllocate_QTextEncoder( hbqt_par_QTextCodec( 1 )->makeEncoder(), false ) );
}

/*
 * virtual int mibEnum () const = 0
 */
HB_FUNC( QT_QTEXTCODEC_MIBENUM )
{
   hb_retni( hbqt_par_QTextCodec( 1 )->mibEnum() );
}

/*
 * virtual QByteArray name () const = 0
 */
HB_FUNC( QT_QTEXTCODEC_NAME )
{
   hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( hbqt_par_QTextCodec( 1 )->name() ), true ) );
}

/*
 * QString toUnicode ( const QByteArray & a ) const
 */
HB_FUNC( QT_QTEXTCODEC_TOUNICODE )
{
   hb_retc( hbqt_par_QTextCodec( 1 )->toUnicode( *hbqt_par_QByteArray( 2 ) ).toAscii().data() );
}

/*
 * QString toUnicode ( const char * chars ) const
 */
HB_FUNC( QT_QTEXTCODEC_TOUNICODE_1 )
{
   hb_retc( hbqt_par_QTextCodec( 1 )->toUnicode( hbqt_par_char( 2 ) ).toAscii().data() );
}

/*
 * QTextCodec * codecForCStrings ()
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORCSTRINGS )
{
   hb_retptrGC( hbqt_gcAllocate_QTextCodec( hbqt_par_QTextCodec( 1 )->codecForCStrings(), false ) );
}

/*
 * QTextCodec * codecForHtml ( const QByteArray & ba, QTextCodec * defaultCodec )
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORHTML )
{
   hb_retptrGC( hbqt_gcAllocate_QTextCodec( hbqt_par_QTextCodec( 1 )->codecForHtml( *hbqt_par_QByteArray( 2 ), hbqt_par_QTextCodec( 3 ) ), false ) );
}

/*
 * QTextCodec * codecForHtml ( const QByteArray & ba )
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORHTML_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QTextCodec( hbqt_par_QTextCodec( 1 )->codecForHtml( *hbqt_par_QByteArray( 2 ) ), false ) );
}

/*
 * QTextCodec * codecForLocale ()
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORLOCALE )
{
   hb_retptrGC( hbqt_gcAllocate_QTextCodec( hbqt_par_QTextCodec( 1 )->codecForLocale(), false ) );
}

/*
 * QTextCodec * codecForMib ( int mib )
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORMIB )
{
   hb_retptrGC( hbqt_gcAllocate_QTextCodec( hbqt_par_QTextCodec( 1 )->codecForMib( hb_parni( 2 ) ), false ) );
}

/*
 * QTextCodec * codecForName ( const QByteArray & name )
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORNAME )
{
   hb_retptrGC( hbqt_gcAllocate_QTextCodec( hbqt_par_QTextCodec( 1 )->codecForName( *hbqt_par_QByteArray( 2 ) ), false ) );
}

/*
 * QTextCodec * codecForName ( const char * name )
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORNAME_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QTextCodec( hbqt_par_QTextCodec( 1 )->codecForName( hbqt_par_char( 2 ) ), false ) );
}

/*
 * QTextCodec * codecForTr ()
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORTR )
{
   hb_retptrGC( hbqt_gcAllocate_QTextCodec( hbqt_par_QTextCodec( 1 )->codecForTr(), false ) );
}

/*
 * void setCodecForCStrings ( QTextCodec * codec )
 */
HB_FUNC( QT_QTEXTCODEC_SETCODECFORCSTRINGS )
{
   hbqt_par_QTextCodec( 1 )->setCodecForCStrings( hbqt_par_QTextCodec( 2 ) );
}

/*
 * void setCodecForLocale ( QTextCodec * c )
 */
HB_FUNC( QT_QTEXTCODEC_SETCODECFORLOCALE )
{
   hbqt_par_QTextCodec( 1 )->setCodecForLocale( hbqt_par_QTextCodec( 2 ) );
}

/*
 * void setCodecForTr ( QTextCodec * c )
 */
HB_FUNC( QT_QTEXTCODEC_SETCODECFORTR )
{
   hbqt_par_QTextCodec( 1 )->setCodecForTr( hbqt_par_QTextCodec( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
