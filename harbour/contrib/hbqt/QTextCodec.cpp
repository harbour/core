/*
 * $Id$
 */

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
#include "hbqt.h"

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
 *  // QByteArray fromUnicode ( const QChar * input, int number, ConverterState * state = 0 ) const
 *  // QString toUnicode ( const char * input, int size, ConverterState * state = 0 ) const
 */


#include <QtCore/QTextCodec>


/*
 *
 *
 */
HB_FUNC( QT_QTEXTCODEC )
{
   //hb_retptr( ( QTextCodec* ) new QTextCodec() );
}

/*
 * DESTRUCTOR
 */
HB_FUNC( QT_QTEXTCODEC_DESTROY )
{

}

/*
 * bool canEncode ( QChar ch ) const
 */
HB_FUNC( QT_QTEXTCODEC_CANENCODE )
{
   hb_retl( hbqt_par_QTextCodec( 1 )->canEncode( hb_parni( 2 ) ) );
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
   hb_retptr( new QByteArray( hbqt_par_QTextCodec( 1 )->fromUnicode( hbqt_par_QString( 2 ) ) ) );
}

/*
 * QTextDecoder * makeDecoder () const
 */
HB_FUNC( QT_QTEXTCODEC_MAKEDECODER )
{
   hb_retptr( ( QTextDecoder* ) hbqt_par_QTextCodec( 1 )->makeDecoder() );
}

/*
 * QTextEncoder * makeEncoder () const
 */
HB_FUNC( QT_QTEXTCODEC_MAKEENCODER )
{
   hb_retptr( ( QTextEncoder* ) hbqt_par_QTextCodec( 1 )->makeEncoder() );
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
   hb_retptr( new QByteArray( hbqt_par_QTextCodec( 1 )->name() ) );
}

/*
 * QString toUnicode ( const QByteArray & a ) const
 */
HB_FUNC( QT_QTEXTCODEC_TOUNICODE )
{
   hb_retc( hbqt_par_QTextCodec( 1 )->toUnicode( *hbqt_par_QByteArray( 2 ) ).toLatin1().data() );
}

/*
 * QString toUnicode ( const char * chars ) const
 */
HB_FUNC( QT_QTEXTCODEC_TOUNICODE_1 )
{
   hb_retc( hbqt_par_QTextCodec( 1 )->toUnicode( hbqt_par_char( 2 ) ).toLatin1().data() );
}

/*
 * QTextCodec * codecForCStrings ()
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORCSTRINGS )
{
   hb_retptr( ( QTextCodec* ) hbqt_par_QTextCodec( 1 )->codecForCStrings() );
}

/*
 * QTextCodec * codecForHtml ( const QByteArray & ba, QTextCodec * defaultCodec )
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORHTML )
{
   hb_retptr( ( QTextCodec* ) hbqt_par_QTextCodec( 1 )->codecForHtml( *hbqt_par_QByteArray( 2 ), hbqt_par_QTextCodec( 3 ) ) );
}

/*
 * QTextCodec * codecForHtml ( const QByteArray & ba )
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORHTML_1 )
{
   hb_retptr( ( QTextCodec* ) hbqt_par_QTextCodec( 1 )->codecForHtml( *hbqt_par_QByteArray( 2 ) ) );
}

/*
 * QTextCodec * codecForLocale ()
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORLOCALE )
{
   hb_retptr( ( QTextCodec* ) hbqt_par_QTextCodec( 1 )->codecForLocale() );
}

/*
 * QTextCodec * codecForMib ( int mib )
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORMIB )
{
   hb_retptr( ( QTextCodec* ) hbqt_par_QTextCodec( 1 )->codecForMib( hb_parni( 2 ) ) );
}

/*
 * QTextCodec * codecForName ( const QByteArray & name )
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORNAME )
{
   hb_retptr( ( QTextCodec* ) hbqt_par_QTextCodec( 1 )->codecForName( *hbqt_par_QByteArray( 2 ) ) );
}

/*
 * QTextCodec * codecForName ( const char * name )
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORNAME_1 )
{
   hb_retptr( ( QTextCodec* ) hbqt_par_QTextCodec( 1 )->codecForName( hbqt_par_char( 2 ) ) );
}

/*
 * QTextCodec * codecForTr ()
 */
HB_FUNC( QT_QTEXTCODEC_CODECFORTR )
{
   hb_retptr( ( QTextCodec* ) hbqt_par_QTextCodec( 1 )->codecForTr() );
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
