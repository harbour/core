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
 *  Constructed[ 18/19 [ 94.74% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  void setValues ( const QList<QPair<QString, QString> > & values )
 */

#include <QtCore/QPointer>

#include <QtNetwork/QHttpHeader>


/*
 * QHttpHeader ()
 * QHttpHeader ( const QHttpHeader & header )
 * QHttpHeader ( const QString & str )
 * virtual ~QHttpHeader ()
 */

QT_G_FUNC( release_QHttpHeader )
{
   HB_SYMBOL_UNUSED( Cargo );
}

HB_FUNC( QT_QHTTPHEADER )
{
}
/*
 * void addValue ( const QString & key, const QString & value )
 */
HB_FUNC( QT_QHTTPHEADER_ADDVALUE )
{
   hbqt_par_QHttpHeader( 1 )->addValue( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) );
}

/*
 * QStringList allValues ( const QString & key ) const
 */
HB_FUNC( QT_QHTTPHEADER_ALLVALUES )
{
   hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( hbqt_par_QHttpHeader( 1 )->allValues( hbqt_par_QString( 2 ) ) ) ) );
}

/*
 * uint contentLength () const
 */
HB_FUNC( QT_QHTTPHEADER_CONTENTLENGTH )
{
   hb_retni( hbqt_par_QHttpHeader( 1 )->contentLength() );
}

/*
 * QString contentType () const
 */
HB_FUNC( QT_QHTTPHEADER_CONTENTTYPE )
{
   hb_retc( hbqt_par_QHttpHeader( 1 )->contentType().toAscii().data() );
}

/*
 * bool hasContentLength () const
 */
HB_FUNC( QT_QHTTPHEADER_HASCONTENTLENGTH )
{
   hb_retl( hbqt_par_QHttpHeader( 1 )->hasContentLength() );
}

/*
 * bool hasContentType () const
 */
HB_FUNC( QT_QHTTPHEADER_HASCONTENTTYPE )
{
   hb_retl( hbqt_par_QHttpHeader( 1 )->hasContentType() );
}

/*
 * bool hasKey ( const QString & key ) const
 */
HB_FUNC( QT_QHTTPHEADER_HASKEY )
{
   hb_retl( hbqt_par_QHttpHeader( 1 )->hasKey( hbqt_par_QString( 2 ) ) );
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QHTTPHEADER_ISVALID )
{
   hb_retl( hbqt_par_QHttpHeader( 1 )->isValid() );
}

/*
 * QStringList keys () const
 */
HB_FUNC( QT_QHTTPHEADER_KEYS )
{
   hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( hbqt_par_QHttpHeader( 1 )->keys() ) ) );
}

/*
 * virtual int majorVersion () const = 0
 */
HB_FUNC( QT_QHTTPHEADER_MAJORVERSION )
{
   hb_retni( hbqt_par_QHttpHeader( 1 )->majorVersion() );
}

/*
 * virtual int minorVersion () const = 0
 */
HB_FUNC( QT_QHTTPHEADER_MINORVERSION )
{
   hb_retni( hbqt_par_QHttpHeader( 1 )->minorVersion() );
}

/*
 * void removeAllValues ( const QString & key )
 */
HB_FUNC( QT_QHTTPHEADER_REMOVEALLVALUES )
{
   hbqt_par_QHttpHeader( 1 )->removeAllValues( hbqt_par_QString( 2 ) );
}

/*
 * void removeValue ( const QString & key )
 */
HB_FUNC( QT_QHTTPHEADER_REMOVEVALUE )
{
   hbqt_par_QHttpHeader( 1 )->removeValue( hbqt_par_QString( 2 ) );
}

/*
 * void setContentLength ( int len )
 */
HB_FUNC( QT_QHTTPHEADER_SETCONTENTLENGTH )
{
   hbqt_par_QHttpHeader( 1 )->setContentLength( hb_parni( 2 ) );
}

/*
 * void setContentType ( const QString & type )
 */
HB_FUNC( QT_QHTTPHEADER_SETCONTENTTYPE )
{
   hbqt_par_QHttpHeader( 1 )->setContentType( hbqt_par_QString( 2 ) );
}

/*
 * void setValue ( const QString & key, const QString & value )
 */
HB_FUNC( QT_QHTTPHEADER_SETVALUE )
{
   hbqt_par_QHttpHeader( 1 )->setValue( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) );
}

/*
 * virtual QString toString () const
 */
HB_FUNC( QT_QHTTPHEADER_TOSTRING )
{
   hb_retc( hbqt_par_QHttpHeader( 1 )->toString().toAscii().data() );
}

/*
 * QString value ( const QString & key ) const
 */
HB_FUNC( QT_QHTTPHEADER_VALUE )
{
   hb_retc( hbqt_par_QHttpHeader( 1 )->value( hbqt_par_QString( 2 ) ).toAscii().data() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
