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

#include <QtCore/QPointer>

#include <QtGui/QTextDocumentWriter>


/*
 * QTextDocumentWriter ()
 * QTextDocumentWriter ( QIODevice * device, const QByteArray & format )
 * QTextDocumentWriter ( const QString & fileName, const QByteArray & format = QByteArray() )
 * ~QTextDocumentWriter ()
 */

QT_G_FUNC( release_QTextDocumentWriter )
{
#if defined(__debug__)
   hb_snprintf( str, sizeof(str), "release_QTextDocumentWriter" );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      delete ( ( QTextDocumentWriter * ) ph );
      ph = NULL;
   }
}

HB_FUNC( QT_QTEXTDOCUMENTWRITER )
{
   void * pObj = NULL;

   pObj = ( QTextDocumentWriter* ) new QTextDocumentWriter() ;

   hb_retptr( pObj );
}
/*
 * QTextCodec * codec () const
 */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_CODEC )
{
   hb_retptr( ( QTextCodec* ) hbqt_par_QTextDocumentWriter( 1 )->codec() );
}

/*
 * QIODevice * device () const
 */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_DEVICE )
{
   hb_retptr( ( QIODevice* ) hbqt_par_QTextDocumentWriter( 1 )->device() );
}

/*
 * QString fileName () const
 */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_FILENAME )
{
   hb_retc( hbqt_par_QTextDocumentWriter( 1 )->fileName().toAscii().data() );
}

/*
 * QByteArray format () const
 */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_FORMAT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QByteArray( hbqt_par_QTextDocumentWriter( 1 )->format() ), release_QByteArray ) );
}

/*
 * void setCodec ( QTextCodec * codec )
 */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_SETCODEC )
{
   hbqt_par_QTextDocumentWriter( 1 )->setCodec( hbqt_par_QTextCodec( 2 ) );
}

/*
 * void setDevice ( QIODevice * device )
 */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_SETDEVICE )
{
   hbqt_par_QTextDocumentWriter( 1 )->setDevice( hbqt_par_QIODevice( 2 ) );
}

/*
 * void setFileName ( const QString & fileName )
 */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_SETFILENAME )
{
   hbqt_par_QTextDocumentWriter( 1 )->setFileName( hbqt_par_QString( 2 ) );
}

/*
 * void setFormat ( const QByteArray & format )
 */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_SETFORMAT )
{
   hbqt_par_QTextDocumentWriter( 1 )->setFormat( *hbqt_par_QByteArray( 2 ) );
}

/*
 * bool write ( const QTextDocument * document )
 */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_WRITE )
{
   hb_retl( hbqt_par_QTextDocumentWriter( 1 )->write( hbqt_par_QTextDocument( 2 ) ) );
}

/*
 * bool write ( const QTextDocumentFragment & fragment )
 */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_WRITE_1 )
{
   hb_retl( hbqt_par_QTextDocumentWriter( 1 )->write( *hbqt_par_QTextDocumentFragment( 2 ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
