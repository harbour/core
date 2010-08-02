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
#include "hbqtgui_garbage.h"
#include "hbqtgui.h"
#include "hbqtcore_garbage.h"
#include "hbqtcore.h"

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

typedef struct
{
   QTextDocumentWriter * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QTextDocumentWriter;

QT_G_FUNC( hbqt_gcRelease_QTextDocumentWriter )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QTextDocumentWriter   /.\\", p->ph ) );
         delete ( ( QTextDocumentWriter * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QTextDocumentWriter   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTextDocumentWriter    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTextDocumentWriter    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextDocumentWriter( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QTextDocumentWriter * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextDocumentWriter;
   p->type = HBQT_TYPE_QTextDocumentWriter;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTextDocumentWriter", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTextDocumentWriter", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTDOCUMENTWRITER )
{
   QTextDocumentWriter * pObj = NULL;

   pObj =  new QTextDocumentWriter() ;

   hb_retptrGC( hbqt_gcAllocate_QTextDocumentWriter( ( void * ) pObj, true ) );
}

/*
 * QTextCodec * codec () const
 */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_CODEC )
{
   QTextDocumentWriter * p = hbqt_par_QTextDocumentWriter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codec(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENTWRITER_CODEC FP=hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codec(), false ) ); p is NULL" ) );
   }
}

/*
 * QIODevice * device () const
 */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_DEVICE )
{
   QTextDocumentWriter * p = hbqt_par_QTextDocumentWriter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIODevice( ( p )->device(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENTWRITER_DEVICE FP=hb_retptrGC( hbqt_gcAllocate_QIODevice( ( p )->device(), false ) ); p is NULL" ) );
   }
}

/*
 * QString fileName () const
 */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_FILENAME )
{
   QTextDocumentWriter * p = hbqt_par_QTextDocumentWriter( 1 );
   if( p )
      hb_retc( ( p )->fileName().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENTWRITER_FILENAME FP=hb_retc( ( p )->fileName().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QByteArray format () const
 */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_FORMAT )
{
   QTextDocumentWriter * p = hbqt_par_QTextDocumentWriter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->format() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENTWRITER_FORMAT FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->format() ), true ) ); p is NULL" ) );
   }
}

/*
 * void setCodec ( QTextCodec * codec )
 */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_SETCODEC )
{
   QTextDocumentWriter * p = hbqt_par_QTextDocumentWriter( 1 );
   if( p )
      ( p )->setCodec( hbqt_par_QTextCodec( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENTWRITER_SETCODEC FP=( p )->setCodec( hbqt_par_QTextCodec( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDevice ( QIODevice * device )
 */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_SETDEVICE )
{
   QTextDocumentWriter * p = hbqt_par_QTextDocumentWriter( 1 );
   if( p )
      ( p )->setDevice( hbqt_par_QIODevice( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENTWRITER_SETDEVICE FP=( p )->setDevice( hbqt_par_QIODevice( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFileName ( const QString & fileName )
 */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_SETFILENAME )
{
   QTextDocumentWriter * p = hbqt_par_QTextDocumentWriter( 1 );
   if( p )
      ( p )->setFileName( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENTWRITER_SETFILENAME FP=( p )->setFileName( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFormat ( const QByteArray & format )
 */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_SETFORMAT )
{
   QTextDocumentWriter * p = hbqt_par_QTextDocumentWriter( 1 );
   if( p )
      ( p )->setFormat( *hbqt_par_QByteArray( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENTWRITER_SETFORMAT FP=( p )->setFormat( *hbqt_par_QByteArray( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool write ( const QTextDocument * document )
 */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_WRITE )
{
   QTextDocumentWriter * p = hbqt_par_QTextDocumentWriter( 1 );
   if( p )
      hb_retl( ( p )->write( hbqt_par_QTextDocument( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENTWRITER_WRITE FP=hb_retl( ( p )->write( hbqt_par_QTextDocument( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool write ( const QTextDocumentFragment & fragment )
 */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_WRITE_1 )
{
   QTextDocumentWriter * p = hbqt_par_QTextDocumentWriter( 1 );
   if( p )
      hb_retl( ( p )->write( *hbqt_par_QTextDocumentFragment( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENTWRITER_WRITE_1 FP=hb_retl( ( p )->write( *hbqt_par_QTextDocumentFragment( 2 ) ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
