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
 *  Constructed[ 19/21 [ 90.48% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  void setUrls ( const QList<QUrl> & urls )
 *  QList<QUrl> urls () const
 */

#include <QtCore/QPointer>

#include <QtCore/QMimeData>
#include <QtCore/QStringList>


/* QMimeData ()
 * ~QMimeData ()
 */

QT_G_FUNC( release_QMimeData )
{
#if defined(__debug__)
   hb_snprintf( str, sizeof(str), "release_QMimeData" );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      const QMetaObject * m = ( ( QObject * ) ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         delete ( ( QMimeData * ) ph );
         ph = NULL;
      }
      else
      {
#if defined(__debug__)
   hb_snprintf( str, sizeof(str), "  Object Name Missing: QMimeData" );  OutputDebugString( str );
#endif
      }
   }
}

HB_FUNC( QT_QMIMEDATA )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );
   QPointer< QMimeData > pObj = NULL;

   pObj = new QMimeData() ;

   p->ph = pObj;
   p->func = release_QMimeData;

   hb_retptrGC( p );
}
/*
 * void clear ()
 */
HB_FUNC( QT_QMIMEDATA_CLEAR )
{
   hbqt_par_QMimeData( 1 )->clear();
}

/*
 * QVariant colorData () const
 */
HB_FUNC( QT_QMIMEDATA_COLORDATA )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QVariant( hbqt_par_QMimeData( 1 )->colorData() ), release_QVariant ) );
}

/*
 * QByteArray data ( const QString & mimeType ) const
 */
HB_FUNC( QT_QMIMEDATA_DATA )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QByteArray( hbqt_par_QMimeData( 1 )->data( hbqt_par_QString( 2 ) ) ), release_QByteArray ) );
}

/*
 * virtual QStringList formats () const
 */
HB_FUNC( QT_QMIMEDATA_FORMATS )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QStringList( hbqt_par_QMimeData( 1 )->formats() ), release_QStringList ) );
}

/*
 * bool hasColor () const
 */
HB_FUNC( QT_QMIMEDATA_HASCOLOR )
{
   hb_retl( hbqt_par_QMimeData( 1 )->hasColor() );
}

/*
 * virtual bool hasFormat ( const QString & mimeType ) const
 */
HB_FUNC( QT_QMIMEDATA_HASFORMAT )
{
   hb_retl( hbqt_par_QMimeData( 1 )->hasFormat( hbqt_par_QString( 2 ) ) );
}

/*
 * bool hasHtml () const
 */
HB_FUNC( QT_QMIMEDATA_HASHTML )
{
   hb_retl( hbqt_par_QMimeData( 1 )->hasHtml() );
}

/*
 * bool hasImage () const
 */
HB_FUNC( QT_QMIMEDATA_HASIMAGE )
{
   hb_retl( hbqt_par_QMimeData( 1 )->hasImage() );
}

/*
 * bool hasText () const
 */
HB_FUNC( QT_QMIMEDATA_HASTEXT )
{
   hb_retl( hbqt_par_QMimeData( 1 )->hasText() );
}

/*
 * bool hasUrls () const
 */
HB_FUNC( QT_QMIMEDATA_HASURLS )
{
   hb_retl( hbqt_par_QMimeData( 1 )->hasUrls() );
}

/*
 * QString html () const
 */
HB_FUNC( QT_QMIMEDATA_HTML )
{
   hb_retc( hbqt_par_QMimeData( 1 )->html().toAscii().data() );
}

/*
 * QVariant imageData () const
 */
HB_FUNC( QT_QMIMEDATA_IMAGEDATA )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QVariant( hbqt_par_QMimeData( 1 )->imageData() ), release_QVariant ) );
}

/*
 * void removeFormat ( const QString & mimeType )
 */
HB_FUNC( QT_QMIMEDATA_REMOVEFORMAT )
{
   hbqt_par_QMimeData( 1 )->removeFormat( hbqt_par_QString( 2 ) );
}

/*
 * void setColorData ( const QVariant & color )
 */
HB_FUNC( QT_QMIMEDATA_SETCOLORDATA )
{
   hbqt_par_QMimeData( 1 )->setColorData( *hbqt_par_QVariant( 2 ) );
}

/*
 * void setData ( const QString & mimeType, const QByteArray & data )
 */
HB_FUNC( QT_QMIMEDATA_SETDATA )
{
   hbqt_par_QMimeData( 1 )->setData( hbqt_par_QString( 2 ), *hbqt_par_QByteArray( 3 ) );
}

/*
 * void setHtml ( const QString & html )
 */
HB_FUNC( QT_QMIMEDATA_SETHTML )
{
   hbqt_par_QMimeData( 1 )->setHtml( hbqt_par_QString( 2 ) );
}

/*
 * void setImageData ( const QVariant & image )
 */
HB_FUNC( QT_QMIMEDATA_SETIMAGEDATA )
{
   hbqt_par_QMimeData( 1 )->setImageData( *hbqt_par_QVariant( 2 ) );
}

/*
 * void setText ( const QString & text )
 */
HB_FUNC( QT_QMIMEDATA_SETTEXT )
{
   hbqt_par_QMimeData( 1 )->setText( hbqt_par_QString( 2 ) );
}

/*
 * QString text () const
 */
HB_FUNC( QT_QMIMEDATA_TEXT )
{
   hb_retc( hbqt_par_QMimeData( 1 )->text().toAscii().data() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
