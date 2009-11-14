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

#include <QtGui/QAbstractTextDocumentLayout>


/* QAbstractTextDocumentLayout ( QTextDocument * document )
 *
 */

QT_G_FUNC( release_QAbstractTextDocumentLayout )
{
   HB_SYMBOL_UNUSED( Cargo );
}

HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT )
{
}
/*
 * QString anchorAt ( const QPointF & position ) const
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_ANCHORAT )
{
   hb_retc( hbqt_par_QAbstractTextDocumentLayout( 1 )->anchorAt( *hbqt_par_QPointF( 2 ) ).toAscii().data() );
}

/*
 * virtual QRectF blockBoundingRect ( const QTextBlock & block ) const = 0
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_BLOCKBOUNDINGRECT )
{
   hb_retptrGC( gcAllocate_QRectF( new QRectF( hbqt_par_QAbstractTextDocumentLayout( 1 )->blockBoundingRect( *hbqt_par_QTextBlock( 2 ) ) ) ) );
}

/*
 * QTextDocument * document () const
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_DOCUMENT )
{
   hb_retptr( ( QTextDocument* ) hbqt_par_QAbstractTextDocumentLayout( 1 )->document() );
}

/*
 * virtual QSizeF documentSize () const = 0
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_DOCUMENTSIZE )
{
   hb_retptrGC( gcAllocate_QSizeF( new QSizeF( hbqt_par_QAbstractTextDocumentLayout( 1 )->documentSize() ) ) );
}

/*
 * virtual QRectF frameBoundingRect ( QTextFrame * frame ) const = 0
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_FRAMEBOUNDINGRECT )
{
   hb_retptrGC( gcAllocate_QRectF( new QRectF( hbqt_par_QAbstractTextDocumentLayout( 1 )->frameBoundingRect( hbqt_par_QTextFrame( 2 ) ) ) ) );
}

/*
 * QTextObjectInterface * handlerForObject ( int objectType ) const
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_HANDLERFOROBJECT )
{
   hb_retptr( ( QTextObjectInterface* ) hbqt_par_QAbstractTextDocumentLayout( 1 )->handlerForObject( hb_parni( 2 ) ) );
}

/*
 * virtual int hitTest ( const QPointF & point, Qt::HitTestAccuracy accuracy ) const = 0
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_HITTEST )
{
   hb_retni( hbqt_par_QAbstractTextDocumentLayout( 1 )->hitTest( *hbqt_par_QPointF( 2 ), ( Qt::HitTestAccuracy ) hb_parni( 3 ) ) );
}

/*
 * virtual int pageCount () const = 0
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_PAGECOUNT )
{
   hb_retni( hbqt_par_QAbstractTextDocumentLayout( 1 )->pageCount() );
}

/*
 * QPaintDevice * paintDevice () const
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_PAINTDEVICE )
{
   hb_retptr( ( QPaintDevice* ) hbqt_par_QAbstractTextDocumentLayout( 1 )->paintDevice() );
}

/*
 * void registerHandler ( int objectType, QObject * component )
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_REGISTERHANDLER )
{
   hbqt_par_QAbstractTextDocumentLayout( 1 )->registerHandler( hb_parni( 2 ), hbqt_par_QObject( 3 ) );
}

/*
 * void setPaintDevice ( QPaintDevice * device )
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_SETPAINTDEVICE )
{
   hbqt_par_QAbstractTextDocumentLayout( 1 )->setPaintDevice( hbqt_par_QPaintDevice( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
