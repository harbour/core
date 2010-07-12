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

#include <QtCore/QPointer>

#include <QtGui/QAbstractTextDocumentLayout>


/* QAbstractTextDocumentLayout ( QTextDocument * document )
 *
 */

typedef struct
{
   QPointer< QAbstractTextDocumentLayout > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QAbstractTextDocumentLayout;

QT_G_FUNC( hbqt_gcRelease_QAbstractTextDocumentLayout )
{
   HB_SYMBOL_UNUSED( Cargo );
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QAbstractTextDocumentLayout( void * pObj, bool bNew )
{
   QGC_POINTER_QAbstractTextDocumentLayout * p = ( QGC_POINTER_QAbstractTextDocumentLayout * ) hb_gcAllocate( sizeof( QGC_POINTER_QAbstractTextDocumentLayout ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QAbstractTextDocumentLayout >( ( QAbstractTextDocumentLayout * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QAbstractTextDocumentLayout;
   p->type = HBQT_TYPE_QAbstractTextDocumentLayout;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QAbstractTextDocumentLayout  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QAbstractTextDocumentLayout", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT )
{
   // hb_retptr( new QAbstractTextDocumentLayout( hbqt_par_QTextDocument( 1 ) ) );
}

/*
 * QString anchorAt ( const QPointF & position ) const
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_ANCHORAT )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
      hb_retc( ( p )->anchorAt( *hbqt_par_QPointF( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTTEXTDOCUMENTLAYOUT_ANCHORAT FP=hb_retc( ( p )->anchorAt( *hbqt_par_QPointF( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * virtual QRectF blockBoundingRect ( const QTextBlock & block ) const = 0
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_BLOCKBOUNDINGRECT )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->blockBoundingRect( *hbqt_par_QTextBlock( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTTEXTDOCUMENTLAYOUT_BLOCKBOUNDINGRECT FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->blockBoundingRect( *hbqt_par_QTextBlock( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QTextDocument * document () const
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_DOCUMENT )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextDocument( ( p )->document(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTTEXTDOCUMENTLAYOUT_DOCUMENT FP=hb_retptrGC( hbqt_gcAllocate_QTextDocument( ( p )->document(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QSizeF documentSize () const = 0
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_DOCUMENTSIZE )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->documentSize() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTTEXTDOCUMENTLAYOUT_DOCUMENTSIZE FP=hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->documentSize() ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual QRectF frameBoundingRect ( QTextFrame * frame ) const = 0
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_FRAMEBOUNDINGRECT )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->frameBoundingRect( hbqt_par_QTextFrame( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTTEXTDOCUMENTLAYOUT_FRAMEBOUNDINGRECT FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->frameBoundingRect( hbqt_par_QTextFrame( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual int hitTest ( const QPointF & point, Qt::HitTestAccuracy accuracy ) const = 0
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_HITTEST )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
      hb_retni( ( p )->hitTest( *hbqt_par_QPointF( 2 ), ( Qt::HitTestAccuracy ) hb_parni( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTTEXTDOCUMENTLAYOUT_HITTEST FP=hb_retni( ( p )->hitTest( *hbqt_par_QPointF( 2 ), ( Qt::HitTestAccuracy ) hb_parni( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual int pageCount () const = 0
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_PAGECOUNT )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
      hb_retni( ( p )->pageCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTTEXTDOCUMENTLAYOUT_PAGECOUNT FP=hb_retni( ( p )->pageCount() ); p is NULL" ) );
   }
}

/*
 * QPaintDevice * paintDevice () const
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_PAINTDEVICE )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPaintDevice( ( p )->paintDevice(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTTEXTDOCUMENTLAYOUT_PAINTDEVICE FP=hb_retptrGC( hbqt_gcAllocate_QPaintDevice( ( p )->paintDevice(), false ) ); p is NULL" ) );
   }
}

/*
 * void registerHandler ( int objectType, QObject * component )
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_REGISTERHANDLER )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
      ( p )->registerHandler( hb_parni( 2 ), hbqt_par_QObject( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTTEXTDOCUMENTLAYOUT_REGISTERHANDLER FP=( p )->registerHandler( hb_parni( 2 ), hbqt_par_QObject( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setPaintDevice ( QPaintDevice * device )
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_SETPAINTDEVICE )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
      ( p )->setPaintDevice( hbqt_par_QPaintDevice( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTTEXTDOCUMENTLAYOUT_SETPAINTDEVICE FP=( p )->setPaintDevice( hbqt_par_QPaintDevice( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
