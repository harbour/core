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
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 10/10 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //virtual void draw ( QPainter * painter, const PaintContext & context ) = 0
 *  //QTextObjectInterface * handlerForObject ( int objectType ) const
 */

#include <QtCore/QPointer>

#include <QtGui/QAbstractTextDocumentLayout>


/* QAbstractTextDocumentLayout ( QTextDocument * document )
 *
 */

typedef struct
{
   QPointer< QAbstractTextDocumentLayout > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QAbstractTextDocumentLayout;

HBQT_GC_FUNC( hbqt_gcRelease_QAbstractTextDocumentLayout )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QAbstractTextDocumentLayout( void * pObj, bool bNew )
{
   HBQT_GC_T_QAbstractTextDocumentLayout * p = ( HBQT_GC_T_QAbstractTextDocumentLayout * ) hb_gcAllocate( sizeof( HBQT_GC_T_QAbstractTextDocumentLayout ), hbqt_gcFuncs() );

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
   // __HB_RETPTRGC__( new QAbstractTextDocumentLayout( hbqt_par_QTextDocument( 1 ) ) );
}

/*
 * QString anchorAt ( const QPointF & position ) const
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_ANCHORAT )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->anchorAt( *hbqt_par_QPointF( 2 ) ).toUtf8().data() );
   }
}

/*
 * virtual QRectF blockBoundingRect ( const QTextBlock & block ) const = 0
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_BLOCKBOUNDINGRECT )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->blockBoundingRect( *hbqt_par_QTextBlock( 2 ) ) ), true ) );
   }
}

/*
 * QTextDocument * document () const
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_DOCUMENT )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextDocument( ( p )->document(), false ) );
   }
}

/*
 * virtual QSizeF documentSize () const = 0
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_DOCUMENTSIZE )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->documentSize() ), true ) );
   }
}

/*
 * virtual QRectF frameBoundingRect ( QTextFrame * frame ) const = 0
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_FRAMEBOUNDINGRECT )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->frameBoundingRect( hbqt_par_QTextFrame( 2 ) ) ), true ) );
   }
}

/*
 * virtual int hitTest ( const QPointF & point, Qt::HitTestAccuracy accuracy ) const = 0
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_HITTEST )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
   {
      hb_retni( ( p )->hitTest( *hbqt_par_QPointF( 2 ), ( Qt::HitTestAccuracy ) hb_parni( 3 ) ) );
   }
}

/*
 * virtual int pageCount () const = 0
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_PAGECOUNT )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
   {
      hb_retni( ( p )->pageCount() );
   }
}

/*
 * QPaintDevice * paintDevice () const
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_PAINTDEVICE )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPaintDevice( ( p )->paintDevice(), false ) );
   }
}

/*
 * void registerHandler ( int objectType, QObject * component )
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_REGISTERHANDLER )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
   {
      ( p )->registerHandler( hb_parni( 2 ), hbqt_par_QObject( 3 ) );
   }
}

/*
 * void setPaintDevice ( QPaintDevice * device )
 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_SETPAINTDEVICE )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
   {
      ( p )->setPaintDevice( hbqt_par_QPaintDevice( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
