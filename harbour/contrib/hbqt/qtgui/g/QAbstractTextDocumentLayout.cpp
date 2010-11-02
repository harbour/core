/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */

#include "hbqtcore.h"
#include "hbqtgui.h"

#if QT_VERSION >= 0x040500

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
      p->ph = NULL;
}

void * hbqt_gcAllocate_QAbstractTextDocumentLayout( void * pObj, bool bNew )
{
   HBQT_GC_T_QAbstractTextDocumentLayout * p = ( HBQT_GC_T_QAbstractTextDocumentLayout * ) hb_gcAllocate( sizeof( HBQT_GC_T_QAbstractTextDocumentLayout ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QAbstractTextDocumentLayout >( ( QAbstractTextDocumentLayout * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QAbstractTextDocumentLayout;
   p->type = HBQT_TYPE_QAbstractTextDocumentLayout;

   return p;
}

HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT )
{
   // __HB_RETPTRGC__( new QAbstractTextDocumentLayout( hbqt_par_QTextDocument( 1 ) ) );
}

/* QString anchorAt ( const QPointF & position ) const */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_ANCHORAT )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
      hb_retstr_utf8( ( p )->anchorAt( *hbqt_par_QPointF( 2 ) ).toUtf8().data() );
}

/* virtual QRectF blockBoundingRect ( const QTextBlock & block ) const = 0 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_BLOCKBOUNDINGRECT )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->blockBoundingRect( *hbqt_par_QTextBlock( 2 ) ) ), true ) );
}

/* QTextDocument * document () const */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_DOCUMENT )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextDocument( ( p )->document(), false ) );
}

/* virtual QSizeF documentSize () const = 0 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_DOCUMENTSIZE )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->documentSize() ), true ) );
}

/* virtual QRectF frameBoundingRect ( QTextFrame * frame ) const = 0 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_FRAMEBOUNDINGRECT )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->frameBoundingRect( hbqt_par_QTextFrame( 2 ) ) ), true ) );
}

/* virtual int hitTest ( const QPointF & point, Qt::HitTestAccuracy accuracy ) const = 0 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_HITTEST )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
      hb_retni( ( p )->hitTest( *hbqt_par_QPointF( 2 ), ( Qt::HitTestAccuracy ) hb_parni( 3 ) ) );
}

/* virtual int pageCount () const = 0 */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_PAGECOUNT )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
      hb_retni( ( p )->pageCount() );
}

/* QPaintDevice * paintDevice () const */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_PAINTDEVICE )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPaintDevice( ( p )->paintDevice(), false ) );
}

/* void registerHandler ( int objectType, QObject * component ) */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_REGISTERHANDLER )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
      ( p )->registerHandler( hb_parni( 2 ), hbqt_par_QObject( 3 ) );
}

/* void setPaintDevice ( QPaintDevice * device ) */
HB_FUNC( QT_QABSTRACTTEXTDOCUMENTLAYOUT_SETPAINTDEVICE )
{
   QAbstractTextDocumentLayout * p = hbqt_par_QAbstractTextDocumentLayout( 1 );
   if( p )
      ( p )->setPaintDevice( hbqt_par_QPaintDevice( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
