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
 *  Constructed[ 13/13 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QTextInlineObject>


/*
 * QTextInlineObject ( int i, QTextEngine * e )
 *
 */

typedef struct
{
   QTextInlineObject * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextInlineObject;

HBQT_GC_FUNC( hbqt_gcRelease_QTextInlineObject )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QTextInlineObject( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextInlineObject * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextInlineObject;
   p->type = HBQT_TYPE_QTextInlineObject;

   return p;
}

HB_FUNC( QT_QTEXTINLINEOBJECT )
{
   // __HB_RETPTRGC__( new QTextInlineObject( hb_parni( 1 ), hbqt_par_QTextEngine( 2 ) ) );
}

/* qreal ascent () const */
HB_FUNC( QT_QTEXTINLINEOBJECT_ASCENT )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      hb_retnd( ( p )->ascent() );
}

/* qreal descent () const */
HB_FUNC( QT_QTEXTINLINEOBJECT_DESCENT )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      hb_retnd( ( p )->descent() );
}

/* QTextFormat format () const */
HB_FUNC( QT_QTEXTINLINEOBJECT_FORMAT )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextFormat( new QTextFormat( ( p )->format() ), true ) );
}

/* int formatIndex () const */
HB_FUNC( QT_QTEXTINLINEOBJECT_FORMATINDEX )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      hb_retni( ( p )->formatIndex() );
}

/* qreal height () const */
HB_FUNC( QT_QTEXTINLINEOBJECT_HEIGHT )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      hb_retnd( ( p )->height() );
}

/* bool isValid () const */
HB_FUNC( QT_QTEXTINLINEOBJECT_ISVALID )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* QRectF rect () const */
HB_FUNC( QT_QTEXTINLINEOBJECT_RECT )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->rect() ), true ) );
}

/* void setAscent ( qreal a ) */
HB_FUNC( QT_QTEXTINLINEOBJECT_SETASCENT )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      ( p )->setAscent( hb_parnd( 2 ) );
}

/* void setDescent ( qreal d ) */
HB_FUNC( QT_QTEXTINLINEOBJECT_SETDESCENT )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      ( p )->setDescent( hb_parnd( 2 ) );
}

/* void setWidth ( qreal w ) */
HB_FUNC( QT_QTEXTINLINEOBJECT_SETWIDTH )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      ( p )->setWidth( hb_parnd( 2 ) );
}

/* Qt::LayoutDirection textDirection () const */
HB_FUNC( QT_QTEXTINLINEOBJECT_TEXTDIRECTION )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      hb_retni( ( Qt::LayoutDirection ) ( p )->textDirection() );
}

/* int textPosition () const */
HB_FUNC( QT_QTEXTINLINEOBJECT_TEXTPOSITION )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      hb_retni( ( p )->textPosition() );
}

/* qreal width () const */
HB_FUNC( QT_QTEXTINLINEOBJECT_WIDTH )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      hb_retnd( ( p )->width() );
}


#endif /* #if QT_VERSION >= 0x040500 */
