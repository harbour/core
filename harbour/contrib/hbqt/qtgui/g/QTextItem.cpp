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
 *  enum RenderFlag { RightToLeft, Overline, Underline, StrikeOut }
 *  flags RenderFlags
 */

/*
 *  Constructed[ 6/6 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QTextItem>


/*
 * QTextItem ()
 *
 */

typedef struct
{
   QTextItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextItem;

HBQT_GC_FUNC( hbqt_gcRelease_QTextItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QTextItem * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextItem;
   p->type = HBQT_TYPE_QTextItem;

   return p;
}

HB_FUNC( QT_QTEXTITEM )
{
   QTextItem * pObj = NULL;

   pObj = new QTextItem() ;

   hb_retptrGC( hbqt_gcAllocate_QTextItem( ( void * ) pObj, true ) );
}

/* qreal ascent () const */
HB_FUNC( QT_QTEXTITEM_ASCENT )
{
   QTextItem * p = hbqt_par_QTextItem( 1 );
   if( p )
      hb_retnd( ( p )->ascent() );
}

/* qreal descent () const */
HB_FUNC( QT_QTEXTITEM_DESCENT )
{
   QTextItem * p = hbqt_par_QTextItem( 1 );
   if( p )
      hb_retnd( ( p )->descent() );
}

/* QFont font () const */
HB_FUNC( QT_QTEXTITEM_FONT )
{
   QTextItem * p = hbqt_par_QTextItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
}

/* RenderFlags renderFlags () const */
HB_FUNC( QT_QTEXTITEM_RENDERFLAGS )
{
   QTextItem * p = hbqt_par_QTextItem( 1 );
   if( p )
      hb_retni( ( QTextItem::RenderFlags ) ( p )->renderFlags() );
}

/* QString text () const */
HB_FUNC( QT_QTEXTITEM_TEXT )
{
   QTextItem * p = hbqt_par_QTextItem( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text().toUtf8().data() );
}

/* qreal width () const */
HB_FUNC( QT_QTEXTITEM_WIDTH )
{
   QTextItem * p = hbqt_par_QTextItem( 1 );
   if( p )
      hb_retnd( ( p )->width() );
}


#endif /* #if QT_VERSION >= 0x040500 */
