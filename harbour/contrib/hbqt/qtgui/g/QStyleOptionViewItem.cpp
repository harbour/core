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
 *  enum Position { Left, Right, Top, Bottom }
 *  enum StyleOptionType { Type }
 *  enum StyleOptionVersion { Version }
 */

/*
 *  Constructed[ 7/7 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOptionViewItem>


/*
 * QStyleOptionViewItem ()
 * QStyleOptionViewItem ( const QStyleOptionViewItem & other )
 */

typedef struct
{
   QStyleOptionViewItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionViewItem;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionViewItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QStyleOptionViewItem * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QStyleOptionViewItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionViewItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionViewItem;
   p->type = HBQT_TYPE_QStyleOptionViewItem;

   return p;
}

HB_FUNC( QT_QSTYLEOPTIONVIEWITEM )
{
   QStyleOptionViewItem * pObj = NULL;

   pObj = new QStyleOptionViewItem() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionViewItem( ( void * ) pObj, true ) );
}

/* Qt::Alignment decorationAlignment */
HB_FUNC( QT_QSTYLEOPTIONVIEWITEM_DECORATIONALIGNMENT )
{
   QStyleOptionViewItem * p = hbqt_par_QStyleOptionViewItem( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->decorationAlignment );
}

/* Position decorationPosition */
HB_FUNC( QT_QSTYLEOPTIONVIEWITEM_DECORATIONPOSITION )
{
   QStyleOptionViewItem * p = hbqt_par_QStyleOptionViewItem( 1 );
   if( p )
      hb_retni( ( QStyleOptionViewItem::Position ) ( p )->decorationPosition );
}

/* QSize decorationSize */
HB_FUNC( QT_QSTYLEOPTIONVIEWITEM_DECORATIONSIZE )
{
   QStyleOptionViewItem * p = hbqt_par_QStyleOptionViewItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->decorationSize ), true ) );
}

/* Qt::Alignment displayAlignment */
HB_FUNC( QT_QSTYLEOPTIONVIEWITEM_DISPLAYALIGNMENT )
{
   QStyleOptionViewItem * p = hbqt_par_QStyleOptionViewItem( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->displayAlignment );
}

/* QFont font */
HB_FUNC( QT_QSTYLEOPTIONVIEWITEM_FONT )
{
   QStyleOptionViewItem * p = hbqt_par_QStyleOptionViewItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font ), true ) );
}

/* bool showDecorationSelected */
HB_FUNC( QT_QSTYLEOPTIONVIEWITEM_SHOWDECORATIONSELECTED )
{
   QStyleOptionViewItem * p = hbqt_par_QStyleOptionViewItem( 1 );
   if( p )
      hb_retl( ( p )->showDecorationSelected );
}

/* Qt::TextElideMode textElideMode */
HB_FUNC( QT_QSTYLEOPTIONVIEWITEM_TEXTELIDEMODE )
{
   QStyleOptionViewItem * p = hbqt_par_QStyleOptionViewItem( 1 );
   if( p )
      hb_retni( ( Qt::TextElideMode ) ( p )->textElideMode );
}


#endif /* #if QT_VERSION >= 0x040500 */
