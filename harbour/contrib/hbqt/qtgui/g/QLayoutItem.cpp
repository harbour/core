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
 *  Constructed[ 17/17 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QLayoutItem>


/*
 * QLayoutItem ( Qt::Alignment alignment = 0 )
 * virtual ~QLayoutItem ()
 */

typedef struct
{
   QLayoutItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QLayoutItem;

HBQT_GC_FUNC( hbqt_gcRelease_QLayoutItem )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QLayoutItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QLayoutItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QLayoutItem;
   p->type = HBQT_TYPE_QLayoutItem;

   return p;
}

HB_FUNC( QT_QLAYOUTITEM )
{

}

/* Qt::Alignment alignment () const */
HB_FUNC( QT_QLAYOUTITEM_ALIGNMENT )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->alignment() );
}

/* QSizePolicy::ControlTypes controlTypes () const */
HB_FUNC( QT_QLAYOUTITEM_CONTROLTYPES )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retni( ( QSizePolicy::ControlTypes ) ( p )->controlTypes() );
}

/* virtual Qt::Orientations expandingDirections () const = 0 */
HB_FUNC( QT_QLAYOUTITEM_EXPANDINGDIRECTIONS )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retni( ( Qt::Orientations ) ( p )->expandingDirections() );
}

/* virtual QRect geometry () const = 0 */
HB_FUNC( QT_QLAYOUTITEM_GEOMETRY )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->geometry() ), true ) );
}

/* virtual bool hasHeightForWidth () const */
HB_FUNC( QT_QLAYOUTITEM_HASHEIGHTFORWIDTH )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retl( ( p )->hasHeightForWidth() );
}

/* virtual int heightForWidth ( int w ) const */
HB_FUNC( QT_QLAYOUTITEM_HEIGHTFORWIDTH )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retni( ( p )->heightForWidth( hb_parni( 2 ) ) );
}

/* virtual void invalidate () */
HB_FUNC( QT_QLAYOUTITEM_INVALIDATE )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      ( p )->invalidate();
}

/* virtual bool isEmpty () const = 0 */
HB_FUNC( QT_QLAYOUTITEM_ISEMPTY )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retl( ( p )->isEmpty() );
}

/* virtual QLayout * layout () */
HB_FUNC( QT_QLAYOUTITEM_LAYOUT )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLayout( ( p )->layout(), false ) );
}

/* virtual QSize maximumSize () const = 0 */
HB_FUNC( QT_QLAYOUTITEM_MAXIMUMSIZE )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->maximumSize() ), true ) );
}

/* virtual int minimumHeightForWidth ( int w ) const */
HB_FUNC( QT_QLAYOUTITEM_MINIMUMHEIGHTFORWIDTH )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retni( ( p )->minimumHeightForWidth( hb_parni( 2 ) ) );
}

/* virtual QSize minimumSize () const = 0 */
HB_FUNC( QT_QLAYOUTITEM_MINIMUMSIZE )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->minimumSize() ), true ) );
}

/* void setAlignment ( Qt::Alignment alignment ) */
HB_FUNC( QT_QLAYOUTITEM_SETALIGNMENT )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      ( p )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
}

/* virtual void setGeometry ( const QRect & r ) = 0 */
HB_FUNC( QT_QLAYOUTITEM_SETGEOMETRY )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      ( p )->setGeometry( *hbqt_par_QRect( 2 ) );
}

/* virtual QSize sizeHint () const = 0 */
HB_FUNC( QT_QLAYOUTITEM_SIZEHINT )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint() ), true ) );
}

/* virtual QSpacerItem * spacerItem () */
HB_FUNC( QT_QLAYOUTITEM_SPACERITEM )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSpacerItem( ( p )->spacerItem(), false ) );
}

/* virtual QWidget * widget () */
HB_FUNC( QT_QLAYOUTITEM_WIDGET )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget(), false ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
