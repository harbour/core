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
 *  enum SizeConstraint { SetDefaultConstraint, SetFixedSize, SetMinimumSize, SetMaximumSize, SetMinAndMaxSize, SetNoConstraint }
 */

/*
 *  Constructed[ 29/29 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QLayout>


/*
 * QLayout ( QWidget * parent )
 * QLayout ()
 */

typedef struct
{
   QPointer< QLayout > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QLayout;

HBQT_GC_FUNC( hbqt_gcRelease_QLayout )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QLayout( void * pObj, bool bNew )
{
   HBQT_GC_T_QLayout * p = ( HBQT_GC_T_QLayout * ) hb_gcAllocate( sizeof( HBQT_GC_T_QLayout ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QLayout >( ( QLayout * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QLayout;
   p->type = HBQT_TYPE_QLayout;

   return p;
}

HB_FUNC( QT_QLAYOUT )
{

}

/* bool activate () */
HB_FUNC( QT_QLAYOUT_ACTIVATE )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retl( ( p )->activate() );
}

/* virtual void addItem ( QLayoutItem * item )    [*D=1*] */
HB_FUNC( QT_QLAYOUT_ADDITEM )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 2 );
      ( p )->addItem( hbqt_par_QLayoutItem( 2 ) );
   }
}

/* void addWidget ( QWidget * w )                 [*D=1*] */
HB_FUNC( QT_QLAYOUT_ADDWIDGET )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 2 );
      ( p )->addWidget( hbqt_par_QWidget( 2 ) );
   }
}

/* QRect contentsRect () const */
HB_FUNC( QT_QLAYOUT_CONTENTSRECT )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->contentsRect() ), true ) );
}

/* virtual int count () const = 0 */
HB_FUNC( QT_QLAYOUT_COUNT )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retni( ( p )->count() );
}

/* virtual Qt::Orientations expandingDirections () const */
HB_FUNC( QT_QLAYOUT_EXPANDINGDIRECTIONS )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retni( ( Qt::Orientations ) ( p )->expandingDirections() );
}

/* void getContentsMargins ( int * left, int * top, int * right, int * bottom ) const */
HB_FUNC( QT_QLAYOUT_GETCONTENTSMARGINS )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   int iLeft = 0;
   int iTop = 0;
   int iRight = 0;
   int iBottom = 0;

   if( p )
      ( p )->getContentsMargins( &iLeft, &iTop, &iRight, &iBottom );

   hb_storni( iLeft, 2 );
   hb_storni( iTop, 3 );
   hb_storni( iRight, 4 );
   hb_storni( iBottom, 5 );
}

/* virtual int indexOf ( QWidget * widget ) const */
HB_FUNC( QT_QLAYOUT_INDEXOF )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retni( ( p )->indexOf( hbqt_par_QWidget( 2 ) ) );
}

/* bool isEnabled () const */
HB_FUNC( QT_QLAYOUT_ISENABLED )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retl( ( p )->isEnabled() );
}

/* virtual QLayoutItem * itemAt ( int index ) const = 0 */
HB_FUNC( QT_QLAYOUT_ITEMAT )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLayoutItem( ( p )->itemAt( hb_parni( 2 ) ), false ) );
}

/* virtual QSize maximumSize () const */
HB_FUNC( QT_QLAYOUT_MAXIMUMSIZE )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->maximumSize() ), true ) );
}

/* QWidget * menuBar () const */
HB_FUNC( QT_QLAYOUT_MENUBAR )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->menuBar(), false ) );
}

/* virtual QSize minimumSize () const */
HB_FUNC( QT_QLAYOUT_MINIMUMSIZE )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->minimumSize() ), true ) );
}

/* QWidget * parentWidget () const */
HB_FUNC( QT_QLAYOUT_PARENTWIDGET )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->parentWidget(), false ) );
}

/* void removeItem ( QLayoutItem * item ) */
HB_FUNC( QT_QLAYOUT_REMOVEITEM )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      ( p )->removeItem( hbqt_par_QLayoutItem( 2 ) );
}

/* void removeWidget ( QWidget * widget ) */
HB_FUNC( QT_QLAYOUT_REMOVEWIDGET )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      ( p )->removeWidget( hbqt_par_QWidget( 2 ) );
}

/* bool setAlignment ( QWidget * w, Qt::Alignment alignment ) */
HB_FUNC( QT_QLAYOUT_SETALIGNMENT )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retl( ( p )->setAlignment( hbqt_par_QWidget( 2 ), ( Qt::Alignment ) hb_parni( 3 ) ) );
}

/* void setAlignment ( Qt::Alignment alignment ) */
HB_FUNC( QT_QLAYOUT_SETALIGNMENT_1 )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      ( p )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
}

/* bool setAlignment ( QLayout * l, Qt::Alignment alignment ) */
HB_FUNC( QT_QLAYOUT_SETALIGNMENT_2 )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retl( ( p )->setAlignment( hbqt_par_QLayout( 2 ), ( Qt::Alignment ) hb_parni( 3 ) ) );
}

/* void setContentsMargins ( int left, int top, int right, int bottom ) */
HB_FUNC( QT_QLAYOUT_SETCONTENTSMARGINS )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      ( p )->setContentsMargins( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/* void setEnabled ( bool enable ) */
HB_FUNC( QT_QLAYOUT_SETENABLED )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      ( p )->setEnabled( hb_parl( 2 ) );
}

/* void setMenuBar ( QWidget * widget ) */
HB_FUNC( QT_QLAYOUT_SETMENUBAR )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      ( p )->setMenuBar( hbqt_par_QWidget( 2 ) );
}

/* void setSizeConstraint ( SizeConstraint ) */
HB_FUNC( QT_QLAYOUT_SETSIZECONSTRAINT )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      ( p )->setSizeConstraint( ( QLayout::SizeConstraint ) hb_parni( 2 ) );
}

/* void setSpacing ( int ) */
HB_FUNC( QT_QLAYOUT_SETSPACING )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      ( p )->setSpacing( hb_parni( 2 ) );
}

/* SizeConstraint sizeConstraint () const */
HB_FUNC( QT_QLAYOUT_SIZECONSTRAINT )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retni( ( QLayout::SizeConstraint ) ( p )->sizeConstraint() );
}

/* int spacing () const */
HB_FUNC( QT_QLAYOUT_SPACING )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retni( ( p )->spacing() );
}

/* virtual QLayoutItem * takeAt ( int index ) = 0 */
HB_FUNC( QT_QLAYOUT_TAKEAT )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLayoutItem( ( p )->takeAt( hb_parni( 2 ) ), false ) );
}

/* void update () */
HB_FUNC( QT_QLAYOUT_UPDATE )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      ( p )->update();
}

/* QSize closestAcceptableSize ( const QWidget * widget, const QSize & size ) */
HB_FUNC( QT_QLAYOUT_CLOSESTACCEPTABLESIZE )
{
   QLayout * p = hbqt_par_QLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->closestAcceptableSize( hbqt_par_QWidget( 2 ), *hbqt_par_QSize( 3 ) ) ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
