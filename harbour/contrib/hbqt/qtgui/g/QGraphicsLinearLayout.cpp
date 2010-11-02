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
 *  Constructed[ 16/16 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QGraphicsLinearLayout>


/*
 * QGraphicsLinearLayout ( QGraphicsLayoutItem * parent = 0 )
 * QGraphicsLinearLayout ( Qt::Orientation orientation, QGraphicsLayoutItem * parent = 0 )
 * virtual ~QGraphicsLinearLayout ()
 */

typedef struct
{
   QGraphicsLinearLayout * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsLinearLayout;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsLinearLayout )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QGraphicsLinearLayout * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QGraphicsLinearLayout( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsLinearLayout * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsLinearLayout;
   p->type = HBQT_TYPE_QGraphicsLinearLayout;

   return p;
}

HB_FUNC( QT_QGRAPHICSLINEARLAYOUT )
{
   QGraphicsLinearLayout * pObj = NULL;

   if( hb_pcount() >= 1 )
   {
      if( HB_ISNUM( 1 ) )
      {
         pObj = new QGraphicsLinearLayout( ( Qt::Orientation ) hb_parni( 1 ), ( HB_ISPOINTER( 2 ) ? hbqt_par_QGraphicsLayoutItem( 2 ) : 0 ) ) ;
      }
      else if( HB_ISPOINTER( 1 ) )
      {
         pObj = new QGraphicsLinearLayout( hbqt_par_QGraphicsLayoutItem( 1 ) ) ;
      }
      else
      {
         pObj = new QGraphicsLinearLayout() ;
      }
   }
   else
   {
      pObj = new QGraphicsLinearLayout() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QGraphicsLinearLayout( ( void * ) pObj, true ) );
}

/* void addItem ( QGraphicsLayoutItem * item ) */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_ADDITEM )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
      ( p )->addItem( hbqt_par_QGraphicsLayoutItem( 2 ) );
}

/* void addStretch ( int stretch = 1 ) */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_ADDSTRETCH )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
      ( p )->addStretch( hb_parnidef( 2, 1 ) );
}

/* Qt::Alignment alignment ( QGraphicsLayoutItem * item ) const */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_ALIGNMENT )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->alignment( hbqt_par_QGraphicsLayoutItem( 2 ) ) );
}

/* void insertItem ( int index, QGraphicsLayoutItem * item ) */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_INSERTITEM )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
      ( p )->insertItem( hb_parni( 2 ), hbqt_par_QGraphicsLayoutItem( 3 ) );
}

/* void insertStretch ( int index, int stretch = 1 ) */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_INSERTSTRETCH )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
      ( p )->insertStretch( hb_parni( 2 ), hb_parnidef( 3, 1 ) );
}

/* qreal itemSpacing ( int index ) const */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_ITEMSPACING )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
      hb_retnd( ( p )->itemSpacing( hb_parni( 2 ) ) );
}

/* Qt::Orientation orientation () const */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_ORIENTATION )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
      hb_retni( ( Qt::Orientation ) ( p )->orientation() );
}

/* virtual void removeAt ( int index ) */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_REMOVEAT )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
      ( p )->removeAt( hb_parni( 2 ) );
}

/* void removeItem ( QGraphicsLayoutItem * item ) */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_REMOVEITEM )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
      ( p )->removeItem( hbqt_par_QGraphicsLayoutItem( 2 ) );
}

/* void setAlignment ( QGraphicsLayoutItem * item, Qt::Alignment alignment ) */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_SETALIGNMENT )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
      ( p )->setAlignment( hbqt_par_QGraphicsLayoutItem( 2 ), ( Qt::Alignment ) hb_parni( 3 ) );
}

/* void setItemSpacing ( int index, qreal spacing ) */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_SETITEMSPACING )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
      ( p )->setItemSpacing( hb_parni( 2 ), hb_parnd( 3 ) );
}

/* void setOrientation ( Qt::Orientation orientation ) */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_SETORIENTATION )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
      ( p )->setOrientation( ( Qt::Orientation ) hb_parni( 2 ) );
}

/* void setSpacing ( qreal spacing ) */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_SETSPACING )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
      ( p )->setSpacing( hb_parnd( 2 ) );
}

/* void setStretchFactor ( QGraphicsLayoutItem * item, int stretch ) */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_SETSTRETCHFACTOR )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
      ( p )->setStretchFactor( hbqt_par_QGraphicsLayoutItem( 2 ), hb_parni( 3 ) );
}

/* qreal spacing () const */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_SPACING )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
      hb_retnd( ( p )->spacing() );
}

/* int stretchFactor ( QGraphicsLayoutItem * item ) const */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_STRETCHFACTOR )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
      hb_retni( ( p )->stretchFactor( hbqt_par_QGraphicsLayoutItem( 2 ) ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
