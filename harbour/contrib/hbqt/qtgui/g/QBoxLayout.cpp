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
 *  enum Direction { LeftToRight, RightToLeft, TopToBottom, BottomToTop }
 */

/*
 *  Constructed[ 20/20 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QBoxLayout>


/*
 * QBoxLayout ( Direction dir, QWidget * parent = 0 )
 * ~QBoxLayout ()
 */

typedef struct
{
   QPointer< QBoxLayout > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QBoxLayout;

HBQT_GC_FUNC( hbqt_gcRelease_QBoxLayout )
{
   HBQT_GC_T_QBoxLayout * p = ( HBQT_GC_T_QBoxLayout * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QBoxLayout * ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete ( p->ph );
            p->ph = NULL;
         }
         else
            p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QBoxLayout( void * pObj, bool bNew )
{
   HBQT_GC_T_QBoxLayout * p = ( HBQT_GC_T_QBoxLayout * ) hb_gcAllocate( sizeof( HBQT_GC_T_QBoxLayout ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QBoxLayout >( ( QBoxLayout * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QBoxLayout;
   p->type = HBQT_TYPE_QBoxLayout;

   return p;
}

HB_FUNC( QT_QBOXLAYOUT )
{
   QBoxLayout * pObj = NULL;

   pObj = new QBoxLayout( ( QBoxLayout::Direction ) hb_parni( 1 ), hbqt_par_QWidget( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QBoxLayout( ( void * ) pObj, true ) );
}

/* void addLayout ( QLayout * layout, int stretch = 0 )   [*D=1*] */
HB_FUNC( QT_QBOXLAYOUT_ADDLAYOUT )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 2 );
      ( p )->addLayout( hbqt_par_QLayout( 2 ), hb_parni( 3 ) );
   }
}

/* void addSpacerItem ( QSpacerItem * spacerItem )   [*D=1*] */
HB_FUNC( QT_QBOXLAYOUT_ADDSPACERITEM )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 2 );
      ( p )->addSpacerItem( hbqt_par_QSpacerItem( 2 ) );
   }
}

/* void addSpacing ( int size ) */
HB_FUNC( QT_QBOXLAYOUT_ADDSPACING )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
      ( p )->addSpacing( hb_parni( 2 ) );
}

/* void addStretch ( int stretch = 0 ) */
HB_FUNC( QT_QBOXLAYOUT_ADDSTRETCH )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
      ( p )->addStretch( hb_parni( 2 ) );
}

/* void addStrut ( int size ) */
HB_FUNC( QT_QBOXLAYOUT_ADDSTRUT )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
      ( p )->addStrut( hb_parni( 2 ) );
}

/* void addWidget ( QWidget * widget, int stretch = 0, Qt::Alignment alignment = 0 )   [*D=1*] */
HB_FUNC( QT_QBOXLAYOUT_ADDWIDGET )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 2 );
      ( p )->addWidget( hbqt_par_QWidget( 2 ), hb_parni( 3 ), ( Qt::Alignment ) hb_parni( 4 ) );
   }
}

/* Direction direction () const */
HB_FUNC( QT_QBOXLAYOUT_DIRECTION )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
      hb_retni( ( QBoxLayout::Direction ) ( p )->direction() );
}

/* void insertLayout ( int index, QLayout * layout, int stretch = 0 ) */
HB_FUNC( QT_QBOXLAYOUT_INSERTLAYOUT )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
      ( p )->insertLayout( hb_parni( 2 ), hbqt_par_QLayout( 3 ), hb_parni( 4 ) );
}

/* void insertSpacerItem ( int index, QSpacerItem * spacerItem ) */
HB_FUNC( QT_QBOXLAYOUT_INSERTSPACERITEM )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
      ( p )->insertSpacerItem( hb_parni( 2 ), hbqt_par_QSpacerItem( 3 ) );
}

/* void insertSpacing ( int index, int size ) */
HB_FUNC( QT_QBOXLAYOUT_INSERTSPACING )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
      ( p )->insertSpacing( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void insertStretch ( int index, int stretch = 0 ) */
HB_FUNC( QT_QBOXLAYOUT_INSERTSTRETCH )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
      ( p )->insertStretch( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void insertWidget ( int index, QWidget * widget, int stretch = 0, Qt::Alignment alignment = 0 ) */
HB_FUNC( QT_QBOXLAYOUT_INSERTWIDGET )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
      ( p )->insertWidget( hb_parni( 2 ), hbqt_par_QWidget( 3 ), hb_parni( 4 ), ( Qt::Alignment ) hb_parni( 5 ) );
}

/* virtual void invalidate () */
HB_FUNC( QT_QBOXLAYOUT_INVALIDATE )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
      ( p )->invalidate();
}

/* void setDirection ( Direction direction ) */
HB_FUNC( QT_QBOXLAYOUT_SETDIRECTION )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
      ( p )->setDirection( ( QBoxLayout::Direction ) hb_parni( 2 ) );
}

/* void setSpacing ( int spacing ) */
HB_FUNC( QT_QBOXLAYOUT_SETSPACING )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
      ( p )->setSpacing( hb_parni( 2 ) );
}

/* void setStretch ( int index, int stretch ) */
HB_FUNC( QT_QBOXLAYOUT_SETSTRETCH )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
      ( p )->setStretch( hb_parni( 2 ), hb_parni( 3 ) );
}

/* bool setStretchFactor ( QWidget * widget, int stretch ) */
HB_FUNC( QT_QBOXLAYOUT_SETSTRETCHFACTOR )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
      hb_retl( ( p )->setStretchFactor( hbqt_par_QWidget( 2 ), hb_parni( 3 ) ) );
}

/* bool setStretchFactor ( QLayout * layout, int stretch ) */
HB_FUNC( QT_QBOXLAYOUT_SETSTRETCHFACTOR_1 )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
      hb_retl( ( p )->setStretchFactor( hbqt_par_QLayout( 2 ), hb_parni( 3 ) ) );
}

/* int spacing () const */
HB_FUNC( QT_QBOXLAYOUT_SPACING )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
      hb_retni( ( p )->spacing() );
}

/* int stretch ( int index ) const */
HB_FUNC( QT_QBOXLAYOUT_STRETCH )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
      hb_retni( ( p )->stretch( hb_parni( 2 ) ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
