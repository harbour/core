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
 *  enum CornerWidget { NoCornerWidgets, LeftCornerWidget, RightCornerWidget }
 *  flags CornerWidgets
 *  enum SelectedPosition { NotAdjacent, NextIsSelected, PreviousIsSelected }
 *  enum StyleOptionType { Type }
 *  enum StyleOptionVersion { Version }
 *  enum TabPosition { Beginning, Middle, End, OnlyOneTab }
 */

/*
 *  Constructed[ 7/7 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOptionTab>


/*
 * QStyleOptionTab ()
 * QStyleOptionTab ( const QStyleOptionTab & other )
 */

typedef struct
{
   QStyleOptionTab * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionTab;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionTab )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QStyleOptionTab * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QStyleOptionTab( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionTab * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionTab;
   p->type = HBQT_TYPE_QStyleOptionTab;

   return p;
}

HB_FUNC( QT_QSTYLEOPTIONTAB )
{
   QStyleOptionTab * pObj = NULL;

   pObj = new QStyleOptionTab() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionTab( ( void * ) pObj, true ) );
}

/* CornerWidgets cornerWidgets */
HB_FUNC( QT_QSTYLEOPTIONTAB_CORNERWIDGETS )
{
   QStyleOptionTab * p = hbqt_par_QStyleOptionTab( 1 );
   if( p )
      hb_retni( ( QStyleOptionTab::CornerWidgets ) ( p )->cornerWidgets );
}

/* QIcon icon */
HB_FUNC( QT_QSTYLEOPTIONTAB_ICON )
{
   QStyleOptionTab * p = hbqt_par_QStyleOptionTab( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon ), true ) );
}

/* TabPosition position */
HB_FUNC( QT_QSTYLEOPTIONTAB_POSITION )
{
   QStyleOptionTab * p = hbqt_par_QStyleOptionTab( 1 );
   if( p )
      hb_retni( ( QStyleOptionTab::TabPosition ) ( p )->position );
}

/* int row */
HB_FUNC( QT_QSTYLEOPTIONTAB_ROW )
{
   QStyleOptionTab * p = hbqt_par_QStyleOptionTab( 1 );
   if( p )
      hb_retni( ( p )->row );
}

/* SelectedPosition selectedPosition */
HB_FUNC( QT_QSTYLEOPTIONTAB_SELECTEDPOSITION )
{
   QStyleOptionTab * p = hbqt_par_QStyleOptionTab( 1 );
   if( p )
      hb_retni( ( QStyleOptionTab::SelectedPosition ) ( p )->selectedPosition );
}

/* QTabBar::Shape shape */
HB_FUNC( QT_QSTYLEOPTIONTAB_SHAPE )
{
   QStyleOptionTab * p = hbqt_par_QStyleOptionTab( 1 );
   if( p )
      hb_retni( ( QTabBar::Shape ) ( p )->shape );
}

/* QString text */
HB_FUNC( QT_QSTYLEOPTIONTAB_TEXT )
{
   QStyleOptionTab * p = hbqt_par_QStyleOptionTab( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text.toUtf8().data() );
}


#endif /* #if QT_VERSION >= 0x040500 */
