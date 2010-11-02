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
 *  enum StyleOptionType { Type }
 *  enum StyleOptionVersion { Version }
 *  enum ToolBarFeature { None, Movable }
 *  flags ToolBarFeatures
 *  enum ToolBarPosition { Beginning, Middle, End, OnlyOne }
 */

/*
 *  Constructed[ 6/6 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOptionToolBar>


/*
 * QStyleOptionToolBar ()
 * QStyleOptionToolBar ( const QStyleOptionToolBar & other )
 */

typedef struct
{
   QStyleOptionToolBar * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionToolBar;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionToolBar )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QStyleOptionToolBar * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStyleOptionToolBar( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionToolBar * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionToolBar;
   p->type = HBQT_TYPE_QStyleOptionToolBar;

   return p;
}

HB_FUNC( QT_QSTYLEOPTIONTOOLBAR )
{
   QStyleOptionToolBar * pObj = NULL;

   pObj = new QStyleOptionToolBar() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionToolBar( ( void * ) pObj, true ) );
}

/* ToolBarFeatures features */
HB_FUNC( QT_QSTYLEOPTIONTOOLBAR_FEATURES )
{
   QStyleOptionToolBar * p = hbqt_par_QStyleOptionToolBar( 1 );
   if( p )
      hb_retni( ( QStyleOptionToolBar::ToolBarFeatures ) ( p )->features );
}

/* int lineWidth */
HB_FUNC( QT_QSTYLEOPTIONTOOLBAR_LINEWIDTH )
{
   QStyleOptionToolBar * p = hbqt_par_QStyleOptionToolBar( 1 );
   if( p )
      hb_retni( ( p )->lineWidth );
}

/* int midLineWidth */
HB_FUNC( QT_QSTYLEOPTIONTOOLBAR_MIDLINEWIDTH )
{
   QStyleOptionToolBar * p = hbqt_par_QStyleOptionToolBar( 1 );
   if( p )
      hb_retni( ( p )->midLineWidth );
}

/* ToolBarPosition positionOfLine */
HB_FUNC( QT_QSTYLEOPTIONTOOLBAR_POSITIONOFLINE )
{
   QStyleOptionToolBar * p = hbqt_par_QStyleOptionToolBar( 1 );
   if( p )
      hb_retni( ( QStyleOptionToolBar::ToolBarPosition ) ( p )->positionOfLine );
}

/* ToolBarPosition positionWithinLine */
HB_FUNC( QT_QSTYLEOPTIONTOOLBAR_POSITIONWITHINLINE )
{
   QStyleOptionToolBar * p = hbqt_par_QStyleOptionToolBar( 1 );
   if( p )
      hb_retni( ( QStyleOptionToolBar::ToolBarPosition ) ( p )->positionWithinLine );
}

/* Qt::ToolBarArea toolBarArea */
HB_FUNC( QT_QSTYLEOPTIONTOOLBAR_TOOLBARAREA )
{
   QStyleOptionToolBar * p = hbqt_par_QStyleOptionToolBar( 1 );
   if( p )
      hb_retni( ( Qt::ToolBarArea ) ( p )->toolBarArea );
}


#endif /* #if QT_VERSION >= 0x040500 */
