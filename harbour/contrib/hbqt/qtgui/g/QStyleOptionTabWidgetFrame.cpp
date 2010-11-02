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
 */

/*
 *  Constructed[ 6/6 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOptionTabWidgetFrame>


/*
 * QStyleOptionTabWidgetFrame ()
 * QStyleOptionTabWidgetFrame ( const QStyleOptionTabWidgetFrame & other )
 */

typedef struct
{
   QStyleOptionTabWidgetFrame * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionTabWidgetFrame;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionTabWidgetFrame )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QStyleOptionTabWidgetFrame * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QStyleOptionTabWidgetFrame( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionTabWidgetFrame * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionTabWidgetFrame;
   p->type = HBQT_TYPE_QStyleOptionTabWidgetFrame;

   return p;
}

HB_FUNC( QT_QSTYLEOPTIONTABWIDGETFRAME )
{
   QStyleOptionTabWidgetFrame * pObj = NULL;

   pObj = new QStyleOptionTabWidgetFrame() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionTabWidgetFrame( ( void * ) pObj, true ) );
}

/* QSize leftCornerWidgetSize */
HB_FUNC( QT_QSTYLEOPTIONTABWIDGETFRAME_LEFTCORNERWIDGETSIZE )
{
   QStyleOptionTabWidgetFrame * p = hbqt_par_QStyleOptionTabWidgetFrame( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->leftCornerWidgetSize ), true ) );
}

/* int lineWidth */
HB_FUNC( QT_QSTYLEOPTIONTABWIDGETFRAME_LINEWIDTH )
{
   QStyleOptionTabWidgetFrame * p = hbqt_par_QStyleOptionTabWidgetFrame( 1 );
   if( p )
      hb_retni( ( p )->lineWidth );
}

/* int midLineWidth */
HB_FUNC( QT_QSTYLEOPTIONTABWIDGETFRAME_MIDLINEWIDTH )
{
   QStyleOptionTabWidgetFrame * p = hbqt_par_QStyleOptionTabWidgetFrame( 1 );
   if( p )
      hb_retni( ( p )->midLineWidth );
}

/* QSize rightCornerWidgetSize */
HB_FUNC( QT_QSTYLEOPTIONTABWIDGETFRAME_RIGHTCORNERWIDGETSIZE )
{
   QStyleOptionTabWidgetFrame * p = hbqt_par_QStyleOptionTabWidgetFrame( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->rightCornerWidgetSize ), true ) );
}

/* QTabBar::Shape shape */
HB_FUNC( QT_QSTYLEOPTIONTABWIDGETFRAME_SHAPE )
{
   QStyleOptionTabWidgetFrame * p = hbqt_par_QStyleOptionTabWidgetFrame( 1 );
   if( p )
      hb_retni( ( QTabBar::Shape ) ( p )->shape );
}

/* QSize tabBarSize */
HB_FUNC( QT_QSTYLEOPTIONTABWIDGETFRAME_TABBARSIZE )
{
   QStyleOptionTabWidgetFrame * p = hbqt_par_QStyleOptionTabWidgetFrame( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->tabBarSize ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
