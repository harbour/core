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
 *  Constructed[ 3/3 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOptionTabBarBase>


/*
 * QStyleOptionTabBarBase ()
 * QStyleOptionTabBarBase ( const QStyleOptionTabBarBase & other )
 */

typedef struct
{
   QStyleOptionTabBarBase * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionTabBarBase;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionTabBarBase )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QStyleOptionTabBarBase * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QStyleOptionTabBarBase( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionTabBarBase * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionTabBarBase;
   p->type = HBQT_TYPE_QStyleOptionTabBarBase;

   return p;
}

HB_FUNC( QT_QSTYLEOPTIONTABBARBASE )
{
   QStyleOptionTabBarBase * pObj = NULL;

   pObj = new QStyleOptionTabBarBase() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionTabBarBase( ( void * ) pObj, true ) );
}

/* QRect selectedTabRect */
HB_FUNC( QT_QSTYLEOPTIONTABBARBASE_SELECTEDTABRECT )
{
   QStyleOptionTabBarBase * p = hbqt_par_QStyleOptionTabBarBase( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->selectedTabRect ), true ) );
}

/* QTabBar::Shape shape */
HB_FUNC( QT_QSTYLEOPTIONTABBARBASE_SHAPE )
{
   QStyleOptionTabBarBase * p = hbqt_par_QStyleOptionTabBarBase( 1 );
   if( p )
      hb_retni( ( QTabBar::Shape ) ( p )->shape );
}

/* QRect tabBarRect */
HB_FUNC( QT_QSTYLEOPTIONTABBARBASE_TABBARRECT )
{
   QStyleOptionTabBarBase * p = hbqt_par_QStyleOptionTabBarBase( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->tabBarRect ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
