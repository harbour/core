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
 *  Constructed[ 4/4 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOptionDockWidget>


/*
 * QStyleOptionDockWidget ()
 * QStyleOptionDockWidget ( const QStyleOptionDockWidget & other )
 */

typedef struct
{
   QStyleOptionDockWidget * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionDockWidget;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionDockWidget )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QStyleOptionDockWidget * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStyleOptionDockWidget( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionDockWidget * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionDockWidget;
   p->type = HBQT_TYPE_QStyleOptionDockWidget;

   return p;
}

HB_FUNC( QT_QSTYLEOPTIONDOCKWIDGET )
{
   QStyleOptionDockWidget * pObj = NULL;

   pObj = new QStyleOptionDockWidget() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionDockWidget( ( void * ) pObj, true ) );
}

/* bool closable */
HB_FUNC( QT_QSTYLEOPTIONDOCKWIDGET_CLOSABLE )
{
   QStyleOptionDockWidget * p = hbqt_par_QStyleOptionDockWidget( 1 );
   if( p )
      hb_retl( ( p )->closable );
}

/* bool floatable */
HB_FUNC( QT_QSTYLEOPTIONDOCKWIDGET_FLOATABLE )
{
   QStyleOptionDockWidget * p = hbqt_par_QStyleOptionDockWidget( 1 );
   if( p )
      hb_retl( ( p )->floatable );
}

/* bool movable */
HB_FUNC( QT_QSTYLEOPTIONDOCKWIDGET_MOVABLE )
{
   QStyleOptionDockWidget * p = hbqt_par_QStyleOptionDockWidget( 1 );
   if( p )
      hb_retl( ( p )->movable );
}

/* QString title */
HB_FUNC( QT_QSTYLEOPTIONDOCKWIDGET_TITLE )
{
   QStyleOptionDockWidget * p = hbqt_par_QStyleOptionDockWidget( 1 );
   if( p )
      hb_retstr_utf8( ( p )->title.toUtf8().data() );
}


#endif /* #if QT_VERSION >= 0x040500 */
