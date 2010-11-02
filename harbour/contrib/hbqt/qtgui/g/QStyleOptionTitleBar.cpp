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

#include <QtGui/QStyleOptionTitleBar>


/*
 * QStyleOptionTitleBar ()
 * QStyleOptionTitleBar ( const QStyleOptionTitleBar & other )
 */

typedef struct
{
   QStyleOptionTitleBar * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionTitleBar;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionTitleBar )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QStyleOptionTitleBar * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QStyleOptionTitleBar( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionTitleBar * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionTitleBar;
   p->type = HBQT_TYPE_QStyleOptionTitleBar;

   return p;
}

HB_FUNC( QT_QSTYLEOPTIONTITLEBAR )
{
   QStyleOptionTitleBar * pObj = NULL;

   pObj = new QStyleOptionTitleBar() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionTitleBar( ( void * ) pObj, true ) );
}

/* QIcon icon */
HB_FUNC( QT_QSTYLEOPTIONTITLEBAR_ICON )
{
   QStyleOptionTitleBar * p = hbqt_par_QStyleOptionTitleBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon ), true ) );
}

/* QString text */
HB_FUNC( QT_QSTYLEOPTIONTITLEBAR_TEXT )
{
   QStyleOptionTitleBar * p = hbqt_par_QStyleOptionTitleBar( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text.toUtf8().data() );
}

/* Qt::WindowFlags titleBarFlags */
HB_FUNC( QT_QSTYLEOPTIONTITLEBAR_TITLEBARFLAGS )
{
   QStyleOptionTitleBar * p = hbqt_par_QStyleOptionTitleBar( 1 );
   if( p )
      hb_retni( ( Qt::WindowFlags ) ( p )->titleBarFlags );
}

/* int titleBarState */
HB_FUNC( QT_QSTYLEOPTIONTITLEBAR_TITLEBARSTATE )
{
   QStyleOptionTitleBar * p = hbqt_par_QStyleOptionTitleBar( 1 );
   if( p )
      hb_retni( ( p )->titleBarState );
}


#endif /* #if QT_VERSION >= 0x040500 */
