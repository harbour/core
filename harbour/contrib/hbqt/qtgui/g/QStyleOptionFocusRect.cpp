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
 *  Constructed[ 1/1 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOptionFocusRect>


/*
 * QStyleOptionFocusRect ()
 * QStyleOptionFocusRect ( const QStyleOptionFocusRect & other )
 */

typedef struct
{
   QStyleOptionFocusRect * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionFocusRect;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionFocusRect )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QStyleOptionFocusRect * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStyleOptionFocusRect( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionFocusRect * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionFocusRect;
   p->type = HBQT_TYPE_QStyleOptionFocusRect;

   return p;
}

HB_FUNC( QT_QSTYLEOPTIONFOCUSRECT )
{
   QStyleOptionFocusRect * pObj = NULL;

   pObj = new QStyleOptionFocusRect() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionFocusRect( ( void * ) pObj, true ) );
}

/* QColor backgroundColor */
HB_FUNC( QT_QSTYLEOPTIONFOCUSRECT_BACKGROUNDCOLOR )
{
   QStyleOptionFocusRect * p = hbqt_par_QStyleOptionFocusRect( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->backgroundColor ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
