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

#include <QtGui/QStyleOptionSizeGrip>


/*
 * QStyleOptionSizeGrip ()
 * QStyleOptionSizeGrip ( const QStyleOptionSizeGrip & other )
 */

typedef struct
{
   QStyleOptionSizeGrip * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionSizeGrip;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionSizeGrip )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QStyleOptionSizeGrip * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QStyleOptionSizeGrip( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionSizeGrip * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionSizeGrip;
   p->type = HBQT_TYPE_QStyleOptionSizeGrip;

   return p;
}

HB_FUNC( QT_QSTYLEOPTIONSIZEGRIP )
{
   QStyleOptionSizeGrip * pObj = NULL;

   pObj = new QStyleOptionSizeGrip() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionSizeGrip( ( void * ) pObj, true ) );
}

/* Qt::Corner corner */
HB_FUNC( QT_QSTYLEOPTIONSIZEGRIP_CORNER )
{
   QStyleOptionSizeGrip * p = hbqt_par_QStyleOptionSizeGrip( 1 );
   if( p )
      hb_retni( ( Qt::Corner ) ( p )->corner );
}


#endif /* #if QT_VERSION >= 0x040500 */
