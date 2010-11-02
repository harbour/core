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
 *  Constructed[ 0/0 [ 0% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QCommonStyle>


/*
 * QCommonStyle ()
 */

typedef struct
{
   QPointer< QCommonStyle > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QCommonStyle;

HBQT_GC_FUNC( hbqt_gcRelease_QCommonStyle )
{
   HBQT_GC_T_QCommonStyle * p = ( HBQT_GC_T_QCommonStyle * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QCommonStyle * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QCommonStyle( void * pObj, bool bNew )
{
   HBQT_GC_T_QCommonStyle * p = ( HBQT_GC_T_QCommonStyle * ) hb_gcAllocate( sizeof( HBQT_GC_T_QCommonStyle ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QCommonStyle >( ( QCommonStyle * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QCommonStyle;
   p->type = HBQT_TYPE_QCommonStyle;

   return p;
}

HB_FUNC( QT_QCOMMONSTYLE )
{
   QCommonStyle * pObj = NULL;

   pObj = new QCommonStyle() ;

   hb_retptrGC( hbqt_gcAllocate_QCommonStyle( ( void * ) pObj, true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
