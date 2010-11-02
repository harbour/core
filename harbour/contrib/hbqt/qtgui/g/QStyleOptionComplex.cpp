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
 *  Constructed[ 2/2 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOptionComplex>


/*
 * QStyleOptionComplex ( int version = QStyleOptionComplex::Version, int type = SO_Complex )
 * QStyleOptionComplex ( const QStyleOptionComplex & other )
 */

typedef struct
{
   QStyleOptionComplex * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionComplex;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionComplex )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QStyleOptionComplex * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStyleOptionComplex( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionComplex * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionComplex;
   p->type = HBQT_TYPE_QStyleOptionComplex;

   return p;
}

HB_FUNC( QT_QSTYLEOPTIONCOMPLEX )
{
   QStyleOptionComplex * pObj = NULL;

   pObj = new QStyleOptionComplex() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionComplex( ( void * ) pObj, true ) );
}

/* QStyle::SubControls activeSubControls */
HB_FUNC( QT_QSTYLEOPTIONCOMPLEX_ACTIVESUBCONTROLS )
{
   QStyleOptionComplex * p = hbqt_par_QStyleOptionComplex( 1 );
   if( p )
      hb_retni( ( QStyle::SubControls ) ( p )->activeSubControls );
}

/* QStyle::SubControls subControls */
HB_FUNC( QT_QSTYLEOPTIONCOMPLEX_SUBCONTROLS )
{
   QStyleOptionComplex * p = hbqt_par_QStyleOptionComplex( 1 );
   if( p )
      hb_retni( ( QStyle::SubControls ) ( p )->subControls );
}


#endif /* #if QT_VERSION >= 0x040500 */
