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

#include <QtGui/QStyleOptionFrame>


/*
 * QStyleOptionFrame ()
 * QStyleOptionFrame ( const QStyleOptionFrame & other )
 */

typedef struct
{
   QStyleOptionFrame * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionFrame;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionFrame )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QStyleOptionFrame * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QStyleOptionFrame( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionFrame * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionFrame;
   p->type = HBQT_TYPE_QStyleOptionFrame;

   return p;
}

HB_FUNC( QT_QSTYLEOPTIONFRAME )
{
   QStyleOptionFrame * pObj = NULL;

   pObj = new QStyleOptionFrame() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionFrame( ( void * ) pObj, true ) );
}

/* int lineWidth */
HB_FUNC( QT_QSTYLEOPTIONFRAME_LINEWIDTH )
{
   QStyleOptionFrame * p = hbqt_par_QStyleOptionFrame( 1 );
   if( p )
      hb_retni( ( p )->lineWidth );
}

/* int midLineWidth */
HB_FUNC( QT_QSTYLEOPTIONFRAME_MIDLINEWIDTH )
{
   QStyleOptionFrame * p = hbqt_par_QStyleOptionFrame( 1 );
   if( p )
      hb_retni( ( p )->midLineWidth );
}


#endif /* #if QT_VERSION >= 0x040500 */
