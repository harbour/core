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

#include <QtGui/QStyleOptionSpinBox>


/*
 * QStyleOptionSpinBox ()
 * QStyleOptionSpinBox ( const QStyleOptionSpinBox & other )
 */

typedef struct
{
   QStyleOptionSpinBox * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionSpinBox;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionSpinBox )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QStyleOptionSpinBox * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QStyleOptionSpinBox( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionSpinBox * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionSpinBox;
   p->type = HBQT_TYPE_QStyleOptionSpinBox;

   return p;
}

HB_FUNC( QT_QSTYLEOPTIONSPINBOX )
{
   QStyleOptionSpinBox * pObj = NULL;

   pObj = new QStyleOptionSpinBox() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionSpinBox( ( void * ) pObj, true ) );
}

/* QAbstractSpinBox::ButtonSymbols buttonSymbols */
HB_FUNC( QT_QSTYLEOPTIONSPINBOX_BUTTONSYMBOLS )
{
   QStyleOptionSpinBox * p = hbqt_par_QStyleOptionSpinBox( 1 );
   if( p )
      hb_retni( ( QAbstractSpinBox::ButtonSymbols ) ( p )->buttonSymbols );
}

/* bool frame */
HB_FUNC( QT_QSTYLEOPTIONSPINBOX_FRAME )
{
   QStyleOptionSpinBox * p = hbqt_par_QStyleOptionSpinBox( 1 );
   if( p )
      hb_retl( ( p )->frame );
}

/* QAbstractSpinBox::StepEnabled stepEnabled */
HB_FUNC( QT_QSTYLEOPTIONSPINBOX_STEPENABLED )
{
   QStyleOptionSpinBox * p = hbqt_par_QStyleOptionSpinBox( 1 );
   if( p )
      hb_retni( ( QAbstractSpinBox::StepEnabled ) ( p )->stepEnabled );
}


#endif /* #if QT_VERSION >= 0x040500 */
