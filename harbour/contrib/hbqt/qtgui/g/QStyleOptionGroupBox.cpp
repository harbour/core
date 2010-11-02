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

#include <QtGui/QStyleOptionGroupBox>


/*
 * QStyleOptionGroupBox ()
 * QStyleOptionGroupBox ( const QStyleOptionGroupBox & other )
 */

typedef struct
{
   QStyleOptionGroupBox * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionGroupBox;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionGroupBox )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QStyleOptionGroupBox * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStyleOptionGroupBox( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionGroupBox * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionGroupBox;
   p->type = HBQT_TYPE_QStyleOptionGroupBox;

   return p;
}

HB_FUNC( QT_QSTYLEOPTIONGROUPBOX )
{
   QStyleOptionGroupBox * pObj = NULL;

   pObj = new QStyleOptionGroupBox() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionGroupBox( ( void * ) pObj, true ) );
}

/* QStyleOptionFrameV2::FrameFeatures features */
HB_FUNC( QT_QSTYLEOPTIONGROUPBOX_FEATURES )
{
   QStyleOptionGroupBox * p = hbqt_par_QStyleOptionGroupBox( 1 );
   if( p )
      hb_retni( ( QStyleOptionFrameV2::FrameFeatures ) ( p )->features );
}

/* int lineWidth */
HB_FUNC( QT_QSTYLEOPTIONGROUPBOX_LINEWIDTH )
{
   QStyleOptionGroupBox * p = hbqt_par_QStyleOptionGroupBox( 1 );
   if( p )
      hb_retni( ( p )->lineWidth );
}

/* int midLineWidth */
HB_FUNC( QT_QSTYLEOPTIONGROUPBOX_MIDLINEWIDTH )
{
   QStyleOptionGroupBox * p = hbqt_par_QStyleOptionGroupBox( 1 );
   if( p )
      hb_retni( ( p )->midLineWidth );
}

/* QString text */
HB_FUNC( QT_QSTYLEOPTIONGROUPBOX_TEXT )
{
   QStyleOptionGroupBox * p = hbqt_par_QStyleOptionGroupBox( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text.toUtf8().data() );
}

/* Qt::Alignment textAlignment */
HB_FUNC( QT_QSTYLEOPTIONGROUPBOX_TEXTALIGNMENT )
{
   QStyleOptionGroupBox * p = hbqt_par_QStyleOptionGroupBox( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->textAlignment );
}

/* QColor textColor */
HB_FUNC( QT_QSTYLEOPTIONGROUPBOX_TEXTCOLOR )
{
   QStyleOptionGroupBox * p = hbqt_par_QStyleOptionGroupBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->textColor ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
