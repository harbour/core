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
 *  flags ButtonFeatures
 *  enum ButtonFeature { None, Flat, HasMenu, DefaultButton, AutoDefaultButton, CommandLinkButton }
 *  enum StyleOptionType { Type }
 *  enum StyleOptionVersion { Version }
 */

/*
 *  Constructed[ 4/4 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOptionButton>


/*
 * QStyleOptionButton ()
 * QStyleOptionButton ( const QStyleOptionButton & other )
 */

typedef struct
{
   QStyleOptionButton * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionButton;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionButton )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QStyleOptionButton * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStyleOptionButton( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionButton * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionButton;
   p->type = HBQT_TYPE_QStyleOptionButton;

   return p;
}

HB_FUNC( QT_QSTYLEOPTIONBUTTON )
{
   QStyleOptionButton * pObj = NULL;

   pObj = new QStyleOptionButton() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionButton( ( void * ) pObj, true ) );
}

/* ButtonFeatures features */
HB_FUNC( QT_QSTYLEOPTIONBUTTON_FEATURES )
{
   QStyleOptionButton * p = hbqt_par_QStyleOptionButton( 1 );
   if( p )
      hb_retni( ( QStyleOptionButton::ButtonFeatures ) ( p )->features );
}

/* QIcon icon */
HB_FUNC( QT_QSTYLEOPTIONBUTTON_ICON )
{
   QStyleOptionButton * p = hbqt_par_QStyleOptionButton( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon ), true ) );
}

/* QSize iconSize */
HB_FUNC( QT_QSTYLEOPTIONBUTTON_ICONSIZE )
{
   QStyleOptionButton * p = hbqt_par_QStyleOptionButton( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->iconSize ), true ) );
}

/* QString text */
HB_FUNC( QT_QSTYLEOPTIONBUTTON_TEXT )
{
   QStyleOptionButton * p = hbqt_par_QStyleOptionButton( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text.toUtf8().data() );
}


#endif /* #if QT_VERSION >= 0x040500 */
