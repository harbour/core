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
 *  Public Functions
 */

/*
 *  Constructed[ 6/6 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOptionProgressBar>


/*
 * QStyleOptionProgressBar ()
 * QStyleOptionProgressBar ( const QStyleOptionProgressBar & other )
 */

typedef struct
{
   QStyleOptionProgressBar * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionProgressBar;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionProgressBar )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QStyleOptionProgressBar * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QStyleOptionProgressBar( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionProgressBar * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionProgressBar;
   p->type = HBQT_TYPE_QStyleOptionProgressBar;

   return p;
}

HB_FUNC( QT_QSTYLEOPTIONPROGRESSBAR )
{
   QStyleOptionProgressBar * pObj = NULL;

   pObj = new QStyleOptionProgressBar() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionProgressBar( ( void * ) pObj, true ) );
}

/* int maximum */
HB_FUNC( QT_QSTYLEOPTIONPROGRESSBAR_MAXIMUM )
{
   QStyleOptionProgressBar * p = hbqt_par_QStyleOptionProgressBar( 1 );
   if( p )
      hb_retni( ( p )->maximum );
}

/* int minimum */
HB_FUNC( QT_QSTYLEOPTIONPROGRESSBAR_MINIMUM )
{
   QStyleOptionProgressBar * p = hbqt_par_QStyleOptionProgressBar( 1 );
   if( p )
      hb_retni( ( p )->minimum );
}

/* int progress */
HB_FUNC( QT_QSTYLEOPTIONPROGRESSBAR_PROGRESS )
{
   QStyleOptionProgressBar * p = hbqt_par_QStyleOptionProgressBar( 1 );
   if( p )
      hb_retni( ( p )->progress );
}

/* QString text */
HB_FUNC( QT_QSTYLEOPTIONPROGRESSBAR_TEXT )
{
   QStyleOptionProgressBar * p = hbqt_par_QStyleOptionProgressBar( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text.toUtf8().data() );
}

/* Qt::Alignment textAlignment */
HB_FUNC( QT_QSTYLEOPTIONPROGRESSBAR_TEXTALIGNMENT )
{
   QStyleOptionProgressBar * p = hbqt_par_QStyleOptionProgressBar( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->textAlignment );
}

/* bool textVisible */
HB_FUNC( QT_QSTYLEOPTIONPROGRESSBAR_TEXTVISIBLE )
{
   QStyleOptionProgressBar * p = hbqt_par_QStyleOptionProgressBar( 1 );
   if( p )
      hb_retl( ( p )->textVisible );
}


#endif /* #if QT_VERSION >= 0x040500 */
