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
 *  enum SectionPosition { Beginning, Middle, End, OnlyOneSection }
 *  enum SelectedPosition { NotAdjacent, NextIsSelected, PreviousIsSelected, NextAndPreviousAreSelected }
 *  enum SortIndicator { None, SortUp, SortDown }
 *  enum StyleOptionType { Type }
 *  enum StyleOptionVersion { Version }
 */

/*
 *  Constructed[ 9/9 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOptionHeader>


/*
 * QStyleOptionHeader ()
 * QStyleOptionHeader ( const QStyleOptionHeader & other )
 */

typedef struct
{
   QStyleOptionHeader * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionHeader;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionHeader )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QStyleOptionHeader * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStyleOptionHeader( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionHeader * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionHeader;
   p->type = HBQT_TYPE_QStyleOptionHeader;

   return p;
}

HB_FUNC( QT_QSTYLEOPTIONHEADER )
{
   QStyleOptionHeader * pObj = NULL;

   pObj = new QStyleOptionHeader() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionHeader( ( void * ) pObj, true ) );
}

/* QIcon icon */
HB_FUNC( QT_QSTYLEOPTIONHEADER_ICON )
{
   QStyleOptionHeader * p = hbqt_par_QStyleOptionHeader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon ), true ) );
}

/* Qt::Alignment iconAlignment */
HB_FUNC( QT_QSTYLEOPTIONHEADER_ICONALIGNMENT )
{
   QStyleOptionHeader * p = hbqt_par_QStyleOptionHeader( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->iconAlignment );
}

/* Qt::Orientation orientation */
HB_FUNC( QT_QSTYLEOPTIONHEADER_ORIENTATION )
{
   QStyleOptionHeader * p = hbqt_par_QStyleOptionHeader( 1 );
   if( p )
      hb_retni( ( Qt::Orientation ) ( p )->orientation );
}

/* SectionPosition position */
HB_FUNC( QT_QSTYLEOPTIONHEADER_POSITION )
{
   QStyleOptionHeader * p = hbqt_par_QStyleOptionHeader( 1 );
   if( p )
      hb_retni( ( QStyleOptionHeader::SectionPosition ) ( p )->position );
}

/* int section */
HB_FUNC( QT_QSTYLEOPTIONHEADER_SECTION )
{
   QStyleOptionHeader * p = hbqt_par_QStyleOptionHeader( 1 );
   if( p )
      hb_retni( ( p )->section );
}

/* SelectedPosition selectedPosition */
HB_FUNC( QT_QSTYLEOPTIONHEADER_SELECTEDPOSITION )
{
   QStyleOptionHeader * p = hbqt_par_QStyleOptionHeader( 1 );
   if( p )
      hb_retni( ( QStyleOptionHeader::SelectedPosition ) ( p )->selectedPosition );
}

/* SortIndicator sortIndicator */
HB_FUNC( QT_QSTYLEOPTIONHEADER_SORTINDICATOR )
{
   QStyleOptionHeader * p = hbqt_par_QStyleOptionHeader( 1 );
   if( p )
      hb_retni( ( QStyleOptionHeader::SortIndicator ) ( p )->sortIndicator );
}

/* QString text */
HB_FUNC( QT_QSTYLEOPTIONHEADER_TEXT )
{
   QStyleOptionHeader * p = hbqt_par_QStyleOptionHeader( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text.toUtf8().data() );
}

/* Qt::Alignment textAlignment */
HB_FUNC( QT_QSTYLEOPTIONHEADER_TEXTALIGNMENT )
{
   QStyleOptionHeader * p = hbqt_par_QStyleOptionHeader( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->textAlignment );
}


#endif /* #if QT_VERSION >= 0x040500 */
