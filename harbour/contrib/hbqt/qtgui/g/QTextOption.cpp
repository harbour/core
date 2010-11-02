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
 *  class Tab
 *  enum Flag { IncludeTrailingSpaces, ShowTabsAndSpaces, ShowLineAndParagraphSeparators, AddSpaceForLineAndParagraphSeparators, SuppressColors }
 *  flags Flags
 *  enum TabType { LeftTab, RightTab, CenterTab, DelimiterTab }
 *  enum WrapMode { NoWrap, WordWrap, ManualWrap, WrapAnywhere, WrapAtWordBoundaryOrAnywhere }
 */

/*
 *  Constructed[ 13/15 [ 86.67% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  void setTabArray ( QList<qreal> tabStops )
 *  void setTabs ( QList<Tab> tabStops )
 *
 *  *** Commented out protostypes ***
 *
 *  //QList<Tab> tabs () const
 */

#include <QtCore/QPointer>

#include <QtGui/QTextOption>


/* QTextOption ()
 * QTextOption ( Qt::Alignment alignment )
 * QTextOption ( const QTextOption & other )
 * ~QTextOption ()
 */

typedef struct
{
   QTextOption * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextOption;

HBQT_GC_FUNC( hbqt_gcRelease_QTextOption )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QTextOption * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextOption( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextOption * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextOption;
   p->type = HBQT_TYPE_QTextOption;

   return p;
}

HB_FUNC( QT_QTEXTOPTION )
{
   QTextOption * pObj = NULL;

   pObj = new QTextOption() ;

   hb_retptrGC( hbqt_gcAllocate_QTextOption( ( void * ) pObj, true ) );
}

/* Qt::Alignment alignment () const */
HB_FUNC( QT_QTEXTOPTION_ALIGNMENT )
{
   QTextOption * p = hbqt_par_QTextOption( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->alignment() );
}

/* Flags flags () const */
HB_FUNC( QT_QTEXTOPTION_FLAGS )
{
   QTextOption * p = hbqt_par_QTextOption( 1 );
   if( p )
      hb_retni( ( QTextOption::Flags ) ( p )->flags() );
}

/* void setAlignment ( Qt::Alignment alignment ) */
HB_FUNC( QT_QTEXTOPTION_SETALIGNMENT )
{
   QTextOption * p = hbqt_par_QTextOption( 1 );
   if( p )
      ( p )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
}

/* void setFlags ( Flags flags ) */
HB_FUNC( QT_QTEXTOPTION_SETFLAGS )
{
   QTextOption * p = hbqt_par_QTextOption( 1 );
   if( p )
      ( p )->setFlags( ( QTextOption::Flags ) hb_parni( 2 ) );
}

/* void setTabStop ( qreal tabStop ) */
HB_FUNC( QT_QTEXTOPTION_SETTABSTOP )
{
   QTextOption * p = hbqt_par_QTextOption( 1 );
   if( p )
      ( p )->setTabStop( hb_parnd( 2 ) );
}

/* void setTextDirection ( Qt::LayoutDirection direction ) */
HB_FUNC( QT_QTEXTOPTION_SETTEXTDIRECTION )
{
   QTextOption * p = hbqt_par_QTextOption( 1 );
   if( p )
      ( p )->setTextDirection( ( Qt::LayoutDirection ) hb_parni( 2 ) );
}

/* void setUseDesignMetrics ( bool enable ) */
HB_FUNC( QT_QTEXTOPTION_SETUSEDESIGNMETRICS )
{
   QTextOption * p = hbqt_par_QTextOption( 1 );
   if( p )
      ( p )->setUseDesignMetrics( hb_parl( 2 ) );
}

/* void setWrapMode ( WrapMode mode ) */
HB_FUNC( QT_QTEXTOPTION_SETWRAPMODE )
{
   QTextOption * p = hbqt_par_QTextOption( 1 );
   if( p )
      ( p )->setWrapMode( ( QTextOption::WrapMode ) hb_parni( 2 ) );
}

/* QList<qreal> tabArray () const */
HB_FUNC( QT_QTEXTOPTION_TABARRAY )
{
   QTextOption * p = hbqt_par_QTextOption( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<qreal>( ( p )->tabArray() ), true ) );
}

/* qreal tabStop () const */
HB_FUNC( QT_QTEXTOPTION_TABSTOP )
{
   QTextOption * p = hbqt_par_QTextOption( 1 );
   if( p )
      hb_retnd( ( p )->tabStop() );
}

/* Qt::LayoutDirection textDirection () const */
HB_FUNC( QT_QTEXTOPTION_TEXTDIRECTION )
{
   QTextOption * p = hbqt_par_QTextOption( 1 );
   if( p )
      hb_retni( ( Qt::LayoutDirection ) ( p )->textDirection() );
}

/* bool useDesignMetrics () const */
HB_FUNC( QT_QTEXTOPTION_USEDESIGNMETRICS )
{
   QTextOption * p = hbqt_par_QTextOption( 1 );
   if( p )
      hb_retl( ( p )->useDesignMetrics() );
}

/* WrapMode wrapMode () const */
HB_FUNC( QT_QTEXTOPTION_WRAPMODE )
{
   QTextOption * p = hbqt_par_QTextOption( 1 );
   if( p )
      hb_retni( ( QTextOption::WrapMode ) ( p )->wrapMode() );
}


#endif /* #if QT_VERSION >= 0x040500 */
