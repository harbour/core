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
 *  enum OptionType { SO_Button, SO_ComboBox, SO_Complex, SO_Default, ..., SO_Q3ListViewItem }
 *  enum StyleOptionType { Type }
 *  enum StyleOptionVersion { Version }
 */

/*
 *  Constructed[ 8/8 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOption>


/*
 * QStyleOption ( int version = QStyleOption::Version, int type = SO_Default )
 * QStyleOption ( const QStyleOption & other )
 * ~QStyleOption ()
 */

typedef struct
{
   QStyleOption * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOption;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOption )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QStyleOption * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStyleOption( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOption * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOption;
   p->type = HBQT_TYPE_QStyleOption;

   return p;
}

HB_FUNC( QT_QSTYLEOPTION )
{
   QStyleOption * pObj = NULL;

   pObj = new QStyleOption() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOption( ( void * ) pObj, true ) );
}

/* void initFrom ( const QWidget * widget ) */
HB_FUNC( QT_QSTYLEOPTION_INITFROM )
{
   QStyleOption * p = hbqt_par_QStyleOption( 1 );
   if( p )
      ( p )->initFrom( hbqt_par_QWidget( 2 ) );
}

/* Qt::LayoutDirection direction */
HB_FUNC( QT_QSTYLEOPTION_DIRECTION )
{
   QStyleOption * p = hbqt_par_QStyleOption( 1 );
   if( p )
      hb_retni( ( Qt::LayoutDirection ) ( p )->direction );
}

/* QFontMetrics fontMetrics */
HB_FUNC( QT_QSTYLEOPTION_FONTMETRICS )
{
   QStyleOption * p = hbqt_par_QStyleOption( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFontMetrics( new QFontMetrics( ( p )->fontMetrics ), true ) );
}

/* QPalette palette */
HB_FUNC( QT_QSTYLEOPTION_PALETTE )
{
   QStyleOption * p = hbqt_par_QStyleOption( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( ( p )->palette ), true ) );
}

/* QRect rect */
HB_FUNC( QT_QSTYLEOPTION_RECT )
{
   QStyleOption * p = hbqt_par_QStyleOption( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->rect ), true ) );
}

/* QStyle::State state */
HB_FUNC( QT_QSTYLEOPTION_STATE )
{
   QStyleOption * p = hbqt_par_QStyleOption( 1 );
   if( p )
      hb_retni( ( QStyle::State ) ( p )->state );
}

/* int type */
HB_FUNC( QT_QSTYLEOPTION_TYPE )
{
   QStyleOption * p = hbqt_par_QStyleOption( 1 );
   if( p )
      hb_retni( ( p )->type );
}

/* int version */
HB_FUNC( QT_QSTYLEOPTION_VERSION )
{
   QStyleOption * p = hbqt_par_QStyleOption( 1 );
   if( p )
      hb_retni( ( p )->version );
}


#endif /* #if QT_VERSION >= 0x040500 */
