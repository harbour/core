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
 *  Constructed[ 6/6 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QTableWidgetSelectionRange>


/* QTableWidgetSelectionRange ()
 * QTableWidgetSelectionRange ( int top, int left, int bottom, int right )
 * QTableWidgetSelectionRange ( const QTableWidgetSelectionRange & other )
 * ~QTableWidgetSelectionRange ()
 */

typedef struct
{
   QTableWidgetSelectionRange * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTableWidgetSelectionRange;

HBQT_GC_FUNC( hbqt_gcRelease_QTableWidgetSelectionRange )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QTableWidgetSelectionRange * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QTableWidgetSelectionRange( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTableWidgetSelectionRange * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTableWidgetSelectionRange;
   p->type = HBQT_TYPE_QTableWidgetSelectionRange;

   return p;
}

HB_FUNC( QT_QTABLEWIDGETSELECTIONRANGE )
{
   QTableWidgetSelectionRange * pObj = NULL;

   pObj = new QTableWidgetSelectionRange() ;

   hb_retptrGC( hbqt_gcAllocate_QTableWidgetSelectionRange( ( void * ) pObj, true ) );
}

/* int bottomRow () const */
HB_FUNC( QT_QTABLEWIDGETSELECTIONRANGE_BOTTOMROW )
{
   QTableWidgetSelectionRange * p = hbqt_par_QTableWidgetSelectionRange( 1 );
   if( p )
      hb_retni( ( p )->bottomRow() );
}

/* int columnCount () const */
HB_FUNC( QT_QTABLEWIDGETSELECTIONRANGE_COLUMNCOUNT )
{
   QTableWidgetSelectionRange * p = hbqt_par_QTableWidgetSelectionRange( 1 );
   if( p )
      hb_retni( ( p )->columnCount() );
}

/* int leftColumn () const */
HB_FUNC( QT_QTABLEWIDGETSELECTIONRANGE_LEFTCOLUMN )
{
   QTableWidgetSelectionRange * p = hbqt_par_QTableWidgetSelectionRange( 1 );
   if( p )
      hb_retni( ( p )->leftColumn() );
}

/* int rightColumn () const */
HB_FUNC( QT_QTABLEWIDGETSELECTIONRANGE_RIGHTCOLUMN )
{
   QTableWidgetSelectionRange * p = hbqt_par_QTableWidgetSelectionRange( 1 );
   if( p )
      hb_retni( ( p )->rightColumn() );
}

/* int rowCount () const */
HB_FUNC( QT_QTABLEWIDGETSELECTIONRANGE_ROWCOUNT )
{
   QTableWidgetSelectionRange * p = hbqt_par_QTableWidgetSelectionRange( 1 );
   if( p )
      hb_retni( ( p )->rowCount() );
}

/* int topRow () const */
HB_FUNC( QT_QTABLEWIDGETSELECTIONRANGE_TOPROW )
{
   QTableWidgetSelectionRange * p = hbqt_par_QTableWidgetSelectionRange( 1 );
   if( p )
      hb_retni( ( p )->topRow() );
}


#endif /* #if QT_VERSION >= 0x040500 */
