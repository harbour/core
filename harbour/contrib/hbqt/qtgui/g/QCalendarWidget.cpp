/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum HorizontalHeaderFormat { SingleLetterDayNames, ShortDayNames, LongDayNames, NoHorizontalHeader }
 *  enum SelectionMode { NoSelection, SingleSelection }
 *  enum VerticalHeaderFormat { ISOWeekNumbers, NoVerticalHeader }
 */

/*
 *  Constructed[ 38/39 [ 97.44% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QMap<QDate, QTextCharFormat> dateTextFormat () const
 */

#include <QtCore/QPointer>

#include <QtGui/QCalendarWidget>
#include <QtGui/QTextCharFormat>

/*
 * QCalendarWidget ( QWidget * parent = 0 )
 * ~QCalendarWidget ()
 */

typedef struct
{
   QPointer< QCalendarWidget > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QCalendarWidget;

HBQT_GC_FUNC( hbqt_gcRelease_QCalendarWidget )
{
   QCalendarWidget  * ph = NULL ;
   HBQT_GC_T_QCalendarWidget * p = ( HBQT_GC_T_QCalendarWidget * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QCalendarWidget   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QCalendarWidget   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QCalendarWidget          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QCalendarWidget    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QCalendarWidget    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QCalendarWidget( void * pObj, bool bNew )
{
   HBQT_GC_T_QCalendarWidget * p = ( HBQT_GC_T_QCalendarWidget * ) hb_gcAllocate( sizeof( HBQT_GC_T_QCalendarWidget ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QCalendarWidget >( ( QCalendarWidget * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QCalendarWidget;
   p->type = HBQT_TYPE_QCalendarWidget;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QCalendarWidget  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QCalendarWidget", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QCALENDARWIDGET )
{
   QCalendarWidget * pObj = NULL;

   pObj =  new QCalendarWidget( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QCalendarWidget( ( void * ) pObj, true ) );
}

/*
 * int dateEditAcceptDelay () const
 */
HB_FUNC( QT_QCALENDARWIDGET_DATEEDITACCEPTDELAY )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retni( ( p )->dateEditAcceptDelay() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_DATEEDITACCEPTDELAY FP=hb_retni( ( p )->dateEditAcceptDelay() ); p is NULL" ) );
   }
}

/*
 * QTextCharFormat dateTextFormat ( const QDate & date ) const
 */
HB_FUNC( QT_QCALENDARWIDGET_DATETEXTFORMAT )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->dateTextFormat( *hbqt_par_QDate( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_DATETEXTFORMAT FP=hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->dateTextFormat( *hbqt_par_QDate( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * Qt::DayOfWeek firstDayOfWeek () const
 */
HB_FUNC( QT_QCALENDARWIDGET_FIRSTDAYOFWEEK )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retni( ( Qt::DayOfWeek ) ( p )->firstDayOfWeek() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_FIRSTDAYOFWEEK FP=hb_retni( ( Qt::DayOfWeek ) ( p )->firstDayOfWeek() ); p is NULL" ) );
   }
}

/*
 * QTextCharFormat headerTextFormat () const
 */
HB_FUNC( QT_QCALENDARWIDGET_HEADERTEXTFORMAT )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->headerTextFormat() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_HEADERTEXTFORMAT FP=hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->headerTextFormat() ), true ) ); p is NULL" ) );
   }
}

/*
 * HorizontalHeaderFormat horizontalHeaderFormat () const
 */
HB_FUNC( QT_QCALENDARWIDGET_HORIZONTALHEADERFORMAT )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retni( ( QCalendarWidget::HorizontalHeaderFormat ) ( p )->horizontalHeaderFormat() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_HORIZONTALHEADERFORMAT FP=hb_retni( ( QCalendarWidget::HorizontalHeaderFormat ) ( p )->horizontalHeaderFormat() ); p is NULL" ) );
   }
}

/*
 * bool isDateEditEnabled () const
 */
HB_FUNC( QT_QCALENDARWIDGET_ISDATEEDITENABLED )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retl( ( p )->isDateEditEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_ISDATEEDITENABLED FP=hb_retl( ( p )->isDateEditEnabled() ); p is NULL" ) );
   }
}

/*
 * bool isGridVisible () const
 */
HB_FUNC( QT_QCALENDARWIDGET_ISGRIDVISIBLE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retl( ( p )->isGridVisible() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_ISGRIDVISIBLE FP=hb_retl( ( p )->isGridVisible() ); p is NULL" ) );
   }
}

/*
 * bool isNavigationBarVisible () const
 */
HB_FUNC( QT_QCALENDARWIDGET_ISNAVIGATIONBARVISIBLE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retl( ( p )->isNavigationBarVisible() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_ISNAVIGATIONBARVISIBLE FP=hb_retl( ( p )->isNavigationBarVisible() ); p is NULL" ) );
   }
}

/*
 * QDate maximumDate () const
 */
HB_FUNC( QT_QCALENDARWIDGET_MAXIMUMDATE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->maximumDate() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_MAXIMUMDATE FP=hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->maximumDate() ), true ) ); p is NULL" ) );
   }
}

/*
 * QDate minimumDate () const
 */
HB_FUNC( QT_QCALENDARWIDGET_MINIMUMDATE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->minimumDate() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_MINIMUMDATE FP=hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->minimumDate() ), true ) ); p is NULL" ) );
   }
}

/*
 * int monthShown () const
 */
HB_FUNC( QT_QCALENDARWIDGET_MONTHSHOWN )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retni( ( p )->monthShown() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_MONTHSHOWN FP=hb_retni( ( p )->monthShown() ); p is NULL" ) );
   }
}

/*
 * QDate selectedDate () const
 */
HB_FUNC( QT_QCALENDARWIDGET_SELECTEDDATE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->selectedDate() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SELECTEDDATE FP=hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->selectedDate() ), true ) ); p is NULL" ) );
   }
}

/*
 * SelectionMode selectionMode () const
 */
HB_FUNC( QT_QCALENDARWIDGET_SELECTIONMODE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retni( ( QCalendarWidget::SelectionMode ) ( p )->selectionMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SELECTIONMODE FP=hb_retni( ( QCalendarWidget::SelectionMode ) ( p )->selectionMode() ); p is NULL" ) );
   }
}

/*
 * void setDateEditAcceptDelay ( int delay )
 */
HB_FUNC( QT_QCALENDARWIDGET_SETDATEEDITACCEPTDELAY )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setDateEditAcceptDelay( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SETDATEEDITACCEPTDELAY FP=( p )->setDateEditAcceptDelay( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDateEditEnabled ( bool enable )
 */
HB_FUNC( QT_QCALENDARWIDGET_SETDATEEDITENABLED )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setDateEditEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SETDATEEDITENABLED FP=( p )->setDateEditEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDateTextFormat ( const QDate & date, const QTextCharFormat & format )
 */
HB_FUNC( QT_QCALENDARWIDGET_SETDATETEXTFORMAT )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setDateTextFormat( *hbqt_par_QDate( 2 ), *hbqt_par_QTextCharFormat( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SETDATETEXTFORMAT FP=( p )->setDateTextFormat( *hbqt_par_QDate( 2 ), *hbqt_par_QTextCharFormat( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setFirstDayOfWeek ( Qt::DayOfWeek dayOfWeek )
 */
HB_FUNC( QT_QCALENDARWIDGET_SETFIRSTDAYOFWEEK )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setFirstDayOfWeek( ( Qt::DayOfWeek ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SETFIRSTDAYOFWEEK FP=( p )->setFirstDayOfWeek( ( Qt::DayOfWeek ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setHeaderTextFormat ( const QTextCharFormat & format )
 */
HB_FUNC( QT_QCALENDARWIDGET_SETHEADERTEXTFORMAT )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setHeaderTextFormat( *hbqt_par_QTextCharFormat( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SETHEADERTEXTFORMAT FP=( p )->setHeaderTextFormat( *hbqt_par_QTextCharFormat( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setHorizontalHeaderFormat ( HorizontalHeaderFormat format )
 */
HB_FUNC( QT_QCALENDARWIDGET_SETHORIZONTALHEADERFORMAT )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setHorizontalHeaderFormat( ( QCalendarWidget::HorizontalHeaderFormat ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SETHORIZONTALHEADERFORMAT FP=( p )->setHorizontalHeaderFormat( ( QCalendarWidget::HorizontalHeaderFormat ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMaximumDate ( const QDate & date )
 */
HB_FUNC( QT_QCALENDARWIDGET_SETMAXIMUMDATE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setMaximumDate( *hbqt_par_QDate( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SETMAXIMUMDATE FP=( p )->setMaximumDate( *hbqt_par_QDate( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMinimumDate ( const QDate & date )
 */
HB_FUNC( QT_QCALENDARWIDGET_SETMINIMUMDATE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setMinimumDate( *hbqt_par_QDate( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SETMINIMUMDATE FP=( p )->setMinimumDate( *hbqt_par_QDate( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSelectionMode ( SelectionMode mode )
 */
HB_FUNC( QT_QCALENDARWIDGET_SETSELECTIONMODE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setSelectionMode( ( QCalendarWidget::SelectionMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SETSELECTIONMODE FP=( p )->setSelectionMode( ( QCalendarWidget::SelectionMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setVerticalHeaderFormat ( VerticalHeaderFormat format )
 */
HB_FUNC( QT_QCALENDARWIDGET_SETVERTICALHEADERFORMAT )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setVerticalHeaderFormat( ( QCalendarWidget::VerticalHeaderFormat ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SETVERTICALHEADERFORMAT FP=( p )->setVerticalHeaderFormat( ( QCalendarWidget::VerticalHeaderFormat ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWeekdayTextFormat ( Qt::DayOfWeek dayOfWeek, const QTextCharFormat & format )
 */
HB_FUNC( QT_QCALENDARWIDGET_SETWEEKDAYTEXTFORMAT )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setWeekdayTextFormat( ( Qt::DayOfWeek ) hb_parni( 2 ), *hbqt_par_QTextCharFormat( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SETWEEKDAYTEXTFORMAT FP=( p )->setWeekdayTextFormat( ( Qt::DayOfWeek ) hb_parni( 2 ), *hbqt_par_QTextCharFormat( 3 ) ); p is NULL" ) );
   }
}

/*
 * VerticalHeaderFormat verticalHeaderFormat () const
 */
HB_FUNC( QT_QCALENDARWIDGET_VERTICALHEADERFORMAT )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retni( ( QCalendarWidget::VerticalHeaderFormat ) ( p )->verticalHeaderFormat() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_VERTICALHEADERFORMAT FP=hb_retni( ( QCalendarWidget::VerticalHeaderFormat ) ( p )->verticalHeaderFormat() ); p is NULL" ) );
   }
}

/*
 * QTextCharFormat weekdayTextFormat ( Qt::DayOfWeek dayOfWeek ) const
 */
HB_FUNC( QT_QCALENDARWIDGET_WEEKDAYTEXTFORMAT )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->weekdayTextFormat( ( Qt::DayOfWeek ) hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_WEEKDAYTEXTFORMAT FP=hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->weekdayTextFormat( ( Qt::DayOfWeek ) hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * int yearShown () const
 */
HB_FUNC( QT_QCALENDARWIDGET_YEARSHOWN )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retni( ( p )->yearShown() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_YEARSHOWN FP=hb_retni( ( p )->yearShown() ); p is NULL" ) );
   }
}

/*
 * void setCurrentPage ( int year, int month )
 */
HB_FUNC( QT_QCALENDARWIDGET_SETCURRENTPAGE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setCurrentPage( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SETCURRENTPAGE FP=( p )->setCurrentPage( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setDateRange ( const QDate & min, const QDate & max )
 */
HB_FUNC( QT_QCALENDARWIDGET_SETDATERANGE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setDateRange( *hbqt_par_QDate( 2 ), *hbqt_par_QDate( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SETDATERANGE FP=( p )->setDateRange( *hbqt_par_QDate( 2 ), *hbqt_par_QDate( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setGridVisible ( bool show )
 */
HB_FUNC( QT_QCALENDARWIDGET_SETGRIDVISIBLE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setGridVisible( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SETGRIDVISIBLE FP=( p )->setGridVisible( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setNavigationBarVisible ( bool visible )
 */
HB_FUNC( QT_QCALENDARWIDGET_SETNAVIGATIONBARVISIBLE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setNavigationBarVisible( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SETNAVIGATIONBARVISIBLE FP=( p )->setNavigationBarVisible( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSelectedDate ( const QDate & date )
 */
HB_FUNC( QT_QCALENDARWIDGET_SETSELECTEDDATE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setSelectedDate( *hbqt_par_QDate( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SETSELECTEDDATE FP=( p )->setSelectedDate( *hbqt_par_QDate( 2 ) ); p is NULL" ) );
   }
}

/*
 * void showNextMonth ()
 */
HB_FUNC( QT_QCALENDARWIDGET_SHOWNEXTMONTH )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->showNextMonth();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SHOWNEXTMONTH FP=( p )->showNextMonth(); p is NULL" ) );
   }
}

/*
 * void showNextYear ()
 */
HB_FUNC( QT_QCALENDARWIDGET_SHOWNEXTYEAR )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->showNextYear();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SHOWNEXTYEAR FP=( p )->showNextYear(); p is NULL" ) );
   }
}

/*
 * void showPreviousMonth ()
 */
HB_FUNC( QT_QCALENDARWIDGET_SHOWPREVIOUSMONTH )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->showPreviousMonth();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SHOWPREVIOUSMONTH FP=( p )->showPreviousMonth(); p is NULL" ) );
   }
}

/*
 * void showPreviousYear ()
 */
HB_FUNC( QT_QCALENDARWIDGET_SHOWPREVIOUSYEAR )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->showPreviousYear();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SHOWPREVIOUSYEAR FP=( p )->showPreviousYear(); p is NULL" ) );
   }
}

/*
 * void showSelectedDate ()
 */
HB_FUNC( QT_QCALENDARWIDGET_SHOWSELECTEDDATE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->showSelectedDate();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SHOWSELECTEDDATE FP=( p )->showSelectedDate(); p is NULL" ) );
   }
}

/*
 * void showToday ()
 */
HB_FUNC( QT_QCALENDARWIDGET_SHOWTODAY )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->showToday();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCALENDARWIDGET_SHOWTODAY FP=( p )->showToday(); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
