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
 *  enum HorizontalHeaderFormat { SingleLetterDayNames, ShortDayNames, LongDayNames, NoHorizontalHeader }
 *  enum SelectionMode { NoSelection, SingleSelection }
 *  enum VerticalHeaderFormat { ISOWeekNumbers, NoVerticalHeader }
 */

/*
 *  Constructed[ 38/39 [ 97.44% ] ]
 *
 *  *** Unconvered Prototypes ***
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
   HBQT_GC_T_QCalendarWidget * p = ( HBQT_GC_T_QCalendarWidget * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QCalendarWidget * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
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

   return p;
}

HB_FUNC( QT_QCALENDARWIDGET )
{
   QCalendarWidget * pObj = NULL;

   pObj = new QCalendarWidget( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QCalendarWidget( ( void * ) pObj, true ) );
}

/* int dateEditAcceptDelay () const */
HB_FUNC( QT_QCALENDARWIDGET_DATEEDITACCEPTDELAY )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retni( ( p )->dateEditAcceptDelay() );
}

/* QTextCharFormat dateTextFormat ( const QDate & date ) const */
HB_FUNC( QT_QCALENDARWIDGET_DATETEXTFORMAT )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->dateTextFormat( *hbqt_par_QDate( 2 ) ) ), true ) );
}

/* Qt::DayOfWeek firstDayOfWeek () const */
HB_FUNC( QT_QCALENDARWIDGET_FIRSTDAYOFWEEK )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retni( ( Qt::DayOfWeek ) ( p )->firstDayOfWeek() );
}

/* QTextCharFormat headerTextFormat () const */
HB_FUNC( QT_QCALENDARWIDGET_HEADERTEXTFORMAT )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->headerTextFormat() ), true ) );
}

/* HorizontalHeaderFormat horizontalHeaderFormat () const */
HB_FUNC( QT_QCALENDARWIDGET_HORIZONTALHEADERFORMAT )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retni( ( QCalendarWidget::HorizontalHeaderFormat ) ( p )->horizontalHeaderFormat() );
}

/* bool isDateEditEnabled () const */
HB_FUNC( QT_QCALENDARWIDGET_ISDATEEDITENABLED )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retl( ( p )->isDateEditEnabled() );
}

/* bool isGridVisible () const */
HB_FUNC( QT_QCALENDARWIDGET_ISGRIDVISIBLE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retl( ( p )->isGridVisible() );
}

/* bool isNavigationBarVisible () const */
HB_FUNC( QT_QCALENDARWIDGET_ISNAVIGATIONBARVISIBLE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retl( ( p )->isNavigationBarVisible() );
}

/* QDate maximumDate () const */
HB_FUNC( QT_QCALENDARWIDGET_MAXIMUMDATE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->maximumDate() ), true ) );
}

/* QDate minimumDate () const */
HB_FUNC( QT_QCALENDARWIDGET_MINIMUMDATE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->minimumDate() ), true ) );
}

/* int monthShown () const */
HB_FUNC( QT_QCALENDARWIDGET_MONTHSHOWN )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retni( ( p )->monthShown() );
}

/* QDate selectedDate () const */
HB_FUNC( QT_QCALENDARWIDGET_SELECTEDDATE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->selectedDate() ), true ) );
}

/* SelectionMode selectionMode () const */
HB_FUNC( QT_QCALENDARWIDGET_SELECTIONMODE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retni( ( QCalendarWidget::SelectionMode ) ( p )->selectionMode() );
}

/* void setDateEditAcceptDelay ( int delay ) */
HB_FUNC( QT_QCALENDARWIDGET_SETDATEEDITACCEPTDELAY )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setDateEditAcceptDelay( hb_parni( 2 ) );
}

/* void setDateEditEnabled ( bool enable ) */
HB_FUNC( QT_QCALENDARWIDGET_SETDATEEDITENABLED )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setDateEditEnabled( hb_parl( 2 ) );
}

/* void setDateTextFormat ( const QDate & date, const QTextCharFormat & format ) */
HB_FUNC( QT_QCALENDARWIDGET_SETDATETEXTFORMAT )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setDateTextFormat( *hbqt_par_QDate( 2 ), *hbqt_par_QTextCharFormat( 3 ) );
}

/* void setFirstDayOfWeek ( Qt::DayOfWeek dayOfWeek ) */
HB_FUNC( QT_QCALENDARWIDGET_SETFIRSTDAYOFWEEK )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setFirstDayOfWeek( ( Qt::DayOfWeek ) hb_parni( 2 ) );
}

/* void setHeaderTextFormat ( const QTextCharFormat & format ) */
HB_FUNC( QT_QCALENDARWIDGET_SETHEADERTEXTFORMAT )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setHeaderTextFormat( *hbqt_par_QTextCharFormat( 2 ) );
}

/* void setHorizontalHeaderFormat ( HorizontalHeaderFormat format ) */
HB_FUNC( QT_QCALENDARWIDGET_SETHORIZONTALHEADERFORMAT )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setHorizontalHeaderFormat( ( QCalendarWidget::HorizontalHeaderFormat ) hb_parni( 2 ) );
}

/* void setMaximumDate ( const QDate & date ) */
HB_FUNC( QT_QCALENDARWIDGET_SETMAXIMUMDATE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setMaximumDate( *hbqt_par_QDate( 2 ) );
}

/* void setMinimumDate ( const QDate & date ) */
HB_FUNC( QT_QCALENDARWIDGET_SETMINIMUMDATE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setMinimumDate( *hbqt_par_QDate( 2 ) );
}

/* void setSelectionMode ( SelectionMode mode ) */
HB_FUNC( QT_QCALENDARWIDGET_SETSELECTIONMODE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setSelectionMode( ( QCalendarWidget::SelectionMode ) hb_parni( 2 ) );
}

/* void setVerticalHeaderFormat ( VerticalHeaderFormat format ) */
HB_FUNC( QT_QCALENDARWIDGET_SETVERTICALHEADERFORMAT )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setVerticalHeaderFormat( ( QCalendarWidget::VerticalHeaderFormat ) hb_parni( 2 ) );
}

/* void setWeekdayTextFormat ( Qt::DayOfWeek dayOfWeek, const QTextCharFormat & format ) */
HB_FUNC( QT_QCALENDARWIDGET_SETWEEKDAYTEXTFORMAT )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setWeekdayTextFormat( ( Qt::DayOfWeek ) hb_parni( 2 ), *hbqt_par_QTextCharFormat( 3 ) );
}

/* VerticalHeaderFormat verticalHeaderFormat () const */
HB_FUNC( QT_QCALENDARWIDGET_VERTICALHEADERFORMAT )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retni( ( QCalendarWidget::VerticalHeaderFormat ) ( p )->verticalHeaderFormat() );
}

/* QTextCharFormat weekdayTextFormat ( Qt::DayOfWeek dayOfWeek ) const */
HB_FUNC( QT_QCALENDARWIDGET_WEEKDAYTEXTFORMAT )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->weekdayTextFormat( ( Qt::DayOfWeek ) hb_parni( 2 ) ) ), true ) );
}

/* int yearShown () const */
HB_FUNC( QT_QCALENDARWIDGET_YEARSHOWN )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      hb_retni( ( p )->yearShown() );
}

/* void setCurrentPage ( int year, int month ) */
HB_FUNC( QT_QCALENDARWIDGET_SETCURRENTPAGE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setCurrentPage( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void setDateRange ( const QDate & min, const QDate & max ) */
HB_FUNC( QT_QCALENDARWIDGET_SETDATERANGE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setDateRange( *hbqt_par_QDate( 2 ), *hbqt_par_QDate( 3 ) );
}

/* void setGridVisible ( bool show ) */
HB_FUNC( QT_QCALENDARWIDGET_SETGRIDVISIBLE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setGridVisible( hb_parl( 2 ) );
}

/* void setNavigationBarVisible ( bool visible ) */
HB_FUNC( QT_QCALENDARWIDGET_SETNAVIGATIONBARVISIBLE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setNavigationBarVisible( hb_parl( 2 ) );
}

/* void setSelectedDate ( const QDate & date ) */
HB_FUNC( QT_QCALENDARWIDGET_SETSELECTEDDATE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->setSelectedDate( *hbqt_par_QDate( 2 ) );
}

/* void showNextMonth () */
HB_FUNC( QT_QCALENDARWIDGET_SHOWNEXTMONTH )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->showNextMonth();
}

/* void showNextYear () */
HB_FUNC( QT_QCALENDARWIDGET_SHOWNEXTYEAR )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->showNextYear();
}

/* void showPreviousMonth () */
HB_FUNC( QT_QCALENDARWIDGET_SHOWPREVIOUSMONTH )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->showPreviousMonth();
}

/* void showPreviousYear () */
HB_FUNC( QT_QCALENDARWIDGET_SHOWPREVIOUSYEAR )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->showPreviousYear();
}

/* void showSelectedDate () */
HB_FUNC( QT_QCALENDARWIDGET_SHOWSELECTEDDATE )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->showSelectedDate();
}

/* void showToday () */
HB_FUNC( QT_QCALENDARWIDGET_SHOWTODAY )
{
   QCalendarWidget * p = hbqt_par_QCalendarWidget( 1 );
   if( p )
      ( p )->showToday();
}


#endif /* #if QT_VERSION >= 0x040500 */
