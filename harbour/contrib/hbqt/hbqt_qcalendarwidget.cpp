/*
 * $Id$
 */
   
/* 
 * Harbour Project source code:
 * QT wrapper main header
 * 
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * www - http://www.harbour-project.org
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

#include "hbapi.h"
#include "hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/


/*
 *  Constructed[ 16/28 [ 57.14% ] ]
 *  
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *  
 *  QMap<QDate, QTextCharFormat> dateTextFormat () const
 *  QTextCharFormat dateTextFormat ( const QDate & date ) const
 *  QTextCharFormat headerTextFormat () const
 *  QDate maximumDate () const
 *  QDate minimumDate () const
 *  QDate selectedDate () const
 *  void setDateTextFormat ( const QDate & date, const QTextCharFormat & format )
 *  void setHeaderTextFormat ( const QTextCharFormat & format )
 *  void setMaximumDate ( const QDate & date )
 *  void setMinimumDate ( const QDate & date )
 *  void setWeekdayTextFormat ( Qt::DayOfWeek dayOfWeek, const QTextCharFormat & format )
 *  QTextCharFormat weekdayTextFormat ( Qt::DayOfWeek dayOfWeek ) const
 */ 


#include <QtGui/QCalendarWidget>


/*
 * QCalendarWidget ( QWidget * parent = 0 )
 * ~QCalendarWidget ()
 */
HB_FUNC( QT_QCALENDARWIDGET )
{
   hb_retptr( ( QCalendarWidget* ) new QCalendarWidget( hbqt_par_QWidget( 1 ) ) );
}

/*
 * int dateEditAcceptDelay () const
 */
HB_FUNC( QT_QCALENDARWIDGET_DATEEDITACCEPTDELAY )
{
   hb_retni( hbqt_par_QCalendarWidget( 1 )->dateEditAcceptDelay(  ) );
}

/*
 * Qt::DayOfWeek firstDayOfWeek () const
 */
HB_FUNC( QT_QCALENDARWIDGET_FIRSTDAYOFWEEK )
{
   hb_retni( hbqt_par_QCalendarWidget( 1 )->firstDayOfWeek(  ) );
}

/*
 * HorizontalHeaderFormat horizontalHeaderFormat () const
 */
HB_FUNC( QT_QCALENDARWIDGET_HORIZONTALHEADERFORMAT )
{
   hb_retni( hbqt_par_QCalendarWidget( 1 )->horizontalHeaderFormat(  ) );
}

/*
 * bool isDateEditEnabled () const
 */
HB_FUNC( QT_QCALENDARWIDGET_ISDATEEDITENABLED )
{
   hb_retl( hbqt_par_QCalendarWidget( 1 )->isDateEditEnabled(  ) );
}

/*
 * bool isGridVisible () const
 */
HB_FUNC( QT_QCALENDARWIDGET_ISGRIDVISIBLE )
{
   hb_retl( hbqt_par_QCalendarWidget( 1 )->isGridVisible(  ) );
}

/*
 * bool isNavigationBarVisible () const
 */
HB_FUNC( QT_QCALENDARWIDGET_ISNAVIGATIONBARVISIBLE )
{
   hb_retl( hbqt_par_QCalendarWidget( 1 )->isNavigationBarVisible(  ) );
}

/*
 * int monthShown () const
 */
HB_FUNC( QT_QCALENDARWIDGET_MONTHSHOWN )
{
   hb_retni( hbqt_par_QCalendarWidget( 1 )->monthShown(  ) );
}

/*
 * SelectionMode selectionMode () const
 */
HB_FUNC( QT_QCALENDARWIDGET_SELECTIONMODE )
{
   hb_retni( hbqt_par_QCalendarWidget( 1 )->selectionMode(  ) );
}

/*
 * void setDateEditAcceptDelay ( int delay )
 */
HB_FUNC( QT_QCALENDARWIDGET_SETDATEEDITACCEPTDELAY )
{
   hbqt_par_QCalendarWidget( 1 )->setDateEditAcceptDelay( hb_parni( 2 ) );
}

/*
 * void setDateEditEnabled ( bool enable )
 */
HB_FUNC( QT_QCALENDARWIDGET_SETDATEEDITENABLED )
{
   hbqt_par_QCalendarWidget( 1 )->setDateEditEnabled( hb_parl( 2 ) );
}

/*
 * void setFirstDayOfWeek ( Qt::DayOfWeek dayOfWeek )
 */
HB_FUNC( QT_QCALENDARWIDGET_SETFIRSTDAYOFWEEK )
{
   hbqt_par_QCalendarWidget( 1 )->setFirstDayOfWeek( ( Qt::DayOfWeek ) hb_parni( 2 ) );
}

/*
 * void setHorizontalHeaderFormat ( HorizontalHeaderFormat format )
 */
HB_FUNC( QT_QCALENDARWIDGET_SETHORIZONTALHEADERFORMAT )
{
   hbqt_par_QCalendarWidget( 1 )->setHorizontalHeaderFormat( ( QCalendarWidget::HorizontalHeaderFormat ) hb_parni( 2 ) );
}

/*
 * void setSelectionMode ( SelectionMode mode )
 */
HB_FUNC( QT_QCALENDARWIDGET_SETSELECTIONMODE )
{
   hbqt_par_QCalendarWidget( 1 )->setSelectionMode( ( QCalendarWidget::SelectionMode ) hb_parni( 2 ) );
}

/*
 * void setVerticalHeaderFormat ( VerticalHeaderFormat format )
 */
HB_FUNC( QT_QCALENDARWIDGET_SETVERTICALHEADERFORMAT )
{
   hbqt_par_QCalendarWidget( 1 )->setVerticalHeaderFormat( ( QCalendarWidget::VerticalHeaderFormat ) hb_parni( 2 ) );
}

/*
 * VerticalHeaderFormat verticalHeaderFormat () const
 */
HB_FUNC( QT_QCALENDARWIDGET_VERTICALHEADERFORMAT )
{
   hb_retni( hbqt_par_QCalendarWidget( 1 )->verticalHeaderFormat(  ) );
}

/*
 * int yearShown () const
 */
HB_FUNC( QT_QCALENDARWIDGET_YEARSHOWN )
{
   hb_retni( hbqt_par_QCalendarWidget( 1 )->yearShown(  ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

