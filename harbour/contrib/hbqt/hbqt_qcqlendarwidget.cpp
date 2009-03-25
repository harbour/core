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

#if QT_VERSION >= 0x040500

#include <QtGui/QCalendarWidget>

/*----------------------------------------------------------------------*/
/*
QCalendarWidget( QWidget * parent = 0 )
*/
HB_FUNC( QT_QCALENDARWIDGET )
{
   QCalendarWidget * cal = NULL;
   QWidget * parent = ( QWidget* ) hb_parptr( 1 );
   cal = new QCalendarWidget( parent );
   hb_retptr( ( QCalendarWidget* ) cal );
}

/*
int dateEditAcceptDelay () const
*/
HB_FUNC( QT_QCALENDARWIDGET_DATEEDITACCEPTDELAY )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   int i = par1->dateEditAcceptDelay();
   hb_retni( i );
}

/*
Qt::DayOfWeek firstDayOfWeek () const
*/
HB_FUNC( QT_QCALENDARWIDGET_FIRSTDAYOFWEEK )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   int i = par1->firstDayOfWeek();
   hb_retni( i );
}

/*
HorizontalHeaderFormat horizontalHeaderFormat () const
*/
HB_FUNC( QT_QCALENDARWIDGET_HORIZONTALHEADERFORMAT )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   int i = par1->horizontalHeaderFormat();
   hb_retni( i );
}

/*
bool isDateEditEnabled () const
*/
HB_FUNC( QT_QCALENDARWIDGET_ISDATEEDITENABLED )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   bool b = par1->isDateEditEnabled();
   hb_retl( b );
}

/*
bool isGridVisible () const
*/
HB_FUNC( QT_QCALENDARWIDGET_ISGRIDVISIBLE )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   bool b = par1->isGridVisible();
   hb_retl( b );
}

/*
bool isNavigationBarVisible () const
*/
HB_FUNC( QT_QCALENDARWIDGET_ISNAVIGATIONBARVISIBLE )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   bool b = par1->isNavigationBarVisible();
   hb_retl( b );
}

/*
int monthShown () const
*/
HB_FUNC( QT_QCALENDARWIDGET_MONTHSHOWN )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   int i = par1->monthShown();
   hb_retni( i );
}

/*
SelectionMode selectionMode () const
*/
HB_FUNC( QT_QCALENDARWIDGET_SELECTIONMODE )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   int i = par1->selectionMode();
   hb_retni( i );
}

/*
void setDateEditAcceptDelay( int delay )
*/
HB_FUNC( QT_QCALENDARWIDGET_SETDATEEDITACCEPTDELAY )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   int par2 = hb_parni( 2 );
   par1->setDateEditAcceptDelay( par2 );
}

/*
void setDateEditEnabled( bool enable )
*/
HB_FUNC( QT_QCALENDARWIDGET_SETDATEEDITENABLED )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   bool par2 = hb_parl( 2 );
   par1->setDateEditEnabled( par2 );
}

/*
void setFirstDayOfWeek( Qt::DayOfWeek dayOfWeek )
*/
HB_FUNC( QT_QCALENDARWIDGET_SETFIRSTDAYOFWEEK )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   int par2 = hb_parni( 2 );
   par1->setFirstDayOfWeek( ( Qt::DayOfWeek ) par2 );
}

/*
void setHorizontalHeaderFormat( HorizontalHeaderFormat format )
*/
HB_FUNC( QT_QCALENDARWIDGET_SETHORIZONTALHEADERFORMAT )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   int par2 = hb_parni( 2 );
   par1->setHorizontalHeaderFormat( ( QCalendarWidget::HorizontalHeaderFormat ) par2 );
}

/*
void setSelectionMode( SelectionMode mode )
*/
HB_FUNC( QT_QCALENDARWIDGET_SETSELECTIONMODE )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   int par2 = hb_parni( 2 );
   par1->setSelectionMode( ( QCalendarWidget::SelectionMode ) par2 );
}

/*
void setVerticalHeaderFormat( VerticalHeaderFormat format )
*/
HB_FUNC( QT_QCALENDARWIDGET_SETVERTICALHEADERFORMAT )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   int par2 = hb_parni( 2 );
   par1->setVerticalHeaderFormat( ( QCalendarWidget::VerticalHeaderFormat ) par2 );
}

/*
VerticalHeaderFormat verticalHeaderFormat () const
*/
HB_FUNC( QT_QCALENDARWIDGET_VERTICALHEADERFORMAT )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   int i = par1->verticalHeaderFormat();
   hb_retni( i );
}

/*
int yearShown () const
*/
HB_FUNC( QT_QCALENDARWIDGET_YEARSHOWN )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   int i = par1->yearShown();
   hb_retni( i );
}

/*
void setCurrentPage( int year, int month )
*/
HB_FUNC( QT_QCALENDARWIDGET_SETCURRENTPAGE )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   int par2 = hb_parni( 2 );
   int par3 = hb_parni( 3 );
   par1->setCurrentPage( par2, par3 );
}

/*
void setGridVisible( bool show )
*/
HB_FUNC( QT_QCALENDARWIDGET_SETGRIDVISIBLE )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   bool par2 = hb_parl( 2 );
   par1->setGridVisible( par2 );
}

/*
void setNavigationBarVisible( bool visible )
*/
HB_FUNC( QT_QCALENDARWIDGET_SETNAVIGATIONBARVISIBLE )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   bool par2 = hb_parl( 2 );
   par1->setNavigationBarVisible( par2 );
}

/*
void showNextMonth ()
*/
HB_FUNC( QT_QCALENDARWIDGET_SHOWNEXTMONTH )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   par1->showNextMonth();
}

/*
void showNextYear ()
*/
HB_FUNC( QT_QCALENDARWIDGET_SHOWNEXTYEAR )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   par1->showNextYear();
}

/*
void showPreviousMonth ()
*/
HB_FUNC( QT_QCALENDARWIDGET_SHOWPREVIOUSMONTH )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   par1->showPreviousMonth();
}

/*
void showPreviousYear ()
*/
HB_FUNC( QT_QCALENDARWIDGET_SHOWPREVIOUSYEAR )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   par1->showPreviousYear();
}

/*
void showSelectedDate ()
*/
HB_FUNC( QT_QCALENDARWIDGET_SHOWSELECTEDDATE )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   par1->showSelectedDate();
}

/*
void showToday ()
*/
HB_FUNC( QT_QCALENDARWIDGET_SHOWTODAY )
{
   QCalendarWidget * par1 = ( QCalendarWidget * ) hb_parptr( 1 );
   par1->showToday();
}

/*----------------------------------------------------------------------*/
#endif
