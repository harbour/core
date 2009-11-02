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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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
#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum Section { NoSection, AmPmSection, MSecSection, SecondSection, ..., YearSection }
 *  flags Sections
 */

#include <QtCore/QPointer>

#include <QtGui/QDateTimeEdit>


/*
 * QDateTimeEdit ( QWidget * parent = 0 )
 * QDateTimeEdit ( const QDateTime & datetime, QWidget * parent = 0 )
 * QDateTimeEdit ( const QDate & date, QWidget * parent = 0 )
 * QDateTimeEdit ( const QTime & time, QWidget * parent = 0 )
 */

QT_G_FUNC( release_QDateTimeEdit )
{
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "release_QDateTimeEdit               %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      const QMetaObject * m = ( ( QObject * ) ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         ( ( QDateTimeEdit * ) ph )->~QDateTimeEdit();
         ph = NULL;
      }
      else
      {
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "  Object Name Missing: QDateTimeEdit" );  OutputDebugString( str );
#endif
      }
   }
   else
   {
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "! ph____QDateTimeEdit" );  OutputDebugString( str );
#endif
   }
}

HB_FUNC( QT_QDATETIMEEDIT )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );
   QPointer< QDateTimeEdit > pObj = NULL;
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "   GC:  new QDateTimeEdit               %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif

   pObj = ( QDateTimeEdit* ) new QDateTimeEdit( hbqt_par_QWidget( 1 ) ) ;

#if defined(__debug__)
hb_snprintf( str, sizeof(str), "   GC:                                  %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif
   p->ph = pObj;
   p->func = release_QDateTimeEdit;

   hb_retptrGC( p );
}
/*
 * bool calendarPopup () const
 */
HB_FUNC( QT_QDATETIMEEDIT_CALENDARPOPUP )
{
   hb_retl( hbqt_par_QDateTimeEdit( 1 )->calendarPopup() );
}

/*
 * QCalendarWidget * calendarWidget () const
 */
HB_FUNC( QT_QDATETIMEEDIT_CALENDARWIDGET )
{
   hb_retptr( ( QCalendarWidget* ) hbqt_par_QDateTimeEdit( 1 )->calendarWidget() );
}

/*
 * void clearMaximumDate ()
 */
HB_FUNC( QT_QDATETIMEEDIT_CLEARMAXIMUMDATE )
{
   hbqt_par_QDateTimeEdit( 1 )->clearMaximumDate();
}

/*
 * void clearMaximumDateTime ()
 */
HB_FUNC( QT_QDATETIMEEDIT_CLEARMAXIMUMDATETIME )
{
   hbqt_par_QDateTimeEdit( 1 )->clearMaximumDateTime();
}

/*
 * void clearMaximumTime ()
 */
HB_FUNC( QT_QDATETIMEEDIT_CLEARMAXIMUMTIME )
{
   hbqt_par_QDateTimeEdit( 1 )->clearMaximumTime();
}

/*
 * void clearMinimumDate ()
 */
HB_FUNC( QT_QDATETIMEEDIT_CLEARMINIMUMDATE )
{
   hbqt_par_QDateTimeEdit( 1 )->clearMinimumDate();
}

/*
 * void clearMinimumDateTime ()
 */
HB_FUNC( QT_QDATETIMEEDIT_CLEARMINIMUMDATETIME )
{
   hbqt_par_QDateTimeEdit( 1 )->clearMinimumDateTime();
}

/*
 * void clearMinimumTime ()
 */
HB_FUNC( QT_QDATETIMEEDIT_CLEARMINIMUMTIME )
{
   hbqt_par_QDateTimeEdit( 1 )->clearMinimumTime();
}

/*
 * Section currentSection () const
 */
HB_FUNC( QT_QDATETIMEEDIT_CURRENTSECTION )
{
   hb_retni( ( QDateTimeEdit::Section ) hbqt_par_QDateTimeEdit( 1 )->currentSection() );
}

/*
 * int currentSectionIndex () const
 */
HB_FUNC( QT_QDATETIMEEDIT_CURRENTSECTIONINDEX )
{
   hb_retni( hbqt_par_QDateTimeEdit( 1 )->currentSectionIndex() );
}

/*
 * QDate date () const
 */
HB_FUNC( QT_QDATETIMEEDIT_DATE )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QDate( hbqt_par_QDateTimeEdit( 1 )->date() ), release_QDate ) );
}

/*
 * QDateTime dateTime () const
 */
HB_FUNC( QT_QDATETIMEEDIT_DATETIME )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QDateTime( hbqt_par_QDateTimeEdit( 1 )->dateTime() ), release_QDateTime ) );
}

/*
 * QString displayFormat () const
 */
HB_FUNC( QT_QDATETIMEEDIT_DISPLAYFORMAT )
{
   hb_retc( hbqt_par_QDateTimeEdit( 1 )->displayFormat().toAscii().data() );
}

/*
 * Sections displayedSections () const
 */
HB_FUNC( QT_QDATETIMEEDIT_DISPLAYEDSECTIONS )
{
   hb_retni( ( QDateTimeEdit::Sections ) hbqt_par_QDateTimeEdit( 1 )->displayedSections() );
}

/*
 * QDate maximumDate () const
 */
HB_FUNC( QT_QDATETIMEEDIT_MAXIMUMDATE )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QDate( hbqt_par_QDateTimeEdit( 1 )->maximumDate() ), release_QDate ) );
}

/*
 * QDateTime maximumDateTime () const
 */
HB_FUNC( QT_QDATETIMEEDIT_MAXIMUMDATETIME )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QDateTime( hbqt_par_QDateTimeEdit( 1 )->maximumDateTime() ), release_QDateTime ) );
}

/*
 * QTime maximumTime () const
 */
HB_FUNC( QT_QDATETIMEEDIT_MAXIMUMTIME )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QTime( hbqt_par_QDateTimeEdit( 1 )->maximumTime() ), release_QTime ) );
}

/*
 * QDate minimumDate () const
 */
HB_FUNC( QT_QDATETIMEEDIT_MINIMUMDATE )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QDate( hbqt_par_QDateTimeEdit( 1 )->minimumDate() ), release_QDate ) );
}

/*
 * QDateTime minimumDateTime () const
 */
HB_FUNC( QT_QDATETIMEEDIT_MINIMUMDATETIME )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QDateTime( hbqt_par_QDateTimeEdit( 1 )->minimumDateTime() ), release_QDateTime ) );
}

/*
 * QTime minimumTime () const
 */
HB_FUNC( QT_QDATETIMEEDIT_MINIMUMTIME )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QTime( hbqt_par_QDateTimeEdit( 1 )->minimumTime() ), release_QTime ) );
}

/*
 * Section sectionAt ( int index ) const
 */
HB_FUNC( QT_QDATETIMEEDIT_SECTIONAT )
{
   hb_retni( ( QDateTimeEdit::Section ) hbqt_par_QDateTimeEdit( 1 )->sectionAt( hb_parni( 2 ) ) );
}

/*
 * int sectionCount () const
 */
HB_FUNC( QT_QDATETIMEEDIT_SECTIONCOUNT )
{
   hb_retni( hbqt_par_QDateTimeEdit( 1 )->sectionCount() );
}

/*
 * QString sectionText ( Section section ) const
 */
HB_FUNC( QT_QDATETIMEEDIT_SECTIONTEXT )
{
   hb_retc( hbqt_par_QDateTimeEdit( 1 )->sectionText( ( QDateTimeEdit::Section ) hb_parni( 2 ) ).toAscii().data() );
}

/*
 * void setCalendarPopup ( bool enable )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETCALENDARPOPUP )
{
   hbqt_par_QDateTimeEdit( 1 )->setCalendarPopup( hb_parl( 2 ) );
}

/*
 * void setCalendarWidget ( QCalendarWidget * calendarWidget )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETCALENDARWIDGET )
{
   hbqt_par_QDateTimeEdit( 1 )->setCalendarWidget( hbqt_par_QCalendarWidget( 2 ) );
}

/*
 * void setCurrentSection ( Section section )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETCURRENTSECTION )
{
   hbqt_par_QDateTimeEdit( 1 )->setCurrentSection( ( QDateTimeEdit::Section ) hb_parni( 2 ) );
}

/*
 * void setCurrentSectionIndex ( int index )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETCURRENTSECTIONINDEX )
{
   hbqt_par_QDateTimeEdit( 1 )->setCurrentSectionIndex( hb_parni( 2 ) );
}

/*
 * void setDateRange ( const QDate & min, const QDate & max )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETDATERANGE )
{
   hbqt_par_QDateTimeEdit( 1 )->setDateRange( *hbqt_par_QDate( 2 ), *hbqt_par_QDate( 3 ) );
}

/*
 * void setDateTimeRange ( const QDateTime & min, const QDateTime & max )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETDATETIMERANGE )
{
   hbqt_par_QDateTimeEdit( 1 )->setDateTimeRange( *hbqt_par_QDateTime( 2 ), *hbqt_par_QDateTime( 3 ) );
}

/*
 * void setDisplayFormat ( const QString & format )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETDISPLAYFORMAT )
{
   hbqt_par_QDateTimeEdit( 1 )->setDisplayFormat( hbqt_par_QString( 2 ) );
}

/*
 * void setMaximumDate ( const QDate & max )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETMAXIMUMDATE )
{
   hbqt_par_QDateTimeEdit( 1 )->setMaximumDate( *hbqt_par_QDate( 2 ) );
}

/*
 * void setMaximumDateTime ( const QDateTime & dt )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETMAXIMUMDATETIME )
{
   hbqt_par_QDateTimeEdit( 1 )->setMaximumDateTime( *hbqt_par_QDateTime( 2 ) );
}

/*
 * void setMaximumTime ( const QTime & max )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETMAXIMUMTIME )
{
   hbqt_par_QDateTimeEdit( 1 )->setMaximumTime( *hbqt_par_QTime( 2 ) );
}

/*
 * void setMinimumDate ( const QDate & min )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETMINIMUMDATE )
{
   hbqt_par_QDateTimeEdit( 1 )->setMinimumDate( *hbqt_par_QDate( 2 ) );
}

/*
 * void setMinimumDateTime ( const QDateTime & dt )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETMINIMUMDATETIME )
{
   hbqt_par_QDateTimeEdit( 1 )->setMinimumDateTime( *hbqt_par_QDateTime( 2 ) );
}

/*
 * void setMinimumTime ( const QTime & min )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETMINIMUMTIME )
{
   hbqt_par_QDateTimeEdit( 1 )->setMinimumTime( *hbqt_par_QTime( 2 ) );
}

/*
 * void setSelectedSection ( Section section )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETSELECTEDSECTION )
{
   hbqt_par_QDateTimeEdit( 1 )->setSelectedSection( ( QDateTimeEdit::Section ) hb_parni( 2 ) );
}

/*
 * void setTimeRange ( const QTime & min, const QTime & max )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETTIMERANGE )
{
   hbqt_par_QDateTimeEdit( 1 )->setTimeRange( *hbqt_par_QTime( 2 ), *hbqt_par_QTime( 3 ) );
}

/*
 * void setTimeSpec ( Qt::TimeSpec spec )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETTIMESPEC )
{
   hbqt_par_QDateTimeEdit( 1 )->setTimeSpec( ( Qt::TimeSpec ) hb_parni( 2 ) );
}

/*
 * QTime time () const
 */
HB_FUNC( QT_QDATETIMEEDIT_TIME )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QTime( hbqt_par_QDateTimeEdit( 1 )->time() ), release_QTime ) );
}

/*
 * Qt::TimeSpec timeSpec () const
 */
HB_FUNC( QT_QDATETIMEEDIT_TIMESPEC )
{
   hb_retni( ( Qt::TimeSpec ) hbqt_par_QDateTimeEdit( 1 )->timeSpec() );
}

/*
 * void setDate ( const QDate & date )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETDATE )
{
   hbqt_par_QDateTimeEdit( 1 )->setDate( *hbqt_par_QDate( 2 ) );
}

/*
 * void setDateTime ( const QDateTime & dateTime )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETDATETIME )
{
   hbqt_par_QDateTimeEdit( 1 )->setDateTime( *hbqt_par_QDateTime( 2 ) );
}

/*
 * void setTime ( const QTime & time )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETTIME )
{
   hbqt_par_QDateTimeEdit( 1 )->setTime( *hbqt_par_QTime( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
