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

#include "hbqt.h"
#include "hbqtgui_garbage.h"
#include "hbqtcore_garbage.h"

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

typedef struct
{
   QPointer< QDateTimeEdit > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QDateTimeEdit;

QT_G_FUNC( hbqt_gcRelease_QDateTimeEdit )
{
   QDateTimeEdit  * ph = NULL ;
   QGC_POINTER_QDateTimeEdit * p = ( QGC_POINTER_QDateTimeEdit * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QDateTimeEdit   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QDateTimeEdit   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QDateTimeEdit          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QDateTimeEdit    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QDateTimeEdit    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QDateTimeEdit( void * pObj, bool bNew )
{
   QGC_POINTER_QDateTimeEdit * p = ( QGC_POINTER_QDateTimeEdit * ) hb_gcAllocate( sizeof( QGC_POINTER_QDateTimeEdit ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDateTimeEdit >( ( QDateTimeEdit * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDateTimeEdit;
   p->type = HBQT_TYPE_QDateTimeEdit;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QDateTimeEdit  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QDateTimeEdit", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QDATETIMEEDIT )
{
   QDateTimeEdit * pObj = NULL;

   pObj =  new QDateTimeEdit( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QDateTimeEdit( ( void * ) pObj, true ) );
}

/*
 * bool calendarPopup () const
 */
HB_FUNC( QT_QDATETIMEEDIT_CALENDARPOPUP )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      hb_retl( ( p )->calendarPopup() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_CALENDARPOPUP FP=hb_retl( ( p )->calendarPopup() ); p is NULL" ) );
   }
}

/*
 * QCalendarWidget * calendarWidget () const
 */
HB_FUNC( QT_QDATETIMEEDIT_CALENDARWIDGET )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QCalendarWidget( ( p )->calendarWidget(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_CALENDARWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QCalendarWidget( ( p )->calendarWidget(), false ) ); p is NULL" ) );
   }
}

/*
 * void clearMaximumDate ()
 */
HB_FUNC( QT_QDATETIMEEDIT_CLEARMAXIMUMDATE )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->clearMaximumDate();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_CLEARMAXIMUMDATE FP=( p )->clearMaximumDate(); p is NULL" ) );
   }
}

/*
 * void clearMaximumDateTime ()
 */
HB_FUNC( QT_QDATETIMEEDIT_CLEARMAXIMUMDATETIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->clearMaximumDateTime();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_CLEARMAXIMUMDATETIME FP=( p )->clearMaximumDateTime(); p is NULL" ) );
   }
}

/*
 * void clearMaximumTime ()
 */
HB_FUNC( QT_QDATETIMEEDIT_CLEARMAXIMUMTIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->clearMaximumTime();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_CLEARMAXIMUMTIME FP=( p )->clearMaximumTime(); p is NULL" ) );
   }
}

/*
 * void clearMinimumDate ()
 */
HB_FUNC( QT_QDATETIMEEDIT_CLEARMINIMUMDATE )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->clearMinimumDate();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_CLEARMINIMUMDATE FP=( p )->clearMinimumDate(); p is NULL" ) );
   }
}

/*
 * void clearMinimumDateTime ()
 */
HB_FUNC( QT_QDATETIMEEDIT_CLEARMINIMUMDATETIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->clearMinimumDateTime();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_CLEARMINIMUMDATETIME FP=( p )->clearMinimumDateTime(); p is NULL" ) );
   }
}

/*
 * void clearMinimumTime ()
 */
HB_FUNC( QT_QDATETIMEEDIT_CLEARMINIMUMTIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->clearMinimumTime();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_CLEARMINIMUMTIME FP=( p )->clearMinimumTime(); p is NULL" ) );
   }
}

/*
 * Section currentSection () const
 */
HB_FUNC( QT_QDATETIMEEDIT_CURRENTSECTION )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      hb_retni( ( QDateTimeEdit::Section ) ( p )->currentSection() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_CURRENTSECTION FP=hb_retni( ( QDateTimeEdit::Section ) ( p )->currentSection() ); p is NULL" ) );
   }
}

/*
 * int currentSectionIndex () const
 */
HB_FUNC( QT_QDATETIMEEDIT_CURRENTSECTIONINDEX )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      hb_retni( ( p )->currentSectionIndex() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_CURRENTSECTIONINDEX FP=hb_retni( ( p )->currentSectionIndex() ); p is NULL" ) );
   }
}

/*
 * QDate date () const
 */
HB_FUNC( QT_QDATETIMEEDIT_DATE )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->date() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_DATE FP=hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->date() ), true ) ); p is NULL" ) );
   }
}

/*
 * QDateTime dateTime () const
 */
HB_FUNC( QT_QDATETIMEEDIT_DATETIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->dateTime() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_DATETIME FP=hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->dateTime() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString displayFormat () const
 */
HB_FUNC( QT_QDATETIMEEDIT_DISPLAYFORMAT )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      hb_retc( ( p )->displayFormat().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_DISPLAYFORMAT FP=hb_retc( ( p )->displayFormat().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * Sections displayedSections () const
 */
HB_FUNC( QT_QDATETIMEEDIT_DISPLAYEDSECTIONS )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      hb_retni( ( QDateTimeEdit::Sections ) ( p )->displayedSections() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_DISPLAYEDSECTIONS FP=hb_retni( ( QDateTimeEdit::Sections ) ( p )->displayedSections() ); p is NULL" ) );
   }
}

/*
 * QDate maximumDate () const
 */
HB_FUNC( QT_QDATETIMEEDIT_MAXIMUMDATE )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->maximumDate() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_MAXIMUMDATE FP=hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->maximumDate() ), true ) ); p is NULL" ) );
   }
}

/*
 * QDateTime maximumDateTime () const
 */
HB_FUNC( QT_QDATETIMEEDIT_MAXIMUMDATETIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->maximumDateTime() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_MAXIMUMDATETIME FP=hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->maximumDateTime() ), true ) ); p is NULL" ) );
   }
}

/*
 * QTime maximumTime () const
 */
HB_FUNC( QT_QDATETIMEEDIT_MAXIMUMTIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->maximumTime() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_MAXIMUMTIME FP=hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->maximumTime() ), true ) ); p is NULL" ) );
   }
}

/*
 * QDate minimumDate () const
 */
HB_FUNC( QT_QDATETIMEEDIT_MINIMUMDATE )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->minimumDate() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_MINIMUMDATE FP=hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->minimumDate() ), true ) ); p is NULL" ) );
   }
}

/*
 * QDateTime minimumDateTime () const
 */
HB_FUNC( QT_QDATETIMEEDIT_MINIMUMDATETIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->minimumDateTime() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_MINIMUMDATETIME FP=hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->minimumDateTime() ), true ) ); p is NULL" ) );
   }
}

/*
 * QTime minimumTime () const
 */
HB_FUNC( QT_QDATETIMEEDIT_MINIMUMTIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->minimumTime() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_MINIMUMTIME FP=hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->minimumTime() ), true ) ); p is NULL" ) );
   }
}

/*
 * Section sectionAt ( int index ) const
 */
HB_FUNC( QT_QDATETIMEEDIT_SECTIONAT )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      hb_retni( ( QDateTimeEdit::Section ) ( p )->sectionAt( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_SECTIONAT FP=hb_retni( ( QDateTimeEdit::Section ) ( p )->sectionAt( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int sectionCount () const
 */
HB_FUNC( QT_QDATETIMEEDIT_SECTIONCOUNT )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      hb_retni( ( p )->sectionCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_SECTIONCOUNT FP=hb_retni( ( p )->sectionCount() ); p is NULL" ) );
   }
}

/*
 * QString sectionText ( Section section ) const
 */
HB_FUNC( QT_QDATETIMEEDIT_SECTIONTEXT )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      hb_retc( ( p )->sectionText( ( QDateTimeEdit::Section ) hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_SECTIONTEXT FP=hb_retc( ( p )->sectionText( ( QDateTimeEdit::Section ) hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void setCalendarPopup ( bool enable )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETCALENDARPOPUP )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->setCalendarPopup( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_SETCALENDARPOPUP FP=( p )->setCalendarPopup( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCalendarWidget ( QCalendarWidget * calendarWidget )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETCALENDARWIDGET )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->setCalendarWidget( hbqt_par_QCalendarWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_SETCALENDARWIDGET FP=( p )->setCalendarWidget( hbqt_par_QCalendarWidget( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCurrentSection ( Section section )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETCURRENTSECTION )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->setCurrentSection( ( QDateTimeEdit::Section ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_SETCURRENTSECTION FP=( p )->setCurrentSection( ( QDateTimeEdit::Section ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCurrentSectionIndex ( int index )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETCURRENTSECTIONINDEX )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->setCurrentSectionIndex( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_SETCURRENTSECTIONINDEX FP=( p )->setCurrentSectionIndex( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDateRange ( const QDate & min, const QDate & max )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETDATERANGE )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->setDateRange( *hbqt_par_QDate( 2 ), *hbqt_par_QDate( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_SETDATERANGE FP=( p )->setDateRange( *hbqt_par_QDate( 2 ), *hbqt_par_QDate( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setDateTimeRange ( const QDateTime & min, const QDateTime & max )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETDATETIMERANGE )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->setDateTimeRange( *hbqt_par_QDateTime( 2 ), *hbqt_par_QDateTime( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_SETDATETIMERANGE FP=( p )->setDateTimeRange( *hbqt_par_QDateTime( 2 ), *hbqt_par_QDateTime( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setDisplayFormat ( const QString & format )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETDISPLAYFORMAT )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->setDisplayFormat( QDateTimeEdit::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_SETDISPLAYFORMAT FP=( p )->setDisplayFormat( QDateTimeEdit::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setMaximumDate ( const QDate & max )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETMAXIMUMDATE )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->setMaximumDate( *hbqt_par_QDate( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_SETMAXIMUMDATE FP=( p )->setMaximumDate( *hbqt_par_QDate( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMaximumDateTime ( const QDateTime & dt )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETMAXIMUMDATETIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->setMaximumDateTime( *hbqt_par_QDateTime( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_SETMAXIMUMDATETIME FP=( p )->setMaximumDateTime( *hbqt_par_QDateTime( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMaximumTime ( const QTime & max )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETMAXIMUMTIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->setMaximumTime( *hbqt_par_QTime( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_SETMAXIMUMTIME FP=( p )->setMaximumTime( *hbqt_par_QTime( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMinimumDate ( const QDate & min )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETMINIMUMDATE )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->setMinimumDate( *hbqt_par_QDate( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_SETMINIMUMDATE FP=( p )->setMinimumDate( *hbqt_par_QDate( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMinimumDateTime ( const QDateTime & dt )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETMINIMUMDATETIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->setMinimumDateTime( *hbqt_par_QDateTime( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_SETMINIMUMDATETIME FP=( p )->setMinimumDateTime( *hbqt_par_QDateTime( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMinimumTime ( const QTime & min )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETMINIMUMTIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->setMinimumTime( *hbqt_par_QTime( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_SETMINIMUMTIME FP=( p )->setMinimumTime( *hbqt_par_QTime( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSelectedSection ( Section section )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETSELECTEDSECTION )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->setSelectedSection( ( QDateTimeEdit::Section ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_SETSELECTEDSECTION FP=( p )->setSelectedSection( ( QDateTimeEdit::Section ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTimeRange ( const QTime & min, const QTime & max )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETTIMERANGE )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->setTimeRange( *hbqt_par_QTime( 2 ), *hbqt_par_QTime( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_SETTIMERANGE FP=( p )->setTimeRange( *hbqt_par_QTime( 2 ), *hbqt_par_QTime( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setTimeSpec ( Qt::TimeSpec spec )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETTIMESPEC )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->setTimeSpec( ( Qt::TimeSpec ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_SETTIMESPEC FP=( p )->setTimeSpec( ( Qt::TimeSpec ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * QTime time () const
 */
HB_FUNC( QT_QDATETIMEEDIT_TIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->time() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_TIME FP=hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->time() ), true ) ); p is NULL" ) );
   }
}

/*
 * Qt::TimeSpec timeSpec () const
 */
HB_FUNC( QT_QDATETIMEEDIT_TIMESPEC )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      hb_retni( ( Qt::TimeSpec ) ( p )->timeSpec() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_TIMESPEC FP=hb_retni( ( Qt::TimeSpec ) ( p )->timeSpec() ); p is NULL" ) );
   }
}

/*
 * void setDate ( const QDate & date )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETDATE )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->setDate( *hbqt_par_QDate( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_SETDATE FP=( p )->setDate( *hbqt_par_QDate( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDateTime ( const QDateTime & dateTime )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETDATETIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->setDateTime( *hbqt_par_QDateTime( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_SETDATETIME FP=( p )->setDateTime( *hbqt_par_QDateTime( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTime ( const QTime & time )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETTIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
      ( p )->setTime( *hbqt_par_QTime( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDATETIMEEDIT_SETTIME FP=( p )->setTime( *hbqt_par_QTime( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
