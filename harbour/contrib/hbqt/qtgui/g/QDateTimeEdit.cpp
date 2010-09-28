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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum Section { NoSection, AmPmSection, MSecSection, SecondSection, ..., YearSection }
 *  flags Sections
 */

/*
 *  Constructed[ 44/44 [ 100.00% ] ]
 *
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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDateTimeEdit;

HBQT_GC_FUNC( hbqt_gcRelease_QDateTimeEdit )
{
   QDateTimeEdit  * ph = NULL ;
   HBQT_GC_T_QDateTimeEdit * p = ( HBQT_GC_T_QDateTimeEdit * ) Cargo;

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
   HBQT_GC_T_QDateTimeEdit * p = ( HBQT_GC_T_QDateTimeEdit * ) hb_gcAllocate( sizeof( HBQT_GC_T_QDateTimeEdit ), hbqt_gcFuncs() );

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
   {
      hb_retl( ( p )->calendarPopup() );
   }
}

/*
 * QCalendarWidget * calendarWidget () const
 */
HB_FUNC( QT_QDATETIMEEDIT_CALENDARWIDGET )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QCalendarWidget( ( p )->calendarWidget(), false ) );
   }
}

/*
 * void clearMaximumDate ()
 */
HB_FUNC( QT_QDATETIMEEDIT_CLEARMAXIMUMDATE )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->clearMaximumDate();
   }
}

/*
 * void clearMaximumDateTime ()
 */
HB_FUNC( QT_QDATETIMEEDIT_CLEARMAXIMUMDATETIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->clearMaximumDateTime();
   }
}

/*
 * void clearMaximumTime ()
 */
HB_FUNC( QT_QDATETIMEEDIT_CLEARMAXIMUMTIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->clearMaximumTime();
   }
}

/*
 * void clearMinimumDate ()
 */
HB_FUNC( QT_QDATETIMEEDIT_CLEARMINIMUMDATE )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->clearMinimumDate();
   }
}

/*
 * void clearMinimumDateTime ()
 */
HB_FUNC( QT_QDATETIMEEDIT_CLEARMINIMUMDATETIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->clearMinimumDateTime();
   }
}

/*
 * void clearMinimumTime ()
 */
HB_FUNC( QT_QDATETIMEEDIT_CLEARMINIMUMTIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->clearMinimumTime();
   }
}

/*
 * Section currentSection () const
 */
HB_FUNC( QT_QDATETIMEEDIT_CURRENTSECTION )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      hb_retni( ( QDateTimeEdit::Section ) ( p )->currentSection() );
   }
}

/*
 * int currentSectionIndex () const
 */
HB_FUNC( QT_QDATETIMEEDIT_CURRENTSECTIONINDEX )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      hb_retni( ( p )->currentSectionIndex() );
   }
}

/*
 * QDate date () const
 */
HB_FUNC( QT_QDATETIMEEDIT_DATE )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->date() ), true ) );
   }
}

/*
 * QDateTime dateTime () const
 */
HB_FUNC( QT_QDATETIMEEDIT_DATETIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->dateTime() ), true ) );
   }
}

/*
 * QString displayFormat () const
 */
HB_FUNC( QT_QDATETIMEEDIT_DISPLAYFORMAT )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->displayFormat().toUtf8().data() );
   }
}

/*
 * Sections displayedSections () const
 */
HB_FUNC( QT_QDATETIMEEDIT_DISPLAYEDSECTIONS )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      hb_retni( ( QDateTimeEdit::Sections ) ( p )->displayedSections() );
   }
}

/*
 * QDate maximumDate () const
 */
HB_FUNC( QT_QDATETIMEEDIT_MAXIMUMDATE )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->maximumDate() ), true ) );
   }
}

/*
 * QDateTime maximumDateTime () const
 */
HB_FUNC( QT_QDATETIMEEDIT_MAXIMUMDATETIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->maximumDateTime() ), true ) );
   }
}

/*
 * QTime maximumTime () const
 */
HB_FUNC( QT_QDATETIMEEDIT_MAXIMUMTIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->maximumTime() ), true ) );
   }
}

/*
 * QDate minimumDate () const
 */
HB_FUNC( QT_QDATETIMEEDIT_MINIMUMDATE )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->minimumDate() ), true ) );
   }
}

/*
 * QDateTime minimumDateTime () const
 */
HB_FUNC( QT_QDATETIMEEDIT_MINIMUMDATETIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->minimumDateTime() ), true ) );
   }
}

/*
 * QTime minimumTime () const
 */
HB_FUNC( QT_QDATETIMEEDIT_MINIMUMTIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->minimumTime() ), true ) );
   }
}

/*
 * Section sectionAt ( int index ) const
 */
HB_FUNC( QT_QDATETIMEEDIT_SECTIONAT )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      hb_retni( ( QDateTimeEdit::Section ) ( p )->sectionAt( hb_parni( 2 ) ) );
   }
}

/*
 * int sectionCount () const
 */
HB_FUNC( QT_QDATETIMEEDIT_SECTIONCOUNT )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      hb_retni( ( p )->sectionCount() );
   }
}

/*
 * QString sectionText ( Section section ) const
 */
HB_FUNC( QT_QDATETIMEEDIT_SECTIONTEXT )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->sectionText( ( QDateTimeEdit::Section ) hb_parni( 2 ) ).toUtf8().data() );
   }
}

/*
 * void setCalendarPopup ( bool enable )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETCALENDARPOPUP )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->setCalendarPopup( hb_parl( 2 ) );
   }
}

/*
 * void setCalendarWidget ( QCalendarWidget * calendarWidget )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETCALENDARWIDGET )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->setCalendarWidget( hbqt_par_QCalendarWidget( 2 ) );
   }
}

/*
 * void setCurrentSection ( Section section )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETCURRENTSECTION )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->setCurrentSection( ( QDateTimeEdit::Section ) hb_parni( 2 ) );
   }
}

/*
 * void setCurrentSectionIndex ( int index )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETCURRENTSECTIONINDEX )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->setCurrentSectionIndex( hb_parni( 2 ) );
   }
}

/*
 * void setDateRange ( const QDate & min, const QDate & max )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETDATERANGE )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->setDateRange( *hbqt_par_QDate( 2 ), *hbqt_par_QDate( 3 ) );
   }
}

/*
 * void setDateTimeRange ( const QDateTime & min, const QDateTime & max )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETDATETIMERANGE )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->setDateTimeRange( *hbqt_par_QDateTime( 2 ), *hbqt_par_QDateTime( 3 ) );
   }
}

/*
 * void setDisplayFormat ( const QString & format )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETDISPLAYFORMAT )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      void * pText;
      ( p )->setDisplayFormat( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setMaximumDate ( const QDate & max )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETMAXIMUMDATE )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->setMaximumDate( *hbqt_par_QDate( 2 ) );
   }
}

/*
 * void setMaximumDateTime ( const QDateTime & dt )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETMAXIMUMDATETIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->setMaximumDateTime( *hbqt_par_QDateTime( 2 ) );
   }
}

/*
 * void setMaximumTime ( const QTime & max )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETMAXIMUMTIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->setMaximumTime( *hbqt_par_QTime( 2 ) );
   }
}

/*
 * void setMinimumDate ( const QDate & min )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETMINIMUMDATE )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->setMinimumDate( *hbqt_par_QDate( 2 ) );
   }
}

/*
 * void setMinimumDateTime ( const QDateTime & dt )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETMINIMUMDATETIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->setMinimumDateTime( *hbqt_par_QDateTime( 2 ) );
   }
}

/*
 * void setMinimumTime ( const QTime & min )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETMINIMUMTIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->setMinimumTime( *hbqt_par_QTime( 2 ) );
   }
}

/*
 * void setSelectedSection ( Section section )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETSELECTEDSECTION )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->setSelectedSection( ( QDateTimeEdit::Section ) hb_parni( 2 ) );
   }
}

/*
 * void setTimeRange ( const QTime & min, const QTime & max )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETTIMERANGE )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->setTimeRange( *hbqt_par_QTime( 2 ), *hbqt_par_QTime( 3 ) );
   }
}

/*
 * void setTimeSpec ( Qt::TimeSpec spec )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETTIMESPEC )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->setTimeSpec( ( Qt::TimeSpec ) hb_parni( 2 ) );
   }
}

/*
 * QTime time () const
 */
HB_FUNC( QT_QDATETIMEEDIT_TIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->time() ), true ) );
   }
}

/*
 * Qt::TimeSpec timeSpec () const
 */
HB_FUNC( QT_QDATETIMEEDIT_TIMESPEC )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      hb_retni( ( Qt::TimeSpec ) ( p )->timeSpec() );
   }
}

/*
 * void setDate ( const QDate & date )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETDATE )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->setDate( *hbqt_par_QDate( 2 ) );
   }
}

/*
 * void setDateTime ( const QDateTime & dateTime )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETDATETIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->setDateTime( *hbqt_par_QDateTime( 2 ) );
   }
}

/*
 * void setTime ( const QTime & time )
 */
HB_FUNC( QT_QDATETIMEEDIT_SETTIME )
{
   QDateTimeEdit * p = hbqt_par_QDateTimeEdit( 1 );
   if( p )
   {
      ( p )->setTime( *hbqt_par_QTime( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
