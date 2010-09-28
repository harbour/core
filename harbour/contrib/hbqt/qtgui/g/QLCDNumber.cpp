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
 *  enum Mode { Hex, Dec, Oct, Bin }
 *  enum SegmentStyle { Outline, Filled, Flat }
 */

/*
 *  Constructed[ 19/19 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QLCDNumber>


/*
 * QLCDNumber ( QWidget * parent = 0 )
 * QLCDNumber ( uint numDigits, QWidget * parent = 0 )
 * ~QLCDNumber ()
 */

typedef struct
{
   QPointer< QLCDNumber > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QLCDNumber;

HBQT_GC_FUNC( hbqt_gcRelease_QLCDNumber )
{
   QLCDNumber  * ph = NULL ;
   HBQT_GC_T_QLCDNumber * p = ( HBQT_GC_T_QLCDNumber * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QLCDNumber   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QLCDNumber   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QLCDNumber          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QLCDNumber    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QLCDNumber    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QLCDNumber( void * pObj, bool bNew )
{
   HBQT_GC_T_QLCDNumber * p = ( HBQT_GC_T_QLCDNumber * ) hb_gcAllocate( sizeof( HBQT_GC_T_QLCDNumber ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QLCDNumber >( ( QLCDNumber * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QLCDNumber;
   p->type = HBQT_TYPE_QLCDNumber;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QLCDNumber  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QLCDNumber", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QLCDNUMBER )
{
   QLCDNumber * pObj = NULL;

   pObj = ( QLCDNumber * ) new QLCDNumber( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QLCDNumber( ( void * ) pObj, true ) );
}

/*
 * bool checkOverflow ( double num ) const
 */
HB_FUNC( QT_QLCDNUMBER_CHECKOVERFLOW )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
   {
      hb_retl( ( p )->checkOverflow( hb_parnd( 2 ) ) );
   }
}

/*
 * bool checkOverflow ( int num ) const
 */
HB_FUNC( QT_QLCDNUMBER_CHECKOVERFLOW_1 )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
   {
      hb_retl( ( p )->checkOverflow( hb_parni( 2 ) ) );
   }
}

/*
 * int intValue () const
 */
HB_FUNC( QT_QLCDNUMBER_INTVALUE )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
   {
      hb_retni( ( p )->intValue() );
   }
}

/*
 * Mode mode () const
 */
HB_FUNC( QT_QLCDNUMBER_MODE )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
   {
      hb_retni( ( QLCDNumber::Mode ) ( p )->mode() );
   }
}

/*
 * int numDigits () const
 */
HB_FUNC( QT_QLCDNUMBER_NUMDIGITS )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
   {
      hb_retni( ( p )->numDigits() );
   }
}

/*
 * SegmentStyle segmentStyle () const
 */
HB_FUNC( QT_QLCDNUMBER_SEGMENTSTYLE )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
   {
      hb_retni( ( QLCDNumber::SegmentStyle ) ( p )->segmentStyle() );
   }
}

/*
 * void setMode ( Mode )
 */
HB_FUNC( QT_QLCDNUMBER_SETMODE )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
   {
      ( p )->setMode( ( QLCDNumber::Mode ) hb_parni( 2 ) );
   }
}

/*
 * void setNumDigits ( int nDigits )
 */
HB_FUNC( QT_QLCDNUMBER_SETNUMDIGITS )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
   {
      ( p )->setNumDigits( hb_parni( 2 ) );
   }
}

/*
 * void setSegmentStyle ( SegmentStyle )
 */
HB_FUNC( QT_QLCDNUMBER_SETSEGMENTSTYLE )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
   {
      ( p )->setSegmentStyle( ( QLCDNumber::SegmentStyle ) hb_parni( 2 ) );
   }
}

/*
 * bool smallDecimalPoint () const
 */
HB_FUNC( QT_QLCDNUMBER_SMALLDECIMALPOINT )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
   {
      hb_retl( ( p )->smallDecimalPoint() );
   }
}

/*
 * double value () const
 */
HB_FUNC( QT_QLCDNUMBER_VALUE )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
   {
      hb_retnd( ( p )->value() );
   }
}

/*
 * void display ( const QString & s )
 */
HB_FUNC( QT_QLCDNUMBER_DISPLAY )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
   {
      void * pText;
      ( p )->display( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void display ( double num )
 */
HB_FUNC( QT_QLCDNUMBER_DISPLAY_1 )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
   {
      ( p )->display( hb_parnd( 2 ) );
   }
}

/*
 * void display ( int num )
 */
HB_FUNC( QT_QLCDNUMBER_DISPLAY_2 )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
   {
      ( p )->display( hb_parni( 2 ) );
   }
}

/*
 * void setBinMode ()
 */
HB_FUNC( QT_QLCDNUMBER_SETBINMODE )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
   {
      ( p )->setBinMode();
   }
}

/*
 * void setDecMode ()
 */
HB_FUNC( QT_QLCDNUMBER_SETDECMODE )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
   {
      ( p )->setDecMode();
   }
}

/*
 * void setHexMode ()
 */
HB_FUNC( QT_QLCDNUMBER_SETHEXMODE )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
   {
      ( p )->setHexMode();
   }
}

/*
 * void setOctMode ()
 */
HB_FUNC( QT_QLCDNUMBER_SETOCTMODE )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
   {
      ( p )->setOctMode();
   }
}

/*
 * void setSmallDecimalPoint ( bool )
 */
HB_FUNC( QT_QLCDNUMBER_SETSMALLDECIMALPOINT )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
   {
      ( p )->setSmallDecimalPoint( hb_parl( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
