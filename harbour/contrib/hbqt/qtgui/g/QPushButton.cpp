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
 *  Constructed[ 9/9 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QPushButton>


/*
 * QPushButton ( QWidget * parent = 0 )
 * QPushButton ( const QString & text, QWidget * parent = 0 )
 * QPushButton ( const QIcon & icon, const QString & text, QWidget * parent = 0 )
 * ~QPushButton ()
 */

typedef struct
{
   QPointer< QPushButton > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPushButton;

HBQT_GC_FUNC( hbqt_gcRelease_QPushButton )
{
   QPushButton  * ph = NULL ;
   HBQT_GC_T_QPushButton * p = ( HBQT_GC_T_QPushButton * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QPushButton   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QPushButton   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QPushButton          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QPushButton    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QPushButton    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPushButton( void * pObj, bool bNew )
{
   HBQT_GC_T_QPushButton * p = ( HBQT_GC_T_QPushButton * ) hb_gcAllocate( sizeof( HBQT_GC_T_QPushButton ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QPushButton >( ( QPushButton * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPushButton;
   p->type = HBQT_TYPE_QPushButton;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QPushButton  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QPushButton", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QPUSHBUTTON )
{
   QPushButton * pObj = NULL;

    pObj = new QPushButton( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QPushButton( ( void * ) pObj, true ) );
}

/*
 * bool autoDefault () const
 */
HB_FUNC( QT_QPUSHBUTTON_AUTODEFAULT )
{
   QPushButton * p = hbqt_par_QPushButton( 1 );
   if( p )
   {
      hb_retl( ( p )->autoDefault() );
   }
}

/*
 * bool isDefault () const
 */
HB_FUNC( QT_QPUSHBUTTON_ISDEFAULT )
{
   QPushButton * p = hbqt_par_QPushButton( 1 );
   if( p )
   {
      hb_retl( ( p )->isDefault() );
   }
}

/*
 * bool isFlat () const
 */
HB_FUNC( QT_QPUSHBUTTON_ISFLAT )
{
   QPushButton * p = hbqt_par_QPushButton( 1 );
   if( p )
   {
      hb_retl( ( p )->isFlat() );
   }
}

/*
 * QMenu * menu () const
 */
HB_FUNC( QT_QPUSHBUTTON_MENU )
{
   QPushButton * p = hbqt_par_QPushButton( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->menu(), false ) );
   }
}

/*
 * void setAutoDefault ( bool )
 */
HB_FUNC( QT_QPUSHBUTTON_SETAUTODEFAULT )
{
   QPushButton * p = hbqt_par_QPushButton( 1 );
   if( p )
   {
      ( p )->setAutoDefault( hb_parl( 2 ) );
   }
}

/*
 * void setDefault ( bool )
 */
HB_FUNC( QT_QPUSHBUTTON_SETDEFAULT )
{
   QPushButton * p = hbqt_par_QPushButton( 1 );
   if( p )
   {
      ( p )->setDefault( hb_parl( 2 ) );
   }
}

/*
 * void setFlat ( bool )
 */
HB_FUNC( QT_QPUSHBUTTON_SETFLAT )
{
   QPushButton * p = hbqt_par_QPushButton( 1 );
   if( p )
   {
      ( p )->setFlat( hb_parl( 2 ) );
   }
}

/*
 * void setMenu ( QMenu * menu )
 */
HB_FUNC( QT_QPUSHBUTTON_SETMENU )
{
   QPushButton * p = hbqt_par_QPushButton( 1 );
   if( p )
   {
      ( p )->setMenu( hbqt_par_QMenu( 2 ) );
   }
}

/*
 * void showMenu ()
 */
HB_FUNC( QT_QPUSHBUTTON_SHOWMENU )
{
   QPushButton * p = hbqt_par_QPushButton( 1 );
   if( p )
   {
      ( p )->showMenu();
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
