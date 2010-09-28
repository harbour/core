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
 *  enum Direction { TopToBottom, BottomToTop }
 */

/*
 *  Constructed[ 21/21 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QProgressBar>


/*
 * QProgressBar ( QWidget * parent = 0 )
 */

typedef struct
{
   QPointer< QProgressBar > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QProgressBar;

HBQT_GC_FUNC( hbqt_gcRelease_QProgressBar )
{
   QProgressBar  * ph = NULL ;
   HBQT_GC_T_QProgressBar * p = ( HBQT_GC_T_QProgressBar * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QProgressBar   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QProgressBar   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QProgressBar          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QProgressBar    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QProgressBar    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QProgressBar( void * pObj, bool bNew )
{
   HBQT_GC_T_QProgressBar * p = ( HBQT_GC_T_QProgressBar * ) hb_gcAllocate( sizeof( HBQT_GC_T_QProgressBar ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QProgressBar >( ( QProgressBar * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QProgressBar;
   p->type = HBQT_TYPE_QProgressBar;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QProgressBar  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QProgressBar", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QPROGRESSBAR )
{
   QProgressBar * pObj = NULL;

   pObj =  new QProgressBar( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QProgressBar( ( void * ) pObj, true ) );
}

/*
 * Qt::Alignment alignment () const
 */
HB_FUNC( QT_QPROGRESSBAR_ALIGNMENT )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
   {
      hb_retni( ( Qt::Alignment ) ( p )->alignment() );
   }
}

/*
 * QString format () const
 */
HB_FUNC( QT_QPROGRESSBAR_FORMAT )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->format().toUtf8().data() );
   }
}

/*
 * bool invertedAppearance ()
 */
HB_FUNC( QT_QPROGRESSBAR_INVERTEDAPPEARANCE )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
   {
      hb_retl( ( p )->invertedAppearance() );
   }
}

/*
 * bool isTextVisible () const
 */
HB_FUNC( QT_QPROGRESSBAR_ISTEXTVISIBLE )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
   {
      hb_retl( ( p )->isTextVisible() );
   }
}

/*
 * int maximum () const
 */
HB_FUNC( QT_QPROGRESSBAR_MAXIMUM )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
   {
      hb_retni( ( p )->maximum() );
   }
}

/*
 * int minimum () const
 */
HB_FUNC( QT_QPROGRESSBAR_MINIMUM )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
   {
      hb_retni( ( p )->minimum() );
   }
}

/*
 * Qt::Orientation orientation () const
 */
HB_FUNC( QT_QPROGRESSBAR_ORIENTATION )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
   {
      hb_retni( ( Qt::Orientation ) ( p )->orientation() );
   }
}

/*
 * void setAlignment ( Qt::Alignment alignment )
 */
HB_FUNC( QT_QPROGRESSBAR_SETALIGNMENT )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
   {
      ( p )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
   }
}

/*
 * void setFormat ( const QString & format )
 */
HB_FUNC( QT_QPROGRESSBAR_SETFORMAT )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
   {
      void * pText;
      ( p )->setFormat( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setInvertedAppearance ( bool invert )
 */
HB_FUNC( QT_QPROGRESSBAR_SETINVERTEDAPPEARANCE )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
   {
      ( p )->setInvertedAppearance( hb_parl( 2 ) );
   }
}

/*
 * void setTextDirection ( QProgressBar::Direction textDirection )
 */
HB_FUNC( QT_QPROGRESSBAR_SETTEXTDIRECTION )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
   {
      ( p )->setTextDirection( ( QProgressBar::Direction ) hb_parni( 2 ) );
   }
}

/*
 * void setTextVisible ( bool visible )
 */
HB_FUNC( QT_QPROGRESSBAR_SETTEXTVISIBLE )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
   {
      ( p )->setTextVisible( hb_parl( 2 ) );
   }
}

/*
 * virtual QString text () const
 */
HB_FUNC( QT_QPROGRESSBAR_TEXT )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->text().toUtf8().data() );
   }
}

/*
 * QProgressBar::Direction textDirection ()
 */
HB_FUNC( QT_QPROGRESSBAR_TEXTDIRECTION )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
   {
      hb_retni( ( QProgressBar::Direction ) ( p )->textDirection() );
   }
}

/*
 * int value () const
 */
HB_FUNC( QT_QPROGRESSBAR_VALUE )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
   {
      hb_retni( ( p )->value() );
   }
}

/*
 * void reset ()
 */
HB_FUNC( QT_QPROGRESSBAR_RESET )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
   {
      ( p )->reset();
   }
}

/*
 * void setMaximum ( int maximum )
 */
HB_FUNC( QT_QPROGRESSBAR_SETMAXIMUM )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
   {
      ( p )->setMaximum( hb_parni( 2 ) );
   }
}

/*
 * void setMinimum ( int minimum )
 */
HB_FUNC( QT_QPROGRESSBAR_SETMINIMUM )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
   {
      ( p )->setMinimum( hb_parni( 2 ) );
   }
}

/*
 * void setOrientation ( Qt::Orientation )
 */
HB_FUNC( QT_QPROGRESSBAR_SETORIENTATION )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
   {
      ( p )->setOrientation( ( Qt::Orientation ) hb_parni( 2 ) );
   }
}

/*
 * void setRange ( int minimum, int maximum )
 */
HB_FUNC( QT_QPROGRESSBAR_SETRANGE )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
   {
      ( p )->setRange( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void setValue ( int value )
 */
HB_FUNC( QT_QPROGRESSBAR_SETVALUE )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
   {
      ( p )->setValue( hb_parni( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
