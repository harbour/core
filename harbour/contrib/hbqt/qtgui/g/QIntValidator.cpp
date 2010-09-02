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
 *  enum State { Invalid, Intermediate, Acceptable }
 */

#include <QtCore/QPointer>

#include <QtGui/QIntValidator>


/* QIntValidator ( QObject * parent )
 * QIntValidator ( int minimum, int maximum, QObject * parent )
* ~QIntValidator ()
 */

typedef struct
{
   QPointer< QIntValidator > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QIntValidator;

HBQT_GC_FUNC( hbqt_gcRelease_QIntValidator )
{
   QIntValidator  * ph = NULL ;
   HBQT_GC_T_QIntValidator * p = ( HBQT_GC_T_QIntValidator * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QIntValidator   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QIntValidator   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QIntValidator          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QIntValidator    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QIntValidator    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QIntValidator( void * pObj, bool bNew )
{
   HBQT_GC_T_QIntValidator * p = ( HBQT_GC_T_QIntValidator * ) hb_gcAllocate( sizeof( HBQT_GC_T_QIntValidator ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QIntValidator >( ( QIntValidator * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QIntValidator;
   p->type = HBQT_TYPE_QIntValidator;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QIntValidator  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QIntValidator", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QINTVALIDATOR )
{
   QIntValidator * pObj = NULL;

   if( hb_pcount() == 3 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISPOINTER( 3 ) )
   {
      pObj = new QIntValidator( hb_parni( 1 ), hb_parni( 2 ), hbqt_par_QObject( 3 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QIntValidator( hbqt_par_QObject( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QIntValidator( ( void * ) pObj, true ) );
}

/*
 * int bottom () const
 */
HB_FUNC( QT_QINTVALIDATOR_BOTTOM )
{
   QIntValidator * p = hbqt_par_QIntValidator( 1 );
   if( p )
      hb_retni( ( p )->bottom() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINTVALIDATOR_BOTTOM FP=hb_retni( ( p )->bottom() ); p is NULL" ) );
   }
}

/*
 * void setBottom ( int )
 */
HB_FUNC( QT_QINTVALIDATOR_SETBOTTOM )
{
   QIntValidator * p = hbqt_par_QIntValidator( 1 );
   if( p )
      ( p )->setBottom( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINTVALIDATOR_SETBOTTOM FP=( p )->setBottom( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setRange ( int bottom, int top )
 */
HB_FUNC( QT_QINTVALIDATOR_SETRANGE )
{
   QIntValidator * p = hbqt_par_QIntValidator( 1 );
   if( p )
      ( p )->setRange( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINTVALIDATOR_SETRANGE FP=( p )->setRange( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setTop ( int )
 */
HB_FUNC( QT_QINTVALIDATOR_SETTOP )
{
   QIntValidator * p = hbqt_par_QIntValidator( 1 );
   if( p )
      ( p )->setTop( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINTVALIDATOR_SETTOP FP=( p )->setTop( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * int top () const
 */
HB_FUNC( QT_QINTVALIDATOR_TOP )
{
   QIntValidator * p = hbqt_par_QIntValidator( 1 );
   if( p )
      hb_retni( ( p )->top() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINTVALIDATOR_TOP FP=hb_retni( ( p )->top() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
