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

#include <QtCore/QPointer>

#include <QtGui/QSpinBox>


/*
 * QSpinBox ( QWidget * parent = 0 )
 */

typedef struct
{
   QPointer< QSpinBox > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QSpinBox;

QT_G_FUNC( hbqt_gcRelease_QSpinBox )
{
   QSpinBox  * ph = NULL ;
   QGC_POINTER_QSpinBox * p = ( QGC_POINTER_QSpinBox * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QSpinBox   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QSpinBox   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QSpinBox          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QSpinBox    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QSpinBox    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QSpinBox( void * pObj, bool bNew )
{
   QGC_POINTER_QSpinBox * p = ( QGC_POINTER_QSpinBox * ) hb_gcAllocate( sizeof( QGC_POINTER_QSpinBox ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QSpinBox >( ( QSpinBox * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSpinBox;
   p->type = HBQT_TYPE_QSpinBox;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QSpinBox  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QSpinBox", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSPINBOX )
{
   QSpinBox * pObj = NULL;

   pObj =  new QSpinBox( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QSpinBox( ( void * ) pObj, true ) );
}

/*
 * QString cleanText () const
 */
HB_FUNC( QT_QSPINBOX_CLEANTEXT )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      hb_retc( ( p )->cleanText().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPINBOX_CLEANTEXT FP=hb_retc( ( p )->cleanText().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * int maximum () const
 */
HB_FUNC( QT_QSPINBOX_MAXIMUM )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      hb_retni( ( p )->maximum() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPINBOX_MAXIMUM FP=hb_retni( ( p )->maximum() ); p is NULL" ) );
   }
}

/*
 * int minimum () const
 */
HB_FUNC( QT_QSPINBOX_MINIMUM )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      hb_retni( ( p )->minimum() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPINBOX_MINIMUM FP=hb_retni( ( p )->minimum() ); p is NULL" ) );
   }
}

/*
 * QString prefix () const
 */
HB_FUNC( QT_QSPINBOX_PREFIX )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      hb_retc( ( p )->prefix().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPINBOX_PREFIX FP=hb_retc( ( p )->prefix().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void setMaximum ( int max )
 */
HB_FUNC( QT_QSPINBOX_SETMAXIMUM )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      ( p )->setMaximum( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPINBOX_SETMAXIMUM FP=( p )->setMaximum( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMinimum ( int min )
 */
HB_FUNC( QT_QSPINBOX_SETMINIMUM )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      ( p )->setMinimum( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPINBOX_SETMINIMUM FP=( p )->setMinimum( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPrefix ( const QString & prefix )
 */
HB_FUNC( QT_QSPINBOX_SETPREFIX )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      ( p )->setPrefix( QSpinBox::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPINBOX_SETPREFIX FP=( p )->setPrefix( QSpinBox::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setRange ( int minimum, int maximum )
 */
HB_FUNC( QT_QSPINBOX_SETRANGE )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      ( p )->setRange( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPINBOX_SETRANGE FP=( p )->setRange( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setSingleStep ( int val )
 */
HB_FUNC( QT_QSPINBOX_SETSINGLESTEP )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      ( p )->setSingleStep( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPINBOX_SETSINGLESTEP FP=( p )->setSingleStep( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSuffix ( const QString & suffix )
 */
HB_FUNC( QT_QSPINBOX_SETSUFFIX )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      ( p )->setSuffix( QSpinBox::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPINBOX_SETSUFFIX FP=( p )->setSuffix( QSpinBox::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int singleStep () const
 */
HB_FUNC( QT_QSPINBOX_SINGLESTEP )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      hb_retni( ( p )->singleStep() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPINBOX_SINGLESTEP FP=hb_retni( ( p )->singleStep() ); p is NULL" ) );
   }
}

/*
 * QString suffix () const
 */
HB_FUNC( QT_QSPINBOX_SUFFIX )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      hb_retc( ( p )->suffix().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPINBOX_SUFFIX FP=hb_retc( ( p )->suffix().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * int value () const
 */
HB_FUNC( QT_QSPINBOX_VALUE )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      hb_retni( ( p )->value() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPINBOX_VALUE FP=hb_retni( ( p )->value() ); p is NULL" ) );
   }
}

/*
 * void setValue ( int val )
 */
HB_FUNC( QT_QSPINBOX_SETVALUE )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      ( p )->setValue( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPINBOX_SETVALUE FP=( p )->setValue( hb_parni( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
