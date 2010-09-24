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
 *  Constructed[ 14/14 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QSpinBox>


/*
 * QSpinBox ( QWidget * parent = 0 )
 */

typedef struct
{
   QPointer< QSpinBox > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QSpinBox;

HBQT_GC_FUNC( hbqt_gcRelease_QSpinBox )
{
   QSpinBox  * ph = NULL ;
   HBQT_GC_T_QSpinBox * p = ( HBQT_GC_T_QSpinBox * ) Cargo;

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
   HBQT_GC_T_QSpinBox * p = ( HBQT_GC_T_QSpinBox * ) hb_gcAllocate( sizeof( HBQT_GC_T_QSpinBox ), hbqt_gcFuncs() );

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
   {
      hb_retstr_utf8( ( p )->cleanText().toUtf8().data() );
   }
}

/*
 * int maximum () const
 */
HB_FUNC( QT_QSPINBOX_MAXIMUM )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
   {
      hb_retni( ( p )->maximum() );
   }
}

/*
 * int minimum () const
 */
HB_FUNC( QT_QSPINBOX_MINIMUM )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
   {
      hb_retni( ( p )->minimum() );
   }
}

/*
 * QString prefix () const
 */
HB_FUNC( QT_QSPINBOX_PREFIX )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->prefix().toUtf8().data() );
   }
}

/*
 * void setMaximum ( int max )
 */
HB_FUNC( QT_QSPINBOX_SETMAXIMUM )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
   {
      ( p )->setMaximum( hb_parni( 2 ) );
   }
}

/*
 * void setMinimum ( int min )
 */
HB_FUNC( QT_QSPINBOX_SETMINIMUM )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
   {
      ( p )->setMinimum( hb_parni( 2 ) );
   }
}

/*
 * void setPrefix ( const QString & prefix )
 */
HB_FUNC( QT_QSPINBOX_SETPREFIX )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
   {
      void * pText;
      ( p )->setPrefix( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setRange ( int minimum, int maximum )
 */
HB_FUNC( QT_QSPINBOX_SETRANGE )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
   {
      ( p )->setRange( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void setSingleStep ( int val )
 */
HB_FUNC( QT_QSPINBOX_SETSINGLESTEP )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
   {
      ( p )->setSingleStep( hb_parni( 2 ) );
   }
}

/*
 * void setSuffix ( const QString & suffix )
 */
HB_FUNC( QT_QSPINBOX_SETSUFFIX )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
   {
      void * pText;
      ( p )->setSuffix( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * int singleStep () const
 */
HB_FUNC( QT_QSPINBOX_SINGLESTEP )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
   {
      hb_retni( ( p )->singleStep() );
   }
}

/*
 * QString suffix () const
 */
HB_FUNC( QT_QSPINBOX_SUFFIX )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->suffix().toUtf8().data() );
   }
}

/*
 * int value () const
 */
HB_FUNC( QT_QSPINBOX_VALUE )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
   {
      hb_retni( ( p )->value() );
   }
}

/*
 * void setValue ( int val )
 */
HB_FUNC( QT_QSPINBOX_SETVALUE )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
   {
      ( p )->setValue( hb_parni( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
