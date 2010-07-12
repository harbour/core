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

#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtGui/QDoubleSpinBox>


/*
 * QDoubleSpinBox ( QWidget * parent = 0 )
 */

typedef struct
{
   QPointer< QDoubleSpinBox > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QDoubleSpinBox;

QT_G_FUNC( hbqt_gcRelease_QDoubleSpinBox )
{
   QDoubleSpinBox  * ph = NULL ;
   QGC_POINTER_QDoubleSpinBox * p = ( QGC_POINTER_QDoubleSpinBox * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QDoubleSpinBox   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QDoubleSpinBox   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QDoubleSpinBox          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QDoubleSpinBox    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QDoubleSpinBox    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QDoubleSpinBox( void * pObj, bool bNew )
{
   QGC_POINTER_QDoubleSpinBox * p = ( QGC_POINTER_QDoubleSpinBox * ) hb_gcAllocate( sizeof( QGC_POINTER_QDoubleSpinBox ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDoubleSpinBox >( ( QDoubleSpinBox * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDoubleSpinBox;
   p->type = QT_TYPE_QDoubleSpinBox;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QDoubleSpinBox  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QDoubleSpinBox", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QDOUBLESPINBOX )
{
   QDoubleSpinBox * pObj = NULL;

   pObj =  new QDoubleSpinBox( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QDoubleSpinBox( ( void * ) pObj, true ) );
}

/*
 * QString cleanText () const
 */
HB_FUNC( QT_QDOUBLESPINBOX_CLEANTEXT )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      hb_retc( ( p )->cleanText().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOUBLESPINBOX_CLEANTEXT FP=hb_retc( ( p )->cleanText().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * int decimals () const
 */
HB_FUNC( QT_QDOUBLESPINBOX_DECIMALS )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      hb_retni( ( p )->decimals() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOUBLESPINBOX_DECIMALS FP=hb_retni( ( p )->decimals() ); p is NULL" ) );
   }
}

/*
 * double maximum () const
 */
HB_FUNC( QT_QDOUBLESPINBOX_MAXIMUM )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      hb_retnd( ( p )->maximum() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOUBLESPINBOX_MAXIMUM FP=hb_retnd( ( p )->maximum() ); p is NULL" ) );
   }
}

/*
 * double minimum () const
 */
HB_FUNC( QT_QDOUBLESPINBOX_MINIMUM )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      hb_retnd( ( p )->minimum() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOUBLESPINBOX_MINIMUM FP=hb_retnd( ( p )->minimum() ); p is NULL" ) );
   }
}

/*
 * QString prefix () const
 */
HB_FUNC( QT_QDOUBLESPINBOX_PREFIX )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      hb_retc( ( p )->prefix().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOUBLESPINBOX_PREFIX FP=hb_retc( ( p )->prefix().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void setDecimals ( int prec )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETDECIMALS )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      ( p )->setDecimals( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOUBLESPINBOX_SETDECIMALS FP=( p )->setDecimals( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMaximum ( double max )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETMAXIMUM )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      ( p )->setMaximum( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOUBLESPINBOX_SETMAXIMUM FP=( p )->setMaximum( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMinimum ( double min )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETMINIMUM )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      ( p )->setMinimum( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOUBLESPINBOX_SETMINIMUM FP=( p )->setMinimum( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPrefix ( const QString & prefix )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETPREFIX )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      ( p )->setPrefix( QDoubleSpinBox::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOUBLESPINBOX_SETPREFIX FP=( p )->setPrefix( QDoubleSpinBox::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setRange ( double minimum, double maximum )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETRANGE )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      ( p )->setRange( hb_parnd( 2 ), hb_parnd( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOUBLESPINBOX_SETRANGE FP=( p )->setRange( hb_parnd( 2 ), hb_parnd( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setSingleStep ( double val )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETSINGLESTEP )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      ( p )->setSingleStep( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOUBLESPINBOX_SETSINGLESTEP FP=( p )->setSingleStep( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSuffix ( const QString & suffix )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETSUFFIX )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      ( p )->setSuffix( QDoubleSpinBox::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOUBLESPINBOX_SETSUFFIX FP=( p )->setSuffix( QDoubleSpinBox::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * double singleStep () const
 */
HB_FUNC( QT_QDOUBLESPINBOX_SINGLESTEP )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      hb_retnd( ( p )->singleStep() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOUBLESPINBOX_SINGLESTEP FP=hb_retnd( ( p )->singleStep() ); p is NULL" ) );
   }
}

/*
 * QString suffix () const
 */
HB_FUNC( QT_QDOUBLESPINBOX_SUFFIX )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      hb_retc( ( p )->suffix().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOUBLESPINBOX_SUFFIX FP=hb_retc( ( p )->suffix().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * virtual QString textFromValue ( double value ) const
 */
HB_FUNC( QT_QDOUBLESPINBOX_TEXTFROMVALUE )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      hb_retc( ( p )->textFromValue( hb_parnd( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOUBLESPINBOX_TEXTFROMVALUE FP=hb_retc( ( p )->textFromValue( hb_parnd( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * double value () const
 */
HB_FUNC( QT_QDOUBLESPINBOX_VALUE )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      hb_retnd( ( p )->value() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOUBLESPINBOX_VALUE FP=hb_retnd( ( p )->value() ); p is NULL" ) );
   }
}

/*
 * virtual double valueFromText ( const QString & text ) const
 */
HB_FUNC( QT_QDOUBLESPINBOX_VALUEFROMTEXT )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      hb_retnd( ( p )->valueFromText( QDoubleSpinBox::tr( hb_parc( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOUBLESPINBOX_VALUEFROMTEXT FP=hb_retnd( ( p )->valueFromText( QDoubleSpinBox::tr( hb_parc( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * void setValue ( double val )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETVALUE )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      ( p )->setValue( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOUBLESPINBOX_SETVALUE FP=( p )->setValue( hb_parnd( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
