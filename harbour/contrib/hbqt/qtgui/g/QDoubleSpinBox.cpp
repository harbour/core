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

#include <QtGui/QDoubleSpinBox>


/*
 * QDoubleSpinBox ( QWidget * parent = 0 )
 */

typedef struct
{
   QPointer< QDoubleSpinBox > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDoubleSpinBox;

HBQT_GC_FUNC( hbqt_gcRelease_QDoubleSpinBox )
{
   QDoubleSpinBox  * ph = NULL ;
   HBQT_GC_T_QDoubleSpinBox * p = ( HBQT_GC_T_QDoubleSpinBox * ) Cargo;

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
   HBQT_GC_T_QDoubleSpinBox * p = ( HBQT_GC_T_QDoubleSpinBox * ) hb_gcAllocate( sizeof( HBQT_GC_T_QDoubleSpinBox ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDoubleSpinBox >( ( QDoubleSpinBox * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDoubleSpinBox;
   p->type = HBQT_TYPE_QDoubleSpinBox;

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
   {
      hb_retstr_utf8( ( p )->cleanText().toUtf8().data() );
   }
}

/*
 * int decimals () const
 */
HB_FUNC( QT_QDOUBLESPINBOX_DECIMALS )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
   {
      hb_retni( ( p )->decimals() );
   }
}

/*
 * double maximum () const
 */
HB_FUNC( QT_QDOUBLESPINBOX_MAXIMUM )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
   {
      hb_retnd( ( p )->maximum() );
   }
}

/*
 * double minimum () const
 */
HB_FUNC( QT_QDOUBLESPINBOX_MINIMUM )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
   {
      hb_retnd( ( p )->minimum() );
   }
}

/*
 * QString prefix () const
 */
HB_FUNC( QT_QDOUBLESPINBOX_PREFIX )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->prefix().toUtf8().data() );
   }
}

/*
 * void setDecimals ( int prec )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETDECIMALS )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
   {
      ( p )->setDecimals( hb_parni( 2 ) );
   }
}

/*
 * void setMaximum ( double max )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETMAXIMUM )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
   {
      ( p )->setMaximum( hb_parnd( 2 ) );
   }
}

/*
 * void setMinimum ( double min )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETMINIMUM )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
   {
      ( p )->setMinimum( hb_parnd( 2 ) );
   }
}

/*
 * void setPrefix ( const QString & prefix )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETPREFIX )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
   {
      void * pText;
      ( p )->setPrefix( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setRange ( double minimum, double maximum )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETRANGE )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
   {
      ( p )->setRange( hb_parnd( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * void setSingleStep ( double val )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETSINGLESTEP )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
   {
      ( p )->setSingleStep( hb_parnd( 2 ) );
   }
}

/*
 * void setSuffix ( const QString & suffix )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETSUFFIX )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
   {
      void * pText;
      ( p )->setSuffix( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * double singleStep () const
 */
HB_FUNC( QT_QDOUBLESPINBOX_SINGLESTEP )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
   {
      hb_retnd( ( p )->singleStep() );
   }
}

/*
 * QString suffix () const
 */
HB_FUNC( QT_QDOUBLESPINBOX_SUFFIX )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->suffix().toUtf8().data() );
   }
}

/*
 * virtual QString textFromValue ( double value ) const
 */
HB_FUNC( QT_QDOUBLESPINBOX_TEXTFROMVALUE )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->textFromValue( hb_parnd( 2 ) ).toUtf8().data() );
   }
}

/*
 * double value () const
 */
HB_FUNC( QT_QDOUBLESPINBOX_VALUE )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
   {
      hb_retnd( ( p )->value() );
   }
}

/*
 * virtual double valueFromText ( const QString & text ) const
 */
HB_FUNC( QT_QDOUBLESPINBOX_VALUEFROMTEXT )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
   {
      void * pText;
      hb_retnd( ( p )->valueFromText( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * void setValue ( double val )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETVALUE )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
   {
      ( p )->setValue( hb_parnd( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
