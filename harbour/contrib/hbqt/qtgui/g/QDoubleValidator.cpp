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
 *  enum Notation { StandardNotation, ScientificNotation }
 */

/*
 *  Constructed[ 9/9 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //virtual QValidator::State validate ( const QString & input, const int & pos ) const
 */

#include <QtCore/QPointer>

#include <QtGui/QDoubleValidator>


/* QDoubleValidator ( QObject * parent )
 * QDoubleValidator ( double bottom, double top, int decimals, QObject * parent )
 * ~QDoubleValidator ()
 */

typedef struct
{
   QPointer< QDoubleValidator > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDoubleValidator;

HBQT_GC_FUNC( hbqt_gcRelease_QDoubleValidator )
{
   QDoubleValidator  * ph = NULL ;
   HBQT_GC_T_QDoubleValidator * p = ( HBQT_GC_T_QDoubleValidator * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QDoubleValidator   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QDoubleValidator   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QDoubleValidator          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QDoubleValidator    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QDoubleValidator    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QDoubleValidator( void * pObj, bool bNew )
{
   HBQT_GC_T_QDoubleValidator * p = ( HBQT_GC_T_QDoubleValidator * ) hb_gcAllocate( sizeof( HBQT_GC_T_QDoubleValidator ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDoubleValidator >( ( QDoubleValidator * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDoubleValidator;
   p->type = HBQT_TYPE_QDoubleValidator;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QDoubleValidator  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QDoubleValidator", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QDOUBLEVALIDATOR )
{
   QDoubleValidator * pObj = NULL;

   if( hb_pcount() >= 3 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
   {
      pObj = new QDoubleValidator( hb_parnd( 1 ), hb_parnd( 2 ), hb_parni( 3 ), HB_ISPOINTER( 4 ) ? hbqt_par_QObject( 4 ) : 0 ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QDoubleValidator( hbqt_par_QObject( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QDoubleValidator( ( void * ) pObj, true ) );
}

/*
 * double bottom () const
 */
HB_FUNC( QT_QDOUBLEVALIDATOR_BOTTOM )
{
   QDoubleValidator * p = hbqt_par_QDoubleValidator( 1 );
   if( p )
   {
      hb_retnd( ( p )->bottom() );
   }
}

/*
 * int decimals () const
 */
HB_FUNC( QT_QDOUBLEVALIDATOR_DECIMALS )
{
   QDoubleValidator * p = hbqt_par_QDoubleValidator( 1 );
   if( p )
   {
      hb_retni( ( p )->decimals() );
   }
}

/*
 * Notation notation () const
 */
HB_FUNC( QT_QDOUBLEVALIDATOR_NOTATION )
{
   QDoubleValidator * p = hbqt_par_QDoubleValidator( 1 );
   if( p )
   {
      hb_retni( ( QDoubleValidator::Notation ) ( p )->notation() );
   }
}

/*
 * void setBottom ( double )
 */
HB_FUNC( QT_QDOUBLEVALIDATOR_SETBOTTOM )
{
   QDoubleValidator * p = hbqt_par_QDoubleValidator( 1 );
   if( p )
   {
      ( p )->setBottom( hb_parnd( 2 ) );
   }
}

/*
 * void setDecimals ( int )
 */
HB_FUNC( QT_QDOUBLEVALIDATOR_SETDECIMALS )
{
   QDoubleValidator * p = hbqt_par_QDoubleValidator( 1 );
   if( p )
   {
      ( p )->setDecimals( hb_parni( 2 ) );
   }
}

/*
 * void setNotation ( Notation )
 */
HB_FUNC( QT_QDOUBLEVALIDATOR_SETNOTATION )
{
   QDoubleValidator * p = hbqt_par_QDoubleValidator( 1 );
   if( p )
   {
      ( p )->setNotation( ( QDoubleValidator::Notation ) hb_parni( 2 ) );
   }
}

/*
 * virtual void setRange ( double minimum, double maximum, int decimals = 0 )
 */
HB_FUNC( QT_QDOUBLEVALIDATOR_SETRANGE )
{
   QDoubleValidator * p = hbqt_par_QDoubleValidator( 1 );
   if( p )
   {
      ( p )->setRange( hb_parnd( 2 ), hb_parnd( 3 ), hb_parni( 4 ) );
   }
}

/*
 * void setTop ( double )
 */
HB_FUNC( QT_QDOUBLEVALIDATOR_SETTOP )
{
   QDoubleValidator * p = hbqt_par_QDoubleValidator( 1 );
   if( p )
   {
      ( p )->setTop( hb_parnd( 2 ) );
   }
}

/*
 * double top () const
 */
HB_FUNC( QT_QDOUBLEVALIDATOR_TOP )
{
   QDoubleValidator * p = hbqt_par_QDoubleValidator( 1 );
   if( p )
   {
      hb_retnd( ( p )->top() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
