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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://www.harbour-project.org
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

#include "hbapi.h"
#include "../hbqt.h"

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
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QSpinBox > pq;
} QGC_POINTER_QSpinBox;

QT_G_FUNC( release_QSpinBox )
{
   QGC_POINTER_QSpinBox * p = ( QGC_POINTER_QSpinBox * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QSpinBox                     p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_QSpinBox                    ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         ( ( QSpinBox * ) p->ph )->~QSpinBox();
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_QSpinBox                    Object deleted!" ) );
         #if defined(__debug__)
            just_debug( "  YES release_QSpinBox                    %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
         #endif
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "release_QSpinBox                    Object Name Missing!" ) );
         #if defined(__debug__)
            just_debug( "  NO  release_QSpinBox" );
         #endif
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "release_QSpinBox                    Object Allready deleted!" ) );
      #if defined(__debug__)
         just_debug( "  DEL release_QSpinBox" );
      #endif
   }
}

void * gcAllocate_QSpinBox( void * pObj )
{
   QGC_POINTER_QSpinBox * p = ( QGC_POINTER_QSpinBox * ) hb_gcAllocate( sizeof( QGC_POINTER_QSpinBox ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QSpinBox;
   new( & p->pq ) QPointer< QSpinBox >( ( QSpinBox * ) pObj );
   #if defined(__debug__)
      just_debug( "          new_QSpinBox                    %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
   #endif
   return( p );
}

HB_FUNC( QT_QSPINBOX )
{
   void * pObj = NULL;

   pObj = ( QSpinBox* ) new QSpinBox( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( gcAllocate_QSpinBox( pObj ) );
}
/*
 * QString cleanText () const
 */
HB_FUNC( QT_QSPINBOX_CLEANTEXT )
{
   hb_retc( hbqt_par_QSpinBox( 1 )->cleanText().toAscii().data() );
}

/*
 * int maximum () const
 */
HB_FUNC( QT_QSPINBOX_MAXIMUM )
{
   hb_retni( hbqt_par_QSpinBox( 1 )->maximum() );
}

/*
 * int minimum () const
 */
HB_FUNC( QT_QSPINBOX_MINIMUM )
{
   hb_retni( hbqt_par_QSpinBox( 1 )->minimum() );
}

/*
 * QString prefix () const
 */
HB_FUNC( QT_QSPINBOX_PREFIX )
{
   hb_retc( hbqt_par_QSpinBox( 1 )->prefix().toAscii().data() );
}

/*
 * void setMaximum ( int max )
 */
HB_FUNC( QT_QSPINBOX_SETMAXIMUM )
{
   hbqt_par_QSpinBox( 1 )->setMaximum( hb_parni( 2 ) );
}

/*
 * void setMinimum ( int min )
 */
HB_FUNC( QT_QSPINBOX_SETMINIMUM )
{
   hbqt_par_QSpinBox( 1 )->setMinimum( hb_parni( 2 ) );
}

/*
 * void setPrefix ( const QString & prefix )
 */
HB_FUNC( QT_QSPINBOX_SETPREFIX )
{
   hbqt_par_QSpinBox( 1 )->setPrefix( hbqt_par_QString( 2 ) );
}

/*
 * void setRange ( int minimum, int maximum )
 */
HB_FUNC( QT_QSPINBOX_SETRANGE )
{
   hbqt_par_QSpinBox( 1 )->setRange( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setSingleStep ( int val )
 */
HB_FUNC( QT_QSPINBOX_SETSINGLESTEP )
{
   hbqt_par_QSpinBox( 1 )->setSingleStep( hb_parni( 2 ) );
}

/*
 * void setSuffix ( const QString & suffix )
 */
HB_FUNC( QT_QSPINBOX_SETSUFFIX )
{
   hbqt_par_QSpinBox( 1 )->setSuffix( hbqt_par_QString( 2 ) );
}

/*
 * int singleStep () const
 */
HB_FUNC( QT_QSPINBOX_SINGLESTEP )
{
   hb_retni( hbqt_par_QSpinBox( 1 )->singleStep() );
}

/*
 * QString suffix () const
 */
HB_FUNC( QT_QSPINBOX_SUFFIX )
{
   hb_retc( hbqt_par_QSpinBox( 1 )->suffix().toAscii().data() );
}

/*
 * int value () const
 */
HB_FUNC( QT_QSPINBOX_VALUE )
{
   hb_retni( hbqt_par_QSpinBox( 1 )->value() );
}

/*
 * void setValue ( int val )
 */
HB_FUNC( QT_QSPINBOX_SETVALUE )
{
   hbqt_par_QSpinBox( 1 )->setValue( hb_parni( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
