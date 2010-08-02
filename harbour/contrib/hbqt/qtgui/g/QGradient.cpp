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
 *  enum CoordinateMode { LogicalMode, StretchToDeviceMode, ObjectBoundingMode }
 *  enum Spread { PadSpread, RepeatSpread, ReflectSpread }
 *  enum Type { LinearGradient, RadialGradient, ConicalGradient, NoGradient }
 */

#include <QtCore/QPointer>

#include <QtGui/QGradient>


/*
 *
 */

typedef struct
{
   QGradient * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QGradient;

QT_G_FUNC( hbqt_gcRelease_QGradient )
{
   HB_SYMBOL_UNUSED( Cargo );
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QGradient( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QGradient * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGradient;
   p->type = HBQT_TYPE_QGradient;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QGradient", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QGradient", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QGRADIENT )
{

}

/*
 * CoordinateMode coordinateMode () const
 */
HB_FUNC( QT_QGRADIENT_COORDINATEMODE )
{
   QGradient * p = hbqt_par_QGradient( 1 );
   if( p )
      hb_retni( ( QGradient::CoordinateMode ) ( p )->coordinateMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRADIENT_COORDINATEMODE FP=hb_retni( ( QGradient::CoordinateMode ) ( p )->coordinateMode() ); p is NULL" ) );
   }
}

/*
 * void setColorAt ( qreal position, const QColor & color )
 */
HB_FUNC( QT_QGRADIENT_SETCOLORAT )
{
   QGradient * p = hbqt_par_QGradient( 1 );
   if( p )
      ( p )->setColorAt( hb_parnd( 2 ), *hbqt_par_QColor( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRADIENT_SETCOLORAT FP=( p )->setColorAt( hb_parnd( 2 ), *hbqt_par_QColor( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setCoordinateMode ( CoordinateMode mode )
 */
HB_FUNC( QT_QGRADIENT_SETCOORDINATEMODE )
{
   QGradient * p = hbqt_par_QGradient( 1 );
   if( p )
      ( p )->setCoordinateMode( ( QGradient::CoordinateMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRADIENT_SETCOORDINATEMODE FP=( p )->setCoordinateMode( ( QGradient::CoordinateMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSpread ( Spread method )
 */
HB_FUNC( QT_QGRADIENT_SETSPREAD )
{
   QGradient * p = hbqt_par_QGradient( 1 );
   if( p )
      ( p )->setSpread( ( QGradient::Spread ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRADIENT_SETSPREAD FP=( p )->setSpread( ( QGradient::Spread ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * Spread spread () const
 */
HB_FUNC( QT_QGRADIENT_SPREAD )
{
   QGradient * p = hbqt_par_QGradient( 1 );
   if( p )
      hb_retni( ( QGradient::Spread ) ( p )->spread() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRADIENT_SPREAD FP=hb_retni( ( QGradient::Spread ) ( p )->spread() ); p is NULL" ) );
   }
}

/*
 * Type type () const
 */
HB_FUNC( QT_QGRADIENT_TYPE )
{
   QGradient * p = hbqt_par_QGradient( 1 );
   if( p )
      hb_retni( ( QGradient::Type ) ( p )->type() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRADIENT_TYPE FP=hb_retni( ( QGradient::Type ) ( p )->type() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
