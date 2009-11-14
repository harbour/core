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

/*
 *  enum CoordinateMode { LogicalMode, StretchToDeviceMode, ObjectBoundingMode }
 *  enum Spread { PadSpread, RepeatSpread, ReflectSpread }
 *  enum Type { LinearGradient, RadialGradient, ConicalGradient, NoGradient }
 */

#include <QtCore/QPointer>

#include <QtGui/QLinearGradient>


/*
 * QLinearGradient ()
 * QLinearGradient ( const QPointF & start, const QPointF & finalStop )
 * QLinearGradient ( qreal x1, qreal y1, qreal x2, qreal y2 )
 */

QT_G_FUNC( release_QLinearGradient )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QLinearGradient              p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "release_QLinearGradient             ph=%p", p->ph ) );

   if( p && p->ph )
   {
      ( ( QLinearGradient * ) p->ph )->~QLinearGradient();
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "release_QLinearGradient             Object deleted!" ) );
      #if defined(__debug__)
         just_debug( "  YES release_QLinearGradient             %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
      #endif
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "release_QLinearGradient             Object Allready deleted!" ) );
      #if defined(__debug__)
         just_debug( "  DEL release_QLinearGradient" );
      #endif
   }
}

void * gcAllocate_QLinearGradient( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QLinearGradient;
   #if defined(__debug__)
      just_debug( "          new_QLinearGradient             %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
   #endif
   return( p );
}

HB_FUNC( QT_QLINEARGRADIENT )
{
   void * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = ( QLinearGradient* ) new QLinearGradient( *hbqt_par_QLinearGradient( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = ( QLinearGradient* ) new QLinearGradient( *hbqt_par_QPoint( 1 ), *hbqt_par_QPoint( 2 ) ) ;
   }
   else if( hb_pcount() == 4 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) )
   {
      pObj = ( QLinearGradient* ) new QLinearGradient( hb_parnd( 1 ), hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) ) ;
   }
   else
   {
      pObj = ( QLinearGradient* ) new QLinearGradient() ;
   }

   hb_retptrGC( gcAllocate_QLinearGradient( pObj ) );
}
/*
 * QPointF finalStop () const
 */
HB_FUNC( QT_QLINEARGRADIENT_FINALSTOP )
{
   hb_retptrGC( gcAllocate_QPointF( new QPointF( hbqt_par_QLinearGradient( 1 )->finalStop() ) ) );
}

/*
 * void setFinalStop ( const QPointF & stop )
 */
HB_FUNC( QT_QLINEARGRADIENT_SETFINALSTOP )
{
   hbqt_par_QLinearGradient( 1 )->setFinalStop( *hbqt_par_QPointF( 2 ) );
}

/*
 * void setFinalStop ( qreal x, qreal y )
 */
HB_FUNC( QT_QLINEARGRADIENT_SETFINALSTOP_1 )
{
   hbqt_par_QLinearGradient( 1 )->setFinalStop( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/*
 * void setStart ( const QPointF & start )
 */
HB_FUNC( QT_QLINEARGRADIENT_SETSTART )
{
   hbqt_par_QLinearGradient( 1 )->setStart( *hbqt_par_QPointF( 2 ) );
}

/*
 * void setStart ( qreal x, qreal y )
 */
HB_FUNC( QT_QLINEARGRADIENT_SETSTART_1 )
{
   hbqt_par_QLinearGradient( 1 )->setStart( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/*
 * QPointF start () const
 */
HB_FUNC( QT_QLINEARGRADIENT_START )
{
   hb_retptrGC( gcAllocate_QPointF( new QPointF( hbqt_par_QLinearGradient( 1 )->start() ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
