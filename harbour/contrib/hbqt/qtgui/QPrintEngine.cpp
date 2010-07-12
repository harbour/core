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

/*
 *  enum PrintEnginePropertyKey { PPK_CollateCopies, PPK_ColorMode, PPK_Creator, PPK_Duplex, ..., PPK_CustomBase }
 */

#include <QtCore/QPointer>

#include <QtGui/QPrintEngine>


/*
 * virtual ~QPrintEngine ()
 */

typedef struct
{
   QPrintEngine * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QPrintEngine;

QT_G_FUNC( hbqt_gcRelease_QPrintEngine )
{
   HB_SYMBOL_UNUSED( Cargo );
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPrintEngine( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QPrintEngine * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPrintEngine;
   p->type = QT_TYPE_QPrintEngine;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QPrintEngine", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QPrintEngine", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QPRINTENGINE )
{

}

/*
 * virtual bool abort ()
 */
HB_FUNC( QT_QPRINTENGINE_ABORT )
{
   QPrintEngine * p = hbqt_par_QPrintEngine( 1 );
   if( p )
      hb_retl( ( p )->abort() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTENGINE_ABORT FP=hb_retl( ( p )->abort() ); p is NULL" ) );
   }
}

/*
 * virtual int metric ( QPaintDevice::PaintDeviceMetric id ) const
 */
HB_FUNC( QT_QPRINTENGINE_METRIC )
{
   QPrintEngine * p = hbqt_par_QPrintEngine( 1 );
   if( p )
      hb_retni( ( p )->metric( ( QPaintDevice::PaintDeviceMetric ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTENGINE_METRIC FP=hb_retni( ( p )->metric( ( QPaintDevice::PaintDeviceMetric ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual bool newPage ()
 */
HB_FUNC( QT_QPRINTENGINE_NEWPAGE )
{
   QPrintEngine * p = hbqt_par_QPrintEngine( 1 );
   if( p )
      hb_retl( ( p )->newPage() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTENGINE_NEWPAGE FP=hb_retl( ( p )->newPage() ); p is NULL" ) );
   }
}

/*
 * virtual QPrinter::PrinterState printerState () const
 */
HB_FUNC( QT_QPRINTENGINE_PRINTERSTATE )
{
   QPrintEngine * p = hbqt_par_QPrintEngine( 1 );
   if( p )
      hb_retni( ( QPrinter::PrinterState ) ( p )->printerState() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTENGINE_PRINTERSTATE FP=hb_retni( ( QPrinter::PrinterState ) ( p )->printerState() ); p is NULL" ) );
   }
}

/*
 * virtual QVariant property ( PrintEnginePropertyKey key ) const
 */
HB_FUNC( QT_QPRINTENGINE_PROPERTY )
{
   QPrintEngine * p = hbqt_par_QPrintEngine( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->property( ( QPrintEngine::PrintEnginePropertyKey ) hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTENGINE_PROPERTY FP=hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->property( ( QPrintEngine::PrintEnginePropertyKey ) hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual void setProperty ( PrintEnginePropertyKey key, const QVariant & value )
 */
HB_FUNC( QT_QPRINTENGINE_SETPROPERTY )
{
   QPrintEngine * p = hbqt_par_QPrintEngine( 1 );
   if( p )
      ( p )->setProperty( ( QPrintEngine::PrintEnginePropertyKey ) hb_parni( 2 ), *hbqt_par_QVariant( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTENGINE_SETPROPERTY FP=( p )->setProperty( ( QPrintEngine::PrintEnginePropertyKey ) hb_parni( 2 ), *hbqt_par_QVariant( 3 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
