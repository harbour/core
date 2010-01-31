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
 *  enum PrintEnginePropertyKey { PPK_CollateCopies, PPK_ColorMode, PPK_Creator, PPK_Duplex, ..., PPK_CustomBase }
 */

#include <QtCore/QPointer>

#include <QtGui/QPrintEngine>


/*
 * virtual ~QPrintEngine ()
 */

typedef struct
{
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
} QGC_POINTER_QPrintEngine;

QT_G_FUNC( hbqt_gcRelease_QPrintEngine )
{
   HB_SYMBOL_UNUSED( Cargo );
}

void * hbqt_gcAllocate_QPrintEngine( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPrintEngine;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QPrintEngine               ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
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
   hb_retl( hbqt_par_QPrintEngine( 1 )->abort() );
}

/*
 * virtual int metric ( QPaintDevice::PaintDeviceMetric id ) const
 */
HB_FUNC( QT_QPRINTENGINE_METRIC )
{
   hb_retni( hbqt_par_QPrintEngine( 1 )->metric( ( QPaintDevice::PaintDeviceMetric ) hb_parni( 2 ) ) );
}

/*
 * virtual bool newPage ()
 */
HB_FUNC( QT_QPRINTENGINE_NEWPAGE )
{
   hb_retl( hbqt_par_QPrintEngine( 1 )->newPage() );
}

/*
 * virtual QPrinter::PrinterState printerState () const
 */
HB_FUNC( QT_QPRINTENGINE_PRINTERSTATE )
{
   hb_retni( ( QPrinter::PrinterState ) hbqt_par_QPrintEngine( 1 )->printerState() );
}

/*
 * virtual QVariant property ( PrintEnginePropertyKey key ) const
 */
HB_FUNC( QT_QPRINTENGINE_PROPERTY )
{
   hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( hbqt_par_QPrintEngine( 1 )->property( ( QPrintEngine::PrintEnginePropertyKey ) hb_parni( 2 ) ) ), true ) );
}

/*
 * virtual void setProperty ( PrintEnginePropertyKey key, const QVariant & value )
 */
HB_FUNC( QT_QPRINTENGINE_SETPROPERTY )
{
   hbqt_par_QPrintEngine( 1 )->setProperty( ( QPrintEngine::PrintEnginePropertyKey ) hb_parni( 2 ), *hbqt_par_QVariant( 3 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
