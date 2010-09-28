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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum PrintEnginePropertyKey { PPK_CollateCopies, PPK_ColorMode, PPK_Creator, PPK_Duplex, ..., PPK_CustomBase }
 */

/*
 *  Constructed[ 6/6 [ 100.00% ] ]
 *
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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPrintEngine;

HBQT_GC_FUNC( hbqt_gcRelease_QPrintEngine )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPrintEngine( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QPrintEngine * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPrintEngine;
   p->type = HBQT_TYPE_QPrintEngine;

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
   {
      hb_retl( ( p )->abort() );
   }
}

/*
 * virtual int metric ( QPaintDevice::PaintDeviceMetric id ) const
 */
HB_FUNC( QT_QPRINTENGINE_METRIC )
{
   QPrintEngine * p = hbqt_par_QPrintEngine( 1 );
   if( p )
   {
      hb_retni( ( p )->metric( ( QPaintDevice::PaintDeviceMetric ) hb_parni( 2 ) ) );
   }
}

/*
 * virtual bool newPage ()
 */
HB_FUNC( QT_QPRINTENGINE_NEWPAGE )
{
   QPrintEngine * p = hbqt_par_QPrintEngine( 1 );
   if( p )
   {
      hb_retl( ( p )->newPage() );
   }
}

/*
 * virtual QPrinter::PrinterState printerState () const
 */
HB_FUNC( QT_QPRINTENGINE_PRINTERSTATE )
{
   QPrintEngine * p = hbqt_par_QPrintEngine( 1 );
   if( p )
   {
      hb_retni( ( QPrinter::PrinterState ) ( p )->printerState() );
   }
}

/*
 * virtual QVariant property ( PrintEnginePropertyKey key ) const
 */
HB_FUNC( QT_QPRINTENGINE_PROPERTY )
{
   QPrintEngine * p = hbqt_par_QPrintEngine( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->property( ( QPrintEngine::PrintEnginePropertyKey ) hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * virtual void setProperty ( PrintEnginePropertyKey key, const QVariant & value )
 */
HB_FUNC( QT_QPRINTENGINE_SETPROPERTY )
{
   QPrintEngine * p = hbqt_par_QPrintEngine( 1 );
   if( p )
   {
      ( p )->setProperty( ( QPrintEngine::PrintEnginePropertyKey ) hb_parni( 2 ), *hbqt_par_QVariant( 3 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
