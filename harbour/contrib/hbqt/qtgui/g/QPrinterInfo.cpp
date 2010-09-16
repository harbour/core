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
 *  Constructed[ 5/6 [ 83.33% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<QPrinter::PaperSize> supportedPaperSizes () const
 */

#include <QtCore/QPointer>

#include <QtGui/QPrinterInfo>


/*
 * QPrinterInfo ()
 * QPrinterInfo ( const QPrinterInfo & src )
 * QPrinterInfo ( const QPrinter & printer )
 * ~QPrinterInfo ()
 */

typedef struct
{
   QPrinterInfo * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPrinterInfo;

HBQT_GC_FUNC( hbqt_gcRelease_QPrinterInfo )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QPrinterInfo   /.\\", p->ph ) );
         delete ( ( QPrinterInfo * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QPrinterInfo   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QPrinterInfo    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QPrinterInfo    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPrinterInfo( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QPrinterInfo * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPrinterInfo;
   p->type = HBQT_TYPE_QPrinterInfo;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QPrinterInfo", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QPrinterInfo", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QPRINTERINFO )
{
   QPrinterInfo * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QPrinterInfo( *hbqt_par_QPrinterInfo( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = new QPrinterInfo( *hbqt_par_QPrinter( 2 ) ) ;
   }
   else
   {
      pObj = new QPrinterInfo() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QPrinterInfo( ( void * ) pObj, true ) );
}

/*
 * bool isDefault () const
 */
HB_FUNC( QT_QPRINTERINFO_ISDEFAULT )
{
   QPrinterInfo * p = hbqt_par_QPrinterInfo( 1 );
   if( p )
   {
      hb_retl( ( p )->isDefault() );
   }
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QPRINTERINFO_ISNULL )
{
   QPrinterInfo * p = hbqt_par_QPrinterInfo( 1 );
   if( p )
   {
      hb_retl( ( p )->isNull() );
   }
}

/*
 * QString printerName () const
 */
HB_FUNC( QT_QPRINTERINFO_PRINTERNAME )
{
   QPrinterInfo * p = hbqt_par_QPrinterInfo( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->printerName().toUtf8().data() );
   }
}

/*
 * QList<QPrinterInfo> availablePrinters ()
 */
HB_FUNC( QT_QPRINTERINFO_AVAILABLEPRINTERS )
{
   QPrinterInfo * p = hbqt_par_QPrinterInfo( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QPrinterInfo>( ( p )->availablePrinters() ), true ) );
   }
}

/*
 * QPrinterInfo defaultPrinter ()
 */
HB_FUNC( QT_QPRINTERINFO_DEFAULTPRINTER )
{
   QPrinterInfo * p = hbqt_par_QPrinterInfo( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPrinterInfo( new QPrinterInfo( ( p )->defaultPrinter() ), true ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
