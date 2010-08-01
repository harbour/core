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
#include "hbqtgui_garbage.h"
#include "hbqtcore_garbage.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum Mode { Clipboard, Selection, FindBuffer }
 */

#include <QtCore/QPointer>

#include <QtGui/QPalette>
#include <QtGui/QClipboard>
#include <QtGui/QApplication>
#include <QtCore/QMimeData>

/*
 *
 *
 */

typedef struct
{
   QPointer< QClipboard > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QClipboard;

QT_G_FUNC( hbqt_gcRelease_QClipboard )
{
   HB_SYMBOL_UNUSED( Cargo );
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QClipboard( void * pObj, bool bNew )
{
   QGC_POINTER_QClipboard * p = ( QGC_POINTER_QClipboard * ) hb_gcAllocate( sizeof( QGC_POINTER_QClipboard ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QClipboard >( ( QClipboard * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QClipboard;
   p->type = HBQT_TYPE_QClipboard;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QClipboard  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QClipboard", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QCLIPBOARD )
{
   QClipboard * pObj = NULL;

   pObj =  QApplication::clipboard() ;

   hb_retptrGC( hbqt_gcAllocate_QClipboard( ( void * ) pObj, true ) );
}

/*
 * void clear ( Mode mode = Clipboard )
 */
HB_FUNC( QT_QCLIPBOARD_CLEAR )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      ( p )->clear( ( HB_ISNUM( 2 ) ? ( QClipboard::Mode ) hb_parni( 2 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCLIPBOARD_CLEAR FP=( p )->clear( ( HB_ISNUM( 2 ) ? ( QClipboard::Mode ) hb_parni( 2 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) ); p is NULL" ) );
   }
}

/*
 * QImage image ( Mode mode = Clipboard ) const
 */
HB_FUNC( QT_QCLIPBOARD_IMAGE )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->image( ( HB_ISNUM( 2 ) ? ( QClipboard::Mode ) hb_parni( 2 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCLIPBOARD_IMAGE FP=hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->image( ( HB_ISNUM( 2 ) ? ( QClipboard::Mode ) hb_parni( 2 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * bool ownsClipboard () const
 */
HB_FUNC( QT_QCLIPBOARD_OWNSCLIPBOARD )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      hb_retl( ( p )->ownsClipboard() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCLIPBOARD_OWNSCLIPBOARD FP=hb_retl( ( p )->ownsClipboard() ); p is NULL" ) );
   }
}

/*
 * bool ownsFindBuffer () const
 */
HB_FUNC( QT_QCLIPBOARD_OWNSFINDBUFFER )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      hb_retl( ( p )->ownsFindBuffer() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCLIPBOARD_OWNSFINDBUFFER FP=hb_retl( ( p )->ownsFindBuffer() ); p is NULL" ) );
   }
}

/*
 * bool ownsSelection () const
 */
HB_FUNC( QT_QCLIPBOARD_OWNSSELECTION )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      hb_retl( ( p )->ownsSelection() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCLIPBOARD_OWNSSELECTION FP=hb_retl( ( p )->ownsSelection() ); p is NULL" ) );
   }
}

/*
 * QPixmap pixmap ( Mode mode = Clipboard ) const
 */
HB_FUNC( QT_QCLIPBOARD_PIXMAP )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap( ( HB_ISNUM( 2 ) ? ( QClipboard::Mode ) hb_parni( 2 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCLIPBOARD_PIXMAP FP=hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap( ( HB_ISNUM( 2 ) ? ( QClipboard::Mode ) hb_parni( 2 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void setImage ( const QImage & image, Mode mode = Clipboard )
 */
HB_FUNC( QT_QCLIPBOARD_SETIMAGE )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      ( p )->setImage( *hbqt_par_QImage( 2 ), ( HB_ISNUM( 3 ) ? ( QClipboard::Mode ) hb_parni( 3 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCLIPBOARD_SETIMAGE FP=( p )->setImage( *hbqt_par_QImage( 2 ), ( HB_ISNUM( 3 ) ? ( QClipboard::Mode ) hb_parni( 3 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) ); p is NULL" ) );
   }
}

/*
 * void setMimeData ( QMimeData * src, Mode mode = Clipboard )
 */
HB_FUNC( QT_QCLIPBOARD_SETMIMEDATA )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      ( p )->setMimeData( hbqt_par_QMimeData( 2 ), ( HB_ISNUM( 3 ) ? ( QClipboard::Mode ) hb_parni( 3 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCLIPBOARD_SETMIMEDATA FP=( p )->setMimeData( hbqt_par_QMimeData( 2 ), ( HB_ISNUM( 3 ) ? ( QClipboard::Mode ) hb_parni( 3 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) ); p is NULL" ) );
   }
}

/*
 * void setPixmap ( const QPixmap & pixmap, Mode mode = Clipboard )
 */
HB_FUNC( QT_QCLIPBOARD_SETPIXMAP )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      ( p )->setPixmap( *hbqt_par_QPixmap( 2 ), ( HB_ISNUM( 3 ) ? ( QClipboard::Mode ) hb_parni( 3 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCLIPBOARD_SETPIXMAP FP=( p )->setPixmap( *hbqt_par_QPixmap( 2 ), ( HB_ISNUM( 3 ) ? ( QClipboard::Mode ) hb_parni( 3 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) ); p is NULL" ) );
   }
}

/*
 * void setText ( const QString & text, Mode mode = Clipboard )
 */
HB_FUNC( QT_QCLIPBOARD_SETTEXT )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      ( p )->setText( QClipboard::tr( hb_parc( 2 ) ), ( HB_ISNUM( 3 ) ? ( QClipboard::Mode ) hb_parni( 3 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCLIPBOARD_SETTEXT FP=( p )->setText( QClipboard::tr( hb_parc( 2 ) ), ( HB_ISNUM( 3 ) ? ( QClipboard::Mode ) hb_parni( 3 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) ); p is NULL" ) );
   }
}

/*
 * bool supportsFindBuffer () const
 */
HB_FUNC( QT_QCLIPBOARD_SUPPORTSFINDBUFFER )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      hb_retl( ( p )->supportsFindBuffer() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCLIPBOARD_SUPPORTSFINDBUFFER FP=hb_retl( ( p )->supportsFindBuffer() ); p is NULL" ) );
   }
}

/*
 * bool supportsSelection () const
 */
HB_FUNC( QT_QCLIPBOARD_SUPPORTSSELECTION )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      hb_retl( ( p )->supportsSelection() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCLIPBOARD_SUPPORTSSELECTION FP=hb_retl( ( p )->supportsSelection() ); p is NULL" ) );
   }
}

/*
 * QString text ( Mode mode = Clipboard ) const
 */
HB_FUNC( QT_QCLIPBOARD_TEXT )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      hb_retc( ( p )->text( ( HB_ISNUM( 2 ) ? ( QClipboard::Mode ) hb_parni( 2 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCLIPBOARD_TEXT FP=hb_retc( ( p )->text( ( HB_ISNUM( 2 ) ? ( QClipboard::Mode ) hb_parni( 2 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) ).toAscii().data() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
