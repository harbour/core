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

#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum Mode { Clipboard, Selection, FindBuffer }
 */

#include <QtCore/QPointer>

#include <qpalette.h>
#include <QtGui/QClipboard>
#include <QtGui/QApplication>
#include <QtCore/QMimeData>

/*
 *
 *
 */

typedef struct
{
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   QPointer< QClipboard > pq;
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

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QClipboard;

   if( bNew )
   {
      new( & p->pq ) QPointer< QClipboard >( ( QClipboard * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QClipboard                 ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QCLIPBOARD )
{
   void * pObj = NULL;

   pObj = ( QClipboard* ) QApplication::clipboard() ;

   hb_retptrGC( hbqt_gcAllocate_QClipboard( pObj, true ) );
}

/*
 * void clear ( Mode mode = Clipboard )
 */
HB_FUNC( QT_QCLIPBOARD_CLEAR )
{
   hbqt_par_QClipboard( 1 )->clear( ( HB_ISNUM( 2 ) ? ( QClipboard::Mode ) hb_parni( 2 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) );
}

/*
 * QImage image ( Mode mode = Clipboard ) const
 */
HB_FUNC( QT_QCLIPBOARD_IMAGE )
{
   hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( hbqt_par_QClipboard( 1 )->image( ( HB_ISNUM( 2 ) ? ( QClipboard::Mode ) hb_parni( 2 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) ) ), true ) );
}

/*
 * bool ownsClipboard () const
 */
HB_FUNC( QT_QCLIPBOARD_OWNSCLIPBOARD )
{
   hb_retl( hbqt_par_QClipboard( 1 )->ownsClipboard() );
}

/*
 * bool ownsFindBuffer () const
 */
HB_FUNC( QT_QCLIPBOARD_OWNSFINDBUFFER )
{
   hb_retl( hbqt_par_QClipboard( 1 )->ownsFindBuffer() );
}

/*
 * bool ownsSelection () const
 */
HB_FUNC( QT_QCLIPBOARD_OWNSSELECTION )
{
   hb_retl( hbqt_par_QClipboard( 1 )->ownsSelection() );
}

/*
 * QPixmap pixmap ( Mode mode = Clipboard ) const
 */
HB_FUNC( QT_QCLIPBOARD_PIXMAP )
{
   hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( hbqt_par_QClipboard( 1 )->pixmap( ( HB_ISNUM( 2 ) ? ( QClipboard::Mode ) hb_parni( 2 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) ) ), true ) );
}

/*
 * void setImage ( const QImage & image, Mode mode = Clipboard )
 */
HB_FUNC( QT_QCLIPBOARD_SETIMAGE )
{
   hbqt_par_QClipboard( 1 )->setImage( *hbqt_par_QImage( 2 ), ( HB_ISNUM( 3 ) ? ( QClipboard::Mode ) hb_parni( 3 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) );
}

/*
 * void setMimeData ( QMimeData * src, Mode mode = Clipboard )
 */
HB_FUNC( QT_QCLIPBOARD_SETMIMEDATA )
{
   hbqt_par_QClipboard( 1 )->setMimeData( hbqt_par_QMimeData( 2 ), ( HB_ISNUM( 3 ) ? ( QClipboard::Mode ) hb_parni( 3 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) );
}

/*
 * void setPixmap ( const QPixmap & pixmap, Mode mode = Clipboard )
 */
HB_FUNC( QT_QCLIPBOARD_SETPIXMAP )
{
   hbqt_par_QClipboard( 1 )->setPixmap( *hbqt_par_QPixmap( 2 ), ( HB_ISNUM( 3 ) ? ( QClipboard::Mode ) hb_parni( 3 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) );
}

/*
 * void setText ( const QString & text, Mode mode = Clipboard )
 */
HB_FUNC( QT_QCLIPBOARD_SETTEXT )
{
   hbqt_par_QClipboard( 1 )->setText( QClipboard::tr( hb_parc( 2 ) ), ( HB_ISNUM( 3 ) ? ( QClipboard::Mode ) hb_parni( 3 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) );
}

/*
 * bool supportsFindBuffer () const
 */
HB_FUNC( QT_QCLIPBOARD_SUPPORTSFINDBUFFER )
{
   hb_retl( hbqt_par_QClipboard( 1 )->supportsFindBuffer() );
}

/*
 * bool supportsSelection () const
 */
HB_FUNC( QT_QCLIPBOARD_SUPPORTSSELECTION )
{
   hb_retl( hbqt_par_QClipboard( 1 )->supportsSelection() );
}

/*
 * QString text ( Mode mode = Clipboard ) const
 */
HB_FUNC( QT_QCLIPBOARD_TEXT )
{
   hb_retc( hbqt_par_QClipboard( 1 )->text( ( HB_ISNUM( 2 ) ? ( QClipboard::Mode ) hb_parni( 2 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) ).toAscii().data() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
