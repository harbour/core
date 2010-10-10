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
 *  enum Mode { Clipboard, Selection, FindBuffer }
 */

/*
 *  Constructed[ 13/13 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //const QMimeData * mimeData ( Mode mode = Clipboard ) const
 *  // QString text ( QString & subtype, Mode mode = Clipboard ) const
 */

#include <QtCore/QPointer>

#include <QtGui/QPalette>
#include <QtGui/QClipboard>
#include <QtGui/QApplication>
#include <QtCore/QMimeData>

/*
 * QApplication::clipboard()
 *
 */

typedef struct
{
   QPointer< QClipboard > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QClipboard;

HBQT_GC_FUNC( hbqt_gcRelease_QClipboard )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QClipboard( void * pObj, bool bNew )
{
   HBQT_GC_T_QClipboard * p = ( HBQT_GC_T_QClipboard * ) hb_gcAllocate( sizeof( HBQT_GC_T_QClipboard ), hbqt_gcFuncs() );

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

   pObj = QApplication::clipboard() ;

   hb_retptrGC( hbqt_gcAllocate_QClipboard( ( void * ) pObj, true ) );
}

/*
 * void clear ( Mode mode = Clipboard )
 */
HB_FUNC( QT_QCLIPBOARD_CLEAR )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
   {
      ( p )->clear( ( HB_ISNUM( 2 ) ? ( QClipboard::Mode ) hb_parni( 2 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) );
   }
}

/*
 * QImage image ( Mode mode = Clipboard ) const
 */
HB_FUNC( QT_QCLIPBOARD_IMAGE )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->image( ( HB_ISNUM( 2 ) ? ( QClipboard::Mode ) hb_parni( 2 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) ) ), true ) );
   }
}

/*
 * bool ownsClipboard () const
 */
HB_FUNC( QT_QCLIPBOARD_OWNSCLIPBOARD )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
   {
      hb_retl( ( p )->ownsClipboard() );
   }
}

/*
 * bool ownsFindBuffer () const
 */
HB_FUNC( QT_QCLIPBOARD_OWNSFINDBUFFER )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
   {
      hb_retl( ( p )->ownsFindBuffer() );
   }
}

/*
 * bool ownsSelection () const
 */
HB_FUNC( QT_QCLIPBOARD_OWNSSELECTION )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
   {
      hb_retl( ( p )->ownsSelection() );
   }
}

/*
 * QPixmap pixmap ( Mode mode = Clipboard ) const
 */
HB_FUNC( QT_QCLIPBOARD_PIXMAP )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap( ( HB_ISNUM( 2 ) ? ( QClipboard::Mode ) hb_parni( 2 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) ) ), true ) );
   }
}

/*
 * void setImage ( const QImage & image, Mode mode = Clipboard )
 */
HB_FUNC( QT_QCLIPBOARD_SETIMAGE )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
   {
      ( p )->setImage( *hbqt_par_QImage( 2 ), ( HB_ISNUM( 3 ) ? ( QClipboard::Mode ) hb_parni( 3 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) );
   }
}

/*
 * void setMimeData ( QMimeData * src, Mode mode = Clipboard )
 */
HB_FUNC( QT_QCLIPBOARD_SETMIMEDATA )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
   {
      ( p )->setMimeData( hbqt_par_QMimeData( 2 ), ( HB_ISNUM( 3 ) ? ( QClipboard::Mode ) hb_parni( 3 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) );
   }
}

/*
 * void setPixmap ( const QPixmap & pixmap, Mode mode = Clipboard )
 */
HB_FUNC( QT_QCLIPBOARD_SETPIXMAP )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
   {
      ( p )->setPixmap( *hbqt_par_QPixmap( 2 ), ( HB_ISNUM( 3 ) ? ( QClipboard::Mode ) hb_parni( 3 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) );
   }
}

/*
 * void setText ( const QString & text, Mode mode = Clipboard )
 */
HB_FUNC( QT_QCLIPBOARD_SETTEXT )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
   {
      void * pText;
      ( p )->setText( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISNUM( 3 ) ? ( QClipboard::Mode ) hb_parni( 3 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) );
      hb_strfree( pText );
   }
}

/*
 * bool supportsFindBuffer () const
 */
HB_FUNC( QT_QCLIPBOARD_SUPPORTSFINDBUFFER )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
   {
      hb_retl( ( p )->supportsFindBuffer() );
   }
}

/*
 * bool supportsSelection () const
 */
HB_FUNC( QT_QCLIPBOARD_SUPPORTSSELECTION )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
   {
      hb_retl( ( p )->supportsSelection() );
   }
}

/*
 * QString text ( Mode mode = Clipboard ) const
 */
HB_FUNC( QT_QCLIPBOARD_TEXT )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->text( ( HB_ISNUM( 2 ) ? ( QClipboard::Mode ) hb_parni( 2 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) ).toUtf8().data() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
