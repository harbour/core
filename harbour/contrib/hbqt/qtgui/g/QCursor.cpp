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
 *  Constructed[ 9/9 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // HCURSOR_or_HANDLE handle () const
 */

#include <QtCore/QPointer>

#include <QtGui/QPixmap>
#include <QtGui/QCursor>
#include <QtGui/QBitmap>

/*
 * QCursor ()
 * QCursor ( Qt::CursorShape shape )
 * QCursor ( const QBitmap & bitmap, const QBitmap & mask, int hotX = -1, int hotY = -1 )
 * QCursor ( const QPixmap & pixmap, int hotX = -1, int hotY = -1 )
 * QCursor ( const QCursor & c )
 * QCursor ( HCURSOR cursor )
 * QCursor ( Qt::HANDLE handle )
 * ~QCursor ()
 */

typedef struct
{
   QCursor * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QCursor;

HBQT_GC_FUNC( hbqt_gcRelease_QCursor )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QCursor   /.\\", p->ph ) );
         delete ( ( QCursor * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QCursor   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QCursor    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QCursor    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QCursor( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QCursor * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QCursor;
   p->type = HBQT_TYPE_QCursor;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QCursor", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QCursor", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QCURSOR )
{
   QCursor * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISNUM( 1 ) )
   {
      pObj =  new QCursor( ( Qt::CursorShape ) hb_parni( 1 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj =  new QCursor( *hbqt_par_QCursor( 1 ) ) ;
   }
   else if( hb_pcount() >= 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      QString objName = hbqt_par_QString( 1 );

      if( objName == ( QString ) "QPixmap" )
      {
         pObj =  new QCursor( *hbqt_par_QPixmap( 2 ), HB_ISNUM( 3 ) ? hb_parni( 3 ) : -1, HB_ISNUM( 4 ) ? hb_parni( 4 ) : -1 ) ;
      }
      else
      {
         pObj =  new QCursor() ;
      }
   }
   else if( hb_pcount() >= 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj =  new QCursor( *hbqt_par_QBitmap( 1 ), *hbqt_par_QBitmap( 2 ), HB_ISNUM( 3 ) ? hb_parni( 3 ) : -1, HB_ISNUM( 4 ) ? hb_parni( 4 ) : -1 ) ;
   }
   else
   {
      pObj =  new QCursor() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QCursor( ( void * ) pObj, true ) );
}

/*
 * const QBitmap * bitmap () const
 */
HB_FUNC( QT_QCURSOR_BITMAP )
{
   QCursor * p = hbqt_par_QCursor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( *( ( p )->bitmap() ) ), true ) );
   }
}

/*
 * QPoint hotSpot () const
 */
HB_FUNC( QT_QCURSOR_HOTSPOT )
{
   QCursor * p = hbqt_par_QCursor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->hotSpot() ), true ) );
   }
}

/*
 * const QBitmap * mask () const
 */
HB_FUNC( QT_QCURSOR_MASK )
{
   QCursor * p = hbqt_par_QCursor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( *( ( p )->mask() ) ), true ) );
   }
}

/*
 * QPixmap pixmap () const
 */
HB_FUNC( QT_QCURSOR_PIXMAP )
{
   QCursor * p = hbqt_par_QCursor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap() ), true ) );
   }
}

/*
 * void setShape ( Qt::CursorShape shape )
 */
HB_FUNC( QT_QCURSOR_SETSHAPE )
{
   QCursor * p = hbqt_par_QCursor( 1 );
   if( p )
   {
      ( p )->setShape( ( Qt::CursorShape ) hb_parni( 2 ) );
   }
}

/*
 * Qt::CursorShape shape () const
 */
HB_FUNC( QT_QCURSOR_SHAPE )
{
   QCursor * p = hbqt_par_QCursor( 1 );
   if( p )
   {
      hb_retni( ( Qt::CursorShape ) ( p )->shape() );
   }
}

/*
 * QPoint pos ()
 */
HB_FUNC( QT_QCURSOR_POS )
{
   QCursor * p = hbqt_par_QCursor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->pos() ), true ) );
   }
}

/*
 * void setPos ( int x, int y )
 */
HB_FUNC( QT_QCURSOR_SETPOS )
{
   QCursor * p = hbqt_par_QCursor( 1 );
   if( p )
   {
      ( p )->setPos( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void setPos ( const QPoint & p )
 */
HB_FUNC( QT_QCURSOR_SETPOS_1 )
{
   QCursor * p = hbqt_par_QCursor( 1 );
   if( p )
   {
      ( p )->setPos( *hbqt_par_QPoint( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
