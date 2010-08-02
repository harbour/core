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

#include "hbqt.h"
#include "hbqtgui_garbage.h"
#include "hbqtgui.h"
#include "hbqtcore_garbage.h"
#include "hbqtcore.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum RenderFlag { RightToLeft, Overline, Underline, StrikeOut }
 *  flags RenderFlags
 */

#include <QtCore/QPointer>

#include <QtGui/QTextItem>


/*
 *
 *
 */

typedef struct
{
   QTextItem * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QTextItem;

QT_G_FUNC( hbqt_gcRelease_QTextItem )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QTextItem   /.\\", p->ph ) );
         delete ( ( QTextItem * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QTextItem   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTextItem    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTextItem    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextItem( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QTextItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextItem;
   p->type = HBQT_TYPE_QTextItem;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTextItem", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTextItem", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTITEM )
{
   QTextItem * pObj = NULL;

   pObj =  new QTextItem() ;

   hb_retptrGC( hbqt_gcAllocate_QTextItem( ( void * ) pObj, true ) );
}

/*
 * qreal ascent () const
 */
HB_FUNC( QT_QTEXTITEM_ASCENT )
{
   QTextItem * p = hbqt_par_QTextItem( 1 );
   if( p )
      hb_retnd( ( p )->ascent() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTITEM_ASCENT FP=hb_retnd( ( p )->ascent() ); p is NULL" ) );
   }
}

/*
 * qreal descent () const
 */
HB_FUNC( QT_QTEXTITEM_DESCENT )
{
   QTextItem * p = hbqt_par_QTextItem( 1 );
   if( p )
      hb_retnd( ( p )->descent() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTITEM_DESCENT FP=hb_retnd( ( p )->descent() ); p is NULL" ) );
   }
}

/*
 * QFont font () const
 */
HB_FUNC( QT_QTEXTITEM_FONT )
{
   QTextItem * p = hbqt_par_QTextItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTITEM_FONT FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) ); p is NULL" ) );
   }
}

/*
 * RenderFlags renderFlags () const
 */
HB_FUNC( QT_QTEXTITEM_RENDERFLAGS )
{
   QTextItem * p = hbqt_par_QTextItem( 1 );
   if( p )
      hb_retni( ( QTextItem::RenderFlags ) ( p )->renderFlags() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTITEM_RENDERFLAGS FP=hb_retni( ( QTextItem::RenderFlags ) ( p )->renderFlags() ); p is NULL" ) );
   }
}

/*
 * QString text () const
 */
HB_FUNC( QT_QTEXTITEM_TEXT )
{
   QTextItem * p = hbqt_par_QTextItem( 1 );
   if( p )
      hb_retc( ( p )->text().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTITEM_TEXT FP=hb_retc( ( p )->text().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * qreal width () const
 */
HB_FUNC( QT_QTEXTITEM_WIDTH )
{
   QTextItem * p = hbqt_par_QTextItem( 1 );
   if( p )
      hb_retnd( ( p )->width() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTITEM_WIDTH FP=hb_retnd( ( p )->width() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
