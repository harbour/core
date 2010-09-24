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
 *  Constructed[ 13/13 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QTextInlineObject>


/*
 * QTextInlineObject ( int i, QTextEngine * e )
 *
 */

typedef struct
{
   QTextInlineObject * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextInlineObject;

HBQT_GC_FUNC( hbqt_gcRelease_QTextInlineObject )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextInlineObject( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextInlineObject * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextInlineObject;
   p->type = HBQT_TYPE_QTextInlineObject;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTextInlineObject", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTextInlineObject", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTINLINEOBJECT )
{
   // hb_retptr( new QTextInlineObject( hb_parni( 1 ), hbqt_par_QTextEngine( 2 ) ) );
}

/*
 * qreal ascent () const
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_ASCENT )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
   {
      hb_retnd( ( p )->ascent() );
   }
}

/*
 * qreal descent () const
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_DESCENT )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
   {
      hb_retnd( ( p )->descent() );
   }
}

/*
 * QTextFormat format () const
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_FORMAT )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextFormat( new QTextFormat( ( p )->format() ), true ) );
   }
}

/*
 * int formatIndex () const
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_FORMATINDEX )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
   {
      hb_retni( ( p )->formatIndex() );
   }
}

/*
 * qreal height () const
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_HEIGHT )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
   {
      hb_retnd( ( p )->height() );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_ISVALID )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
   {
      hb_retl( ( p )->isValid() );
   }
}

/*
 * QRectF rect () const
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_RECT )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->rect() ), true ) );
   }
}

/*
 * void setAscent ( qreal a )
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_SETASCENT )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
   {
      ( p )->setAscent( hb_parnd( 2 ) );
   }
}

/*
 * void setDescent ( qreal d )
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_SETDESCENT )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
   {
      ( p )->setDescent( hb_parnd( 2 ) );
   }
}

/*
 * void setWidth ( qreal w )
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_SETWIDTH )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
   {
      ( p )->setWidth( hb_parnd( 2 ) );
   }
}

/*
 * Qt::LayoutDirection textDirection () const
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_TEXTDIRECTION )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
   {
      hb_retni( ( Qt::LayoutDirection ) ( p )->textDirection() );
   }
}

/*
 * int textPosition () const
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_TEXTPOSITION )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
   {
      hb_retni( ( p )->textPosition() );
   }
}

/*
 * qreal width () const
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_WIDTH )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
   {
      hb_retnd( ( p )->width() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
