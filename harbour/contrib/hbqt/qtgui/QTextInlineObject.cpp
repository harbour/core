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
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QTextInlineObject;

QT_G_FUNC( hbqt_gcRelease_QTextInlineObject )
{
   HB_SYMBOL_UNUSED( Cargo );
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextInlineObject( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QTextInlineObject * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextInlineObject;
   p->type = QT_TYPE_QTextInlineObject;

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
      hb_retnd( ( p )->ascent() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTINLINEOBJECT_ASCENT FP=hb_retnd( ( p )->ascent() ); p is NULL" ) );
   }
}

/*
 * qreal descent () const
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_DESCENT )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      hb_retnd( ( p )->descent() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTINLINEOBJECT_DESCENT FP=hb_retnd( ( p )->descent() ); p is NULL" ) );
   }
}

/*
 * QTextFormat format () const
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_FORMAT )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextFormat( new QTextFormat( ( p )->format() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTINLINEOBJECT_FORMAT FP=hb_retptrGC( hbqt_gcAllocate_QTextFormat( new QTextFormat( ( p )->format() ), true ) ); p is NULL" ) );
   }
}

/*
 * int formatIndex () const
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_FORMATINDEX )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      hb_retni( ( p )->formatIndex() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTINLINEOBJECT_FORMATINDEX FP=hb_retni( ( p )->formatIndex() ); p is NULL" ) );
   }
}

/*
 * qreal height () const
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_HEIGHT )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      hb_retnd( ( p )->height() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTINLINEOBJECT_HEIGHT FP=hb_retnd( ( p )->height() ); p is NULL" ) );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_ISVALID )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTINLINEOBJECT_ISVALID FP=hb_retl( ( p )->isValid() ); p is NULL" ) );
   }
}

/*
 * QRectF rect () const
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_RECT )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->rect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTINLINEOBJECT_RECT FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->rect() ), true ) ); p is NULL" ) );
   }
}

/*
 * void setAscent ( qreal a )
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_SETASCENT )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      ( p )->setAscent( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTINLINEOBJECT_SETASCENT FP=( p )->setAscent( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDescent ( qreal d )
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_SETDESCENT )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      ( p )->setDescent( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTINLINEOBJECT_SETDESCENT FP=( p )->setDescent( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWidth ( qreal w )
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_SETWIDTH )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      ( p )->setWidth( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTINLINEOBJECT_SETWIDTH FP=( p )->setWidth( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * Qt::LayoutDirection textDirection () const
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_TEXTDIRECTION )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      hb_retni( ( Qt::LayoutDirection ) ( p )->textDirection() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTINLINEOBJECT_TEXTDIRECTION FP=hb_retni( ( Qt::LayoutDirection ) ( p )->textDirection() ); p is NULL" ) );
   }
}

/*
 * int textPosition () const
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_TEXTPOSITION )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      hb_retni( ( p )->textPosition() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTINLINEOBJECT_TEXTPOSITION FP=hb_retni( ( p )->textPosition() ); p is NULL" ) );
   }
}

/*
 * qreal width () const
 */
HB_FUNC( QT_QTEXTINLINEOBJECT_WIDTH )
{
   QTextInlineObject * p = hbqt_par_QTextInlineObject( 1 );
   if( p )
      hb_retnd( ( p )->width() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTINLINEOBJECT_WIDTH FP=hb_retnd( ( p )->width() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
