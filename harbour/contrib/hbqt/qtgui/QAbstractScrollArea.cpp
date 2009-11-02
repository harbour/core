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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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

#include <QtCore/QPointer>

#include <QtGui/QAbstractScrollArea>


/*
 * QAbstractScrollArea ( QWidget * parent = 0 )
 * ~QAbstractScrollArea ()
 */

QT_G_FUNC( release_QAbstractScrollArea )
{
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "release_QAbstractScrollArea         %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      const QMetaObject * m = ( ( QObject * ) ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         ( ( QAbstractScrollArea * ) ph )->~QAbstractScrollArea();
         ph = NULL;
      }
      else
      {
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "  Object Name Missing: QAbstractScrollArea" );  OutputDebugString( str );
#endif
      }
   }
   else
   {
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "! ph____QAbstractScrollArea" );  OutputDebugString( str );
#endif
   }
}

HB_FUNC( QT_QABSTRACTSCROLLAREA )
{
}
/*
 * void addScrollBarWidget ( QWidget * widget, Qt::Alignment alignment )
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_ADDSCROLLBARWIDGET )
{
   hbqt_par_QAbstractScrollArea( 1 )->addScrollBarWidget( hbqt_par_QWidget( 2 ), ( Qt::Alignment ) hb_parni( 3 ) );
}

/*
 * QWidget * cornerWidget () const
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_CORNERWIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QAbstractScrollArea( 1 )->cornerWidget() );
}

/*
 * QScrollBar * horizontalScrollBar () const
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_HORIZONTALSCROLLBAR )
{
   hb_retptr( ( QScrollBar* ) hbqt_par_QAbstractScrollArea( 1 )->horizontalScrollBar() );
}

/*
 * Qt::ScrollBarPolicy horizontalScrollBarPolicy () const
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_HORIZONTALSCROLLBARPOLICY )
{
   hb_retni( ( Qt::ScrollBarPolicy ) hbqt_par_QAbstractScrollArea( 1 )->horizontalScrollBarPolicy() );
}

/*
 * QSize maximumViewportSize () const
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_MAXIMUMVIEWPORTSIZE )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QSize( hbqt_par_QAbstractScrollArea( 1 )->maximumViewportSize() ), release_QSize ) );
}

/*
 * void setCornerWidget ( QWidget * widget )
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_SETCORNERWIDGET )
{
   hbqt_par_QAbstractScrollArea( 1 )->setCornerWidget( hbqt_par_QWidget( 2 ) );
}

/*
 * void setHorizontalScrollBar ( QScrollBar * scrollBar )
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_SETHORIZONTALSCROLLBAR )
{
   hbqt_par_QAbstractScrollArea( 1 )->setHorizontalScrollBar( hbqt_par_QScrollBar( 2 ) );
}

/*
 * void setHorizontalScrollBarPolicy ( Qt::ScrollBarPolicy )
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_SETHORIZONTALSCROLLBARPOLICY )
{
   hbqt_par_QAbstractScrollArea( 1 )->setHorizontalScrollBarPolicy( ( Qt::ScrollBarPolicy ) hb_parni( 2 ) );
}

/*
 * void setVerticalScrollBar ( QScrollBar * scrollBar )
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_SETVERTICALSCROLLBAR )
{
   hbqt_par_QAbstractScrollArea( 1 )->setVerticalScrollBar( hbqt_par_QScrollBar( 2 ) );
}

/*
 * void setVerticalScrollBarPolicy ( Qt::ScrollBarPolicy )
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_SETVERTICALSCROLLBARPOLICY )
{
   hbqt_par_QAbstractScrollArea( 1 )->setVerticalScrollBarPolicy( ( Qt::ScrollBarPolicy ) hb_parni( 2 ) );
}

/*
 * void setViewport ( QWidget * widget )
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_SETVIEWPORT )
{
   hbqt_par_QAbstractScrollArea( 1 )->setViewport( hbqt_par_QWidget( 2 ) );
}

/*
 * QScrollBar * verticalScrollBar () const
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_VERTICALSCROLLBAR )
{
   hb_retptr( ( QScrollBar* ) hbqt_par_QAbstractScrollArea( 1 )->verticalScrollBar() );
}

/*
 * Qt::ScrollBarPolicy verticalScrollBarPolicy () const
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_VERTICALSCROLLBARPOLICY )
{
   hb_retni( ( Qt::ScrollBarPolicy ) hbqt_par_QAbstractScrollArea( 1 )->verticalScrollBarPolicy() );
}

/*
 * QWidget * viewport () const
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_VIEWPORT )
{
   hb_retptr( ( QWidget* ) hbqt_par_QAbstractScrollArea( 1 )->viewport() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
