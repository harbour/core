/*
 * $Id$
 */

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
#include "hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 21/23 [ 91.30% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  void setSizes ( const QList<int> & list )
 *  QList<int> sizes () const
 */


#include <QtGui/QSplitter>


/*
 * QSplitter ( QWidget * parent = 0 )
 * QSplitter ( Qt::Orientation orientation, QWidget * parent = 0 )
 * ~QSplitter ()
 */
HB_FUNC( QT_QSPLITTER )
{
   if( hb_pcount() >= 1 && HB_ISNUM( 1 ) )
      hb_retptr( ( QSplitter* ) new QSplitter( ( Qt::Orientation ) hb_parni( 1 ), hbqt_par_QWidget( 2 ) ) );
   else
      hb_retptr( ( QSplitter* ) new QSplitter( hbqt_par_QWidget( 1 ) ) );
}

/*
 * DESTRUCTOR
 */
HB_FUNC( QT_QSPLITTER_DESTROY )
{
   hbqt_par_QSplitter( 1 )->~QSplitter();
}

/*
 * void addWidget ( QWidget * widget )
 */
HB_FUNC( QT_QSPLITTER_ADDWIDGET )
{
   hbqt_par_QSplitter( 1 )->addWidget( hbqt_par_QWidget( 2 ) );
}

/*
 * bool childrenCollapsible () const
 */
HB_FUNC( QT_QSPLITTER_CHILDRENCOLLAPSIBLE )
{
   hb_retl( hbqt_par_QSplitter( 1 )->childrenCollapsible() );
}

/*
 * int count () const
 */
HB_FUNC( QT_QSPLITTER_COUNT )
{
   hb_retni( hbqt_par_QSplitter( 1 )->count() );
}

/*
 * void getRange ( int index, int * min, int * max ) const
 */
HB_FUNC( QT_QSPLITTER_GETRANGE )
{
   int iMin = 0;
   int iMax = 0;

   hbqt_par_QSplitter( 1 )->getRange( hb_parni( 2 ), &iMin, &iMax );

   hb_storni( iMin, 3 );
   hb_storni( iMax, 4 );
}

/*
 * QSplitterHandle * handle ( int index ) const
 */
HB_FUNC( QT_QSPLITTER_HANDLE )
{
   hb_retptr( ( QSplitterHandle* ) hbqt_par_QSplitter( 1 )->handle( hb_parni( 2 ) ) );
}

/*
 * int handleWidth () const
 */
HB_FUNC( QT_QSPLITTER_HANDLEWIDTH )
{
   hb_retni( hbqt_par_QSplitter( 1 )->handleWidth() );
}

/*
 * int indexOf ( QWidget * widget ) const
 */
HB_FUNC( QT_QSPLITTER_INDEXOF )
{
   hb_retni( hbqt_par_QSplitter( 1 )->indexOf( hbqt_par_QWidget( 2 ) ) );
}

/*
 * void insertWidget ( int index, QWidget * widget )
 */
HB_FUNC( QT_QSPLITTER_INSERTWIDGET )
{
   hbqt_par_QSplitter( 1 )->insertWidget( hb_parni( 2 ), hbqt_par_QWidget( 3 ) );
}

/*
 * bool isCollapsible ( int index ) const
 */
HB_FUNC( QT_QSPLITTER_ISCOLLAPSIBLE )
{
   hb_retl( hbqt_par_QSplitter( 1 )->isCollapsible( hb_parni( 2 ) ) );
}

/*
 * bool opaqueResize () const
 */
HB_FUNC( QT_QSPLITTER_OPAQUERESIZE )
{
   hb_retl( hbqt_par_QSplitter( 1 )->opaqueResize() );
}

/*
 * Qt::Orientation orientation () const
 */
HB_FUNC( QT_QSPLITTER_ORIENTATION )
{
   hb_retni( ( Qt::Orientation ) hbqt_par_QSplitter( 1 )->orientation() );
}

/*
 * void refresh ()
 */
HB_FUNC( QT_QSPLITTER_REFRESH )
{
   hbqt_par_QSplitter( 1 )->refresh();
}

/*
 * bool restoreState ( const QByteArray & state )
 */
HB_FUNC( QT_QSPLITTER_RESTORESTATE )
{
   hb_retl( hbqt_par_QSplitter( 1 )->restoreState( *hbqt_par_QByteArray( 2 ) ) );
}

/*
 * QByteArray saveState () const
 */
HB_FUNC( QT_QSPLITTER_SAVESTATE )
{
   hb_retptr( new QByteArray( hbqt_par_QSplitter( 1 )->saveState() ) );
}

/*
 * void setChildrenCollapsible ( bool )
 */
HB_FUNC( QT_QSPLITTER_SETCHILDRENCOLLAPSIBLE )
{
   hbqt_par_QSplitter( 1 )->setChildrenCollapsible( hb_parl( 2 ) );
}

/*
 * void setCollapsible ( int index, bool collapse )
 */
HB_FUNC( QT_QSPLITTER_SETCOLLAPSIBLE )
{
   hbqt_par_QSplitter( 1 )->setCollapsible( hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setHandleWidth ( int )
 */
HB_FUNC( QT_QSPLITTER_SETHANDLEWIDTH )
{
   hbqt_par_QSplitter( 1 )->setHandleWidth( hb_parni( 2 ) );
}

/*
 * void setOpaqueResize ( bool opaque = true )
 */
HB_FUNC( QT_QSPLITTER_SETOPAQUERESIZE )
{
   hbqt_par_QSplitter( 1 )->setOpaqueResize( hb_parl( 2 ) );
}

/*
 * void setOrientation ( Qt::Orientation )
 */
HB_FUNC( QT_QSPLITTER_SETORIENTATION )
{
   hbqt_par_QSplitter( 1 )->setOrientation( ( Qt::Orientation ) hb_parni( 2 ) );
}

/*
 * void setStretchFactor ( int index, int stretch )
 */
HB_FUNC( QT_QSPLITTER_SETSTRETCHFACTOR )
{
   hbqt_par_QSplitter( 1 )->setStretchFactor( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * QWidget * widget ( int index ) const
 */
HB_FUNC( QT_QSPLITTER_WIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QSplitter( 1 )->widget( hb_parni( 2 ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
