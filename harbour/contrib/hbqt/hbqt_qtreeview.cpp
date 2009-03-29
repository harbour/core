/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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



#include <QtGui/QTreeView>



/*
 * QTreeView ( QWidget * parent = 0 )
 * ~QTreeView ()
 */
HB_FUNC( QT_QTREEVIEW )
{
   hb_retptr( ( QTreeView* ) new QTreeView( hbqt_par_QWidget( 1 ) ) );
}


/*
 * bool allColumnsShowFocus () const
 */
HB_FUNC( QT_QTREEVIEW_ALLCOLUMNSSHOWFOCUS )
{
   hb_retl( hbqt_par_QTreeView( 1 )->allColumnsShowFocus(  ) );
}

/*
 * int autoExpandDelay () const
 */
HB_FUNC( QT_QTREEVIEW_AUTOEXPANDDELAY )
{
   hb_retni( hbqt_par_QTreeView( 1 )->autoExpandDelay(  ) );
}

/*
 * int columnAt ( int x ) const
 */
HB_FUNC( QT_QTREEVIEW_COLUMNAT )
{
   hb_retni( hbqt_par_QTreeView( 1 )->columnAt( hb_parni( 1 ) ) );
}

/*
 * int columnViewportPosition ( int column ) const
 */
HB_FUNC( QT_QTREEVIEW_COLUMNVIEWPORTPOSITION )
{
   hb_retni( hbqt_par_QTreeView( 1 )->columnViewportPosition( hb_parni( 1 ) ) );
}

/*
 * int columnWidth ( int column ) const
 */
HB_FUNC( QT_QTREEVIEW_COLUMNWIDTH )
{
   hb_retni( hbqt_par_QTreeView( 1 )->columnWidth( hb_parni( 1 ) ) );
}

/*
 * bool expandsOnDoubleClick () const
 */
HB_FUNC( QT_QTREEVIEW_EXPANDSONDOUBLECLICK )
{
   hb_retl( hbqt_par_QTreeView( 1 )->expandsOnDoubleClick(  ) );
}

/*
 * QHeaderView * header () const
 */
HB_FUNC( QT_QTREEVIEW_HEADER )
{
   hb_retptr( ( QHeaderView* ) hbqt_par_QTreeView( 1 )->header(  ) );
}

/*
 * int indentation () const
 */
HB_FUNC( QT_QTREEVIEW_INDENTATION )
{
   hb_retni( hbqt_par_QTreeView( 1 )->indentation(  ) );
}

/*
 * bool isAnimated () const
 */
HB_FUNC( QT_QTREEVIEW_ISANIMATED )
{
   hb_retl( hbqt_par_QTreeView( 1 )->isAnimated(  ) );
}

/*
 * bool isColumnHidden ( int column ) const
 */
HB_FUNC( QT_QTREEVIEW_ISCOLUMNHIDDEN )
{
   hb_retl( hbqt_par_QTreeView( 1 )->isColumnHidden( hb_parni( 1 ) ) );
}

/*
 * bool isHeaderHidden () const
 */
HB_FUNC( QT_QTREEVIEW_ISHEADERHIDDEN )
{
   hb_retl( hbqt_par_QTreeView( 1 )->isHeaderHidden(  ) );
}

/*
 * bool isSortingEnabled () const
 */
HB_FUNC( QT_QTREEVIEW_ISSORTINGENABLED )
{
   hb_retl( hbqt_par_QTreeView( 1 )->isSortingEnabled(  ) );
}

/*
 * bool itemsExpandable () const
 */
HB_FUNC( QT_QTREEVIEW_ITEMSEXPANDABLE )
{
   hb_retl( hbqt_par_QTreeView( 1 )->itemsExpandable(  ) );
}

/*
 * bool rootIsDecorated () const
 */
HB_FUNC( QT_QTREEVIEW_ROOTISDECORATED )
{
   hb_retl( hbqt_par_QTreeView( 1 )->rootIsDecorated(  ) );
}

/*
 * void setAllColumnsShowFocus ( bool enable )
 */
HB_FUNC( QT_QTREEVIEW_SETALLCOLUMNSSHOWFOCUS )
{
   hbqt_par_QTreeView( 1 )->setAllColumnsShowFocus( hb_parl( 1 ) );
}

/*
 * void setAnimated ( bool enable )
 */
HB_FUNC( QT_QTREEVIEW_SETANIMATED )
{
   hbqt_par_QTreeView( 1 )->setAnimated( hb_parl( 1 ) );
}

/*
 * void setAutoExpandDelay ( int delay )
 */
HB_FUNC( QT_QTREEVIEW_SETAUTOEXPANDDELAY )
{
   hbqt_par_QTreeView( 1 )->setAutoExpandDelay( hb_parni( 1 ) );
}

/*
 * void setColumnHidden ( int column, bool hide )
 */
HB_FUNC( QT_QTREEVIEW_SETCOLUMNHIDDEN )
{
   hbqt_par_QTreeView( 1 )->setColumnHidden( hb_parni( 1 ), hb_parl( 2 ) );
}

/*
 * void setColumnWidth ( int column, int width )
 */
HB_FUNC( QT_QTREEVIEW_SETCOLUMNWIDTH )
{
   hbqt_par_QTreeView( 1 )->setColumnWidth( hb_parni( 1 ), hb_parni( 2 ) );
}

/*
 * void setExpandsOnDoubleClick ( bool enable )
 */
HB_FUNC( QT_QTREEVIEW_SETEXPANDSONDOUBLECLICK )
{
   hbqt_par_QTreeView( 1 )->setExpandsOnDoubleClick( hb_parl( 1 ) );
}

/*
 * void setHeader ( QHeaderView * header )
 */
HB_FUNC( QT_QTREEVIEW_SETHEADER )
{
   hbqt_par_QTreeView( 1 )->setHeader( hbqt_par_QHeaderView( 1 ) );
}

/*
 * void setHeaderHidden ( bool hide )
 */
HB_FUNC( QT_QTREEVIEW_SETHEADERHIDDEN )
{
   hbqt_par_QTreeView( 1 )->setHeaderHidden( hb_parl( 1 ) );
}

/*
 * void setIndentation ( int i )
 */
HB_FUNC( QT_QTREEVIEW_SETINDENTATION )
{
   hbqt_par_QTreeView( 1 )->setIndentation( hb_parni( 1 ) );
}

/*
 * void setItemsExpandable ( bool enable )
 */
HB_FUNC( QT_QTREEVIEW_SETITEMSEXPANDABLE )
{
   hbqt_par_QTreeView( 1 )->setItemsExpandable( hb_parl( 1 ) );
}

/*
 * void setRootIsDecorated ( bool show )
 */
HB_FUNC( QT_QTREEVIEW_SETROOTISDECORATED )
{
   hbqt_par_QTreeView( 1 )->setRootIsDecorated( hb_parl( 1 ) );
}

/*
 * void setSortingEnabled ( bool enable )
 */
HB_FUNC( QT_QTREEVIEW_SETSORTINGENABLED )
{
   hbqt_par_QTreeView( 1 )->setSortingEnabled( hb_parl( 1 ) );
}

/*
 * void setUniformRowHeights ( bool uniform )
 */
HB_FUNC( QT_QTREEVIEW_SETUNIFORMROWHEIGHTS )
{
   hbqt_par_QTreeView( 1 )->setUniformRowHeights( hb_parl( 1 ) );
}

/*
 * void setWordWrap ( bool on )
 */
HB_FUNC( QT_QTREEVIEW_SETWORDWRAP )
{
   hbqt_par_QTreeView( 1 )->setWordWrap( hb_parl( 1 ) );
}

/*
 * void sortByColumn ( int column, Qt::SortOrder order )
 */
HB_FUNC( QT_QTREEVIEW_SORTBYCOLUMN )
{
   hbqt_par_QTreeView( 1 )->sortByColumn( hb_parni( 1 ), ( Qt::SortOrder ) hb_parni( 2 ) );
}

/*
 * bool uniformRowHeights () const
 */
HB_FUNC( QT_QTREEVIEW_UNIFORMROWHEIGHTS )
{
   hb_retl( hbqt_par_QTreeView( 1 )->uniformRowHeights(  ) );
}

/*
 * bool wordWrap () const
 */
HB_FUNC( QT_QTREEVIEW_WORDWRAP )
{
   hb_retl( hbqt_par_QTreeView( 1 )->wordWrap(  ) );
}



#endif
/*----------------------------------------------------------------------*/

