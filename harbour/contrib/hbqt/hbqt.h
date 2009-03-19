/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
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

#ifndef __HBQT_H
#define __HBQT_H

#include <Qt/qglobal.h>

#if QT_VERSION >= 0x040500

#define hbqt_par_QWidget( n )                ( ( QWidget* ) hb_parptr( n ) )
#define hbqt_par_QDialog( n )                ( ( QDialog* ) hb_parptr( n ) )
#define hbqt_par_QAbstractButton( n )        ( ( QAbstractButton* ) hb_parptr( n ) )
#define hbqt_par_QAbstractItemView( n )      ( ( QAbstractItemView* ) hb_parptr( n ) )
#define hbqt_par_QAbstractItemDelegate( n )  ( ( QAbstractItemDelegate* ) hb_parptr( n ) )
#define hbqt_par_QAbstractPrintDialog( n )   ( ( QAbstractPrintDialog* ) hb_parptr( n ) )
#define hbqt_par_QAction( n )                ( ( QAction* ) hb_parptr( n ) )
#define hbqt_par_QWindowSurface( n )         ( ( QWindowSurface* ) hb_parptr( n ) )
#define hbqt_par_QStyle( n )                 ( ( QStyle* ) hb_parptr( n ) )
#define hbqt_par_QLayout( n )                ( ( QLayout* ) hb_parptr( n ) )
#define hbqt_par_QInputContext( n )          ( ( QInputContext* ) hb_parptr( n ) )

#define hbqt_par_QString( n )                ( ( QString ) hb_parc( n ) )
#define hbqt_par_Bool( n )                   ( hb_parl( n ) )

#define hbqt_par_WindowFlags( n )            ( ( Qt::WindowFlags ) hb_parni( n ) )

#define hbqt_ret_QWidget( p )                ( hb_retptr( ( QWidget* ) p ) )
#define hbqt_ret_QAbstractItemDelegate( p )  ( hb_retptr( ( QAbstractItemDelegate* ) p ) )
#define hbqt_ret_QAbstractItemModel( p )     ( hb_retptr( ( QAbstractItemModel* ) p ) )
#define hbqt_ret_QPrinter( p )               ( hb_retptr( ( QPrinter* ) p ) )

#endif

#endif /* __HBQT_H */
