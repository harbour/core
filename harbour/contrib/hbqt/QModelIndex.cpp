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


#include <QtCore/QModelIndex>


/*
 * QModelIndex ()
 * QModelIndex ( const QModelIndex & other )
 * ~QModelIndex ()
 */
HB_FUNC( QT_QMODELINDEX )
{
   hb_retptr( ( QModelIndex * ) new QModelIndex() );
}

/*
 * DESTRUCTOR
 */
HB_FUNC( QT_QMODELINDEX_DESTROY )
{
   hbqt_par_QModelIndex( 1 )->~QModelIndex();
}

/*
 * QModelIndex child ( int row, int column ) const
 */
HB_FUNC( QT_QMODELINDEX_CHILD )
{
   hb_retptr( new QModelIndex( hbqt_par_QModelIndex( 1 )->child( hb_parni( 2 ), hb_parni( 3 ) ) ) );
}

/*
 * int column () const
 */
HB_FUNC( QT_QMODELINDEX_COLUMN )
{
   hb_retni( hbqt_par_QModelIndex( 1 )->column() );
}

/*
 * QVariant data ( int role = Qt::DisplayRole ) const
 */
HB_FUNC( QT_QMODELINDEX_DATA )
{
   hb_retptr( new QVariant( hbqt_par_QModelIndex( 1 )->data( ( HB_ISNUM( 2 ) ? hb_parni( 2 ) : Qt::DisplayRole ) ) ) );
}

/*
 * Qt::ItemFlags flags () const
 */
HB_FUNC( QT_QMODELINDEX_FLAGS )
{
   hb_retni( ( Qt::ItemFlags ) hbqt_par_QModelIndex( 1 )->flags() );
}

/*
 * qint64 internalId () const
 */
HB_FUNC( QT_QMODELINDEX_INTERNALID )
{
   hb_retnint( hbqt_par_QModelIndex( 1 )->internalId() );
}

/*
 * void * internalPointer () const
 */
HB_FUNC( QT_QMODELINDEX_INTERNALPOINTER )
{
   hbqt_par_QModelIndex( 1 )->internalPointer();
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QMODELINDEX_ISVALID )
{
   hb_retl( hbqt_par_QModelIndex( 1 )->isValid() );
}

/*
 * const QAbstractItemModel * model () const
 */
HB_FUNC( QT_QMODELINDEX_MODEL )
{
   hb_retptr( ( QAbstractItemModel* ) hbqt_par_QModelIndex( 1 )->model() );
}

/*
 * QModelIndex parent () const
 */
HB_FUNC( QT_QMODELINDEX_PARENT )
{
   hb_retptr( new QModelIndex( hbqt_par_QModelIndex( 1 )->parent() ) );
}

/*
 * int row () const
 */
HB_FUNC( QT_QMODELINDEX_ROW )
{
   hb_retni( hbqt_par_QModelIndex( 1 )->row() );
}

/*
 * QModelIndex sibling ( int row, int column ) const
 */
HB_FUNC( QT_QMODELINDEX_SIBLING )
{
   hb_retptr( new QModelIndex( hbqt_par_QModelIndex( 1 )->sibling( hb_parni( 2 ), hb_parni( 3 ) ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

