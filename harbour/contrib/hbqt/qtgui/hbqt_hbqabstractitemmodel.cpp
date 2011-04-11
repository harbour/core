/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * Copyright 2010 Carlos Bacco <carlosbacco at gmail.com>
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

#include "hbqt.h"

#include "hbapiitm.h"
#include "hbvm.h"

#if QT_VERSION >= 0x040500

#include "hbqt_hbqabstractitemmodel.h"

#include <QtGui/QIcon>
#include <QtGui/QFont>
#include <QtGui/QPixmap>
#include <QtGui/QWidget>
#include <QtCore/QFlags>


QVariant hbqt_fetchData( PHB_ITEM block, int type, int role, int par1, int par2 )
{
   QVariant vv;

   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, type );
      PHB_ITEM p1  = hb_itemPutNI( NULL, role );
      PHB_ITEM p2  = hb_itemPutNI( NULL, par1 );
      PHB_ITEM p3  = hb_itemPutNI( NULL, par2 );

      PHB_ITEM ret = hb_vmEvalBlockV( block, 4, p0, p1, p2, p3 );

      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );
      hb_itemRelease( p3 );

      if( hb_itemType( ret ) & HB_IT_STRING )
      {
         void * pText01 = NULL;
         vv = hb_itemGetStrUTF8( ret, &pText01, NULL );
         hb_strfree( pText01 );
         HB_TRACE( HB_TR_DEBUG, ( "   fetchData[ s = %s ]", hb_itemGetCPtr( ret ) ) );
         hb_itemRelease( ret );
      }
      else if( hb_itemType( ret ) & HB_IT_LOGICAL )
      {
         vv = hb_itemGetL( ret );
         HB_TRACE( HB_TR_DEBUG, ( "   fetchData[ l = %i ]", hb_itemGetL( ret ) ) );
      }
      else if( hb_itemType( ret ) & HB_IT_DOUBLE  )
      {
         vv = hb_itemGetND( ret );
         HB_TRACE( HB_TR_DEBUG, ( "   fetchData[ d = %f ]", hb_itemGetND( ret ) ) );
      }
      else if( hb_itemType( ret ) & HB_IT_NUMERIC )
      {
         vv = hb_itemGetNI( ret );
         HB_TRACE( HB_TR_DEBUG, ( "   fetchData[ n = %i ]", hb_itemGetNI( ret ) ) );
      }
      else if( hb_itemType( ret ) & HB_IT_OBJECT )
      {
         hb_vmPushSymbol( hb_dynsymSymbol( hb_dynsymFindName( "PPTR" ) ) );
         hb_vmPush( ret );
         hb_vmSend( 0 );

         HBQT_GC_T * p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), -1 );

         if( p->type == HBQT_TYPE_QBrush )
            vv = * ( ( QBrush * ) ( p->ph ) );
         else if( p->type == HBQT_TYPE_QColor )
            vv = * ( ( QColor * ) ( p->ph ) );
         else if( p->type == HBQT_TYPE_QSize )
            vv = * ( ( QSize * ) ( p->ph ) );
         else if( p->type == HBQT_TYPE_QIcon )
            vv = * ( ( QIcon * ) ( p->ph ) );
         else if( p->type == HBQT_TYPE_QPixmap )
            vv = * ( ( QPixmap * ) ( p->ph ) );
         else if( p->type == HBQT_TYPE_QFont )
            vv = * ( ( QFont * ) ( p->ph ) );

      }
      if( ret )
         hb_itemRelease( ret );

      hb_vmRequestRestore();
   }

   return vv;
}

HBQAbstractItemModel::HBQAbstractItemModel( PHB_ITEM pBlock ) : QAbstractItemModel()
{
   if( pBlock)
   {
      block = hb_itemNew( pBlock );
      hb_gcUnlock( block );
   }
}

HBQAbstractItemModel::~HBQAbstractItemModel( void )
{
   if( block )
   {
      hb_itemRelease( block );
      block = NULL;
   }
}

Qt::ItemFlags HBQAbstractItemModel::flags( const QModelIndex & index ) const
{
   if( ! index.isValid() )
      return 0;

   QVariant ret = hbqt_fetchData( block, HBQT_QAIM_flags, 0, index.column(), index.row() );
   if( ! ret.isValid() )
      return Qt::ItemIsEnabled | Qt::ItemIsSelectable;

   return ( QFlags<Qt::ItemFlag> ) ret.toInt();
}

QVariant HBQAbstractItemModel::data( const QModelIndex & index, int role ) const
{
   if( !index.isValid() )
      return QVariant();

   return hbqt_fetchData( block, HBQT_QAIM_data, role, index.column(), index.row() );
}

QVariant HBQAbstractItemModel::headerData( int section, Qt::Orientation orientation, int role ) const
{
   return hbqt_fetchData( block, HBQT_QAIM_headerData, role, orientation, section );
}

int HBQAbstractItemModel::rowCount( const QModelIndex & /*parent = QModelIndex()*/ ) const
{
   return hbqt_fetchData( block, HBQT_QAIM_rowCount, 0, 0, 0 ).toInt();
}

int HBQAbstractItemModel::columnCount( const QModelIndex & /*parent = QModelIndex()*/ ) const
{
   return hbqt_fetchData( block, HBQT_QAIM_columnCount, 0, 0, 0 ).toInt();
}

QModelIndex HBQAbstractItemModel::index( int row, int column, const QModelIndex & parent ) const
{
   HB_SYMBOL_UNUSED( parent );
   return createIndex( row, column, row * column );
}

QModelIndex HBQAbstractItemModel::parent( const QModelIndex & /* child */ ) const
{
   return QModelIndex();
}

void HBQAbstractItemModel::reset()
{
   QAbstractItemModel::reset();
}

#endif

