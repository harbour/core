/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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

#include "hbapiitm.h"
#include "hbvm.h"

#if QT_VERSION >= 0x040500

#include "hbqt_hbdbfmodel.h"

#include <QtGui/QIcon>
#include <QtGui/QWidget>

#define HBQT_BRW_CELLVALUE                        1001

#define HBQT_BRW_COLCOUNT                         1002
#define HBQT_BRW_ROWCOUNT                         1003

#define HBQT_BRW_COLHEADER                        1004
#define HBQT_BRW_COLALIGN                         1006
#define HBQT_BRW_COLFGCOLOR                       1007
#define HBQT_BRW_COLBGCOLOR                       1008
#define HBQT_BRW_COLHEIGHT                        1011

#define HBQT_BRW_ROWHEADER                        1005
#define HBQT_BRW_DATFGCOLOR                       1009
#define HBQT_BRW_DATBGCOLOR                       1010
#define HBQT_BRW_DATHEIGHT                        1012
#define HBQT_BRW_DATALIGN                         1013
#define HBQT_BRW_CELLDECORATION                   1014

static QVariant hbqt_fetchRole( PHB_ITEM block, int what, int par1, int par2 )
{
   QVariant vv;

   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, what );
      PHB_ITEM p1  = hb_itemPutNI( NULL, par1 );
      PHB_ITEM p2  = hb_itemPutNI( NULL, par2 );

      PHB_ITEM ret = hb_vmEvalBlockV( block, 3, p0, p1, p2 );

      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );

      if( hb_itemType( ret ) & HB_IT_STRING )
      {
         vv = hb_itemGetCPtr( ret );
         HB_TRACE( HB_TR_DEBUG, ( "   fetchRole[ s = %s ]", hb_itemGetCPtr( ret ) ) );
      }
      else if( hb_itemType( ret ) & HB_IT_LOGICAL )
      {
         vv = hb_itemGetL( ret );
         HB_TRACE( HB_TR_DEBUG, ( "   fetchRole[ l = %i ]", hb_itemGetL( ret ) ) );
      }
      else if( hb_itemType( ret ) & HB_IT_DOUBLE  )
      {
         vv = hb_itemGetND( ret );
         HB_TRACE( HB_TR_DEBUG, ( "   fetchRole[ d = %f ]", hb_itemGetND( ret ) ) );
      }
      else if( hb_itemType( ret ) & HB_IT_NUMERIC )
      {
         vv = hb_itemGetNI( ret );
         HB_TRACE( HB_TR_DEBUG, ( "   fetchRole[ n = %i ]", hb_itemGetNI( ret ) ) );
      }
      #if 0
      else if( hb_itemType( ret ) & HB_IT_OBJECT )
      {
         hb_vmPushSymbol( hb_dynsymSymbol( hb_dynsymFindName( "PPTR" ) ) );
         hb_vmPush( ret );
         hb_vmSend( 0 );
         QGC_POINTER * p = ( QGC_POINTER * ) hb_parptrGC( hbqt_gcFuncs(), -1 );

         vv = qvariant_cast<QColor *>( * ( ( QColor * ) ( p->ph ) ) );
      }
      #endif
      else
      {
         vv = QVariant();
      }

      hb_vmRequestRestore();
   }

   return vv;
}

HBDbfModel::HBDbfModel( PHB_ITEM pBlock ) : QAbstractItemModel()
{
   block = hb_itemNew( pBlock );
   iRows = 0;
   iCols = 0;
}

HBDbfModel::~HBDbfModel( void )
{
   if( block )
   {
      hb_itemRelease( block );
      block = NULL;
   }
}

Qt::ItemFlags HBDbfModel::flags( const QModelIndex & index ) const
{
   if( ! index.isValid() )
      return 0;

   return Qt::ItemIsEnabled | Qt::ItemIsSelectable;
}

QVariant HBDbfModel::data( const QModelIndex & index, int role ) const
{
   HB_TRACE( HB_TR_DEBUG, ( "HBDbfModel::data( row=%i col=%i role=%i )", index.row(), index.column(), role ) );

   if( !index.isValid() )
      return QVariant();

   switch( role )
   {
      case Qt::SizeHintRole:
      {
         int iHeight = hbqt_fetchRole( block, HBQT_BRW_DATHEIGHT, index.row()+1, index.column()+1 ).toInt();
         return QSize( 20, iHeight );
      }
      case Qt::TextAlignmentRole:
      {
         return hbqt_fetchRole( block, HBQT_BRW_DATALIGN, index.row()+1, index.column()+1 );
      }
      case Qt::BackgroundRole:
      {
         int iClr = hbqt_fetchRole( block, HBQT_BRW_DATBGCOLOR, index.row()+1, index.column()+1 ).toInt();
         if( iClr < 25 )
            return QColor( ( Qt::GlobalColor ) iClr );
         else
            return QColor( ( QRgb ) iClr );
      }
      case Qt::ForegroundRole:
      {
         int iClr = hbqt_fetchRole( block, HBQT_BRW_DATFGCOLOR, index.row()+1, index.column()+1 ).toInt();
         if( iClr < 25 )
            return QColor( ( Qt::GlobalColor ) iClr );
         else
            return QColor( ( QRgb ) iClr );
      }
      case Qt::DecorationRole:
      {
         QVariant image = hbqt_fetchRole( block, HBQT_BRW_CELLDECORATION, index.row()+1, index.column()+1 );
         if( image.toString() == ( QString ) "" )
            return QVariant();
         return QIcon( image.toString() );
         //return QPixmap( image.toString() );
      }
      case Qt::DisplayRole:
      {
         return hbqt_fetchRole( block, HBQT_BRW_CELLVALUE, index.row()+1, index.column()+1 );
      }
   }
   return QVariant();
}

QVariant HBDbfModel::headerData( int section, Qt::Orientation orientation, int role ) const
{
   HB_TRACE( HB_TR_DEBUG, ( "HBDbfModel::headerData( section=%i orient=%i role=%i )", section, orientation, role ) );

   if( orientation == Qt::Horizontal )
   {
      switch( role )
      {
         case Qt::TextAlignmentRole:
         {
            return hbqt_fetchRole( block, HBQT_BRW_COLALIGN, 0, section+1 ).toInt();
         }
         case Qt::SizeHintRole:
         {
            int iHeight = hbqt_fetchRole( block, HBQT_BRW_COLHEIGHT, 0, section+1 ).toInt();
            return QSize( 20, iHeight );
         }
         case Qt::BackgroundRole:
         {
            int iClr = hbqt_fetchRole( block, HBQT_BRW_COLBGCOLOR, 0, section+1 ).toInt();
            if( iClr < 25 )
               return QColor( ( Qt::GlobalColor ) iClr );
            else
               return QColor( ( QRgb ) iClr );
         }
         case Qt::ForegroundRole:
         {
            int iClr = hbqt_fetchRole( block, HBQT_BRW_COLFGCOLOR, 0, section+1 ).toInt();
            if( iClr < 25 )
               return QColor( ( Qt::GlobalColor ) iClr );
            else
               return QColor( ( QRgb ) iClr );
         }
         case Qt::DisplayRole:
         {
            return hbqt_fetchRole( block, HBQT_BRW_COLHEADER, 0, section+1 );
         }
         case Qt::FontRole:
         case Qt::DecorationRole:
            break;
      }
   }

   if( orientation == Qt::Vertical && role == Qt::DisplayRole )
   {
      return section + 1;
   }

   return QVariant();
}

int HBDbfModel::rowCount( const QModelIndex & /*parent = QModelIndex()*/ ) const
{
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0 = hb_itemPutNI( NULL, HBQT_BRW_ROWCOUNT );

      int result = hb_itemGetNI( hb_vmEvalBlockV( block, 1, p0 ) );

      hb_itemRelease( p0 );

      hb_vmRequestRestore();
      return result;
   }
   else
      return 0;
}

int HBDbfModel::columnCount( const QModelIndex & /*parent = QModelIndex()*/ ) const
{
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0 = hb_itemPutNI( NULL, HBQT_BRW_COLCOUNT );

      int result = hb_itemGetNI( hb_vmEvalBlockV( block, 1, p0 ) );

      hb_itemRelease( p0 );

      hb_vmRequestRestore();
      return result;
   }
   else
      return 0;
}

QModelIndex HBDbfModel::index( int row, int column, const QModelIndex & parent ) const
{
   HB_SYMBOL_UNUSED( parent );
   return createIndex( row, column, row * column );
}

QModelIndex HBDbfModel::parent( const QModelIndex & /* child */ ) const
{
   return QModelIndex();
}

void HBDbfModel::reset()
{
   QAbstractItemModel::reset();
}

void HBDbfModel::hbSetRowColumns( int rows, int cols )
{
   iRows = rows;
   iCols = cols;
}

#endif
