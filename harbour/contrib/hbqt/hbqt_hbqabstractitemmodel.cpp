
/* PLEASE, SOMEONE ADD THE CORRECT HEADER */

/* carlosbacco@gmail.com */

#include "hbqt.h"

#include "hbapiitm.h"
#include "hbvm.h"

#if QT_VERSION >= 0x040500

#include "hbqt_hbqabstractitemmodel.h"

#include <QtGui/QIcon>
#include <QtGui/QWidget>
#include <string>

using namespace std;

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
         vv = hb_itemGetCPtr( ret );
         HB_TRACE( HB_TR_DEBUG, ( "   fetchData[ s = %s ]", hb_itemGetCPtr( ret ) ) );
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
           QGC_POINTER * p = ( QGC_POINTER * ) hb_parptrGC( hbqt_gcFuncs(), -1 );
           if ( p->type == QT_TYPE_QBrush )
            vv = * ( ( QBrush * ) ( p->ph ) );
           else if ( p->type == QT_TYPE_QColor )
            vv = * ( ( QColor * ) ( p->ph ) );
           else if ( p->type == QT_TYPE_QSize )
            vv = * ( ( QSize * ) ( p->ph ) );
           else if ( p->type == QT_TYPE_QIcon )
            vv = * ( ( QIcon * ) ( p->ph ) );
           else if ( p->type == QT_TYPE_QPixmap )
            vv = * ( ( QPixmap * ) ( p->ph ) );
           else if ( p->type == QT_TYPE_QFont )
            vv = * ( ( QFont * ) ( p->ph ) );
      }
      else
      {
         vv = NULL;
      }

      hb_vmRequestRestore();
   }

   return vv;
}

HBQAbstractItemModel::HBQAbstractItemModel( PHB_ITEM pBlock ) : QAbstractItemModel()
{
   if( pBlock)
   {
      block = hb_itemNew( pBlock );
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
      
   QVariant ret = hbqt_fetchData( block, QT_QAIM_flags, 0, index.column(), index.row() );
   if ( ret == NULL )
      return Qt::ItemIsEnabled | Qt::ItemIsSelectable; 

   return Qt::ItemIsEnabled | Qt::ItemIsSelectable; 
   /* TODO: Fix return ret ; */
}

QVariant HBQAbstractItemModel::data( const QModelIndex & index, int role ) const
{
   if( !index.isValid() )
      return QVariant();

   QVariant ret = hbqt_fetchData( block, QT_QAIM_data, role, index.column(), index.row() );
   if ( ret == NULL )
      return QVariant();

   return ret;
}

QVariant HBQAbstractItemModel::headerData( int section, Qt::Orientation orientation, int role ) const
{
   QVariant ret = hbqt_fetchData( block, QT_QAIM_headerData, role, orientation, section );
   if ( ret == NULL )
      return QVariant();

   return ret;
}

int HBQAbstractItemModel::rowCount( const QModelIndex & /*parent = QModelIndex()*/ ) const
{
   QVariant ret = hbqt_fetchData( block, QT_QAIM_rowCount , 0, 0, 0);

   return ret.toInt() ;
}

int HBQAbstractItemModel::columnCount( const QModelIndex & /*parent = QModelIndex()*/ ) const
{
   QVariant ret = hbqt_fetchData( block, QT_QAIM_columnCount , 0, 0, 0);

   return ret.toInt() ;
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
