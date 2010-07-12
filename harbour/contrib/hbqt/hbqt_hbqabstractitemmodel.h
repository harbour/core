
#ifndef HBQT_HBQAbstractItemModel_H
#define HBQT_HBQAbstractItemModel_H

#include "hbapi.h"

/*----------------------------------------------------------------------*/

#include <QtCore/QAbstractItemModel>
#include <QtCore/QPointer>

class HBQAbstractItemModel : public QAbstractItemModel
{
   Q_OBJECT

public:
   HBQAbstractItemModel( PHB_ITEM pBlock );
   virtual ~HBQAbstractItemModel( void );

   PHB_ITEM block;

   Qt::ItemFlags flags( const QModelIndex & index ) const;
   QVariant      data( const QModelIndex & index, int role = Qt::DisplayRole ) const;
   QVariant      headerData( int section, Qt::Orientation orientation, int role = Qt::DisplayRole ) const;
   int           rowCount( const QModelIndex & parent = QModelIndex() ) const;
   int           columnCount( const QModelIndex & parent = QModelIndex() ) const;
   QModelIndex   index(int row, int column, const QModelIndex &parent = QModelIndex()) const;
   QModelIndex   parent(const QModelIndex &child) const;
   void          reset();
};

/*----------------------------------------------------------------------*/

#endif
