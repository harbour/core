/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */

#include "hbqtcore.h"
#include "hbqtgui.h"

#if QT_VERSION >= 0x040500

/*
 *  Constructed[ 28/32 [ 87.50% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  void appendColumn ( const QList<QStandardItem *> & items )
 *  void appendRow ( const QList<QStandardItem *> & items )
 *  void insertColumn ( int column, const QList<QStandardItem *> & items )
 *  void insertRow ( int row, const QList<QStandardItem *> & items )
 *
 *  *** Commented out protostypes ***
 *
 *  // const QStandardItem * itemPrototype () const
 */

#include <QtCore/QPointer>

#include <QtGui/QStandardItemModel>


/*
 * QStandardItemModel ( QObject * parent = 0 )
 * QStandardItemModel ( int rows, int columns, QObject * parent = 0 )
 * ~QStandardItemModel ()
 */

typedef struct
{
   QPointer< QStandardItemModel > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStandardItemModel;

HBQT_GC_FUNC( hbqt_gcRelease_QStandardItemModel )
{
   HBQT_GC_T_QStandardItemModel * p = ( HBQT_GC_T_QStandardItemModel * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QStandardItemModel * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStandardItemModel( void * pObj, bool bNew )
{
   HBQT_GC_T_QStandardItemModel * p = ( HBQT_GC_T_QStandardItemModel * ) hb_gcAllocate( sizeof( HBQT_GC_T_QStandardItemModel ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QStandardItemModel >( ( QStandardItemModel * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStandardItemModel;
   p->type = HBQT_TYPE_QStandardItemModel;

   return p;
}

HB_FUNC( QT_QSTANDARDITEMMODEL )
{
   QStandardItemModel * pObj = NULL;

   pObj = new QStandardItemModel( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QStandardItemModel( ( void * ) pObj, true ) );
}

/* void appendRow ( QStandardItem * item ) */
HB_FUNC( QT_QSTANDARDITEMMODEL_APPENDROW )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->appendRow( hbqt_par_QStandardItem( 2 ) );
}

/* void clear () */
HB_FUNC( QT_QSTANDARDITEMMODEL_CLEAR )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->clear();
}

/* QList<QStandardItem *> findItems ( const QString & text, Qt::MatchFlags flags = Qt::MatchExactly, int column = 0 ) const */
HB_FUNC( QT_QSTANDARDITEMMODEL_FINDITEMS )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QStandardItem *>( ( p )->findItems( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISNUM( 3 ) ? ( Qt::MatchFlags ) hb_parni( 3 ) : ( Qt::MatchFlags ) Qt::MatchExactly ), hb_parni( 4 ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* QStandardItem * horizontalHeaderItem ( int column ) const */
HB_FUNC( QT_QSTANDARDITEMMODEL_HORIZONTALHEADERITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->horizontalHeaderItem( hb_parni( 2 ) ), false ) );
}

/* QModelIndex indexFromItem ( const QStandardItem * item ) const */
HB_FUNC( QT_QSTANDARDITEMMODEL_INDEXFROMITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->indexFromItem( hbqt_par_QStandardItem( 2 ) ) ), true ) );
}

/* bool insertColumn ( int column, const QModelIndex & parent = QModelIndex() ) */
HB_FUNC( QT_QSTANDARDITEMMODEL_INSERTCOLUMN )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retl( ( p )->insertColumn( hb_parni( 2 ), ( HB_ISOBJECT( 3 ) ? *hbqt_par_QModelIndex( 3 ) : QModelIndex() ) ) );
}

/* bool insertRow ( int row, const QModelIndex & parent = QModelIndex() ) */
HB_FUNC( QT_QSTANDARDITEMMODEL_INSERTROW )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retl( ( p )->insertRow( hb_parni( 2 ), ( HB_ISOBJECT( 3 ) ? *hbqt_par_QModelIndex( 3 ) : QModelIndex() ) ) );
}

/* void insertRow ( int row, QStandardItem * item ) */
HB_FUNC( QT_QSTANDARDITEMMODEL_INSERTROW_1 )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->insertRow( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) );
}

/* QStandardItem * invisibleRootItem () const */
HB_FUNC( QT_QSTANDARDITEMMODEL_INVISIBLEROOTITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->invisibleRootItem(), false ) );
}

/* QStandardItem * item ( int row, int column = 0 ) const */
HB_FUNC( QT_QSTANDARDITEMMODEL_ITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->item( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
}

/* QStandardItem * itemFromIndex ( const QModelIndex & index ) const */
HB_FUNC( QT_QSTANDARDITEMMODEL_ITEMFROMINDEX )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->itemFromIndex( *hbqt_par_QModelIndex( 2 ) ), false ) );
}

/* void setColumnCount ( int columns ) */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETCOLUMNCOUNT )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->setColumnCount( hb_parni( 2 ) );
}

/* void setHorizontalHeaderItem ( int column, QStandardItem * item ) */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETHORIZONTALHEADERITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->setHorizontalHeaderItem( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) );
}

/* void setHorizontalHeaderLabels ( const QStringList & labels ) */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETHORIZONTALHEADERLABELS )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->setHorizontalHeaderLabels( *hbqt_par_QStringList( 2 ) );
}

/* void setItem ( int row, int column, QStandardItem * item ) */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->setItem( hb_parni( 2 ), hb_parni( 3 ), hbqt_par_QStandardItem( 4 ) );
}

/* void setItem ( int row, QStandardItem * item ) */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETITEM_1 )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->setItem( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) );
}

/* void setItemPrototype ( const QStandardItem * item ) */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETITEMPROTOTYPE )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->setItemPrototype( hbqt_par_QStandardItem( 2 ) );
}

/* void setRowCount ( int rows ) */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETROWCOUNT )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->setRowCount( hb_parni( 2 ) );
}

/* void setSortRole ( int role ) */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETSORTROLE )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->setSortRole( hb_parni( 2 ) );
}

/* void setVerticalHeaderItem ( int row, QStandardItem * item ) */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETVERTICALHEADERITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->setVerticalHeaderItem( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) );
}

/* void setVerticalHeaderLabels ( const QStringList & labels ) */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETVERTICALHEADERLABELS )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->setVerticalHeaderLabels( *hbqt_par_QStringList( 2 ) );
}

/* int sortRole () const */
HB_FUNC( QT_QSTANDARDITEMMODEL_SORTROLE )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retni( ( p )->sortRole() );
}

/* QList<QStandardItem *> takeColumn ( int column ) */
HB_FUNC( QT_QSTANDARDITEMMODEL_TAKECOLUMN )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QStandardItem *>( ( p )->takeColumn( hb_parni( 2 ) ) ), true ) );
}

/* QStandardItem * takeHorizontalHeaderItem ( int column ) */
HB_FUNC( QT_QSTANDARDITEMMODEL_TAKEHORIZONTALHEADERITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->takeHorizontalHeaderItem( hb_parni( 2 ) ), false ) );
}

/* QStandardItem * takeItem ( int row, int column = 0 ) */
HB_FUNC( QT_QSTANDARDITEMMODEL_TAKEITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->takeItem( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
}

/* QList<QStandardItem *> takeRow ( int row ) */
HB_FUNC( QT_QSTANDARDITEMMODEL_TAKEROW )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QStandardItem *>( ( p )->takeRow( hb_parni( 2 ) ) ), true ) );
}

/* QStandardItem * takeVerticalHeaderItem ( int row ) */
HB_FUNC( QT_QSTANDARDITEMMODEL_TAKEVERTICALHEADERITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->takeVerticalHeaderItem( hb_parni( 2 ) ), false ) );
}

/* QStandardItem * verticalHeaderItem ( int row ) const */
HB_FUNC( QT_QSTANDARDITEMMODEL_VERTICALHEADERITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->verticalHeaderItem( hb_parni( 2 ) ), false ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
