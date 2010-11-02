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
 *  Constructed[ 32/32 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QListWidget>


/*
 * QListWidget ( QWidget * parent = 0 )
 * ~QListWidget ()
 */

typedef struct
{
   QPointer< QListWidget > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QListWidget;

HBQT_GC_FUNC( hbqt_gcRelease_QListWidget )
{
   QListWidget  * ph = NULL;
   HBQT_GC_T_QListWidget * p = ( HBQT_GC_T_QListWidget * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete ( p->ph );
            p->ph = NULL;
         }
         else
            p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QListWidget( void * pObj, bool bNew )
{
   HBQT_GC_T_QListWidget * p = ( HBQT_GC_T_QListWidget * ) hb_gcAllocate( sizeof( HBQT_GC_T_QListWidget ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QListWidget >( ( QListWidget * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QListWidget;
   p->type = HBQT_TYPE_QListWidget;

   return p;
}

HB_FUNC( QT_QLISTWIDGET )
{
   QListWidget * pObj = NULL;

   pObj = new QListWidget( HB_ISPOINTER( 1 ) ? hbqt_par_QWidget( 1 ) : 0 ) ;

   hb_retptrGC( hbqt_gcAllocate_QListWidget( ( void * ) pObj, true ) );
}

/* void addItem ( const QString & label ) */
HB_FUNC( QT_QLISTWIDGET_ADDITEM )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
   {
      void * pText;
      ( p )->addItem( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void addItem ( QListWidgetItem * item )              [*D=1*] */
HB_FUNC( QT_QLISTWIDGET_ADDITEM_1 )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 2 );
      ( p )->addItem( hbqt_par_QListWidgetItem( 2 ) );
   }
}

/* void addItems ( const QStringList & labels ) */
HB_FUNC( QT_QLISTWIDGET_ADDITEMS )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      ( p )->addItems( *hbqt_par_QStringList( 2 ) );
}

/* void closePersistentEditor ( QListWidgetItem * item ) */
HB_FUNC( QT_QLISTWIDGET_CLOSEPERSISTENTEDITOR )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      ( p )->closePersistentEditor( hbqt_par_QListWidgetItem( 2 ) );
}

/* int count () const */
HB_FUNC( QT_QLISTWIDGET_COUNT )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      hb_retni( ( p )->count() );
}

/* QListWidgetItem * currentItem () const */
HB_FUNC( QT_QLISTWIDGET_CURRENTITEM )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QListWidgetItem( ( p )->currentItem(), false ) );
}

/* int currentRow () const */
HB_FUNC( QT_QLISTWIDGET_CURRENTROW )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      hb_retni( ( p )->currentRow() );
}

/* void editItem ( QListWidgetItem * item ) */
HB_FUNC( QT_QLISTWIDGET_EDITITEM )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      ( p )->editItem( hbqt_par_QListWidgetItem( 2 ) );
}

/* QList<QListWidgetItem *> findItems ( const QString & text, Qt::MatchFlags flags ) const */
HB_FUNC( QT_QLISTWIDGET_FINDITEMS )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QListWidgetItem *>( ( p )->findItems( hb_parstr_utf8( 2, &pText, NULL ), ( Qt::MatchFlags ) hb_parni( 3 ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* void insertItem ( int row, QListWidgetItem * item )  [*D=2*] */
HB_FUNC( QT_QLISTWIDGET_INSERTITEM )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 3 );
      ( p )->insertItem( hb_parni( 2 ), hbqt_par_QListWidgetItem( 3 ) );
   }
}

/* void insertItem ( int row, const QString & label ) */
HB_FUNC( QT_QLISTWIDGET_INSERTITEM_1 )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
   {
      void * pText;
      ( p )->insertItem( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void insertItems ( int row, const QStringList & labels ) */
HB_FUNC( QT_QLISTWIDGET_INSERTITEMS )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      ( p )->insertItems( hb_parni( 2 ), *hbqt_par_QStringList( 3 ) );
}

/* bool isSortingEnabled () const */
HB_FUNC( QT_QLISTWIDGET_ISSORTINGENABLED )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      hb_retl( ( p )->isSortingEnabled() );
}

/* QListWidgetItem * item ( int row ) const */
HB_FUNC( QT_QLISTWIDGET_ITEM )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QListWidgetItem( ( p )->item( hb_parni( 2 ) ), false ) );
}

/* QListWidgetItem * itemAt ( const QPoint & p ) const */
HB_FUNC( QT_QLISTWIDGET_ITEMAT )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QListWidgetItem( ( p )->itemAt( *hbqt_par_QPoint( 2 ) ), false ) );
}

/* QListWidgetItem * itemAt ( int x, int y ) const */
HB_FUNC( QT_QLISTWIDGET_ITEMAT_1 )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QListWidgetItem( ( p )->itemAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
}

/* QWidget * itemWidget ( QListWidgetItem * item ) const */
HB_FUNC( QT_QLISTWIDGET_ITEMWIDGET )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->itemWidget( hbqt_par_QListWidgetItem( 2 ) ), false ) );
}

/* void openPersistentEditor ( QListWidgetItem * item ) */
HB_FUNC( QT_QLISTWIDGET_OPENPERSISTENTEDITOR )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      ( p )->openPersistentEditor( hbqt_par_QListWidgetItem( 2 ) );
}

/* void removeItemWidget ( QListWidgetItem * item ) */
HB_FUNC( QT_QLISTWIDGET_REMOVEITEMWIDGET )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      ( p )->removeItemWidget( hbqt_par_QListWidgetItem( 2 ) );
}

/* int row ( const QListWidgetItem * item ) const */
HB_FUNC( QT_QLISTWIDGET_ROW )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      hb_retni( ( p )->row( hbqt_par_QListWidgetItem( 2 ) ) );
}

/* QList<QListWidgetItem *> selectedItems () const */
HB_FUNC( QT_QLISTWIDGET_SELECTEDITEMS )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QListWidgetItem *>( ( p )->selectedItems() ), true ) );
}

/* void setCurrentItem ( QListWidgetItem * item ) */
HB_FUNC( QT_QLISTWIDGET_SETCURRENTITEM )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      ( p )->setCurrentItem( hbqt_par_QListWidgetItem( 2 ) );
}

/* void setCurrentItem ( QListWidgetItem * item, QItemSelectionModel::SelectionFlags command ) */
HB_FUNC( QT_QLISTWIDGET_SETCURRENTITEM_1 )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      ( p )->setCurrentItem( hbqt_par_QListWidgetItem( 2 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 3 ) );
}

/* void setCurrentRow ( int row ) */
HB_FUNC( QT_QLISTWIDGET_SETCURRENTROW )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      ( p )->setCurrentRow( hb_parni( 2 ) );
}

/* void setCurrentRow ( int row, QItemSelectionModel::SelectionFlags command ) */
HB_FUNC( QT_QLISTWIDGET_SETCURRENTROW_1 )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      ( p )->setCurrentRow( hb_parni( 2 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 3 ) );
}

/* void setItemWidget ( QListWidgetItem * item, QWidget * widget ) */
HB_FUNC( QT_QLISTWIDGET_SETITEMWIDGET )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      ( p )->setItemWidget( hbqt_par_QListWidgetItem( 2 ), hbqt_par_QWidget( 3 ) );
}

/* void setSortingEnabled ( bool enable ) */
HB_FUNC( QT_QLISTWIDGET_SETSORTINGENABLED )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      ( p )->setSortingEnabled( hb_parl( 2 ) );
}

/* void sortItems ( Qt::SortOrder order = Qt::AscendingOrder ) */
HB_FUNC( QT_QLISTWIDGET_SORTITEMS )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      ( p )->sortItems( ( HB_ISNUM( 2 ) ? ( Qt::SortOrder ) hb_parni( 2 ) : ( Qt::SortOrder ) Qt::AscendingOrder ) );
}

/* QListWidgetItem * takeItem ( int row ) */
HB_FUNC( QT_QLISTWIDGET_TAKEITEM )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QListWidgetItem( ( p )->takeItem( hb_parni( 2 ) ), false ) );
}

/* QRect visualItemRect ( const QListWidgetItem * item ) const */
HB_FUNC( QT_QLISTWIDGET_VISUALITEMRECT )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->visualItemRect( hbqt_par_QListWidgetItem( 2 ) ) ), true ) );
}

/* void clear () */
HB_FUNC( QT_QLISTWIDGET_CLEAR )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      ( p )->clear();
}

/* void scrollToItem ( const QListWidgetItem * item, QAbstractItemView::ScrollHint hint = EnsureVisible ) */
HB_FUNC( QT_QLISTWIDGET_SCROLLTOITEM )
{
   QListWidget * p = hbqt_par_QListWidget( 1 );
   if( p )
      ( p )->scrollToItem( hbqt_par_QListWidgetItem( 2 ), ( HB_ISNUM( 3 ) ? ( QAbstractItemView::ScrollHint ) hb_parni( 3 ) : ( QAbstractItemView::ScrollHint ) QListWidget::EnsureVisible ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
