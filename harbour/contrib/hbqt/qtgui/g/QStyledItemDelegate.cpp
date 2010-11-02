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
 *  Constructed[ 7/7 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // QItemEditorFactory * itemEditorFactory () const
 *  // void setItemEditorFactory ( QItemEditorFactory * factory )
 */

#include <QtCore/QPointer>

#include <QtGui/QStyledItemDelegate>


/*
 * QStyledItemDelegate ( QObject * parent = 0 )
 * ~QStyledItemDelegate ()
 */

typedef struct
{
   QPointer< QStyledItemDelegate > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyledItemDelegate;

HBQT_GC_FUNC( hbqt_gcRelease_QStyledItemDelegate )
{
   HBQT_GC_T_QStyledItemDelegate * p = ( HBQT_GC_T_QStyledItemDelegate * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QStyledItemDelegate * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStyledItemDelegate( void * pObj, bool bNew )
{
   HBQT_GC_T_QStyledItemDelegate * p = ( HBQT_GC_T_QStyledItemDelegate * ) hb_gcAllocate( sizeof( HBQT_GC_T_QStyledItemDelegate ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QStyledItemDelegate >( ( QStyledItemDelegate * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyledItemDelegate;
   p->type = HBQT_TYPE_QStyledItemDelegate;

   return p;
}

HB_FUNC( QT_QSTYLEDITEMDELEGATE )
{
   QStyledItemDelegate * pObj = NULL;

   pObj = new QStyledItemDelegate( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QStyledItemDelegate( ( void * ) pObj, true ) );
}

/* virtual QWidget * createEditor ( QWidget * parent, const QStyleOptionViewItem & option, const QModelIndex & index ) const */
HB_FUNC( QT_QSTYLEDITEMDELEGATE_CREATEEDITOR )
{
   QStyledItemDelegate * p = hbqt_par_QStyledItemDelegate( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->createEditor( hbqt_par_QWidget( 2 ), *hbqt_par_QStyleOptionViewItem( 3 ), *hbqt_par_QModelIndex( 4 ) ), false ) );
}

/* virtual QString displayText ( const QVariant & value, const QLocale & locale ) const */
HB_FUNC( QT_QSTYLEDITEMDELEGATE_DISPLAYTEXT )
{
   QStyledItemDelegate * p = hbqt_par_QStyledItemDelegate( 1 );
   if( p )
      hb_retstr_utf8( ( p )->displayText( *hbqt_par_QVariant( 2 ), *hbqt_par_QLocale( 3 ) ).toUtf8().data() );
}

/* virtual void paint ( QPainter * painter, const QStyleOptionViewItem & option, const QModelIndex & index ) const */
HB_FUNC( QT_QSTYLEDITEMDELEGATE_PAINT )
{
   QStyledItemDelegate * p = hbqt_par_QStyledItemDelegate( 1 );
   if( p )
      ( p )->paint( hbqt_par_QPainter( 2 ), *hbqt_par_QStyleOptionViewItem( 3 ), *hbqt_par_QModelIndex( 4 ) );
}

/* virtual void setEditorData ( QWidget * editor, const QModelIndex & index ) const */
HB_FUNC( QT_QSTYLEDITEMDELEGATE_SETEDITORDATA )
{
   QStyledItemDelegate * p = hbqt_par_QStyledItemDelegate( 1 );
   if( p )
      ( p )->setEditorData( hbqt_par_QWidget( 2 ), *hbqt_par_QModelIndex( 3 ) );
}

/* virtual void setModelData ( QWidget * editor, QAbstractItemModel * model, const QModelIndex & index ) const */
HB_FUNC( QT_QSTYLEDITEMDELEGATE_SETMODELDATA )
{
   QStyledItemDelegate * p = hbqt_par_QStyledItemDelegate( 1 );
   if( p )
      ( p )->setModelData( hbqt_par_QWidget( 2 ), hbqt_par_QAbstractItemModel( 3 ), *hbqt_par_QModelIndex( 4 ) );
}

/* virtual QSize sizeHint ( const QStyleOptionViewItem & option, const QModelIndex & index ) const */
HB_FUNC( QT_QSTYLEDITEMDELEGATE_SIZEHINT )
{
   QStyledItemDelegate * p = hbqt_par_QStyledItemDelegate( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint( *hbqt_par_QStyleOptionViewItem( 2 ), *hbqt_par_QModelIndex( 3 ) ) ), true ) );
}

/* virtual void updateEditorGeometry ( QWidget * editor, const QStyleOptionViewItem & option, const QModelIndex & index ) const */
HB_FUNC( QT_QSTYLEDITEMDELEGATE_UPDATEEDITORGEOMETRY )
{
   QStyledItemDelegate * p = hbqt_par_QStyledItemDelegate( 1 );
   if( p )
      ( p )->updateEditorGeometry( hbqt_par_QWidget( 2 ), *hbqt_par_QStyleOptionViewItem( 3 ), *hbqt_par_QModelIndex( 4 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
