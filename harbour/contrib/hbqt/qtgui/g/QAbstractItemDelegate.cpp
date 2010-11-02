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
 *  enum EndEditHint { NoHint, EditNextItem, EditPreviousItem, SubmitModelCache, RevertModelCache }
 */

/*
 *  Constructed[ 7/7 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // bool helpEvent ( QHelpEvent * event, QAbstractItemView * view, const QStyleOptionViewItem & option, const QModelIndex & index )
 */

#include <QtCore/QPointer>

#include <QtGui/QAbstractItemDelegate>


/* QAbstractItemDelegate ( QObject * parent = 0 )
 * virtual ~QAbstractItemDelegate ()
 */

typedef struct
{
   QPointer< QAbstractItemDelegate > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QAbstractItemDelegate;

HBQT_GC_FUNC( hbqt_gcRelease_QAbstractItemDelegate )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QAbstractItemDelegate( void * pObj, bool bNew )
{
   HBQT_GC_T_QAbstractItemDelegate * p = ( HBQT_GC_T_QAbstractItemDelegate * ) hb_gcAllocate( sizeof( HBQT_GC_T_QAbstractItemDelegate ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QAbstractItemDelegate >( ( QAbstractItemDelegate * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QAbstractItemDelegate;
   p->type = HBQT_TYPE_QAbstractItemDelegate;

   return p;
}

HB_FUNC( QT_QABSTRACTITEMDELEGATE )
{
   // __HB_RETPTRGC__( new QAbstractItemDelegate( 0 ) );
}

/* virtual QWidget * createEditor ( QWidget * parent, const QStyleOptionViewItem & option, const QModelIndex & index ) const */
HB_FUNC( QT_QABSTRACTITEMDELEGATE_CREATEEDITOR )
{
   QAbstractItemDelegate * p = hbqt_par_QAbstractItemDelegate( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->createEditor( hbqt_par_QWidget( 2 ), *hbqt_par_QStyleOptionViewItem( 3 ), *hbqt_par_QModelIndex( 4 ) ), false ) );
}

/* virtual bool editorEvent ( QEvent * event, QAbstractItemModel * model, const QStyleOptionViewItem & option, const QModelIndex & index ) */
HB_FUNC( QT_QABSTRACTITEMDELEGATE_EDITOREVENT )
{
   QAbstractItemDelegate * p = hbqt_par_QAbstractItemDelegate( 1 );
   if( p )
      hb_retl( ( p )->editorEvent( hbqt_par_QEvent( 2 ), hbqt_par_QAbstractItemModel( 3 ), *hbqt_par_QStyleOptionViewItem( 4 ), *hbqt_par_QModelIndex( 5 ) ) );
}

/* virtual void paint ( QPainter * painter, const QStyleOptionViewItem & option, const QModelIndex & index ) const = 0 */
HB_FUNC( QT_QABSTRACTITEMDELEGATE_PAINT )
{
   QAbstractItemDelegate * p = hbqt_par_QAbstractItemDelegate( 1 );
   if( p )
      ( p )->paint( hbqt_par_QPainter( 2 ), *hbqt_par_QStyleOptionViewItem( 3 ), *hbqt_par_QModelIndex( 4 ) );
}

/* virtual void setEditorData ( QWidget * editor, const QModelIndex & index ) const */
HB_FUNC( QT_QABSTRACTITEMDELEGATE_SETEDITORDATA )
{
   QAbstractItemDelegate * p = hbqt_par_QAbstractItemDelegate( 1 );
   if( p )
      ( p )->setEditorData( hbqt_par_QWidget( 2 ), *hbqt_par_QModelIndex( 3 ) );
}

/* virtual void setModelData ( QWidget * editor, QAbstractItemModel * model, const QModelIndex & index ) const */
HB_FUNC( QT_QABSTRACTITEMDELEGATE_SETMODELDATA )
{
   QAbstractItemDelegate * p = hbqt_par_QAbstractItemDelegate( 1 );
   if( p )
      ( p )->setModelData( hbqt_par_QWidget( 2 ), hbqt_par_QAbstractItemModel( 3 ), *hbqt_par_QModelIndex( 4 ) );
}

/* virtual QSize sizeHint ( const QStyleOptionViewItem & option, const QModelIndex & index ) const = 0 */
HB_FUNC( QT_QABSTRACTITEMDELEGATE_SIZEHINT )
{
   QAbstractItemDelegate * p = hbqt_par_QAbstractItemDelegate( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint( *hbqt_par_QStyleOptionViewItem( 2 ), *hbqt_par_QModelIndex( 3 ) ) ), true ) );
}

/* virtual void updateEditorGeometry ( QWidget * editor, const QStyleOptionViewItem & option, const QModelIndex & index ) const */
HB_FUNC( QT_QABSTRACTITEMDELEGATE_UPDATEEDITORGEOMETRY )
{
   QAbstractItemDelegate * p = hbqt_par_QAbstractItemDelegate( 1 );
   if( p )
      ( p )->updateEditorGeometry( hbqt_par_QWidget( 2 ), *hbqt_par_QStyleOptionViewItem( 3 ), *hbqt_par_QModelIndex( 4 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
