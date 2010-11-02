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
 *  Constructed[ 10/10 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QItemDelegate>


/* QItemDelegate ( QObject * parent = 0 )
 * ~QItemDelegate ()
 *
 */

typedef struct
{
   QPointer< QItemDelegate > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QItemDelegate;

HBQT_GC_FUNC( hbqt_gcRelease_QItemDelegate )
{
   HBQT_GC_T_QItemDelegate * p = ( HBQT_GC_T_QItemDelegate * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QItemDelegate * ph = p->ph;
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

void * hbqt_gcAllocate_QItemDelegate( void * pObj, bool bNew )
{
   HBQT_GC_T_QItemDelegate * p = ( HBQT_GC_T_QItemDelegate * ) hb_gcAllocate( sizeof( HBQT_GC_T_QItemDelegate ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QItemDelegate >( ( QItemDelegate * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QItemDelegate;
   p->type = HBQT_TYPE_QItemDelegate;

   return p;
}

HB_FUNC( QT_QITEMDELEGATE )
{
   QItemDelegate * pObj = NULL;

   if( HB_ISPOINTER( 1 ) )
   {
      pObj = new QItemDelegate( hbqt_par_QObject( 1 ) ) ;
   }
   else
   {
      pObj = new QItemDelegate() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QItemDelegate( ( void * ) pObj, true ) );
}

/* bool hasClipping () const */
HB_FUNC( QT_QITEMDELEGATE_HASCLIPPING )
{
   QItemDelegate * p = hbqt_par_QItemDelegate( 1 );
   if( p )
      hb_retl( ( p )->hasClipping() );
}

/* QItemEditorFactory * itemEditorFactory () const */
HB_FUNC( QT_QITEMDELEGATE_ITEMEDITORFACTORY )
{
   QItemDelegate * p = hbqt_par_QItemDelegate( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QItemEditorFactory( ( p )->itemEditorFactory(), false ) );
}

/* void setClipping ( bool clip ) */
HB_FUNC( QT_QITEMDELEGATE_SETCLIPPING )
{
   QItemDelegate * p = hbqt_par_QItemDelegate( 1 );
   if( p )
      ( p )->setClipping( hb_parl( 2 ) );
}

/* void setItemEditorFactory ( QItemEditorFactory * factory ) */
HB_FUNC( QT_QITEMDELEGATE_SETITEMEDITORFACTORY )
{
   QItemDelegate * p = hbqt_par_QItemDelegate( 1 );
   if( p )
      ( p )->setItemEditorFactory( hbqt_par_QItemEditorFactory( 2 ) );
}

/* virtual QWidget * createEditor ( QWidget * parent, const QStyleOptionViewItem & option, const QModelIndex & index ) const */
HB_FUNC( QT_QITEMDELEGATE_CREATEEDITOR )
{
   QItemDelegate * p = hbqt_par_QItemDelegate( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->createEditor( hbqt_par_QWidget( 2 ), *hbqt_par_QStyleOptionViewItem( 3 ), *hbqt_par_QModelIndex( 4 ) ), false ) );
}

/* virtual void paint ( QPainter * painter, const QStyleOptionViewItem & option, const QModelIndex & index ) const */
HB_FUNC( QT_QITEMDELEGATE_PAINT )
{
   QItemDelegate * p = hbqt_par_QItemDelegate( 1 );
   if( p )
      ( p )->paint( hbqt_par_QPainter( 2 ), *hbqt_par_QStyleOptionViewItem( 3 ), *hbqt_par_QModelIndex( 4 ) );
}

/* virtual void setEditorData ( QWidget * editor, const QModelIndex & index ) const */
HB_FUNC( QT_QITEMDELEGATE_SETEDITORDATA )
{
   QItemDelegate * p = hbqt_par_QItemDelegate( 1 );
   if( p )
      ( p )->setEditorData( hbqt_par_QWidget( 2 ), *hbqt_par_QModelIndex( 3 ) );
}

/* virtual void setModelData ( QWidget * editor, QAbstractItemModel * model, const QModelIndex & index ) const */
HB_FUNC( QT_QITEMDELEGATE_SETMODELDATA )
{
   QItemDelegate * p = hbqt_par_QItemDelegate( 1 );
   if( p )
      ( p )->setModelData( hbqt_par_QWidget( 2 ), hbqt_par_QAbstractItemModel( 3 ), *hbqt_par_QModelIndex( 4 ) );
}

/* virtual QSize sizeHint ( const QStyleOptionViewItem & option, const QModelIndex & index ) const */
HB_FUNC( QT_QITEMDELEGATE_SIZEHINT )
{
   QItemDelegate * p = hbqt_par_QItemDelegate( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint( *hbqt_par_QStyleOptionViewItem( 2 ), *hbqt_par_QModelIndex( 3 ) ) ), true ) );
}

/* virtual void updateEditorGeometry ( QWidget * editor, const QStyleOptionViewItem & option, const QModelIndex & index ) const */
HB_FUNC( QT_QITEMDELEGATE_UPDATEEDITORGEOMETRY )
{
   QItemDelegate * p = hbqt_par_QItemDelegate( 1 );
   if( p )
      ( p )->updateEditorGeometry( hbqt_par_QWidget( 2 ), *hbqt_par_QStyleOptionViewItem( 3 ), *hbqt_par_QModelIndex( 4 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
