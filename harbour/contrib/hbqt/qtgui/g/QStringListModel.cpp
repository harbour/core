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
 *  Constructed[ 8/8 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStringListModel>


/*
 * QStringListModel ( QObject * parent = 0 )
 * QStringListModel ( const QStringList & strings, QObject * parent = 0 )
 */

typedef struct
{
   QPointer< QStringListModel > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStringListModel;

HBQT_GC_FUNC( hbqt_gcRelease_QStringListModel )
{
   HBQT_GC_T_QStringListModel * p = ( HBQT_GC_T_QStringListModel * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QStringListModel * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStringListModel( void * pObj, bool bNew )
{
   HBQT_GC_T_QStringListModel * p = ( HBQT_GC_T_QStringListModel * ) hb_gcAllocate( sizeof( HBQT_GC_T_QStringListModel ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QStringListModel >( ( QStringListModel * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStringListModel;
   p->type = HBQT_TYPE_QStringListModel;

   return p;
}

HB_FUNC( QT_QSTRINGLISTMODEL )
{
   QStringListModel * pObj = NULL;

   pObj = new QStringListModel() ;

   hb_retptrGC( hbqt_gcAllocate_QStringListModel( ( void * ) pObj, true ) );
}

/* virtual QVariant data ( const QModelIndex & index, int role ) const */
HB_FUNC( QT_QSTRINGLISTMODEL_DATA )
{
   QStringListModel * p = hbqt_par_QStringListModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->data( *hbqt_par_QModelIndex( 2 ), hb_parni( 3 ) ) ), true ) );
}

/* virtual Qt::ItemFlags flags ( const QModelIndex & index ) const */
HB_FUNC( QT_QSTRINGLISTMODEL_FLAGS )
{
   QStringListModel * p = hbqt_par_QStringListModel( 1 );
   if( p )
      hb_retni( ( Qt::ItemFlags ) ( p )->flags( *hbqt_par_QModelIndex( 2 ) ) );
}

/* virtual bool insertRows ( int row, int count, const QModelIndex & parent = QModelIndex() ) */
HB_FUNC( QT_QSTRINGLISTMODEL_INSERTROWS )
{
   QStringListModel * p = hbqt_par_QStringListModel( 1 );
   if( p )
      hb_retl( ( p )->insertRows( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISOBJECT( 4 ) ? *hbqt_par_QModelIndex( 4 ) : QModelIndex() ) ) );
}

/* virtual bool removeRows ( int row, int count, const QModelIndex & parent = QModelIndex() ) */
HB_FUNC( QT_QSTRINGLISTMODEL_REMOVEROWS )
{
   QStringListModel * p = hbqt_par_QStringListModel( 1 );
   if( p )
      hb_retl( ( p )->removeRows( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISOBJECT( 4 ) ? *hbqt_par_QModelIndex( 4 ) : QModelIndex() ) ) );
}

/* virtual int rowCount ( const QModelIndex & parent = QModelIndex() ) const */
HB_FUNC( QT_QSTRINGLISTMODEL_ROWCOUNT )
{
   QStringListModel * p = hbqt_par_QStringListModel( 1 );
   if( p )
      hb_retni( ( p )->rowCount( ( HB_ISOBJECT( 2 ) ? *hbqt_par_QModelIndex( 2 ) : QModelIndex() ) ) );
}

/* virtual bool setData ( const QModelIndex & index, const QVariant & value, int role = Qt::EditRole ) */
HB_FUNC( QT_QSTRINGLISTMODEL_SETDATA )
{
   QStringListModel * p = hbqt_par_QStringListModel( 1 );
   if( p )
      hb_retl( ( p )->setData( *hbqt_par_QModelIndex( 2 ), *hbqt_par_QVariant( 3 ), hb_parnidef( 4, Qt::EditRole ) ) );
}

/* void setStringList ( const QStringList & strings ) */
HB_FUNC( QT_QSTRINGLISTMODEL_SETSTRINGLIST )
{
   QStringListModel * p = hbqt_par_QStringListModel( 1 );
   if( p )
      ( p )->setStringList( *hbqt_par_QStringList( 2 ) );
}

/* QStringList stringList () const */
HB_FUNC( QT_QSTRINGLISTMODEL_STRINGLIST )
{
   QStringListModel * p = hbqt_par_QStringListModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->stringList() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
