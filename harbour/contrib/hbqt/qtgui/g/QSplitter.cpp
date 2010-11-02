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
 *  Constructed[ 21/22 [ 95.45% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  void setSizes ( const QList<int> & list )
 *
 *  *** Commented out protostypes ***
 *
 *  //QSplitterHandle * handle ( int index ) const
 */

#include <QtCore/QPointer>

#include <QtGui/QSplitter>


/*
 * QSplitter ( QWidget * parent = 0 )
 * QSplitter ( Qt::Orientation orientation, QWidget * parent = 0 )
 * ~QSplitter ()
 */

typedef struct
{
   QPointer< QSplitter > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QSplitter;

HBQT_GC_FUNC( hbqt_gcRelease_QSplitter )
{
   HBQT_GC_T_QSplitter * p = ( HBQT_GC_T_QSplitter * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QSplitter * ph = p->ph;
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

void * hbqt_gcAllocate_QSplitter( void * pObj, bool bNew )
{
   HBQT_GC_T_QSplitter * p = ( HBQT_GC_T_QSplitter * ) hb_gcAllocate( sizeof( HBQT_GC_T_QSplitter ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QSplitter >( ( QSplitter * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSplitter;
   p->type = HBQT_TYPE_QSplitter;

   return p;
}

HB_FUNC( QT_QSPLITTER )
{
   QSplitter * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISNUM( 1 ) )
      pObj = new QSplitter( ( Qt::Orientation ) hb_parni( 1 ), hbqt_par_QWidget( 2 ) ) ;
   else
      pObj = new QSplitter( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QSplitter( ( void * ) pObj, true ) );
}

/* void addWidget ( QWidget * widget ) */
HB_FUNC( QT_QSPLITTER_ADDWIDGET )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      ( p )->addWidget( hbqt_par_QWidget( 2 ) );
}

/* bool childrenCollapsible () const */
HB_FUNC( QT_QSPLITTER_CHILDRENCOLLAPSIBLE )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      hb_retl( ( p )->childrenCollapsible() );
}

/* int count () const */
HB_FUNC( QT_QSPLITTER_COUNT )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      hb_retni( ( p )->count() );
}

/* void getRange ( int index, int * min, int * max ) const */
HB_FUNC( QT_QSPLITTER_GETRANGE )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   int iMin = 0;
   int iMax = 0;

   if( p )
      ( p )->getRange( hb_parni( 2 ), &iMin, &iMax );

   hb_storni( iMin, 3 );
   hb_storni( iMax, 4 );
}

/* int handleWidth () const */
HB_FUNC( QT_QSPLITTER_HANDLEWIDTH )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      hb_retni( ( p )->handleWidth() );
}

/* int indexOf ( QWidget * widget ) const */
HB_FUNC( QT_QSPLITTER_INDEXOF )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      hb_retni( ( p )->indexOf( hbqt_par_QWidget( 2 ) ) );
}

/* void insertWidget ( int index, QWidget * widget ) */
HB_FUNC( QT_QSPLITTER_INSERTWIDGET )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      ( p )->insertWidget( hb_parni( 2 ), hbqt_par_QWidget( 3 ) );
}

/* bool isCollapsible ( int index ) const */
HB_FUNC( QT_QSPLITTER_ISCOLLAPSIBLE )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      hb_retl( ( p )->isCollapsible( hb_parni( 2 ) ) );
}

/* bool opaqueResize () const */
HB_FUNC( QT_QSPLITTER_OPAQUERESIZE )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      hb_retl( ( p )->opaqueResize() );
}

/* Qt::Orientation orientation () const */
HB_FUNC( QT_QSPLITTER_ORIENTATION )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      hb_retni( ( Qt::Orientation ) ( p )->orientation() );
}

/* void refresh () */
HB_FUNC( QT_QSPLITTER_REFRESH )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      ( p )->refresh();
}

/* bool restoreState ( const QByteArray & state ) */
HB_FUNC( QT_QSPLITTER_RESTORESTATE )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      hb_retl( ( p )->restoreState( *hbqt_par_QByteArray( 2 ) ) );
}

/* QByteArray saveState () const */
HB_FUNC( QT_QSPLITTER_SAVESTATE )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->saveState() ), true ) );
}

/* void setChildrenCollapsible ( bool ) */
HB_FUNC( QT_QSPLITTER_SETCHILDRENCOLLAPSIBLE )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      ( p )->setChildrenCollapsible( hb_parl( 2 ) );
}

/* void setCollapsible ( int index, bool collapse ) */
HB_FUNC( QT_QSPLITTER_SETCOLLAPSIBLE )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      ( p )->setCollapsible( hb_parni( 2 ), hb_parl( 3 ) );
}

/* void setHandleWidth ( int ) */
HB_FUNC( QT_QSPLITTER_SETHANDLEWIDTH )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      ( p )->setHandleWidth( hb_parni( 2 ) );
}

/* void setOpaqueResize ( bool opaque = true ) */
HB_FUNC( QT_QSPLITTER_SETOPAQUERESIZE )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      ( p )->setOpaqueResize( hb_parl( 2 ) );
}

/* void setOrientation ( Qt::Orientation ) */
HB_FUNC( QT_QSPLITTER_SETORIENTATION )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      ( p )->setOrientation( ( Qt::Orientation ) hb_parni( 2 ) );
}

/* void setStretchFactor ( int index, int stretch ) */
HB_FUNC( QT_QSPLITTER_SETSTRETCHFACTOR )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      ( p )->setStretchFactor( hb_parni( 2 ), hb_parni( 3 ) );
}

/* QList<int> sizes () const */
HB_FUNC( QT_QSPLITTER_SIZES )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<int>( ( p )->sizes() ), true ) );
}

/* QWidget * widget ( int index ) const */
HB_FUNC( QT_QSPLITTER_WIDGET )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget( hb_parni( 2 ) ), false ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
