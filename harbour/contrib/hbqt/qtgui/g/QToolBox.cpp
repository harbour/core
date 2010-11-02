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
 *  Constructed[ 20/20 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QToolBox>


/*
 * QToolBox ( QWidget * parent = 0, Qt::WindowFlags f = 0 )
 * ~QToolBox ()
 */

typedef struct
{
   QPointer< QToolBox > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QToolBox;

HBQT_GC_FUNC( hbqt_gcRelease_QToolBox )
{
   HBQT_GC_T_QToolBox * p = ( HBQT_GC_T_QToolBox * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QToolBox * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QToolBox( void * pObj, bool bNew )
{
   HBQT_GC_T_QToolBox * p = ( HBQT_GC_T_QToolBox * ) hb_gcAllocate( sizeof( HBQT_GC_T_QToolBox ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QToolBox >( ( QToolBox * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QToolBox;
   p->type = HBQT_TYPE_QToolBox;

   return p;
}

HB_FUNC( QT_QTOOLBOX )
{
   QToolBox * pObj = NULL;

   pObj = new QToolBox( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) hb_parni( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QToolBox( ( void * ) pObj, true ) );
}

/* int addItem ( QWidget * widget, const QIcon & iconSet, const QString & text ) */
HB_FUNC( QT_QTOOLBOX_ADDITEM )
{
   QToolBox * p = hbqt_par_QToolBox( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->addItem( hbqt_par_QWidget( 2 ), ( HB_ISCHAR( 3 ) ? QIcon( hbqt_par_QString( 3 ) ) : *hbqt_par_QIcon( 3 )), hb_parstr_utf8( 4, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* int addItem ( QWidget * w, const QString & text ) */
HB_FUNC( QT_QTOOLBOX_ADDITEM_1 )
{
   QToolBox * p = hbqt_par_QToolBox( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->addItem( hbqt_par_QWidget( 2 ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* int count () const */
HB_FUNC( QT_QTOOLBOX_COUNT )
{
   QToolBox * p = hbqt_par_QToolBox( 1 );
   if( p )
      hb_retni( ( p )->count() );
}

/* int currentIndex () const */
HB_FUNC( QT_QTOOLBOX_CURRENTINDEX )
{
   QToolBox * p = hbqt_par_QToolBox( 1 );
   if( p )
      hb_retni( ( p )->currentIndex() );
}

/* QWidget * currentWidget () const */
HB_FUNC( QT_QTOOLBOX_CURRENTWIDGET )
{
   QToolBox * p = hbqt_par_QToolBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->currentWidget(), false ) );
}

/* int indexOf ( QWidget * widget ) const */
HB_FUNC( QT_QTOOLBOX_INDEXOF )
{
   QToolBox * p = hbqt_par_QToolBox( 1 );
   if( p )
      hb_retni( ( p )->indexOf( hbqt_par_QWidget( 2 ) ) );
}

/* int insertItem ( int index, QWidget * widget, const QIcon & icon, const QString & text ) */
HB_FUNC( QT_QTOOLBOX_INSERTITEM )
{
   QToolBox * p = hbqt_par_QToolBox( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->insertItem( hb_parni( 2 ), hbqt_par_QWidget( 3 ), ( HB_ISCHAR( 4 ) ? QIcon( hbqt_par_QString( 4 ) ) : *hbqt_par_QIcon( 4 )), hb_parstr_utf8( 5, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* int insertItem ( int index, QWidget * widget, const QString & text ) */
HB_FUNC( QT_QTOOLBOX_INSERTITEM_1 )
{
   QToolBox * p = hbqt_par_QToolBox( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->insertItem( hb_parni( 2 ), hbqt_par_QWidget( 3 ), hb_parstr_utf8( 4, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* bool isItemEnabled ( int index ) const */
HB_FUNC( QT_QTOOLBOX_ISITEMENABLED )
{
   QToolBox * p = hbqt_par_QToolBox( 1 );
   if( p )
      hb_retl( ( p )->isItemEnabled( hb_parni( 2 ) ) );
}

/* QIcon itemIcon ( int index ) const */
HB_FUNC( QT_QTOOLBOX_ITEMICON )
{
   QToolBox * p = hbqt_par_QToolBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->itemIcon( hb_parni( 2 ) ) ), true ) );
}

/* QString itemText ( int index ) const */
HB_FUNC( QT_QTOOLBOX_ITEMTEXT )
{
   QToolBox * p = hbqt_par_QToolBox( 1 );
   if( p )
      hb_retstr_utf8( ( p )->itemText( hb_parni( 2 ) ).toUtf8().data() );
}

/* QString itemToolTip ( int index ) const */
HB_FUNC( QT_QTOOLBOX_ITEMTOOLTIP )
{
   QToolBox * p = hbqt_par_QToolBox( 1 );
   if( p )
      hb_retstr_utf8( ( p )->itemToolTip( hb_parni( 2 ) ).toUtf8().data() );
}

/* void removeItem ( int index ) */
HB_FUNC( QT_QTOOLBOX_REMOVEITEM )
{
   QToolBox * p = hbqt_par_QToolBox( 1 );
   if( p )
      ( p )->removeItem( hb_parni( 2 ) );
}

/* void setItemEnabled ( int index, bool enabled ) */
HB_FUNC( QT_QTOOLBOX_SETITEMENABLED )
{
   QToolBox * p = hbqt_par_QToolBox( 1 );
   if( p )
      ( p )->setItemEnabled( hb_parni( 2 ), hb_parl( 3 ) );
}

/* void setItemIcon ( int index, const QIcon & icon ) */
HB_FUNC( QT_QTOOLBOX_SETITEMICON )
{
   QToolBox * p = hbqt_par_QToolBox( 1 );
   if( p )
      ( p )->setItemIcon( hb_parni( 2 ), ( HB_ISCHAR( 3 ) ? QIcon( hbqt_par_QString( 3 ) ) : *hbqt_par_QIcon( 3 )) );
}

/* void setItemText ( int index, const QString & text ) */
HB_FUNC( QT_QTOOLBOX_SETITEMTEXT )
{
   QToolBox * p = hbqt_par_QToolBox( 1 );
   if( p )
   {
      void * pText;
      ( p )->setItemText( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setItemToolTip ( int index, const QString & toolTip ) */
HB_FUNC( QT_QTOOLBOX_SETITEMTOOLTIP )
{
   QToolBox * p = hbqt_par_QToolBox( 1 );
   if( p )
   {
      void * pText;
      ( p )->setItemToolTip( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* QWidget * widget ( int index ) const */
HB_FUNC( QT_QTOOLBOX_WIDGET )
{
   QToolBox * p = hbqt_par_QToolBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget( hb_parni( 2 ) ), false ) );
}

/* void setCurrentIndex ( int index ) */
HB_FUNC( QT_QTOOLBOX_SETCURRENTINDEX )
{
   QToolBox * p = hbqt_par_QToolBox( 1 );
   if( p )
      ( p )->setCurrentIndex( hb_parni( 2 ) );
}

/* void setCurrentWidget ( QWidget * widget ) */
HB_FUNC( QT_QTOOLBOX_SETCURRENTWIDGET )
{
   QToolBox * p = hbqt_par_QToolBox( 1 );
   if( p )
      ( p )->setCurrentWidget( hbqt_par_QWidget( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
