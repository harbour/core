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
 *  Constructed[ 28/28 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QToolBar>
#include <QtGui/QIcon>


/* QToolBar ( const QString & title, QWidget * parent = 0 )
 * QToolBar ( QWidget * parent = 0 )
 * ~QToolBar ()
 */

typedef struct
{
   QPointer< QToolBar > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QToolBar;

HBQT_GC_FUNC( hbqt_gcRelease_QToolBar )
{
   HBQT_GC_T_QToolBar * p = ( HBQT_GC_T_QToolBar * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QToolBar * ph = p->ph;
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

void * hbqt_gcAllocate_QToolBar( void * pObj, bool bNew )
{
   HBQT_GC_T_QToolBar * p = ( HBQT_GC_T_QToolBar * ) hb_gcAllocate( sizeof( HBQT_GC_T_QToolBar ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QToolBar >( ( QToolBar * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QToolBar;
   p->type = HBQT_TYPE_QToolBar;

   return p;
}

HB_FUNC( QT_QTOOLBAR )
{
   QToolBar * pObj = NULL;

   if( hb_param( 1, HB_IT_STRING ) )
      pObj = new QToolBar( hbqt_par_QString( 1 ), hbqt_par_QWidget( 2 ) ) ;
   else
      pObj = new QToolBar( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QToolBar( ( void * ) pObj, true ) );
}

/* QAction * actionAt ( const QPoint & p ) const */
HB_FUNC( QT_QTOOLBAR_ACTIONAT )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionAt( *hbqt_par_QPoint( 2 ) ), false ) );
}

/* QAction * actionAt ( int x, int y ) const */
HB_FUNC( QT_QTOOLBAR_ACTIONAT_1 )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
}

/* void addAction ( QAction * action )   [*D=1*] */
HB_FUNC( QT_QTOOLBAR_ADDACTION )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 2 );
      ( p )->addAction( hbqt_par_QAction( 2 ) );
   }
}

/* QAction * addAction ( const QString & text ) */
HB_FUNC( QT_QTOOLBAR_ADDACTION_1 )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( hb_parstr_utf8( 2, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/* QAction * addAction ( const QIcon & icon, const QString & text ) */
HB_FUNC( QT_QTOOLBAR_ADDACTION_2 )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( ( HB_ISCHAR( 2 ) ? QIcon( hbqt_par_QString( 2 ) ) : *hbqt_par_QIcon( 2 )), hb_parstr_utf8( 3, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/* QAction * addAction ( const QString & text, const QObject * receiver, const char * member ) */
HB_FUNC( QT_QTOOLBAR_ADDACTION_3 )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( hb_parstr_utf8( 2, &pText, NULL ), hbqt_par_QObject( 3 ), ( const char * ) hb_parc( 4 ) ), false ) );
      hb_strfree( pText );
   }
}

/* QAction * addAction ( const QIcon & icon, const QString & text, const QObject * receiver, const char * member )   [*D=3*] */
HB_FUNC( QT_QTOOLBAR_ADDACTION_4 )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 4 );
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( ( HB_ISCHAR( 2 ) ? QIcon( hbqt_par_QString( 2 ) ) : *hbqt_par_QIcon( 2 )), hb_parstr_utf8( 3, &pText, NULL ), hbqt_par_QObject( 4 ), ( const char * ) hb_parc( 5 ) ), false ) );
      hb_strfree( pText );
   }
}

/* QAction * addSeparator () */
HB_FUNC( QT_QTOOLBAR_ADDSEPARATOR )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addSeparator(), false ) );
}

/* QAction * addWidget ( QWidget * widget ) */
HB_FUNC( QT_QTOOLBAR_ADDWIDGET )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addWidget( hbqt_par_QWidget( 2 ) ), false ) );
}

/* Qt::ToolBarAreas allowedAreas () const */
HB_FUNC( QT_QTOOLBAR_ALLOWEDAREAS )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retni( ( Qt::ToolBarAreas ) ( p )->allowedAreas() );
}

/* void clear () */
HB_FUNC( QT_QTOOLBAR_CLEAR )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      ( p )->clear();
}

/* QSize iconSize () const */
HB_FUNC( QT_QTOOLBAR_ICONSIZE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->iconSize() ), true ) );
}

/* QAction * insertSeparator ( QAction * before ) */
HB_FUNC( QT_QTOOLBAR_INSERTSEPARATOR )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->insertSeparator( hbqt_par_QAction( 2 ) ), false ) );
}

/* QAction * insertWidget ( QAction * before, QWidget * widget ) */
HB_FUNC( QT_QTOOLBAR_INSERTWIDGET )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->insertWidget( hbqt_par_QAction( 2 ), hbqt_par_QWidget( 3 ) ), false ) );
}

/* bool isAreaAllowed ( Qt::ToolBarArea area ) const */
HB_FUNC( QT_QTOOLBAR_ISAREAALLOWED )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retl( ( p )->isAreaAllowed( ( Qt::ToolBarArea ) hb_parni( 2 ) ) );
}

/* bool isFloatable () const */
HB_FUNC( QT_QTOOLBAR_ISFLOATABLE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retl( ( p )->isFloatable() );
}

/* bool isFloating () const */
HB_FUNC( QT_QTOOLBAR_ISFLOATING )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retl( ( p )->isFloating() );
}

/* bool isMovable () const */
HB_FUNC( QT_QTOOLBAR_ISMOVABLE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retl( ( p )->isMovable() );
}

/* Qt::Orientation orientation () const */
HB_FUNC( QT_QTOOLBAR_ORIENTATION )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retni( ( Qt::Orientation ) ( p )->orientation() );
}

/* void setAllowedAreas ( Qt::ToolBarAreas areas ) */
HB_FUNC( QT_QTOOLBAR_SETALLOWEDAREAS )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      ( p )->setAllowedAreas( ( Qt::ToolBarAreas ) hb_parni( 2 ) );
}

/* void setFloatable ( bool floatable ) */
HB_FUNC( QT_QTOOLBAR_SETFLOATABLE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      ( p )->setFloatable( hb_parl( 2 ) );
}

/* void setMovable ( bool movable ) */
HB_FUNC( QT_QTOOLBAR_SETMOVABLE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      ( p )->setMovable( hb_parl( 2 ) );
}

/* void setOrientation ( Qt::Orientation orientation ) */
HB_FUNC( QT_QTOOLBAR_SETORIENTATION )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      ( p )->setOrientation( ( Qt::Orientation ) hb_parni( 2 ) );
}

/* QAction * toggleViewAction () const */
HB_FUNC( QT_QTOOLBAR_TOGGLEVIEWACTION )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->toggleViewAction(), false ) );
}

/* Qt::ToolButtonStyle toolButtonStyle () const */
HB_FUNC( QT_QTOOLBAR_TOOLBUTTONSTYLE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retni( ( Qt::ToolButtonStyle ) ( p )->toolButtonStyle() );
}

/* QWidget * widgetForAction ( QAction * action ) const */
HB_FUNC( QT_QTOOLBAR_WIDGETFORACTION )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widgetForAction( hbqt_par_QAction( 2 ) ), false ) );
}

/* void setIconSize ( const QSize & iconSize ) */
HB_FUNC( QT_QTOOLBAR_SETICONSIZE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      ( p )->setIconSize( *hbqt_par_QSize( 2 ) );
}

/* void setToolButtonStyle ( Qt::ToolButtonStyle toolButtonStyle ) */
HB_FUNC( QT_QTOOLBAR_SETTOOLBUTTONSTYLE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      ( p )->setToolButtonStyle( ( Qt::ToolButtonStyle ) hb_parni( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
