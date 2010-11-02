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
 *  enum ButtonPosition { LeftSide, RightSide }
 *  enum SelectionBehavior { SelectLeftTab, SelectRightTab, SelectPreviousTab }
 *  enum Shape { RoundedNorth, RoundedSouth, RoundedWest, RoundedEast, ..., TriangularEast }
 */

/*
 *  Constructed[ 44/44 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // int addTab ( const QIcon & icon, const QString & text )
 *  // int insertTab ( int index, const QIcon & icon, const QString & text )
 *  // void setTabIcon ( int index, const QIcon & icon )
 */

#include <QtCore/QPointer>

#include <QtGui/QTabBar>
#include <QtCore/QVariant>
#include <QtGui/QIcon>

/*
 * QTabBar ( QWidget * parent = 0 )
 * ~QTabBar ()
 */

typedef struct
{
   QPointer< QTabBar > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTabBar;

HBQT_GC_FUNC( hbqt_gcRelease_QTabBar )
{
   HBQT_GC_T_QTabBar * p = ( HBQT_GC_T_QTabBar * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QTabBar * ph = p->ph;
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

void * hbqt_gcAllocate_QTabBar( void * pObj, bool bNew )
{
   HBQT_GC_T_QTabBar * p = ( HBQT_GC_T_QTabBar * ) hb_gcAllocate( sizeof( HBQT_GC_T_QTabBar ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTabBar >( ( QTabBar * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTabBar;
   p->type = HBQT_TYPE_QTabBar;

   return p;
}

HB_FUNC( QT_QTABBAR )
{
   QTabBar * pObj = NULL;

   pObj = new QTabBar( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTabBar( ( void * ) pObj, true ) );
}

/* int addTab ( const QString & text ) */
HB_FUNC( QT_QTABBAR_ADDTAB )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->addTab( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* int count () const */
HB_FUNC( QT_QTABBAR_COUNT )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retni( ( p )->count() );
}

/* int currentIndex () const */
HB_FUNC( QT_QTABBAR_CURRENTINDEX )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retni( ( p )->currentIndex() );
}

/* bool documentMode () const */
HB_FUNC( QT_QTABBAR_DOCUMENTMODE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retl( ( p )->documentMode() );
}

/* bool drawBase () const */
HB_FUNC( QT_QTABBAR_DRAWBASE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retl( ( p )->drawBase() );
}

/* Qt::TextElideMode elideMode () const */
HB_FUNC( QT_QTABBAR_ELIDEMODE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retni( ( Qt::TextElideMode ) ( p )->elideMode() );
}

/* bool expanding () const */
HB_FUNC( QT_QTABBAR_EXPANDING )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retl( ( p )->expanding() );
}

/* QSize iconSize () const */
HB_FUNC( QT_QTABBAR_ICONSIZE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->iconSize() ), true ) );
}

/* int insertTab ( int index, const QString & text ) */
HB_FUNC( QT_QTABBAR_INSERTTAB )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->insertTab( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* bool isMovable () const */
HB_FUNC( QT_QTABBAR_ISMOVABLE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retl( ( p )->isMovable() );
}

/* bool isTabEnabled ( int index ) const */
HB_FUNC( QT_QTABBAR_ISTABENABLED )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retl( ( p )->isTabEnabled( hb_parni( 2 ) ) );
}

/* void moveTab ( int from, int to ) */
HB_FUNC( QT_QTABBAR_MOVETAB )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->moveTab( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void removeTab ( int index ) */
HB_FUNC( QT_QTABBAR_REMOVETAB )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->removeTab( hb_parni( 2 ) );
}

/* SelectionBehavior selectionBehaviorOnRemove () const */
HB_FUNC( QT_QTABBAR_SELECTIONBEHAVIORONREMOVE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retni( ( QTabBar::SelectionBehavior ) ( p )->selectionBehaviorOnRemove() );
}

/* void setDocumentMode ( bool set ) */
HB_FUNC( QT_QTABBAR_SETDOCUMENTMODE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setDocumentMode( hb_parl( 2 ) );
}

/* void setDrawBase ( bool drawTheBase ) */
HB_FUNC( QT_QTABBAR_SETDRAWBASE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setDrawBase( hb_parl( 2 ) );
}

/* void setElideMode ( Qt::TextElideMode ) */
HB_FUNC( QT_QTABBAR_SETELIDEMODE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setElideMode( ( Qt::TextElideMode ) hb_parni( 2 ) );
}

/* void setExpanding ( bool enabled ) */
HB_FUNC( QT_QTABBAR_SETEXPANDING )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setExpanding( hb_parl( 2 ) );
}

/* void setIconSize ( const QSize & size ) */
HB_FUNC( QT_QTABBAR_SETICONSIZE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setIconSize( *hbqt_par_QSize( 2 ) );
}

/* void setMovable ( bool movable ) */
HB_FUNC( QT_QTABBAR_SETMOVABLE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setMovable( hb_parl( 2 ) );
}

/* void setSelectionBehaviorOnRemove ( SelectionBehavior behavior ) */
HB_FUNC( QT_QTABBAR_SETSELECTIONBEHAVIORONREMOVE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setSelectionBehaviorOnRemove( ( QTabBar::SelectionBehavior ) hb_parni( 2 ) );
}

/* void setShape ( Shape shape ) */
HB_FUNC( QT_QTABBAR_SETSHAPE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setShape( ( QTabBar::Shape ) hb_parni( 2 ) );
}

/* void setTabButton ( int index, ButtonPosition position, QWidget * widget ) */
HB_FUNC( QT_QTABBAR_SETTABBUTTON )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setTabButton( hb_parni( 2 ), ( QTabBar::ButtonPosition ) hb_parni( 3 ), hbqt_par_QWidget( 4 ) );
}

/* void setTabData ( int index, const QVariant & data ) */
HB_FUNC( QT_QTABBAR_SETTABDATA )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setTabData( hb_parni( 2 ), *hbqt_par_QVariant( 3 ) );
}

/* void setTabEnabled ( int index, bool enabled ) */
HB_FUNC( QT_QTABBAR_SETTABENABLED )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setTabEnabled( hb_parni( 2 ), hb_parl( 3 ) );
}

/* void setTabText ( int index, const QString & text ) */
HB_FUNC( QT_QTABBAR_SETTABTEXT )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
   {
      void * pText;
      ( p )->setTabText( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setTabTextColor ( int index, const QColor & color ) */
HB_FUNC( QT_QTABBAR_SETTABTEXTCOLOR )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setTabTextColor( hb_parni( 2 ), *hbqt_par_QColor( 3 ) );
}

/* void setTabToolTip ( int index, const QString & tip ) */
HB_FUNC( QT_QTABBAR_SETTABTOOLTIP )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
   {
      void * pText;
      ( p )->setTabToolTip( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setTabWhatsThis ( int index, const QString & text ) */
HB_FUNC( QT_QTABBAR_SETTABWHATSTHIS )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
   {
      void * pText;
      ( p )->setTabWhatsThis( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setTabsClosable ( bool closable ) */
HB_FUNC( QT_QTABBAR_SETTABSCLOSABLE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setTabsClosable( hb_parl( 2 ) );
}

/* void setUsesScrollButtons ( bool useButtons ) */
HB_FUNC( QT_QTABBAR_SETUSESSCROLLBUTTONS )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setUsesScrollButtons( hb_parl( 2 ) );
}

/* Shape shape () const */
HB_FUNC( QT_QTABBAR_SHAPE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retni( ( QTabBar::Shape ) ( p )->shape() );
}

/* int tabAt ( const QPoint & position ) const */
HB_FUNC( QT_QTABBAR_TABAT )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retni( ( p )->tabAt( *hbqt_par_QPoint( 2 ) ) );
}

/* QWidget * tabButton ( int index, ButtonPosition position ) const */
HB_FUNC( QT_QTABBAR_TABBUTTON )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->tabButton( hb_parni( 2 ), ( QTabBar::ButtonPosition ) hb_parni( 3 ) ), false ) );
}

/* QVariant tabData ( int index ) const */
HB_FUNC( QT_QTABBAR_TABDATA )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->tabData( hb_parni( 2 ) ) ), true ) );
}

/* QIcon tabIcon ( int index ) const */
HB_FUNC( QT_QTABBAR_TABICON )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->tabIcon( hb_parni( 2 ) ) ), true ) );
}

/* QRect tabRect ( int index ) const */
HB_FUNC( QT_QTABBAR_TABRECT )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->tabRect( hb_parni( 2 ) ) ), true ) );
}

/* QString tabText ( int index ) const */
HB_FUNC( QT_QTABBAR_TABTEXT )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retstr_utf8( ( p )->tabText( hb_parni( 2 ) ).toUtf8().data() );
}

/* QColor tabTextColor ( int index ) const */
HB_FUNC( QT_QTABBAR_TABTEXTCOLOR )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->tabTextColor( hb_parni( 2 ) ) ), true ) );
}

/* QString tabToolTip ( int index ) const */
HB_FUNC( QT_QTABBAR_TABTOOLTIP )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retstr_utf8( ( p )->tabToolTip( hb_parni( 2 ) ).toUtf8().data() );
}

/* QString tabWhatsThis ( int index ) const */
HB_FUNC( QT_QTABBAR_TABWHATSTHIS )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retstr_utf8( ( p )->tabWhatsThis( hb_parni( 2 ) ).toUtf8().data() );
}

/* bool tabsClosable () const */
HB_FUNC( QT_QTABBAR_TABSCLOSABLE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retl( ( p )->tabsClosable() );
}

/* bool usesScrollButtons () const */
HB_FUNC( QT_QTABBAR_USESSCROLLBUTTONS )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retl( ( p )->usesScrollButtons() );
}

/* void setCurrentIndex ( int index ) */
HB_FUNC( QT_QTABBAR_SETCURRENTINDEX )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setCurrentIndex( hb_parni( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
