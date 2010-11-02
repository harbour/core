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
 *  enum ToolButtonPopupMode { DelayedPopup, MenuButtonPopup, InstantPopup }
 */

/*
 *  Constructed[ 13/13 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QToolButton>


/*
 * QToolButton ( QWidget * parent = 0 )
 * ~QToolButton ()
 */

typedef struct
{
   QPointer< QToolButton > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QToolButton;

HBQT_GC_FUNC( hbqt_gcRelease_QToolButton )
{
   HBQT_GC_T_QToolButton * p = ( HBQT_GC_T_QToolButton * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QToolButton * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QToolButton( void * pObj, bool bNew )
{
   HBQT_GC_T_QToolButton * p = ( HBQT_GC_T_QToolButton * ) hb_gcAllocate( sizeof( HBQT_GC_T_QToolButton ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QToolButton >( ( QToolButton * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QToolButton;
   p->type = HBQT_TYPE_QToolButton;

   return p;
}

HB_FUNC( QT_QTOOLBUTTON )
{
   QToolButton * pObj = NULL;

   pObj = new QToolButton( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QToolButton( ( void * ) pObj, true ) );
}

/* Qt::ArrowType arrowType () const */
HB_FUNC( QT_QTOOLBUTTON_ARROWTYPE )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      hb_retni( ( Qt::ArrowType ) ( p )->arrowType() );
}

/* bool autoRaise () const */
HB_FUNC( QT_QTOOLBUTTON_AUTORAISE )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      hb_retl( ( p )->autoRaise() );
}

/* QAction * defaultAction () const */
HB_FUNC( QT_QTOOLBUTTON_DEFAULTACTION )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->defaultAction(), false ) );
}

/* QMenu * menu () const */
HB_FUNC( QT_QTOOLBUTTON_MENU )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->menu(), false ) );
}

/* ToolButtonPopupMode popupMode () const */
HB_FUNC( QT_QTOOLBUTTON_POPUPMODE )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      hb_retni( ( QToolButton::ToolButtonPopupMode ) ( p )->popupMode() );
}

/* void setArrowType ( Qt::ArrowType type ) */
HB_FUNC( QT_QTOOLBUTTON_SETARROWTYPE )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      ( p )->setArrowType( ( Qt::ArrowType ) hb_parni( 2 ) );
}

/* void setAutoRaise ( bool enable ) */
HB_FUNC( QT_QTOOLBUTTON_SETAUTORAISE )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      ( p )->setAutoRaise( hb_parl( 2 ) );
}

/* void setMenu ( QMenu * menu ) */
HB_FUNC( QT_QTOOLBUTTON_SETMENU )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      ( p )->setMenu( hbqt_par_QMenu( 2 ) );
}

/* void setPopupMode ( ToolButtonPopupMode mode ) */
HB_FUNC( QT_QTOOLBUTTON_SETPOPUPMODE )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      ( p )->setPopupMode( ( QToolButton::ToolButtonPopupMode ) hb_parni( 2 ) );
}

/* Qt::ToolButtonStyle toolButtonStyle () const */
HB_FUNC( QT_QTOOLBUTTON_TOOLBUTTONSTYLE )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      hb_retni( ( Qt::ToolButtonStyle ) ( p )->toolButtonStyle() );
}

/* void setDefaultAction ( QAction * action ) */
HB_FUNC( QT_QTOOLBUTTON_SETDEFAULTACTION )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      ( p )->setDefaultAction( hbqt_par_QAction( 2 ) );
}

/* void setToolButtonStyle ( Qt::ToolButtonStyle style ) */
HB_FUNC( QT_QTOOLBUTTON_SETTOOLBUTTONSTYLE )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      ( p )->setToolButtonStyle( ( Qt::ToolButtonStyle ) hb_parni( 2 ) );
}

/* void showMenu () */
HB_FUNC( QT_QTOOLBUTTON_SHOWMENU )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      ( p )->showMenu();
}


#endif /* #if QT_VERSION >= 0x040500 */
