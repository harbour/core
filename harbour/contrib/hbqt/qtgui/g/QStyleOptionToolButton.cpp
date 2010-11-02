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
 *  enum StyleOptionType { Type }
 *  enum StyleOptionVersion { Version }
 *  enum ToolButtonFeature { None, Arrow, Menu, PopupDelay, HasMenu, MenuButtonPopup }
 *  flags ToolButtonFeatures
 */

/*
 *  Constructed[ 8/8 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOptionToolButton>


/*
 * QStyleOptionToolButton ()
 * QStyleOptionToolButton ( const QStyleOptionToolButton & other )
 */

typedef struct
{
   QStyleOptionToolButton * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionToolButton;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionToolButton )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QStyleOptionToolButton * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QStyleOptionToolButton( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionToolButton * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionToolButton;
   p->type = HBQT_TYPE_QStyleOptionToolButton;

   return p;
}

HB_FUNC( QT_QSTYLEOPTIONTOOLBUTTON )
{
   QStyleOptionToolButton * pObj = NULL;

   pObj = new QStyleOptionToolButton() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionToolButton( ( void * ) pObj, true ) );
}

/* Qt::ArrowType arrowType */
HB_FUNC( QT_QSTYLEOPTIONTOOLBUTTON_ARROWTYPE )
{
   QStyleOptionToolButton * p = hbqt_par_QStyleOptionToolButton( 1 );
   if( p )
      hb_retni( ( Qt::ArrowType ) ( p )->arrowType );
}

/* ToolButtonFeatures features */
HB_FUNC( QT_QSTYLEOPTIONTOOLBUTTON_FEATURES )
{
   QStyleOptionToolButton * p = hbqt_par_QStyleOptionToolButton( 1 );
   if( p )
      hb_retni( ( QStyleOptionToolButton::ToolButtonFeatures ) ( p )->features );
}

/* QFont font */
HB_FUNC( QT_QSTYLEOPTIONTOOLBUTTON_FONT )
{
   QStyleOptionToolButton * p = hbqt_par_QStyleOptionToolButton( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font ), true ) );
}

/* QIcon icon */
HB_FUNC( QT_QSTYLEOPTIONTOOLBUTTON_ICON )
{
   QStyleOptionToolButton * p = hbqt_par_QStyleOptionToolButton( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon ), true ) );
}

/* QSize iconSize */
HB_FUNC( QT_QSTYLEOPTIONTOOLBUTTON_ICONSIZE )
{
   QStyleOptionToolButton * p = hbqt_par_QStyleOptionToolButton( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->iconSize ), true ) );
}

/* QPoint pos */
HB_FUNC( QT_QSTYLEOPTIONTOOLBUTTON_POS )
{
   QStyleOptionToolButton * p = hbqt_par_QStyleOptionToolButton( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->pos ), true ) );
}

/* QString text */
HB_FUNC( QT_QSTYLEOPTIONTOOLBUTTON_TEXT )
{
   QStyleOptionToolButton * p = hbqt_par_QStyleOptionToolButton( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text.toUtf8().data() );
}

/* Qt::ToolButtonStyle toolButtonStyle */
HB_FUNC( QT_QSTYLEOPTIONTOOLBUTTON_TOOLBUTTONSTYLE )
{
   QStyleOptionToolButton * p = hbqt_par_QStyleOptionToolButton( 1 );
   if( p )
      hb_retni( ( Qt::ToolButtonStyle ) ( p )->toolButtonStyle );
}


#endif /* #if QT_VERSION >= 0x040500 */
