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
 *  enum CheckType { NotCheckable, Exclusive, NonExclusive }
 *  enum MenuItemType { Normal, DefaultItem, Separator, SubMenu, ..., EmptyArea }
 *  enum StyleOptionType { Type }
 *  enum StyleOptionVersion { Version }
 */

/*
 *  Constructed[ 10/10 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOptionMenuItem>


/*
 * QStyleOptionMenuItem ()
 * QStyleOptionMenuItem ( const QStyleOptionMenuItem & other )
 */

typedef struct
{
   QStyleOptionMenuItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionMenuItem;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionMenuItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QStyleOptionMenuItem * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStyleOptionMenuItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionMenuItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionMenuItem;
   p->type = HBQT_TYPE_QStyleOptionMenuItem;

   return p;
}

HB_FUNC( QT_QSTYLEOPTIONMENUITEM )
{
   QStyleOptionMenuItem * pObj = NULL;

   pObj = new QStyleOptionMenuItem() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionMenuItem( ( void * ) pObj, true ) );
}

/* CheckType checkType */
HB_FUNC( QT_QSTYLEOPTIONMENUITEM_CHECKTYPE )
{
   QStyleOptionMenuItem * p = hbqt_par_QStyleOptionMenuItem( 1 );
   if( p )
      hb_retni( ( QStyleOptionMenuItem::CheckType ) ( p )->checkType );
}

/* bool checked */
HB_FUNC( QT_QSTYLEOPTIONMENUITEM_CHECKED )
{
   QStyleOptionMenuItem * p = hbqt_par_QStyleOptionMenuItem( 1 );
   if( p )
      hb_retl( ( p )->checked );
}

/* QFont font */
HB_FUNC( QT_QSTYLEOPTIONMENUITEM_FONT )
{
   QStyleOptionMenuItem * p = hbqt_par_QStyleOptionMenuItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font ), true ) );
}

/* QIcon icon */
HB_FUNC( QT_QSTYLEOPTIONMENUITEM_ICON )
{
   QStyleOptionMenuItem * p = hbqt_par_QStyleOptionMenuItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon ), true ) );
}

/* int maxIconWidth */
HB_FUNC( QT_QSTYLEOPTIONMENUITEM_MAXICONWIDTH )
{
   QStyleOptionMenuItem * p = hbqt_par_QStyleOptionMenuItem( 1 );
   if( p )
      hb_retni( ( p )->maxIconWidth );
}

/* bool menuHasCheckableItems */
HB_FUNC( QT_QSTYLEOPTIONMENUITEM_MENUHASCHECKABLEITEMS )
{
   QStyleOptionMenuItem * p = hbqt_par_QStyleOptionMenuItem( 1 );
   if( p )
      hb_retl( ( p )->menuHasCheckableItems );
}

/* MenuItemType menuItemType */
HB_FUNC( QT_QSTYLEOPTIONMENUITEM_MENUITEMTYPE )
{
   QStyleOptionMenuItem * p = hbqt_par_QStyleOptionMenuItem( 1 );
   if( p )
      hb_retni( ( QStyleOptionMenuItem::MenuItemType ) ( p )->menuItemType );
}

/* QRect menuRect */
HB_FUNC( QT_QSTYLEOPTIONMENUITEM_MENURECT )
{
   QStyleOptionMenuItem * p = hbqt_par_QStyleOptionMenuItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->menuRect ), true ) );
}

/* int tabWidth */
HB_FUNC( QT_QSTYLEOPTIONMENUITEM_TABWIDTH )
{
   QStyleOptionMenuItem * p = hbqt_par_QStyleOptionMenuItem( 1 );
   if( p )
      hb_retni( ( p )->tabWidth );
}

/* QString text */
HB_FUNC( QT_QSTYLEOPTIONMENUITEM_TEXT )
{
   QStyleOptionMenuItem * p = hbqt_par_QStyleOptionMenuItem( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text.toUtf8().data() );
}


#endif /* #if QT_VERSION >= 0x040500 */
