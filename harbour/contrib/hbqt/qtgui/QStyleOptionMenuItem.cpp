/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/

#include "hbapi.h"
#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum CheckType { NotCheckable, Exclusive, NonExclusive }
 *  enum MenuItemType { Normal, DefaultItem, Separator, SubMenu, ..., EmptyArea }
 *  enum StyleOptionType { Type }
 *  enum StyleOptionVersion { Version }
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOptionMenuItem>


/*
 * QStyleOptionMenuItem ()
 * QStyleOptionMenuItem ( const QStyleOptionMenuItem & other )
 */

QT_G_FUNC( release_QStyleOptionMenuItem )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QStyleOptionMenuItem         p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "release_QStyleOptionMenuItem        ph=%p", p->ph ) );

   if( p && p->ph )
   {
      ( ( QStyleOptionMenuItem * ) p->ph )->~QStyleOptionMenuItem();
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "release_QStyleOptionMenuItem        Object deleted!" ) );
      #if defined(__debug__)
         just_debug( "  YES release_QStyleOptionMenuItem        %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
      #endif
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "release_QStyleOptionMenuItem        Object Allready deleted!" ) );
      #if defined(__debug__)
         just_debug( "  DEL release_QStyleOptionMenuItem" );
      #endif
   }
}

void * gcAllocate_QStyleOptionMenuItem( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QStyleOptionMenuItem;
   #if defined(__debug__)
      just_debug( "          new_QStyleOptionMenuItem        %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
   #endif
   return( p );
}

HB_FUNC( QT_QSTYLEOPTIONMENUITEM )
{
   void * pObj = NULL;

   pObj = ( QStyleOptionMenuItem* ) new QStyleOptionMenuItem() ;

   hb_retptrGC( gcAllocate_QStyleOptionMenuItem( pObj ) );
}
/*
 * CheckType checkType
 */
HB_FUNC( QT_QSTYLEOPTIONMENUITEM_CHECKTYPE )
{
   hb_retni( ( QStyleOptionMenuItem::CheckType ) hbqt_par_QStyleOptionMenuItem( 1 )->checkType );
}

/*
 * bool checked
 */
HB_FUNC( QT_QSTYLEOPTIONMENUITEM_CHECKED )
{
   hb_retl( hbqt_par_QStyleOptionMenuItem( 1 )->checked );
}

/*
 * QFont font
 */
HB_FUNC( QT_QSTYLEOPTIONMENUITEM_FONT )
{
   hb_retptrGC( gcAllocate_QFont( new QFont( hbqt_par_QStyleOptionMenuItem( 1 )->font ) ) );
}

/*
 * QIcon icon
 */
HB_FUNC( QT_QSTYLEOPTIONMENUITEM_ICON )
{
   hb_retptrGC( gcAllocate_QIcon( new QIcon( hbqt_par_QStyleOptionMenuItem( 1 )->icon ) ) );
}

/*
 * int maxIconWidth
 */
HB_FUNC( QT_QSTYLEOPTIONMENUITEM_MAXICONWIDTH )
{
   hb_retni( hbqt_par_QStyleOptionMenuItem( 1 )->maxIconWidth );
}

/*
 * bool menuHasCheckableItems
 */
HB_FUNC( QT_QSTYLEOPTIONMENUITEM_MENUHASCHECKABLEITEMS )
{
   hb_retl( hbqt_par_QStyleOptionMenuItem( 1 )->menuHasCheckableItems );
}

/*
 * MenuItemType menuItemType
 */
HB_FUNC( QT_QSTYLEOPTIONMENUITEM_MENUITEMTYPE )
{
   hb_retni( ( QStyleOptionMenuItem::MenuItemType ) hbqt_par_QStyleOptionMenuItem( 1 )->menuItemType );
}

/*
 * QRect menuRect
 */
HB_FUNC( QT_QSTYLEOPTIONMENUITEM_MENURECT )
{
   hb_retptrGC( gcAllocate_QRect( new QRect( hbqt_par_QStyleOptionMenuItem( 1 )->menuRect ) ) );
}

/*
 * int tabWidth
 */
HB_FUNC( QT_QSTYLEOPTIONMENUITEM_TABWIDTH )
{
   hb_retni( hbqt_par_QStyleOptionMenuItem( 1 )->tabWidth );
}

/*
 * QString text
 */
HB_FUNC( QT_QSTYLEOPTIONMENUITEM_TEXT )
{
   hb_retc( hbqt_par_QStyleOptionMenuItem( 1 )->text.toLatin1().data() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
