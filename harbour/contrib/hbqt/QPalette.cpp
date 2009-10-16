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
#include "hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum ColorGroup { Disabled, Active, Inactive, Normal }
 *  enum ColorRole { Window, Background, WindowText, Foreground, ..., NoRole }
 */

#include <QtCore/QPointer>

#include <QtGui/QPalette>


/*
 * QPalette ()
 * QPalette ( const QColor & button )
 * QPalette ( Qt::GlobalColor button )
 * QPalette ( const QColor & button, const QColor & window )
 * QPalette ( const QBrush & windowText, const QBrush & button, const QBrush & light, const QBrush & dark, const QBrush & mid, const QBrush & text, const QBrush & bright_text, const QBrush & base, const QBrush & window )
 * QPalette ( const QPalette & p )
 * ~QPalette ()
 */

HB_FUNC( QT_QPALETTE )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAlloc( sizeof( QGC_POINTER ), Q_release );
   void * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISNUM( 1 ) )
   {
      pObj = ( QPalette* ) new QPalette( ( Qt::GlobalColor ) hb_parni( 1 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = ( QPalette* ) new QPalette( *hbqt_par_QPalette( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = ( QPalette* ) new QPalette( *hbqt_par_QColor( 1 ), *hbqt_par_QColor( 2 ) ) ;
   }
   else
   {
      pObj = ( QPalette* ) new QPalette() ;
   }

   p->ph = pObj;
   p->type = hbqt_getIdByName( ( QString ) "QPalette" );
   hb_retptrGC( p );
}
/*
 * const QBrush & alternateBase () const
 */
HB_FUNC( QT_QPALETTE_ALTERNATEBASE )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QPalette( 1 )->alternateBase() ) ) );
}

/*
 * const QBrush & base () const
 */
HB_FUNC( QT_QPALETTE_BASE )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QPalette( 1 )->base() ) ) );
}

/*
 * const QBrush & brightText () const
 */
HB_FUNC( QT_QPALETTE_BRIGHTTEXT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QPalette( 1 )->brightText() ) ) );
}

/*
 * const QBrush & brush ( ColorGroup group, ColorRole role ) const
 */
HB_FUNC( QT_QPALETTE_BRUSH )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QPalette( 1 )->brush( ( QPalette::ColorGroup ) hb_parni( 2 ), ( QPalette::ColorRole ) hb_parni( 3 ) ) ) ) );
}

/*
 * const QBrush & brush ( ColorRole role ) const
 */
HB_FUNC( QT_QPALETTE_BRUSH_1 )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QPalette( 1 )->brush( ( QPalette::ColorRole ) hb_parni( 2 ) ) ) ) );
}

/*
 * const QBrush & button () const
 */
HB_FUNC( QT_QPALETTE_BUTTON )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QPalette( 1 )->button() ) ) );
}

/*
 * const QBrush & buttonText () const
 */
HB_FUNC( QT_QPALETTE_BUTTONTEXT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QPalette( 1 )->buttonText() ) ) );
}

/*
 * qint64 cacheKey () const
 */
HB_FUNC( QT_QPALETTE_CACHEKEY )
{
   hb_retnint( hbqt_par_QPalette( 1 )->cacheKey() );
}

/*
 * const QColor & color ( ColorGroup group, ColorRole role ) const
 */
HB_FUNC( QT_QPALETTE_COLOR )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QColor( hbqt_par_QPalette( 1 )->color( ( QPalette::ColorGroup ) hb_parni( 2 ), ( QPalette::ColorRole ) hb_parni( 3 ) ) ) ) );
}

/*
 * const QColor & color ( ColorRole role ) const
 */
HB_FUNC( QT_QPALETTE_COLOR_1 )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QColor( hbqt_par_QPalette( 1 )->color( ( QPalette::ColorRole ) hb_parni( 2 ) ) ) ) );
}

/*
 * ColorGroup currentColorGroup () const
 */
HB_FUNC( QT_QPALETTE_CURRENTCOLORGROUP )
{
   hb_retni( ( QPalette::ColorGroup ) hbqt_par_QPalette( 1 )->currentColorGroup() );
}

/*
 * const QBrush & dark () const
 */
HB_FUNC( QT_QPALETTE_DARK )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QPalette( 1 )->dark() ) ) );
}

/*
 * const QBrush & highlight () const
 */
HB_FUNC( QT_QPALETTE_HIGHLIGHT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QPalette( 1 )->highlight() ) ) );
}

/*
 * const QBrush & highlightedText () const
 */
HB_FUNC( QT_QPALETTE_HIGHLIGHTEDTEXT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QPalette( 1 )->highlightedText() ) ) );
}

/*
 * bool isBrushSet ( ColorGroup cg, ColorRole cr ) const
 */
HB_FUNC( QT_QPALETTE_ISBRUSHSET )
{
   hb_retl( hbqt_par_QPalette( 1 )->isBrushSet( ( QPalette::ColorGroup ) hb_parni( 2 ), ( QPalette::ColorRole ) hb_parni( 3 ) ) );
}

/*
 * bool isCopyOf ( const QPalette & p ) const
 */
HB_FUNC( QT_QPALETTE_ISCOPYOF )
{
   hb_retl( hbqt_par_QPalette( 1 )->isCopyOf( *hbqt_par_QPalette( 2 ) ) );
}

/*
 * bool isEqual ( ColorGroup cg1, ColorGroup cg2 ) const
 */
HB_FUNC( QT_QPALETTE_ISEQUAL )
{
   hb_retl( hbqt_par_QPalette( 1 )->isEqual( ( QPalette::ColorGroup ) hb_parni( 2 ), ( QPalette::ColorGroup ) hb_parni( 3 ) ) );
}

/*
 * const QBrush & light () const
 */
HB_FUNC( QT_QPALETTE_LIGHT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QPalette( 1 )->light() ) ) );
}

/*
 * const QBrush & link () const
 */
HB_FUNC( QT_QPALETTE_LINK )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QPalette( 1 )->link() ) ) );
}

/*
 * const QBrush & linkVisited () const
 */
HB_FUNC( QT_QPALETTE_LINKVISITED )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QPalette( 1 )->linkVisited() ) ) );
}

/*
 * const QBrush & mid () const
 */
HB_FUNC( QT_QPALETTE_MID )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QPalette( 1 )->mid() ) ) );
}

/*
 * const QBrush & midlight () const
 */
HB_FUNC( QT_QPALETTE_MIDLIGHT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QPalette( 1 )->midlight() ) ) );
}

/*
 * QPalette resolve ( const QPalette & other ) const
 */
HB_FUNC( QT_QPALETTE_RESOLVE )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QPalette( hbqt_par_QPalette( 1 )->resolve( *hbqt_par_QPalette( 2 ) ) ) ) );
}

/*
 * void setBrush ( ColorRole role, const QBrush & brush )
 */
HB_FUNC( QT_QPALETTE_SETBRUSH )
{
   hbqt_par_QPalette( 1 )->setBrush( ( QPalette::ColorRole ) hb_parni( 2 ), *hbqt_par_QBrush( 3 ) );
}

/*
 * void setBrush ( ColorGroup group, ColorRole role, const QBrush & brush )
 */
HB_FUNC( QT_QPALETTE_SETBRUSH_1 )
{
   hbqt_par_QPalette( 1 )->setBrush( ( QPalette::ColorGroup ) hb_parni( 2 ), ( QPalette::ColorRole ) hb_parni( 3 ), *hbqt_par_QBrush( 4 ) );
}

/*
 * void setColor ( ColorRole role, const QColor & color )
 */
HB_FUNC( QT_QPALETTE_SETCOLOR )
{
   hbqt_par_QPalette( 1 )->setColor( ( QPalette::ColorRole ) hb_parni( 2 ), *hbqt_par_QColor( 3 ) );
}

/*
 * void setColor ( ColorGroup group, ColorRole role, const QColor & color )
 */
HB_FUNC( QT_QPALETTE_SETCOLOR_1 )
{
   hbqt_par_QPalette( 1 )->setColor( ( QPalette::ColorGroup ) hb_parni( 2 ), ( QPalette::ColorRole ) hb_parni( 3 ), *hbqt_par_QColor( 4 ) );
}

/*
 * void setColorGroup ( ColorGroup cg, const QBrush & windowText, const QBrush & button, const QBrush & light, const QBrush & dark, const QBrush & mid, const QBrush & text, const QBrush & bright_text, const QBrush & base, const QBrush & window )
 */
HB_FUNC( QT_QPALETTE_SETCOLORGROUP )
{
   hbqt_par_QPalette( 1 )->setColorGroup( ( QPalette::ColorGroup ) hb_parni( 2 ), *hbqt_par_QBrush( 3 ), *hbqt_par_QBrush( 4 ), *hbqt_par_QBrush( 5 ), *hbqt_par_QBrush( 6 ), *hbqt_par_QBrush( 7 ), *hbqt_par_QBrush( 8 ), *hbqt_par_QBrush( 9 ), *hbqt_par_QBrush( 10 ), *hbqt_par_QBrush( 11 ) );
}

/*
 * void setCurrentColorGroup ( ColorGroup cg )
 */
HB_FUNC( QT_QPALETTE_SETCURRENTCOLORGROUP )
{
   hbqt_par_QPalette( 1 )->setCurrentColorGroup( ( QPalette::ColorGroup ) hb_parni( 2 ) );
}

/*
 * const QBrush & shadow () const
 */
HB_FUNC( QT_QPALETTE_SHADOW )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QPalette( 1 )->shadow() ) ) );
}

/*
 * const QBrush & text () const
 */
HB_FUNC( QT_QPALETTE_TEXT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QPalette( 1 )->text() ) ) );
}

/*
 * const QBrush & toolTipBase () const
 */
HB_FUNC( QT_QPALETTE_TOOLTIPBASE )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QPalette( 1 )->toolTipBase() ) ) );
}

/*
 * const QBrush & toolTipText () const
 */
HB_FUNC( QT_QPALETTE_TOOLTIPTEXT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QPalette( 1 )->toolTipText() ) ) );
}

/*
 * const QBrush & window () const
 */
HB_FUNC( QT_QPALETTE_WINDOW )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QPalette( 1 )->window() ) ) );
}

/*
 * const QBrush & windowText () const
 */
HB_FUNC( QT_QPALETTE_WINDOWTEXT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QPalette( 1 )->windowText() ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
