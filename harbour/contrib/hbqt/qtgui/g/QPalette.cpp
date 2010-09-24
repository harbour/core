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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://harbour-project.org
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

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum ColorGroup { Disabled, Active, Inactive, Normal }
 *  enum ColorRole { Window, Background, WindowText, Foreground, ..., NoRole }
 */

/*
 *  Constructed[ 35/35 [ 100.00% ] ]
 *
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

typedef struct
{
   QPalette * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPalette;

HBQT_GC_FUNC( hbqt_gcRelease_QPalette )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QPalette   /.\\", p->ph ) );
         delete ( ( QPalette * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QPalette   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QPalette    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QPalette    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPalette( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QPalette * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPalette;
   p->type = HBQT_TYPE_QPalette;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QPalette", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QPalette", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QPALETTE )
{
   QPalette * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISNUM( 1 ) )
   {
      pObj =  new QPalette( ( Qt::GlobalColor ) hb_parni( 1 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj =  new QPalette( *hbqt_par_QPalette( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj =  new QPalette( *hbqt_par_QColor( 1 ), *hbqt_par_QColor( 2 ) ) ;
   }
   else
   {
      pObj =  new QPalette() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QPalette( ( void * ) pObj, true ) );
}

/*
 * const QBrush & alternateBase () const
 */
HB_FUNC( QT_QPALETTE_ALTERNATEBASE )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->alternateBase() ), true ) );
   }
}

/*
 * const QBrush & base () const
 */
HB_FUNC( QT_QPALETTE_BASE )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->base() ), true ) );
   }
}

/*
 * const QBrush & brightText () const
 */
HB_FUNC( QT_QPALETTE_BRIGHTTEXT )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->brightText() ), true ) );
   }
}

/*
 * const QBrush & brush ( ColorGroup group, ColorRole role ) const
 */
HB_FUNC( QT_QPALETTE_BRUSH )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->brush( ( QPalette::ColorGroup ) hb_parni( 2 ), ( QPalette::ColorRole ) hb_parni( 3 ) ) ), true ) );
   }
}

/*
 * const QBrush & brush ( ColorRole role ) const
 */
HB_FUNC( QT_QPALETTE_BRUSH_1 )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->brush( ( QPalette::ColorRole ) hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * const QBrush & button () const
 */
HB_FUNC( QT_QPALETTE_BUTTON )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->button() ), true ) );
   }
}

/*
 * const QBrush & buttonText () const
 */
HB_FUNC( QT_QPALETTE_BUTTONTEXT )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->buttonText() ), true ) );
   }
}

/*
 * qint64 cacheKey () const
 */
HB_FUNC( QT_QPALETTE_CACHEKEY )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retnint( ( p )->cacheKey() );
   }
}

/*
 * const QColor & color ( ColorGroup group, ColorRole role ) const
 */
HB_FUNC( QT_QPALETTE_COLOR )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->color( ( QPalette::ColorGroup ) hb_parni( 2 ), ( QPalette::ColorRole ) hb_parni( 3 ) ) ), true ) );
   }
}

/*
 * const QColor & color ( ColorRole role ) const
 */
HB_FUNC( QT_QPALETTE_COLOR_1 )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->color( ( QPalette::ColorRole ) hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * ColorGroup currentColorGroup () const
 */
HB_FUNC( QT_QPALETTE_CURRENTCOLORGROUP )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retni( ( QPalette::ColorGroup ) ( p )->currentColorGroup() );
   }
}

/*
 * const QBrush & dark () const
 */
HB_FUNC( QT_QPALETTE_DARK )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->dark() ), true ) );
   }
}

/*
 * const QBrush & highlight () const
 */
HB_FUNC( QT_QPALETTE_HIGHLIGHT )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->highlight() ), true ) );
   }
}

/*
 * const QBrush & highlightedText () const
 */
HB_FUNC( QT_QPALETTE_HIGHLIGHTEDTEXT )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->highlightedText() ), true ) );
   }
}

/*
 * bool isBrushSet ( ColorGroup cg, ColorRole cr ) const
 */
HB_FUNC( QT_QPALETTE_ISBRUSHSET )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retl( ( p )->isBrushSet( ( QPalette::ColorGroup ) hb_parni( 2 ), ( QPalette::ColorRole ) hb_parni( 3 ) ) );
   }
}

/*
 * bool isCopyOf ( const QPalette & p ) const
 */
HB_FUNC( QT_QPALETTE_ISCOPYOF )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retl( ( p )->isCopyOf( *hbqt_par_QPalette( 2 ) ) );
   }
}

/*
 * bool isEqual ( ColorGroup cg1, ColorGroup cg2 ) const
 */
HB_FUNC( QT_QPALETTE_ISEQUAL )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retl( ( p )->isEqual( ( QPalette::ColorGroup ) hb_parni( 2 ), ( QPalette::ColorGroup ) hb_parni( 3 ) ) );
   }
}

/*
 * const QBrush & light () const
 */
HB_FUNC( QT_QPALETTE_LIGHT )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->light() ), true ) );
   }
}

/*
 * const QBrush & link () const
 */
HB_FUNC( QT_QPALETTE_LINK )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->link() ), true ) );
   }
}

/*
 * const QBrush & linkVisited () const
 */
HB_FUNC( QT_QPALETTE_LINKVISITED )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->linkVisited() ), true ) );
   }
}

/*
 * const QBrush & mid () const
 */
HB_FUNC( QT_QPALETTE_MID )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->mid() ), true ) );
   }
}

/*
 * const QBrush & midlight () const
 */
HB_FUNC( QT_QPALETTE_MIDLIGHT )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->midlight() ), true ) );
   }
}

/*
 * QPalette resolve ( const QPalette & other ) const
 */
HB_FUNC( QT_QPALETTE_RESOLVE )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( ( p )->resolve( *hbqt_par_QPalette( 2 ) ) ), true ) );
   }
}

/*
 * void setBrush ( ColorRole role, const QBrush & brush )
 */
HB_FUNC( QT_QPALETTE_SETBRUSH )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      ( p )->setBrush( ( QPalette::ColorRole ) hb_parni( 2 ), *hbqt_par_QBrush( 3 ) );
   }
}

/*
 * void setBrush ( ColorGroup group, ColorRole role, const QBrush & brush )
 */
HB_FUNC( QT_QPALETTE_SETBRUSH_1 )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      ( p )->setBrush( ( QPalette::ColorGroup ) hb_parni( 2 ), ( QPalette::ColorRole ) hb_parni( 3 ), *hbqt_par_QBrush( 4 ) );
   }
}

/*
 * void setColor ( ColorRole role, const QColor & color )
 */
HB_FUNC( QT_QPALETTE_SETCOLOR )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      ( p )->setColor( ( QPalette::ColorRole ) hb_parni( 2 ), *hbqt_par_QColor( 3 ) );
   }
}

/*
 * void setColor ( ColorGroup group, ColorRole role, const QColor & color )
 */
HB_FUNC( QT_QPALETTE_SETCOLOR_1 )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      ( p )->setColor( ( QPalette::ColorGroup ) hb_parni( 2 ), ( QPalette::ColorRole ) hb_parni( 3 ), *hbqt_par_QColor( 4 ) );
   }
}

/*
 * void setColorGroup ( ColorGroup cg, const QBrush & windowText, const QBrush & button, const QBrush & light, const QBrush & dark, const QBrush & mid, const QBrush & text, const QBrush & bright_text, const QBrush & base, const QBrush & window )
 */
HB_FUNC( QT_QPALETTE_SETCOLORGROUP )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      ( p )->setColorGroup( ( QPalette::ColorGroup ) hb_parni( 2 ), *hbqt_par_QBrush( 3 ), *hbqt_par_QBrush( 4 ), *hbqt_par_QBrush( 5 ), *hbqt_par_QBrush( 6 ), *hbqt_par_QBrush( 7 ), *hbqt_par_QBrush( 8 ), *hbqt_par_QBrush( 9 ), *hbqt_par_QBrush( 10 ), *hbqt_par_QBrush( 11 ) );
   }
}

/*
 * void setCurrentColorGroup ( ColorGroup cg )
 */
HB_FUNC( QT_QPALETTE_SETCURRENTCOLORGROUP )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      ( p )->setCurrentColorGroup( ( QPalette::ColorGroup ) hb_parni( 2 ) );
   }
}

/*
 * const QBrush & shadow () const
 */
HB_FUNC( QT_QPALETTE_SHADOW )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->shadow() ), true ) );
   }
}

/*
 * const QBrush & text () const
 */
HB_FUNC( QT_QPALETTE_TEXT )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->text() ), true ) );
   }
}

/*
 * const QBrush & toolTipBase () const
 */
HB_FUNC( QT_QPALETTE_TOOLTIPBASE )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->toolTipBase() ), true ) );
   }
}

/*
 * const QBrush & toolTipText () const
 */
HB_FUNC( QT_QPALETTE_TOOLTIPTEXT )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->toolTipText() ), true ) );
   }
}

/*
 * const QBrush & window () const
 */
HB_FUNC( QT_QPALETTE_WINDOW )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->window() ), true ) );
   }
}

/*
 * const QBrush & windowText () const
 */
HB_FUNC( QT_QPALETTE_WINDOWTEXT )
{
   QPalette * p = hbqt_par_QPalette( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->windowText() ), true ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
