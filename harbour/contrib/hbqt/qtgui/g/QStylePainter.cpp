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
 *  Constructed[ 8/8 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStylePainter>


/*
 * QStylePainter ()
 * QStylePainter ( QWidget * widget )
 * QStylePainter ( QPaintDevice * pd, QWidget * widget )
 */

typedef struct
{
   QStylePainter * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStylePainter;

HBQT_GC_FUNC( hbqt_gcRelease_QStylePainter )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QStylePainter   /.\\", p->ph ) );
         delete ( ( QStylePainter * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QStylePainter   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QStylePainter    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QStylePainter    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStylePainter( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStylePainter * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStylePainter;
   p->type = HBQT_TYPE_QStylePainter;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QStylePainter", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QStylePainter", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSTYLEPAINTER )
{
   QStylePainter * pObj = NULL;

   pObj =  new QStylePainter() ;

   hb_retptrGC( hbqt_gcAllocate_QStylePainter( ( void * ) pObj, true ) );
}

/*
 * bool begin ( QWidget * widget )
 */
HB_FUNC( QT_QSTYLEPAINTER_BEGIN )
{
   QStylePainter * p = hbqt_par_QStylePainter( 1 );
   if( p )
   {
      hb_retl( ( p )->begin( hbqt_par_QWidget( 2 ) ) );
   }
}

/*
 * bool begin ( QPaintDevice * pd, QWidget * widget )
 */
HB_FUNC( QT_QSTYLEPAINTER_BEGIN_1 )
{
   QStylePainter * p = hbqt_par_QStylePainter( 1 );
   if( p )
   {
      hb_retl( ( p )->begin( hbqt_par_QPaintDevice( 2 ), hbqt_par_QWidget( 3 ) ) );
   }
}

/*
 * void drawComplexControl ( QStyle::ComplexControl cc, const QStyleOptionComplex & option )
 */
HB_FUNC( QT_QSTYLEPAINTER_DRAWCOMPLEXCONTROL )
{
   QStylePainter * p = hbqt_par_QStylePainter( 1 );
   if( p )
   {
      ( p )->drawComplexControl( ( QStyle::ComplexControl ) hb_parni( 2 ), *hbqt_par_QStyleOptionComplex( 3 ) );
   }
}

/*
 * void drawControl ( QStyle::ControlElement ce, const QStyleOption & option )
 */
HB_FUNC( QT_QSTYLEPAINTER_DRAWCONTROL )
{
   QStylePainter * p = hbqt_par_QStylePainter( 1 );
   if( p )
   {
      ( p )->drawControl( ( QStyle::ControlElement ) hb_parni( 2 ), *hbqt_par_QStyleOption( 3 ) );
   }
}

/*
 * void drawItemPixmap ( const QRect & rect, int flags, const QPixmap & pixmap )
 */
HB_FUNC( QT_QSTYLEPAINTER_DRAWITEMPIXMAP )
{
   QStylePainter * p = hbqt_par_QStylePainter( 1 );
   if( p )
   {
      ( p )->drawItemPixmap( *hbqt_par_QRect( 2 ), hb_parni( 3 ), *hbqt_par_QPixmap( 4 ) );
   }
}

/*
 * void drawItemText ( const QRect & rect, int flags, const QPalette & pal, bool enabled, const QString & text, QPalette::ColorRole textRole = QPalette::NoRole )
 */
HB_FUNC( QT_QSTYLEPAINTER_DRAWITEMTEXT )
{
   QStylePainter * p = hbqt_par_QStylePainter( 1 );
   if( p )
   {
      void * pText;
      ( p )->drawItemText( *hbqt_par_QRect( 2 ), hb_parni( 3 ), *hbqt_par_QPalette( 4 ), hb_parl( 5 ), hb_parstr_utf8( 6, &pText, NULL ), ( HB_ISNUM( 7 ) ? ( QPalette::ColorRole ) hb_parni( 7 ) : ( QPalette::ColorRole ) QPalette::NoRole ) );
      hb_strfree( pText );
   }
}

/*
 * void drawPrimitive ( QStyle::PrimitiveElement pe, const QStyleOption & option )
 */
HB_FUNC( QT_QSTYLEPAINTER_DRAWPRIMITIVE )
{
   QStylePainter * p = hbqt_par_QStylePainter( 1 );
   if( p )
   {
      ( p )->drawPrimitive( ( QStyle::PrimitiveElement ) hb_parni( 2 ), *hbqt_par_QStyleOption( 3 ) );
   }
}

/*
 * QStyle * style () const
 */
HB_FUNC( QT_QSTYLEPAINTER_STYLE )
{
   QStylePainter * p = hbqt_par_QStylePainter( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStyle( ( p )->style(), false ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
