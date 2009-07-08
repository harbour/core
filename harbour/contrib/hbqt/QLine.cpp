/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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


#include <QtCore/QLine>


/*
 * QLine ()
 * QLine ( const QPoint & p1, const QPoint & p2 )
 * QLine ( int x1, int y1, int x2, int y2 )
 */
HB_FUNC( QT_QLINE )
{
   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      hb_retptr( ( QLine* ) new QLine( *hbqt_par_QLine( 1 ) ) );
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      hb_retptr( ( QLine* ) new QLine( *hbqt_par_QPoint( 1 ), *hbqt_par_QPoint( 2 ) ) );
   }
   else if( hb_pcount() == 4 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) )
   {
      hb_retptr( ( QLine* ) new QLine( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) );
   }
   else
   {
      hb_retptr( ( QLine* ) new QLine() );
   }
}

/*
 * QPoint p1 () const
 */
HB_FUNC( QT_QLINE_P1 )
{
   hb_retptr( new QPoint( hbqt_par_QLine( 1 )->p1() ) );
}

/*
 * QPoint p2 () const
 */
HB_FUNC( QT_QLINE_P2 )
{
   hb_retptr( new QPoint( hbqt_par_QLine( 1 )->p2() ) );
}

/*
 * int x1 () const
 */
HB_FUNC( QT_QLINE_X1 )
{
   hb_retni( hbqt_par_QLine( 1 )->x1() );
}

/*
 * int x2 () const
 */
HB_FUNC( QT_QLINE_X2 )
{
   hb_retni( hbqt_par_QLine( 1 )->x2() );
}

/*
 * int y1 () const
 */
HB_FUNC( QT_QLINE_Y1 )
{
   hb_retni( hbqt_par_QLine( 1 )->y1() );
}

/*
 * int y2 () const
 */
HB_FUNC( QT_QLINE_Y2 )
{
   hb_retni( hbqt_par_QLine( 1 )->y2() );
}

/*
 * int dx () const
 */
HB_FUNC( QT_QLINE_DX )
{
   hb_retni( hbqt_par_QLine( 1 )->dx() );
}

/*
 * int dy () const
 */
HB_FUNC( QT_QLINE_DY )
{
   hb_retni( hbqt_par_QLine( 1 )->dy() );
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QLINE_ISNULL )
{
   hb_retl( hbqt_par_QLine( 1 )->isNull() );
}

/*
 * void setP1 ( const QPoint & p1 )
 */
HB_FUNC( QT_QLINE_SETP1 )
{
   hbqt_par_QLine( 1 )->setP1( *hbqt_par_QPoint( 2 ) );
}

/*
 * void setP2 ( const QPoint & p2 )
 */
HB_FUNC( QT_QLINE_SETP2 )
{
   hbqt_par_QLine( 1 )->setP2( *hbqt_par_QPoint( 2 ) );
}

/*
 * void setLine ( int x1, int y1, int x2, int y2 )
 */
HB_FUNC( QT_QLINE_SETLINE )
{
   hbqt_par_QLine( 1 )->setLine( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/*
 * void setPoints ( const QPoint & p1, const QPoint & p2 )
 */
HB_FUNC( QT_QLINE_SETPOINTS )
{
   hbqt_par_QLine( 1 )->setPoints( *hbqt_par_QPoint( 2 ), *hbqt_par_QPoint( 3 ) );
}

/*
 * void translate ( const QPoint & offset )
 */
HB_FUNC( QT_QLINE_TRANSLATE )
{
   hbqt_par_QLine( 1 )->translate( *hbqt_par_QPoint( 2 ) );
}

/*
 * void translate ( int dx, int dy )
 */
HB_FUNC( QT_QLINE_TRANSLATE_1 )
{
   hbqt_par_QLine( 1 )->translate( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * QLine translated ( const QPoint & offset ) const
 */
HB_FUNC( QT_QLINE_TRANSLATED )
{
   hb_retptr( new QLine( hbqt_par_QLine( 1 )->translated( *hbqt_par_QPoint( 2 ) ) ) );
}

/*
 * QLine translated ( int dx, int dy ) const
 */
HB_FUNC( QT_QLINE_TRANSLATED_1 )
{
   hb_retptr( new QLine( hbqt_par_QLine( 1 )->translated( hb_parni( 2 ), hb_parni( 3 ) ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

