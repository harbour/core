/*
 * Harbour Project source code:
 *    QT Console
 * Copyright 2013 Przemyslaw Czerpak <druzus /at/ priv.onet.pl>
 * Slightly based on GTQTC
 * Copyright 2009-2011 Pritpal Bedi <pritpal@vouchcac.com>
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
 * along with this software; see the file COPYING.txt   If not, write to
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

#include "gtqtc.h"

static  int             s_GtId;
static  HB_GT_FUNCS     SuperTable;
#define HB_GTSUPER      (&SuperTable)
#define HB_GTID_PTR     (&s_GtId)

#define HB_GTQTC_GET(p) ( ( PHB_GTQTC ) HB_GTLOCAL( p ) )


#if defined( HB_OS_UNIX )
#  if !defined( HB_QT_NEEDLOCKS )
#     define HB_QT_NEEDLOCKS
#  endif
#  if !defined( HB_XLIB_NEEDLOCKS )
/* #     define HB_XLIB_NEEDLOCKS */
#  endif
#endif

#ifdef HB_QT_NEEDLOCKS
   static QMutex s_qMtx( QMutex::Recursive );
#  define HB_QTC_LOCK()       do { s_qMtx.lock()
#  define HB_QTC_UNLOCK()     s_qMtx.unlock(); } while( 0 )
#else
#  define HB_QTC_LOCK()       do {} while( 0 )
#  define HB_QTC_UNLOCK()     do {} while( 0 )
#endif

static QApplication * s_qtapp = NULL;

/* *********************************************************************** */

static void hb_gt_qtc_appFree( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   s_qtapp->quit();
   delete s_qtapp;
   s_qtapp = NULL;
}

static void hb_gt_qtc_itemGetQString( PHB_ITEM pItem, QString * pqStr )
{
   const HB_WCHAR * wStr;
   HB_SIZE nSize;
   void * hStr;

   if( ( wStr = hb_itemGetStrU16( pItem, HB_CDP_ENDIAN_NATIVE, &hStr, &nSize ) ) != NULL )
   {
      * pqStr = QString::fromUtf16( ( const ushort * ) wStr, nSize );
      hb_strfree( hStr );
   }
}

static PHB_ITEM hb_gt_qtc_itemPutQString( PHB_ITEM pItem, const QString * pqStr )
{
   return hb_itemPutStrLenU16( pItem, HB_CDP_ENDIAN_NATIVE,
                               ( const HB_WCHAR * ) pqStr->constData(),
                               pqStr->size() );
}

/* *********************************************************************** */

#define hb_bm_paint_begin( qBitMap )         do { \
            QPainter painter( qBitMap ); \
            painter.setBackgroundMode( Qt::OpaqueMode ); \
            painter.setBrush( Qt::SolidPattern ); \
            painter.setPen( Qt::SolidLine )

#define hb_bm_paint_end()           } while( 0 )

#define hb_bm_color( c )                  painter.setPen( QTC_NUM2RGB( c ) )
#define hb_bm_rect( x, y, w, h )          painter.drawRect( x, y, w, h )
#define hb_bm_fillrectc( x, y, w, h, c )  painter.fillRect( x, y, w, h, QTC_NUM2RGB( c ) )

#define hb_bm_line( x1, y1, x2, y2 )      painter.drawLine( x1, y1, x2, y2 )
#define hb_bm_point( x, y )               painter.drawPoint( x, y )
#define hb_bm_fillrect( x, y, w, h )      painter.fillRect( x, y, w, h, Qt::color1 )
#define hb_bm_polygon( pts, n )           painter.drawPolygon( pts, n )
#define hb_bm_invertrect( x, y, w, h )    do { \
               painter.setCompositionMode( QPainter::RasterOp_SourceXorDestination ); \
               painter.fillRect( 0, 0, w, h, Qt::color1 ); \
            } while( 0 )
#define hb_bm_text( ch )                  do { \
               QPainter painter( qBitMap ); \
               painter.setBackgroundMode( Qt::OpaqueMode ); \
               painter.setFont( QFont( pQTC->qWnd->qConsole->font, painter.device() ) ); \
               painter.drawText( 0, pQTC->fontAscent, QString( ch ) ); \
            } while( 0 )

static QBitmap * hb_gt_qtc_bitmap_char( int cellx, int celly )
{
   QBitmap * qBitMap = new QBitmap( cellx, celly );
   qBitMap->clear();
   return qBitMap;
}

static QBitmap * hb_gt_qtc_defineBoxButtonL( int cellx, int celly )
{
   QBitmap * qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );

   hb_bm_paint_begin( qBitMap );

   hb_bm_line( cellx - 1, 0, 0, 0 );
   hb_bm_line( 0, 0, 0, celly - 1 );
   hb_bm_line( 0, celly - 1, cellx - 1, celly - 1 );

   hb_bm_line( 2, celly - 2, cellx - 1, celly - 2 );

   hb_bm_paint_end();

   return qBitMap;
}

static QBitmap * hb_gt_qtc_defineBoxButtonR( int cellx, int celly )
{
   QBitmap * qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );

   hb_bm_paint_begin( qBitMap );

   hb_bm_line( 0, 0, cellx - 1, 0 );
   hb_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );
   hb_bm_line( cellx - 1, celly - 1, 0, celly - 1 );

   hb_bm_line( cellx - 2, 3, cellx - 2, celly - 2 );
   hb_bm_line( cellx - 2, celly - 2, 0, celly - 2 );

   hb_bm_paint_end();

   return qBitMap;
}

static QBitmap * hb_gt_qtc_defineBoxChar( PHB_GTQTC pQTC, HB_USHORT usCh )
{
   QBitmap * qBitMap = NULL;
   int cellx = pQTC->cellX;
   int celly = pQTC->cellY;
   int i, y, x, yy, xx, skip, start, mod;
   QPoint pts[ 3 ];

   if( usCh >= HB_BOXCH_RC_MIN && usCh <= HB_BOXCH_RC_MAX )
      switch( usCh )
      {
         case HB_BOXCH_RC_ARROW_DL:
            qBitMap = hb_gt_qtc_defineBoxButtonL( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            yy = celly / 2 - 1;
            for( y = celly - 4, x = cellx - 1; x >= 3 && y >= yy; --x, --y )
               hb_bm_line( x, y, cellx - 1, y );
            xx = HB_MAX( cellx * 2 / 5, 3 ) | 1;
            for( x = cellx - xx / 2 - 1; y >= 3; --y )
               hb_bm_line( x, y, cellx - 1, y );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_ARROW_DR:
            qBitMap = hb_gt_qtc_defineBoxButtonR( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            yy = ( celly + 1 ) / 2;
            for( y = celly - 5, x = 0; x < cellx - 4 && y >= yy; ++x, --y )
               hb_bm_line( 0, y, x, y );
            xx = HB_MAX( cellx * 2 / 5, 3 ) | 1;
            for( x = xx / 2 - 1; y >= 3; --y )
               hb_bm_line( 0, y, x, y );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_ARROW_UL:
            qBitMap = hb_gt_qtc_defineBoxButtonL( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            yy = ( celly + 1 ) / 2;
            for( y = 3, x = cellx - 1; x >= 3 && y <= yy; --x, ++y )
               hb_bm_line( x, y, cellx - 1, y );
            xx = HB_MAX( cellx * 2 / 5, 3 ) | 1;
            for( x = cellx - xx / 2 - 1; y < celly - 3; ++y )
               hb_bm_line( x, y, cellx - 1, y );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_ARROW_UR:
            qBitMap = hb_gt_qtc_defineBoxButtonR( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            yy = ( celly + 1 ) / 2;
            for( y = 4, x = 0; x < cellx - 4 && y <= yy; ++x, ++y )
               hb_bm_line( 0, y, x, y );
            xx = HB_MAX( cellx * 2 / 5, 3 ) | 1;
            for( x = xx / 2 - 1; y < celly - 3; ++y )
               hb_bm_line( 0, y, x, y );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_ARROW_VL:
            qBitMap = hb_gt_qtc_defineBoxButtonL( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            yy = ( celly - 1 ) / 2;
            for( y = 3, x = cellx - 1; x >= 3 && y < yy; --x, ++y )
               hb_bm_line( x, y, cellx - 1, y );
            for( y = yy + 2, ++x; x <= cellx - 1 && y < celly - 3; ++x, ++y )
               hb_bm_line( x, y, cellx - 1, y );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_ARROW_VR:
            qBitMap = hb_gt_qtc_defineBoxButtonR( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            yy = ( celly - 1 ) / 2;
            for( y = 4, x = 0; x < cellx - 4 && y < yy; ++x, ++y )
               hb_bm_line( 0, y, x, y );
            for( y = yy + 2, --x; x >= 0 && y < celly - 3; --x, ++y )
               hb_bm_line( 0, y, x, y );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_BUTTON_L:
            qBitMap = hb_gt_qtc_defineBoxButtonL( cellx, celly );
            break;

         case HB_BOXCH_RC_BUTTON_R:
            qBitMap = hb_gt_qtc_defineBoxButtonR( cellx, celly );
            break;

         case HB_BOXCH_RC_ARROW_LL:
            qBitMap = hb_gt_qtc_defineBoxButtonL( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            yy = ( celly - 1 ) / 2;
            for( x = 3, y = 0; x < cellx; ++x, ++y )
               hb_bm_line( x, yy - y, x, yy + y );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_ARROW_LR:
            qBitMap = hb_gt_qtc_defineBoxButtonR( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            yy = HB_MAX( celly / 5, 3 ) | 1;
            for( y = ( celly - yy ) / 2; yy--; ++y )
               hb_bm_line( 0, y, cellx - 4, y );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_ARROW_RL:
            qBitMap = hb_gt_qtc_defineBoxButtonL( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            yy = HB_MAX( celly / 5, 3 ) | 1;
            for( y = ( celly - yy ) / 2; yy--; ++y )
               hb_bm_line( 3, y, cellx - 1, y );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_ARROW_RR:
            qBitMap = hb_gt_qtc_defineBoxButtonR( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            yy = ( celly - 1 ) / 2;
            for( x = cellx - 4, y = 0; x >= 0; --x, ++y )
               hb_bm_line( x, yy - y, x, yy + y );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_ENTER1:
            /* TODO */
            break;
         case HB_BOXCH_RC_ENTER2:
            /* TODO */
            break;
         case HB_BOXCH_RC_ENTER3:
            /* TODO */
            break;

         case HB_BOXCH_RC_VSCRL_LD:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );

            hb_bm_line( 0, 0, 0, celly - 1 );
            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            hb_bm_line( 2, celly / 2 - 1, cellx - 1, celly / 2 - 1 );

            for( y = celly / 2 + 1; y < celly; y++ )
            {
               for( x = ( y & 1 ) + 2; x < cellx; x += 2 )
                  hb_bm_point( x, y );
            }
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_VSCRL_RD:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );

            hb_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );
            hb_bm_line( cellx - 2, 0, cellx - 2, celly / 2 - 1 );
            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            hb_bm_line( 0, celly / 2 - 1, cellx - 1, celly / 2 - 1 );

            for( y = celly / 2 + 1; y < celly; y++ )
            {
               for( x = ( y ^ cellx ) & 1; x < cellx - 2; x += 2 )
                  hb_bm_point( x, y );
            }
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_VSCRL_LU:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );

            hb_bm_line( 0, 0, 0, celly - 1 );
            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );

            for( y = 0; y < celly / 2; y++ )
            {
               for( x = ( y & 1 ) + 2; x < cellx; x += 2 )
                  hb_bm_point( x, y );
            }
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_VSCRL_RU:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );

            hb_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );
            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            hb_bm_line( cellx - 2, celly / 2 + 3, cellx - 2, celly - 1 );

            for( y = 0; y < celly / 2; y++ )
            {
               for( x = ( y ^ cellx ) & 1; x < cellx - 2; x += 2 )
                  hb_bm_point( x, y );
            }
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_VSCRL_L:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );

            hb_bm_line( 0, 0, 0, celly - 1 );

            for( y = 0; y < celly; y++ )
            {
               for( x = ( y & 1 ) + 2; x < cellx; x += 2 )
                  hb_bm_point( x, y );
            }
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_VSCRL_R:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );

            hb_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );

            for( y = 0; y < celly; y++ )
            {
               for( x = ( y ^ cellx ) & 1; x < cellx - 2; x += 2 )
                  hb_bm_point( x, y );
            }
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_HSCRL:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );

            hb_bm_line( 0, 0, cellx - 1, 0 );
            hb_bm_line( 0, celly - 1, cellx - 1, celly - 1 );

            for( y = 2; y < celly - 2; y++ )
            {
               for( x = y & 1; x < cellx; x += 2 )
                  hb_bm_point( x, y );
            }
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_0:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_text( "0" );
            break;

         case HB_BOXCH_RC_1:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_text( "1" );
            break;

         case HB_BOXCH_RC_2:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_text( "2" );
            break;

         case HB_BOXCH_RC_3:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_text( "3" );
            break;

         case HB_BOXCH_RC_4:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_text( "4" );
            break;

         case HB_BOXCH_RC_5:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_text( "5" );
            break;

         case HB_BOXCH_RC_6:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_text( "6" );
            break;

         case HB_BOXCH_RC_7:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_text( "7" );
            break;

         case HB_BOXCH_RC_8:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_text( "8" );
            break;

         case HB_BOXCH_RC_9:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_text( "9" );
            break;

         case HB_BOXCH_RC_DOT:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_text( "." );
            break;

         case HB_BOXCH_RC_ACC:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_text( "'" );
            break;

         case HB_BOXCH_RC_BOX_ML:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            hb_bm_line( 0, 0, 0, celly - 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_BOX_MR:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            hb_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_HWND_L:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx - 1, 0, 0, 0 );
            hb_bm_line( 0, 0, 0, celly - 1 );
            hb_bm_line( 0, celly - 1, cellx - 1, celly - 1 );
            hb_bm_line( cellx - 1, celly / 4 + 2, cellx / 4 + 1, celly / 4 + 2 );
            hb_bm_line( cellx / 4 + 1, celly / 4 + 2, cellx / 4 + 1, celly - 4 - celly / 4 );
            hb_bm_line( cellx / 4 + 1, celly - 4 - celly / 4, cellx - 1, celly - 4 - celly / 4 );
            hb_bm_line( cellx / 4 + 2, celly - 3 - celly / 4, cellx - 1, celly - 3 - celly / 4 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_HWND_R:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( 0, 0, cellx - 1, 0 );
            hb_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );
            hb_bm_line( cellx - 1, celly - 1, 0, celly - 1 );
            hb_bm_line( 0, celly / 4 + 2, cellx - cellx / 4 - 2, celly / 4 + 2 );
            hb_bm_line( cellx - cellx / 4 - 2, celly / 4 + 2, cellx - cellx / 4 - 2, celly - 4 - celly / 4 );
            hb_bm_line( cellx - cellx / 4 - 2, celly - 4 - celly / 4, 0, celly - 4 - celly / 4 );
            hb_bm_line( 0, celly - 3 - celly / 4, cellx - cellx / 4 - 1, celly - 3 - celly / 4 );
            hb_bm_line( cellx - cellx / 4 - 1, celly - 3 - celly / 4, cellx - cellx / 4 - 1, celly / 4 + 2 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_BOX_TL:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( 0, 0, cellx - 1, 0 );
            hb_bm_line( 0, 0, 0, celly - 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_BOX_T:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( 0, 0, cellx - 1, 0 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_BOX_TR:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( 0, 0, cellx - 1, 0 );
            hb_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_BOX_R:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_BOX_BR:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );
            hb_bm_line( 0, celly - 1, cellx - 1, celly - 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_BOX_B:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( 0, celly - 1, cellx - 1, celly - 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_BOX_BL:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( 0, 0, 0, celly - 1 );
            hb_bm_line( 0, celly - 1, cellx - 1, celly - 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_BOX_L:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( 0, 0, 0, celly - 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_BOX_MT:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            hb_bm_line( 0, 0, cellx - 1, 0 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_BOX_MB:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            hb_bm_line( 0, celly - 1, cellx - 1, celly - 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_BUTTON_CL:
            qBitMap = hb_gt_qtc_defineBoxButtonL( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            yy = celly - 2 / 3;
            xx = cellx - 4;
            if( yy > xx )
               yy = xx;
            xx = ( xx * 2 + 1 ) / 3;
            if( xx < 2 )
               xx = 2;
            for( y = celly - yy - 3 - xx, i = 0; i < xx; ++y, ++i )
               hb_bm_line( 3, y, 3 + yy - 1, y + yy - 1 );
            y = celly - 5 - xx;
            hb_bm_line( cellx - 1, y, cellx - 1, y + xx - 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_BUTTON_CR:
            qBitMap = hb_gt_qtc_defineBoxButtonR( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            yy = celly - 2 / 3;
            xx = cellx - 4;
            if( yy > xx )
               yy = xx;
            xx = ( xx * 2 + 1 ) / 3;
            if( xx < 2 )
               xx = 2;
            for( y = celly - 6 - xx, i = 0; i < xx; ++y, ++i )
               hb_bm_line( 0, y, yy, y - yy );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_FARROW_DL:
            qBitMap = hb_gt_qtc_defineBoxButtonL( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            yy = ( celly - cellx ) / 2 + 1;
            yy = HB_MAX( yy, 2 );
            for( y = celly - yy - 1, x = cellx - 1; x >= 2 && y >= 3; --x, --y )
               hb_bm_line( x, y, cellx - 1, y );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_FARROW_DR:
            qBitMap = hb_gt_qtc_defineBoxButtonR( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            yy = ( celly - cellx ) / 2 + 1;
            yy = HB_MAX( yy, 2 );
            for( y = celly - yy - 2, x = 0; x < cellx - 3 && y >= 3; ++x, --y )
               hb_bm_line( 0, y, x, y );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_DOTS:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );

            for( x = 1; x < cellx; x += 2 )
               hb_bm_point( x, celly / 2 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_DOTS_L:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );

            i = cellx / 2;
            xx = i - i / 2;
            yy = HB_MAX( 2, xx - 1 );

            hb_bm_fillrect( cellx - xx / 2 - i, celly / 3 * 2, xx    , yy );
            hb_bm_fillrect( cellx - xx / 2    , celly / 3 * 2, xx / 2, yy );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_RC_DOTS_R:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );

            i = cellx / 2;
            xx = i - i / 2;
            yy = HB_MAX( 2, xx - 1 );

            hb_bm_fillrect( 0         , celly / 3 * 2, xx - xx / 2, yy );
            hb_bm_fillrect( i - xx / 2, celly / 3 * 2, xx         , yy );
            hb_bm_paint_end();
            break;
      }
   else
      switch( usCh )
      {
         case HB_BOXCH_FILLER1:
         case HB_BOXCH_FILLER2:
         case HB_BOXCH_FILLER3:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );

            if( usCh == HB_BOXCH_FILLER1 )
            {
               skip = 4;
               start = mod = 1;
            }
            else if( usCh == HB_BOXCH_FILLER2 )
            {
               skip = 2;
               start = 0;
               mod = 1;
            }
            else
            {
               skip = 4;
               start = mod = 0;
            }
            for( y = 0; y < celly; y++ )
            {
               for( x = start + ( skip >> 1 ) * ( ( y & 1 ) ^ mod ); x < cellx; x += skip )
                  hb_bm_point( x, y );
            }
            if( usCh == HB_BOXCH_FILLER3 )
               hb_bm_invertrect( 0, 0, cellx, celly );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_ARROW_R:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );

            i = HB_MIN( ( celly >> 1 ), cellx ) - 3;
            pts[ 0 ].setX( ( cellx - i ) >> 1 );
            pts[ 0 ].setY( ( celly >> 1 ) - i );
            pts[ 1 ].setX( pts[ 0 ].x() + i );
            pts[ 1 ].setY( pts[ 0 ].y() + i );
            pts[ 2 ].setX( pts[ 1 ].x() - i );
            pts[ 2 ].setY( pts[ 1 ].y() + i );
            hb_bm_polygon( pts, 3 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_ARROW_L:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );

            i = HB_MIN( ( celly >> 1 ), cellx ) - 3;
            pts[ 0 ].setX( ( ( cellx - i ) >> 1 ) + i );
            pts[ 0 ].setY( ( celly >> 1 ) - i );
            pts[ 1 ].setX( pts[ 0 ].x() - i );
            pts[ 1 ].setY( pts[ 0 ].y() + i );
            pts[ 2 ].setX( pts[ 1 ].x() + i );
            pts[ 2 ].setY( pts[ 1 ].y() + i );
            hb_bm_polygon( pts, 3 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_ARROW_U:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );

            i = HB_MIN( celly, cellx >> 1 );
            pts[ 0 ].setX( ( cellx >> 1 ) - i );
            pts[ 0 ].setY( ( ( celly - i ) >> 1 ) + i );
            pts[ 1 ].setX( pts[ 0 ].x() + i );
            pts[ 1 ].setY( pts[ 0 ].y() - i );
            pts[ 2 ].setX( pts[ 1 ].x() + i );
            pts[ 2 ].setY( pts[ 1 ].y() + i );
            hb_bm_polygon( pts, 3 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_ARROW_D:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );

            i = HB_MIN( celly, cellx >> 1 );
            pts[ 0 ].setX( ( cellx >> 1 ) - i );
            pts[ 0 ].setY( ( celly - i ) >> 1 );
            pts[ 1 ].setX( pts[ 0 ].x() + i );
            pts[ 1 ].setY( pts[ 0 ].y() + i );
            pts[ 2 ].setX( pts[ 1 ].x() + i );
            pts[ 2 ].setY( pts[ 1 ].y() - i );
            hb_bm_polygon( pts, 3 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_FULL:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_fillrect( 0, 0, cellx, celly );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_FULL_B:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_fillrect( 0, celly / 2 + 1, cellx, ( celly + 1 ) / 2 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_FULL_T:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_fillrect( 0, 0, cellx, celly / 2 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_FULL_R:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_fillrect( cellx / 2 + 1, 0, ( cellx + 1 ) / 2, celly );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_FULL_L:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_fillrect( 0, 0, cellx / 2, celly );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_SNG_LT:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2, celly - 1, cellx / 2, celly / 2 );
            hb_bm_line( cellx / 2, celly / 2, cellx - 1, celly / 2 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_SNG_TD:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            hb_bm_line( cellx / 2, celly / 2, cellx / 2, celly - 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_SNG_RT:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2, celly - 1, cellx / 2, celly / 2 );
            hb_bm_line( cellx / 2, celly / 2, 0, celly / 2 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_SNG_LB:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2, 0, cellx / 2, celly / 2 );
            hb_bm_line( cellx / 2, celly / 2, cellx - 1, celly / 2 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_SNG_BU:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2, 0, cellx / 2, celly / 2 );
            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_SNG_RB:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2, 0, cellx / 2, celly / 2 );
            hb_bm_line( cellx / 2, celly / 2, 0, celly / 2 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_SNG_VL:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            hb_bm_line( cellx / 2, celly / 2, cellx - 1, celly / 2 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_SNG_VR:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            hb_bm_line( cellx / 2, celly / 2, 0, celly / 2 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_SNG_CRS:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_SNG_HOR:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_SNG_VRT:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_DBL_LT:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2 - 1, celly - 1, cellx / 2 - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 + 1, celly - 1, cellx / 2 + 1, celly / 2 + 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_DBL_TD:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( 0, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( 0, celly / 2 + 1, cellx / 2 - 1, celly / 2 + 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 + 1, cellx / 2 - 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 + 1, cellx / 2 + 1, celly - 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_DBL_RT:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2 - 1, celly - 1, cellx / 2 - 1, celly / 2 + 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 + 1, 0, celly / 2 + 1 );
            hb_bm_line( cellx / 2 + 1, celly - 1, cellx / 2 + 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 - 1, 0, celly / 2 - 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_DBL_LB:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly / 2 + 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_DBL_BU:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( 0, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            hb_bm_line( 0, celly / 2 - 1, cellx / 2 - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 - 1, cellx / 2 - 1, 0 );
            hb_bm_line( cellx / 2 + 1, celly / 2 - 1, cellx / 2 + 1, 0 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_DBL_RB:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 - 1, 0, celly / 2 - 1 );
            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly / 2 + 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 + 1, 0, celly / 2 + 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_DBL_VL:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 + 1, cellx / 2 + 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_DBL_VR:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly - 1 );
            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 + 1, cellx / 2 - 1, celly - 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 - 1, 0, celly / 2 - 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 + 1, 0, celly / 2 + 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_DBL_CRS:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 + 1, cellx / 2 - 1, celly - 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 - 1, 0, celly / 2 - 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2 + 1, 0, celly / 2 + 1 );
            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 + 1, cellx / 2 + 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_DBL_HOR:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( 0, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( 0, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_DBL_VRT:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly - 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_SNG_L_DBL_T:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            hb_bm_line( cellx / 2, celly / 2 - 1, cellx / 2, celly - 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_SNG_T_DBL_D:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            hb_bm_line( cellx / 2 - 1, celly / 2, cellx / 2 - 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2, cellx / 2 + 1, celly - 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_SNG_R_DBL_T:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( 0, celly / 2, cellx / 2 + 1, celly / 2 );
            hb_bm_line( cellx / 2 - 1, celly / 2, cellx / 2 - 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2, cellx / 2 + 1, celly - 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_SNG_L_DBL_B:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            hb_bm_line( cellx / 2, 0, cellx / 2, celly / 2 + 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_SNG_B_DBL_U:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly / 2 );
            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly / 2 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_SNG_R_DBL_B:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( 0, celly / 2, cellx / 2 + 1, celly / 2 );
            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly / 2 );
            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly / 2 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_SNG_V_DBL_L:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            hb_bm_line( cellx / 2, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( cellx / 2, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_SNG_V_DBL_R:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            hb_bm_line( 0, celly / 2 - 1, cellx / 2, celly / 2 - 1 );
            hb_bm_line( 0, celly / 2 + 1, cellx / 2, celly / 2 + 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_SNG_DBL_CRS:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            hb_bm_line( 0, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( 0, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_DBL_L_SNG_T:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2 - 1, celly / 2, cellx / 2 - 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2, cellx / 2 + 1, celly - 1 );
            hb_bm_line( cellx / 2 - 1, celly / 2, cellx - 1, celly / 2 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_DBL_T_SNG_D:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( 0, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( 0, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            hb_bm_line( cellx / 2, celly / 2 + 1, cellx / 2, celly - 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_DBL_R_SNG_T:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( 0, celly / 2 - 1, cellx / 2, celly / 2 - 1 );
            hb_bm_line( 0, celly / 2 + 1, cellx / 2, celly / 2 + 1 );
            hb_bm_line( cellx / 2, celly / 2 - 1, cellx / 2, celly - 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_DBL_L_SNG_B:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly / 2 );
            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly / 2 );
            hb_bm_line( cellx / 2 - 1, celly / 2, cellx - 1, celly / 2 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_DBL_B_SNG_U:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( 0, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            hb_bm_line( 0, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            hb_bm_line( cellx / 2, 0, cellx / 2, celly / 2 - 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_DBL_R_SNG_B:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( 0, celly / 2 - 1, cellx / 2, celly / 2 - 1 );
            hb_bm_line( 0, celly / 2 + 1, cellx / 2, celly / 2 + 1 );
            hb_bm_line( cellx / 2, 0, cellx / 2, celly / 2 + 1 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_DBL_V_SNG_L:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, celly / 2, cellx - 1, celly / 2 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_DBL_V_SNG_R:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly - 1 );
            hb_bm_line( 0, celly / 2, cellx / 2 - 1, celly / 2 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_DBL_SNG_CRS:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            hb_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly - 1 );
            hb_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly - 1 );
            hb_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            hb_bm_paint_end();
            break;

         case HB_BOXCH_SQUARE:
            qBitMap = hb_gt_qtc_bitmap_char( cellx, celly );
            hb_bm_paint_begin( qBitMap );
            xx = yy = cellx - HB_MAX( cellx >> 2, 2 );
            hb_bm_fillrect( ( cellx - xx ) >> 1, ( celly - yy ) >> 1, xx, yy );
            hb_bm_paint_end();
            break;
      }

   return qBitMap;
}

static QBitmap * hb_gt_qtc_getBoxChar( PHB_GTQTC pQTC, HB_USHORT * puc16 )
{
   HB_USHORT uc16 = *puc16;
   int iPos, iTrans;

   if( ( pQTC->fontAttribute & HB_GTI_FONTA_DRAWBOX ) == 0 )
      return NULL;

   if( uc16 >= HB_BOXCH_RC_0 && uc16 <= HB_BOXCH_RC_ACC )
   {
      switch( uc16 )
      {
         case HB_BOXCH_RC_0:
            *puc16 = '0';
            break;
         case HB_BOXCH_RC_1:
            *puc16 = '1';
            break;
         case HB_BOXCH_RC_2:
            *puc16 = '2';
            break;
         case HB_BOXCH_RC_3:
            *puc16 = '3';
            break;
         case HB_BOXCH_RC_4:
            *puc16 = '4';
            break;
         case HB_BOXCH_RC_5:
            *puc16 = '5';
            break;
         case HB_BOXCH_RC_6:
            *puc16 = '6';
            break;
         case HB_BOXCH_RC_7:
            *puc16 = '7';
            break;
         case HB_BOXCH_RC_8:
            *puc16 = '8';
            break;
         case HB_BOXCH_RC_9:
            *puc16 = '9';
            break;
         case HB_BOXCH_RC_DOT:
            *puc16 = '.';
            break;
         case HB_BOXCH_RC_ACC:
            *puc16 = '\'';
            break;
      }
      return NULL;
   }

   if     ( uc16 == HB_BOXCH_ARROW_R )
      iPos = 0;
   else if( uc16 == HB_BOXCH_ARROW_L )
      iPos = 1;
   else if( uc16 == HB_BOXCH_ARROW_U )
      iPos = 2;
   else if( uc16 == HB_BOXCH_ARROW_D )
      iPos = 3;
   else if( uc16 >= HB_BOXCH_BOX_MIN && uc16 <= HB_BOXCH_BOX_MAX )
      iPos = HB_BOXCH_CHR_BASE +
             ( uc16 - HB_BOXCH_BOX_MIN );
   else if( uc16 >= HB_BOXCH_RC_MIN && uc16 <= HB_BOXCH_RC_MAX )
      iPos = HB_BOXCH_CHR_BASE + ( HB_BOXCH_BOX_MAX - HB_BOXCH_BOX_MIN + 1 ) +
             ( uc16 - HB_BOXCH_RC_MIN );
   else
      return NULL;

   iTrans = pQTC->boxIndex[ iPos ];
   if( iTrans == HB_BOXCH_TRANS_MAX )
   {
      if( pQTC->boxCount < HB_BOXCH_TRANS_MAX - 1 )
      {
         iTrans = pQTC->boxCount + 1;
         pQTC->boxImage[ iTrans ] = hb_gt_qtc_defineBoxChar( pQTC, uc16 );
         if( pQTC->boxImage[ iTrans ] )
            pQTC->boxCount = iTrans;
         else
            iTrans = 0;
      }
      else
         iTrans = 0;
      pQTC->boxIndex[ iPos ] = iTrans;
   }

   return pQTC->boxImage[ iTrans ];
}

static void hb_gt_qtc_resetBoxCharBitmaps( PHB_GTQTC pQTC )
{
   int i;

   for( i = 1; i <= pQTC->boxCount; i++ )
      delete pQTC->boxImage[ i ];

   memset( pQTC->boxImage, 0, sizeof( pQTC->boxImage ) );
   pQTC->boxCount = 0;

   for( i = 0; i < HB_BOXCH_TRANS_COUNT; ++i )
      pQTC->boxIndex[ i ] = HB_BOXCH_TRANS_MAX;
}

/* *********************************************************************** */

static void hb_gt_qtc_free( PHB_GTQTC pQTC )
{
   if( pQTC->qEventLoop )
   {
      pQTC->qEventLoop->exit();
      delete pQTC->qEventLoop;
   }

   if( pQTC->qWnd )
      delete pQTC->qWnd;

   hb_gt_qtc_resetBoxCharBitmaps( pQTC );

   if( pQTC->fontName )
      delete pQTC->fontName;

   if( pQTC->wndTitle )
      delete pQTC->wndTitle;

   if( pQTC->qIcon )
      delete pQTC->qIcon;

   if( pQTC->textLine )
      hb_xfree( pQTC->textLine );

   hb_xfree( pQTC );
}

static PHB_GTQTC hb_gt_qtc_new( PHB_GT pGT )
{
   PHB_GTQTC pQTC;

   pQTC = ( PHB_GTQTC ) hb_xgrabz( sizeof( HB_GTQTC ) );
   pQTC->pGT = pGT;

   pQTC->colors[  0 ] = BLACK;
   pQTC->colors[  1 ] = BLUE;
   pQTC->colors[  2 ] = GREEN;
   pQTC->colors[  3 ] = CYAN;
   pQTC->colors[  4 ] = RED;
   pQTC->colors[  5 ] = MAGENTA;
   pQTC->colors[  6 ] = BROWN;
   pQTC->colors[  7 ] = LIGHT_GRAY;
   pQTC->colors[  8 ] = GRAY;
   pQTC->colors[  9 ] = BRIGHT_BLUE;
   pQTC->colors[ 10 ] = BRIGHT_GREEN;
   pQTC->colors[ 11 ] = BRIGHT_CYAN;
   pQTC->colors[ 12 ] = BRIGHT_RED;
   pQTC->colors[ 13 ] = BRIGHT_MAGENTA;
   pQTC->colors[ 14 ] = YELLOW;
   pQTC->colors[ 15 ] = WHITE;

   pQTC->iRows         = QTC_DEFAULT_ROWS;
   pQTC->iCols         = QTC_DEFAULT_COLS;

   pQTC->textLine      = ( QChar * ) hb_xgrab( pQTC->iCols * sizeof( QChar ) );

   pQTC->iNewPosX      = -1;
   pQTC->iNewPosY      = -1;

   pQTC->fontWidth     = QTC_DEFAULT_FONT_WIDTH;
   pQTC->fontHeight    = QTC_DEFAULT_FONT_HEIGHT;
   pQTC->fontWeight    = QTC_DEFAULT_FONT_WEIGHT;
   pQTC->fontAttribute = QTC_DEFAULT_FONT_ATTRIBUTE;
   pQTC->fontAscent    = 0;
   pQTC->fontName      = new QString( QTC_DEFAULT_FONT_NAME );
   pQTC->cellY         = pQTC->fontHeight;
   pQTC->cellX         = pQTC->fontWidth == 0 ? pQTC->cellY / 2: pQTC->fontWidth;
   pQTC->iCloseMode    = 0;
   pQTC->iResizeMode   = HB_GTI_RESIZEMODE_FONT;
   pQTC->fResizable    = HB_TRUE;
   pQTC->fResizeInc    = HB_FALSE;
   pQTC->fAltEnter     = HB_FALSE;
   pQTC->fMaximized    = HB_FALSE;
   pQTC->fFullScreen   = HB_FALSE;
   pQTC->fSelectCopy   = HB_FALSE;
   pQTC->fRepaint      = HB_TRUE;

   {
      PHB_ITEM pItem = hb_itemPutCPtr( NULL, hb_cmdargBaseProgName() );
      pQTC->wndTitle = new QString();
      hb_gt_qtc_itemGetQString( pItem, pQTC->wndTitle );
      hb_itemRelease( pItem );
   }
   return pQTC;
}

static void hb_gt_qtc_updateCursor( PHB_GTQTC pQTC )
{
   int cursorType = pQTC->cursorVisible ? pQTC->cursorType : SC_NONE;

   if( pQTC->lastCursorType != cursorType ||
       pQTC->lastCursorCol != pQTC->cursorCol ||
       pQTC->lastCursorRow != pQTC->cursorRow )
   {
      switch( cursorType )
      {
         case SC_NORMAL:
            pQTC->cursorSize   = 2;
            pQTC->cursorOffset = pQTC->cellY - pQTC->cursorSize - 1;
            break;
         case SC_INSERT:
            pQTC->cursorSize   = ( pQTC->cellY - 2 ) >> 1;
            pQTC->cursorOffset = pQTC->cellY - pQTC->cursorSize - 1;
            break;
         case SC_SPECIAL1:
            pQTC->cursorSize   = pQTC->cellY - 2;
            pQTC->cursorOffset = 1;
            break;
         case SC_SPECIAL2:
            pQTC->cursorSize   = ( pQTC->cellY - 2 ) >> 1;
            pQTC->cursorOffset = 1;
            break;
         default:
            pQTC->cursorSize   = 0;
            break;
      }
      if( pQTC->lastCursorType != SC_NONE )
      {
         pQTC->qWnd->qConsole->update( pQTC->lastCursorCol * pQTC->cellX + pQTC->marginLeft,
                                       pQTC->lastCursorRow * pQTC->cellY + pQTC->marginTop,
                                       pQTC->cellX, pQTC->cellY );
      }
      if( pQTC->cursorSize != 0 &&
          ( pQTC->lastCursorType == SC_NONE ||
            pQTC->lastCursorCol != pQTC->cursorCol ||
            pQTC->lastCursorRow != pQTC->cursorRow ) )
      {
         pQTC->qWnd->qConsole->update( pQTC->cursorCol * pQTC->cellX + pQTC->marginLeft,
                                       pQTC->cursorRow * pQTC->cellY + pQTC->marginTop,
                                       pQTC->cellX, pQTC->cellY );
      }
      pQTC->lastCursorType = cursorType;
      pQTC->lastCursorCol  = pQTC->cursorCol;
      pQTC->lastCursorRow  = pQTC->cursorRow;
   }
}

/*
 *  functions for handling the input queues for the mouse and keyboard
 */
static void hb_gt_qtc_addKeyToInputQueue( PHB_GTQTC pQTC, int iKey )
{
   int iHead = pQTC->keyHead;

   if( pQTC->keyHead != pQTC->keyTail )
   {
      if( HB_INKEY_ISMOUSEPOS( iKey ) )
      {
         int iLastKey = pQTC->keyBuffer[ pQTC->keyLast ];

         /* Clipper strips repeated mouse movemnt - let's do the same */
         if( HB_INKEY_ISMOUSEPOS( iLastKey ) )
         {
            pQTC->keyBuffer[ pQTC->keyLast ] = iKey;
            return;
         }
      }
      else if( iKey == HB_K_RESIZE &&
               iKey == pQTC->keyBuffer[ pQTC->keyLast ] )
         return;
   }

   /* When the buffer is full new event overwrite the last one
    * in the buffer - it's Clipper behavior [druzus]
    */
   pQTC->keyBuffer[ pQTC->keyLast = iHead ] = iKey;
   if( ++iHead >= QTC_KEY_QUEUE_SIZE )
      iHead = 0;
   if( iHead != pQTC->keyTail )
      pQTC->keyHead = iHead;
}

static HB_BOOL hb_gt_qtc_getKeyFromInputQueue( PHB_GTQTC pQTC, int * piKey )
{
   if( pQTC && pQTC->keyTail != pQTC->keyHead )
   {
      *piKey = pQTC->keyBuffer[ pQTC->keyTail ];
      if( ++pQTC->keyTail >= QTC_KEY_QUEUE_SIZE )
         pQTC->keyTail = 0;
      return HB_TRUE;
   }
   *piKey = 0;
   return HB_FALSE;
}

static int hb_gt_qtc_getKbdState( void )
{
   int iKbdState = 0;
   Qt::KeyboardModifiers kbState = QApplication::keyboardModifiers();

   if( kbState & Qt::ShiftModifier   ) iKbdState |= HB_GTI_KBD_SHIFT;
   if( kbState & Qt::ControlModifier ) iKbdState |= HB_GTI_KBD_CTRL;
   if( kbState & Qt::AltModifier     ) iKbdState |= HB_GTI_KBD_ALT;

   return iKbdState;
}

static int hb_gt_qtc_getKeyFlags( Qt::KeyboardModifiers keyFlags )
{
   int iFlags = 0;

   if( keyFlags & Qt::AltModifier     ) iFlags |= HB_KF_ALT;
   if( keyFlags & Qt::ControlModifier ) iFlags |= HB_KF_CTRL;
   if( keyFlags & Qt::ShiftModifier   ) iFlags |= HB_KF_SHIFT;
   if( keyFlags & Qt::KeypadModifier  ) iFlags |= HB_KF_KEYPAD;
#ifdef HB_OS_DARWIN
   if( keyFlags & Qt::MetaModifier )    iFlags |= HB_KF_CTRL;
#endif

   return iFlags;
}

static void hb_gt_qtc_setMouseKey( PHB_GTQTC pQTC, int x, int y, int iKey,
                                   Qt::KeyboardModifiers keyFlags )
{
   x -= pQTC->marginLeft;
   y -= pQTC->marginTop;
   pQTC->mousePosX = x;
   pQTC->mousePosY = y;
   x /= pQTC->cellX;
   y /= pQTC->cellY;
   if( pQTC->mouseCol != x || pQTC->mouseRow != y )
   {
      pQTC->mouseCol = x;
      pQTC->mouseRow = y;
      hb_gt_qtc_addKeyToInputQueue( pQTC, HB_INKEY_NEW_MPOS( x, y ) );
   }

   if( iKey != 0 )
   {
      int iFlags = hb_gt_qtc_getKeyFlags( keyFlags );
      hb_gt_qtc_addKeyToInputQueue( pQTC, HB_INKEY_NEW_MKEY( iKey, iFlags ) );
   }
}

static HB_BOOL hb_gt_qtc_setWindowSize( PHB_GTQTC pQTC, int iRows, int iCols )
{
   if( HB_GTSELF_RESIZE( pQTC->pGT, iRows, iCols ) )
   {
      if( pQTC->iCols != iCols )
         pQTC->textLine = ( QChar * ) hb_xrealloc( pQTC->textLine,
                                                   iCols * sizeof( QChar ) );

      if( pQTC->qWnd && ( iRows != pQTC->iRows || iCols != pQTC->iCols ) )
         hb_gt_qtc_addKeyToInputQueue( pQTC, HB_K_RESIZE );

      pQTC->iRows = iRows;
      pQTC->iCols = iCols;

      return HB_TRUE;
   }
   return HB_FALSE;
}

static void hb_gt_qtc_setWindowFlags( PHB_GTQTC pQTC, Qt::WindowFlags flags, HB_BOOL fSet )
{
   Qt::WindowFlags currFlags = pQTC->qWnd->windowFlags(), newFlags;

   if( fSet )
      newFlags = currFlags | flags;
   else
      newFlags = currFlags & ~flags;

   if( newFlags != currFlags )
   {
      pQTC->qWnd->setWindowFlags( newFlags );
      HB_QTC_LOCK();
      pQTC->qWnd->show();
      HB_QTC_UNLOCK();
   }
}

static void hb_gt_qtc_setWindowState( PHB_GTQTC pQTC, Qt::WindowStates state, HB_BOOL fSet )
{
   Qt::WindowStates currState = pQTC->qWnd->windowState(), newState;

   if( fSet )
      newState = currState | state;
   else
      newState = currState & ~state;

   if( newState != currState )
   {
      pQTC->qWnd->setWindowState( newState );
      HB_QTC_LOCK();
      pQTC->qWnd->show();
      HB_QTC_UNLOCK();
   }
}

static void hb_gt_qtc_initWindow( PHB_GTQTC pQTC, HB_BOOL fCenter )
{
   pQTC->qWnd->qConsole->resetWindowSize();
   if( fCenter || pQTC->iNewPosX < 0 || pQTC->iNewPosY < 0 )
   {
      QRect rc( QApplication::desktop()->availableGeometry() );
      pQTC->iNewPosX = rc.left() + ( ( rc.width() - pQTC->qWnd->width() ) >> 1 );
      pQTC->iNewPosY = rc.top() + ( ( rc.height() - pQTC->qWnd->height() ) >> 1 );
   }
   /* initial moving fulscreen or maximized window in MS-Windows
    * disables this modes [druzus]
    */
   if( ( pQTC->qWnd->windowState() & ( Qt::WindowMaximized | Qt::WindowFullScreen ) ) == 0 )
      pQTC->qWnd->move( pQTC->iNewPosX, pQTC->iNewPosY );
}

static void hb_gt_qtc_createConsoleWindow( PHB_GTQTC pQTC )
{
   pQTC->qWnd = new QTCWindow( pQTC );
   if( !pQTC->qWnd )
      hb_errInternal( 10002, "Failed to create QTC window", NULL, NULL );

   hb_gt_qtc_initWindow( pQTC, HB_FALSE );

   HB_QTC_LOCK();
   pQTC->qWnd->show();
   HB_QTC_UNLOCK();
   pQTC->qWnd->update();
}

/* *********************************************************************** */

static void hb_gt_qtc_InitMT( void );

static void hb_gt_qtc_Init( PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr )
{
   PHB_GTQTC pQTC;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_qtc_Init(%p,%p,%p,%p)", pGT, ( void * ) ( HB_PTRDIFF ) hFilenoStdin, ( void * ) ( HB_PTRDIFF ) hFilenoStdout, ( void * ) ( HB_PTRDIFF ) hFilenoStderr ) );

   if( ! s_qtapp )
   {
      hb_gt_qtc_InitMT();

      s_qtapp = qApp;
      if( ! s_qtapp )
      {
         /* QT requires these variables to be valid for whole application life
          * so we have to declare them as static [druzus]
          */
         static char ** s_argv;
         static int s_argc;

         s_argc = hb_cmdargARGC();
         s_argv = hb_cmdargARGV();

         s_qtapp = new QApplication( s_argc, s_argv );
         if( ! s_qtapp )
            hb_errInternal( 10001, "QT initialization error.", NULL, NULL );

         hb_vmAtQuit( hb_gt_qtc_appFree, NULL );
         hb_cmdargInit( s_argc, s_argv );
      }
   }

   pQTC = hb_gt_qtc_new( pGT );
   HB_GTLOCAL( pGT ) = ( void * ) pQTC;

   if( ! pQTC->qEventLoop )
      pQTC->qEventLoop = new QEventLoop();

   /* SUPER GT initialization */
   HB_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );
   HB_GTSELF_RESIZE( pGT, pQTC->iRows, pQTC->iCols );
   HB_GTSELF_SETFLAG( pGT, HB_GTI_REDRAWMAX, 1 );
   HB_GTSELF_SEMICOLD( pGT );
}

/* *********************************************************************** */

static void hb_gt_qtc_Exit( PHB_GT pGT )
{
   PHB_GTQTC pQTC;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_qtc_Exit(%p)", pGT ) );

   pQTC = HB_GTQTC_GET( pGT );
   HB_GTSUPER_EXIT( pGT );

   if( pQTC )
      hb_gt_qtc_free( pQTC );
}

/* *********************************************************************** */

static void hb_gt_qtc_Redraw( PHB_GT pGT, int iRow, int iCol, int iSize )
{
   PHB_GTQTC pQTC;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_qtc_Redraw(%p,%d,%d,%d)", pGT, iRow, iCol, iSize ) );

   pQTC = HB_GTQTC_GET( pGT );
   if( pQTC )
   {
      if( !pQTC->qWnd )
         hb_gt_qtc_createConsoleWindow( pQTC );

      pQTC->qWnd->qConsole->repaintChars( QRect( iCol * pQTC->cellX,
                                                 iRow * pQTC->cellY,
                                                 iSize * pQTC->cellX,
                                                 pQTC->cellY ) );
   }
}

/* *********************************************************************** */

static void hb_gt_qtc_Refresh( PHB_GT pGT )
{
   PHB_GTQTC pQTC;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_qtc_Refresh(%p)", pGT ) );

   HB_GTSUPER_REFRESH( pGT );

   pQTC = HB_GTQTC_GET( pGT );
   if( pQTC )
   {
      HB_GTSELF_GETSCRCURSOR( pGT, &pQTC->cursorRow, &pQTC->cursorCol,
                                   &pQTC->cursorType );
      if( pQTC->qWnd )
         hb_gt_qtc_updateCursor( pQTC );
   }
}

/* *********************************************************************** */

static HB_BOOL hb_gt_qtc_SetMode( PHB_GT pGT, int iRows, int iCols )
{
   PHB_GTQTC pQTC;
   HB_BOOL fResult, fCenter;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_qtc_SetMode(%p,%d,%d)", pGT, iRows, iCols ) );

   pQTC = HB_GTQTC_GET( pGT );
   fCenter = iRows != pQTC->iRows || iCols != pQTC->iCols;
   fResult = hb_gt_qtc_setWindowSize( pQTC, iRows, iCols );
   if( fResult )
   {
      if( pQTC->qWnd )
      {
         hb_gt_qtc_initWindow( pQTC, fCenter );
         HB_GTSELF_REFRESH( pGT );
      }
      else
         HB_GTSELF_SEMICOLD( pGT );
   }

   return fResult;
}

/* *********************************************************************** */

static const char * hb_gt_qtc_Version( PHB_GT pGT, int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_qtc_Version(%p,%d)", pGT, iType ) );

   HB_SYMBOL_UNUSED( pGT );

   if( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: QT GUI console (QTC)";
}

/* *********************************************************************** */

static int hb_gt_qtc_ReadKey( PHB_GT pGT, int iEventMask )
{
   PHB_GTQTC pQTC;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_qtc_ReadKey(%p,%d)", pGT, iEventMask ) );

   HB_SYMBOL_UNUSED( iEventMask );

   pQTC = HB_GTQTC_GET( pGT );
   if( pQTC )
   {
      int iKey;

      HB_QTC_LOCK();
      if( pQTC->qEventLoop )
         pQTC->qEventLoop->processEvents( QEventLoop::AllEvents );
      else
         QApplication::processEvents( QEventLoop::AllEvents );
      HB_QTC_UNLOCK();

      if( hb_gt_qtc_getKeyFromInputQueue( pQTC, &iKey ) )
         return iKey;
   }
   return 0;
}

/* *********************************************************************** */

static void hb_gt_qtc_Tone( PHB_GT pGT, double dFrequency, double dDuration )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_qtc_Tone(%p,%lf,%lf)", pGT, dFrequency, dDuration ) );

   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( dFrequency );
   HB_SYMBOL_UNUSED( dDuration );

   /* TODO: add support for sth more advanced then simple system beep */
   QApplication::beep();
}

/* *********************************************************************** */

static HB_BOOL hb_gt_qtc_mouse_IsPresent( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_qtc_mouse_IsPresent(%p)", pGT ) );

   HB_SYMBOL_UNUSED( pGT );

   return HB_TRUE;
}

static void hb_gt_qtc_mouse_GetPos( PHB_GT pGT, int * piRow, int * piCol )
{
   PHB_GTQTC pQTC;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_qtc_mouse_GetPos(%p,%p,%p)", pGT, piRow, piCol ) );

   pQTC = HB_GTQTC_GET( pGT );
   *piRow = pQTC->mouseRow;
   *piCol = pQTC->mouseCol;
}

static HB_BOOL hb_gt_qtc_mouse_ButtonState( PHB_GT pGT, int iButton )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_qtc_mouse_ButtonState(%p,%i)", pGT, iButton ) );

   HB_SYMBOL_UNUSED( pGT );

   switch( iButton )
   {
      case 0:
         return ( QApplication::mouseButtons() & Qt::LeftButton ) != 0;
      case 1:
         return ( QApplication::mouseButtons() & Qt::RightButton ) != 0;
      case 2:
         return ( QApplication::mouseButtons() & Qt::MidButton ) != 0;
   }
   return HB_FALSE;
}

static int hb_gt_qtc_mouse_CountButton( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_qtc_mouse_CountButton(%p)", pGT ) );

   HB_SYMBOL_UNUSED( pGT );

   return 3;
}

/* *********************************************************************** */

static HB_BOOL hb_gt_qtc_Info( PHB_GT pGT, int iType, PHB_GT_INFO pInfo )
{
   PHB_GTQTC pQTC;
   int iVal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_qtc_Info(%p,%d,%p)", pGT, iType, pInfo ) );

   pQTC = HB_GTQTC_GET( pGT );

   switch( iType )
   {
      case HB_GTI_ISSCREENPOS:
      case HB_GTI_KBDSUPPORT:
      case HB_GTI_ISGRAPHIC:
      case HB_GTI_ISUNICODE:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_TRUE );
         break;

      case HB_GTI_ONLINE:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, pQTC->qWnd != NULL );
         break;

      case HB_GTI_FONTSIZE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pQTC->cellY );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            pQTC->fontHeight = iVal;
            if( pQTC->qWnd )
            {
               pQTC->qWnd->qConsole->resetWindowSize();
               HB_GTSELF_REFRESH( pGT );
            }
         }
         break;

      case HB_GTI_FONTWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pQTC->cellX );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
            /* store font status for next operation on fontsize */
            pQTC->fontWidth = iVal;
         break;

      case HB_GTI_FONTWEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pQTC->fontWeight );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            /* store font status for next operation on fontsize */
            iVal = hb_itemGetNI( pInfo->pNewVal );
            switch( iVal )
            {
               case HB_GTI_FONTW_THIN:
               case HB_GTI_FONTW_NORMAL:
               case HB_GTI_FONTW_BOLD:
                  pQTC->fontWeight = iVal;
                  break;
            }
         }
         break;

      case HB_GTI_FONTNAME:
         pInfo->pResult = hb_gt_qtc_itemPutQString( pInfo->pResult, pQTC->fontName );
         if( pInfo->pNewVal && HB_IS_STRING( pInfo->pNewVal ) )
            /* store font status for next operation on fontsize */
            hb_gt_qtc_itemGetQString( pInfo->pNewVal, pQTC->fontName );
         break;

      case HB_GTI_FONTATTRIBUTE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pQTC->fontAttribute );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            pQTC->fontAttribute = hb_itemGetNI( pInfo->pNewVal ) &
                                                ( HB_GTI_FONTA_FIXMETRIC |
                                                  HB_GTI_FONTA_CLRBKG    |
                                                  HB_GTI_FONTA_CTRLCHARS |
                                                  HB_GTI_FONTA_DRAWBOX   |
                                                  HB_GTI_FONTA_NOSTRETCH );
         break;

      case HB_GTI_SCREENHEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pQTC->cellY * pQTC->iRows );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
            HB_GTSELF_SETMODE( pGT, ( HB_USHORT ) ( iVal / pQTC->cellY ), pQTC->iCols );
         break;

      case HB_GTI_SCREENWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pQTC->cellX * pQTC->iCols );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
            HB_GTSELF_SETMODE( pGT, pQTC->iRows, ( HB_USHORT ) ( iVal / pQTC->cellX ) );
         break;

      case HB_GTI_DESKTOPWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult,
                                        QApplication::desktop()->screenGeometry().width() );
         break;

      case HB_GTI_DESKTOPHEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult,
                                        QApplication::desktop()->screenGeometry().height() );
         break;

      case HB_GTI_DESKTOPCOLS:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult,
                                        QApplication::desktop()->screenGeometry().width() / pQTC->cellX );
         break;

      case HB_GTI_DESKTOPROWS:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult,
                                        QApplication::desktop()->screenGeometry().height() / pQTC->cellY );
         break;

      case HB_GTI_WINTITLE:
         pInfo->pResult = hb_gt_qtc_itemPutQString( pInfo->pResult, pQTC->wndTitle );
         if( pInfo->pNewVal && HB_IS_STRING( pInfo->pNewVal ) )
         {
            hb_gt_qtc_itemGetQString( pInfo->pNewVal, pQTC->wndTitle );
            if( pQTC->qWnd )
               pQTC->qWnd->setWindowTitle( *pQTC->wndTitle );
         }
         break;

      case HB_GTI_ICONFILE:
         if( pInfo->pNewVal && HB_IS_STRING( pInfo->pNewVal ) )
         {
            QString qStr;
            hb_gt_qtc_itemGetQString( pInfo->pNewVal, &qStr );
            if( pQTC->qIcon )
               delete pQTC->qIcon;
            pQTC->qIcon = new QIcon( qStr );
            if( pQTC->qWnd )
               pQTC->qWnd->setWindowIcon( *pQTC->qIcon );
         }
         break;

      case HB_GTI_KBDSHIFTS:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, hb_gt_qtc_getKbdState() );
         break;

      case HB_GTI_CURSORBLINKRATE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, QApplication::cursorFlashTime() );
         if( pInfo->pNewVal && HB_IS_NUMERIC( pInfo->pNewVal ) )
         {
            iVal = hb_itemGetNI( pInfo->pNewVal );
            if( iVal < 0 )
               iVal = 0;
            QApplication::setCursorFlashTime( iVal );
            if( pQTC->qWnd )
            {
               if( pQTC->qWnd->qConsole->timer->isActive() )
                  pQTC->qWnd->qConsole->timer->stop();
               if( QApplication::cursorFlashTime() / 2 > 0 )
                  pQTC->qWnd->qConsole->timer->start( QApplication::cursorFlashTime() / 2, pQTC->qWnd->qConsole );
            }
         }
         break;

      case HB_GTI_CLIPBOARDDATA:
      {
         QString qStr = QApplication::clipboard()->text();
         pInfo->pResult = hb_gt_qtc_itemPutQString( pInfo->pResult, &qStr );
         if( pInfo->pNewVal && HB_IS_STRING( pInfo->pNewVal ) )
         {
            hb_gt_qtc_itemGetQString( pInfo->pNewVal, &qStr );
            QApplication::clipboard()->setText( qStr );
         }
         break;
      }
      case HB_GTI_SCREENSIZE:
         if( ! pInfo->pResult )
            pInfo->pResult = hb_itemNew( NULL );

         hb_arrayNew( pInfo->pResult, 2 );
         hb_arraySetNI( pInfo->pResult, 2, pQTC->cellY * pQTC->iRows );
         hb_arraySetNI( pInfo->pResult, 1, pQTC->cellX * pQTC->iCols );
         break;

      case HB_GTI_MAXIMIZED:
         if( pQTC->qWnd )
            pQTC->fMaximized = ( pQTC->qWnd->windowState() & Qt::WindowMaximized ) != 0;
         pInfo->pResult = hb_itemPutL( pInfo->pResult, pQTC->fMaximized );
         if( pInfo->pNewVal && HB_IS_LOGICAL( pInfo->pNewVal ) &&
             ( hb_itemGetL( pInfo->pNewVal ) ? ! pQTC->fMaximized : pQTC->fMaximized ) )
         {
            pQTC->fMaximized = ! pQTC->fMaximized;
            if( pQTC->qWnd )
               hb_gt_qtc_setWindowState( pQTC, Qt::WindowMaximized, pQTC->fMaximized );
         }
         break;

      case HB_GTI_ISFULLSCREEN:
         if( pQTC->qWnd )
            pQTC->fFullScreen = ( pQTC->qWnd->windowState() & Qt::WindowFullScreen ) != 0;
         pInfo->pResult = hb_itemPutL( pInfo->pResult, pQTC->fFullScreen );
         if( pInfo->pNewVal && HB_IS_LOGICAL( pInfo->pNewVal ) &&
             ( hb_itemGetL( pInfo->pNewVal ) ? ! pQTC->fFullScreen : pQTC->fFullScreen ) )
         {
            pQTC->fFullScreen = ! pQTC->fFullScreen;
            if( pQTC->qWnd )
               hb_gt_qtc_setWindowState( pQTC, Qt::WindowFullScreen, pQTC->fFullScreen );
         }
         break;

      case HB_GTI_ALTENTER:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, pQTC->fAltEnter );
         if( pInfo->pNewVal && HB_IS_LOGICAL( pInfo->pNewVal ) )
            pQTC->fAltEnter = hb_itemGetL( pInfo->pNewVal );
         break;

      case HB_GTI_CLOSABLE:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, pQTC->iCloseMode == 0 );
         if( pInfo->pNewVal && HB_IS_LOGICAL( pInfo->pNewVal ) &&
             ( hb_itemGetL( pInfo->pNewVal ) ? ( pQTC->iCloseMode != 0 ) :
                                               ( pQTC->iCloseMode == 0 ) ) )
         {
            iVal = pQTC->iCloseMode;
            pQTC->iCloseMode = iVal == 0 ? 1 : 0;
            if( pQTC->qWnd )
               hb_gt_qtc_setWindowFlags( pQTC, Qt::WindowCloseButtonHint, pQTC->iCloseMode < 2 );
         }
         break;

      case HB_GTI_CLOSEMODE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pQTC->iCloseMode );
         if( pInfo->pNewVal && HB_IS_NUMERIC( pInfo->pNewVal ) )
         {
            iVal = hb_itemGetNI( pInfo->pNewVal );
            if( iVal >= 0 && iVal <= 2 )
            {
               pQTC->iCloseMode = iVal;
               if( pQTC->qWnd )
                  hb_gt_qtc_setWindowFlags( pQTC, Qt::WindowCloseButtonHint, pQTC->iCloseMode < 2 );
            }
         }
         break;

      case HB_GTI_RESIZABLE:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, pQTC->fResizable );
         if( pInfo->pNewVal && HB_IS_LOGICAL( pInfo->pNewVal ) &&
             ( hb_itemGetL( pInfo->pNewVal ) ? ! pQTC->fResizable : pQTC->fResizable ) )
         {
            pQTC->fResizable = ! pQTC->fResizable;
            if( pQTC->qWnd )
            {
               pQTC->qWnd->setResizing();
               hb_gt_qtc_setWindowFlags( pQTC, Qt::WindowMaximizeButtonHint, pQTC->fResizable );
            }
         }
         break;

      case HB_GTI_RESIZEMODE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pQTC->iResizeMode );
         if( pInfo->pNewVal && HB_IS_NUMERIC( pInfo->pNewVal ) )
         {
            iVal = hb_itemGetNI( pInfo->pNewVal );
            switch( iVal )
            {
               case HB_GTI_RESIZEMODE_FONT:
               case HB_GTI_RESIZEMODE_ROWS:
                  if( pQTC->iResizeMode != iVal )
                  {
                     pQTC->iResizeMode = iVal;
                     if( pQTC->qWnd )
                        pQTC->qWnd->setResizing();
                  }
                  break;
            }
         }
         break;

      case HB_GTI_RESIZESTEP:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, pQTC->fResizeInc );
         if( pInfo->pNewVal && HB_IS_LOGICAL( pInfo->pNewVal ) &&
             ( hb_itemGetL( pInfo->pNewVal ) ? ! pQTC->fResizeInc : pQTC->fResizeInc ) )
         {
            pQTC->fResizeInc = ! pQTC->fResizeInc;
            if( pQTC->qWnd )
               pQTC->qWnd->setResizing();
         }
         break;

      case HB_GTI_SELECTCOPY:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, pQTC->fSelectCopy );
         if( pInfo->pNewVal && HB_IS_LOGICAL( pInfo->pNewVal ) )
            pQTC->fSelectCopy = hb_itemGetL( pInfo->pNewVal );
         break;

      case HB_GTI_MOUSEPOS_XY:
         if( ! pInfo->pResult )
            pInfo->pResult = hb_itemNew( NULL );
         hb_arrayNew( pInfo->pResult, 2 );
         hb_arraySetNI( pInfo->pResult, 1, pQTC->mousePosX );
         hb_arraySetNI( pInfo->pResult, 2, pQTC->mousePosY );
         break;

      case HB_GTI_SETPOS_XY:
      case HB_GTI_SETPOS_ROWCOL:
      {
         int x = pQTC->iNewPosX, y = pQTC->iNewPosY;

         if( pQTC->qWnd )
         {
            x = pQTC->qWnd->pos().x();
            y = pQTC->qWnd->pos().y();
         }
         if( ! pInfo->pResult )
            pInfo->pResult = hb_itemNew( NULL );
         hb_arrayNew( pInfo->pResult, 2 );

         if( iType == HB_GTI_SETPOS_ROWCOL )
         {
            iVal = x;
            x = y / pQTC->cellY;
            y = iVal / pQTC->cellX;
         }
         hb_arraySetNI( pInfo->pResult, 1, x );
         hb_arraySetNI( pInfo->pResult, 2, y );

         if( pInfo->pNewVal && HB_IS_NUMERIC( pInfo->pNewVal ) &&
             pInfo->pNewVal2 && HB_IS_NUMERIC( pInfo->pNewVal2 ) )
         {
            x = hb_itemGetNI( pInfo->pNewVal );
            y = hb_itemGetNI( pInfo->pNewVal2 );
         }
         else if( pInfo->pNewVal && HB_IS_ARRAY( pInfo->pNewVal ) &&
                  hb_arrayLen( pInfo->pNewVal ) == 2 )
         {
            x = hb_arrayGetNI( pInfo->pNewVal, 1 );
            y = hb_arrayGetNI( pInfo->pNewVal, 2 );
         }
         else
            break;

         if( iType == HB_GTI_SETPOS_ROWCOL )
         {
            iVal = x;
            x = y * pQTC->cellX;
            y = iVal * pQTC->cellY;
         }
         if( pQTC->qWnd )
            pQTC->qWnd->move( x, y );
         else
         {
            pQTC->iNewPosX = x;
            pQTC->iNewPosY = y;
         }
         break;
      }
      case HB_GTI_PALETTE:
         if( pInfo->pNewVal && HB_IS_NUMERIC( pInfo->pNewVal ) )
         {
            iVal = hb_itemGetNI( pInfo->pNewVal );
            if( iVal >= 0 && iVal < 16 )
            {
               pInfo->pResult = hb_itemPutNI( pInfo->pResult,
                                              QTC_RGB2NUM( pQTC->colors[ iVal ] ) );
               if( pInfo->pNewVal2 && HB_IS_NUMERIC( pInfo->pNewVal2 ) )
               {
                  int iColor = hb_itemGetNI( pInfo->pNewVal2 );
                  QRgb rgb = QTC_NUM2RGB( iColor );
                  if( rgb != pQTC->colors[ iVal ] )
                  {
                     pQTC->colors[ iVal ] = rgb;
                     if( pQTC->qWnd )
                        HB_GTSELF_EXPOSEAREA( pQTC->pGT, 0, 0, pQTC->iRows, pQTC->iCols );
                  }
               }
            }
         }
         else
         {
            if( ! pInfo->pResult )
               pInfo->pResult = hb_itemNew( NULL );
            hb_arrayNew( pInfo->pResult, 16 );
            for( iVal = 0; iVal < 16; iVal++ )
               hb_arraySetNL( pInfo->pResult, iVal + 1,
                              QTC_RGB2NUM( pQTC->colors[ iVal ] ) );
            if( pInfo->pNewVal && HB_IS_ARRAY( pInfo->pNewVal ) &&
                hb_arrayLen( pInfo->pNewVal ) == 16 )
            {
               for( iVal = 0; iVal < 16; iVal++ )
               {
                  int iColor = hb_arrayGetNI( pInfo->pNewVal, iVal + 1 );
                  pQTC->colors[ iVal ] = QTC_NUM2RGB( iColor );
               }
               if( pQTC->qWnd )
                  HB_GTSELF_EXPOSEAREA( pQTC->pGT, 0, 0, pQTC->iRows, pQTC->iCols );
            }
         }
         break;

      case HB_GTI_WINHANDLE:
         pInfo->pResult = hb_itemPutPtr( pInfo->pResult, pQTC->qWnd );
         break;

      case HB_GTI_DISPIMAGE:
         if( pInfo->pNewVal && HB_IS_STRING( pInfo->pNewVal ) && pQTC->qWnd )
         {
            QRect rx = pQTC->qWnd->qConsole->image->rect();
            QString qStr;
            hb_gt_qtc_itemGetQString( pInfo->pNewVal, &qStr );
            QImage qImg( qStr );

            if( pInfo->pNewVal2 && HB_IS_ARRAY( pInfo->pNewVal2 ) )
            {
               switch( hb_arrayLen( pInfo->pNewVal2 ) )
               {
                  case 2:
                     rx.setLeft( hb_arrayGetNI( pInfo->pNewVal2, 1 ) );
                     rx.setTop( hb_arrayGetNI( pInfo->pNewVal2, 2 ) );
                     if( !qImg.isNull() )
                        rx.setSize( qImg.size() );
                     break;
                  case 4:
                     rx.setCoords( hb_arrayGetNI( pInfo->pNewVal2, 1 ),
                                   hb_arrayGetNI( pInfo->pNewVal2, 2 ),
                                   hb_arrayGetNI( pInfo->pNewVal2, 3 ),
                                   hb_arrayGetNI( pInfo->pNewVal2, 4 ) );
               }
            }

            if( qImg.isNull() )
               pQTC->qWnd->qConsole->repaintChars( rx );
            else
            {
               QPainter painter( pQTC->qWnd->qConsole->image );
               painter.drawImage( rx, qImg );
            }
            pQTC->qWnd->qConsole->update();
         }
         break;

      default:
         return HB_GTSUPER_INFO( pGT, iType, pInfo );
   }

   return HB_TRUE;
}

#define hb_gfx_cord( t, l, b, r, tmp )    \
               do { \
                     if( l > r ) { tmp = r; r = l; l = tmp; } \
                     if( t > b ) { tmp = b; b = t; t = tmp; } \
               } while( 0 )

#define hb_gfx_update( x, y, w, h ) pQTC->qWnd->qConsole->update( x, y, w, h )

static int hb_gt_qtc_gfx_Primitive( PHB_GT pGT, int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
   PHB_GTQTC pQTC;
   int iRet = 1, iTmp;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_qtc_gfx_Primitive(%p,%d,%d,%d,%d,%d,%d)", pGT, iType, iTop, iLeft, iBottom, iRight, iColor ) );

   pQTC = HB_GTQTC_GET( pGT );

   if( pQTC->qWnd )
   {
      switch( iType )
      {
         case HB_GFX_MAKECOLOR:
         {
            QRgb qColor = qRgb( iTop & 0xFF, iLeft & 0xFF, iBottom & 0xFF );
            iRet = QTC_RGB2NUM( qColor );
            break;
         }
         case HB_GFX_PUTPIXEL:
            hb_bm_paint_begin( pQTC->qWnd->qConsole->image );
            hb_bm_color( iBottom );
            hb_bm_point( iLeft, iTop );
            hb_bm_paint_end();
            hb_gfx_update( iLeft, iTop, 1, 1 );
            break;

         case HB_GFX_LINE:
            hb_bm_paint_begin( pQTC->qWnd->qConsole->image );
            hb_bm_color( iColor );
            hb_bm_line( iLeft, iTop, iRight, iBottom );
            hb_bm_paint_end();
            hb_gfx_cord( iTop, iLeft, iBottom, iRight, iTmp );
            hb_gfx_update( iLeft, iTop, iRight - iLeft + 1, iBottom - iTop + 1 );
            break;

         case HB_GFX_RECT:
            hb_gfx_cord( iTop, iLeft, iBottom, iRight, iTmp );
            hb_bm_paint_begin( pQTC->qWnd->qConsole->image );
            hb_bm_color( iColor );
            painter.setBrush( Qt::NoBrush );
            hb_bm_rect( iLeft, iTop, iRight - iLeft, iBottom - iTop );
            hb_bm_paint_end();
            hb_gfx_update( iLeft, iTop, iRight - iLeft + 1, iBottom - iTop + 1 );
            break;

         case HB_GFX_FILLEDRECT:
            hb_gfx_cord( iTop, iLeft, iBottom, iRight, iTmp );
            hb_bm_paint_begin( pQTC->qWnd->qConsole->image );
            hb_bm_color( iColor );
            hb_bm_fillrectc( iLeft, iTop, iRight - iLeft, iBottom - iTop, iColor );
            hb_bm_paint_end();
            hb_gfx_update( iLeft, iTop, iRight - iLeft + 1, iBottom - iTop + 1 );
            break;

         case HB_GFX_CIRCLE:
            hb_bm_paint_begin( pQTC->qWnd->qConsole->image );
            hb_bm_color( iRight );
            painter.setBrush( Qt::NoBrush );
            painter.drawEllipse( QPoint( iLeft, iTop ), iBottom, iBottom );
            hb_bm_paint_end();
            hb_gfx_update( iLeft - iBottom, iTop - iBottom, iLeft + iBottom, iTop + iBottom );
            break;

         case HB_GFX_FILLEDCIRCLE:
            hb_bm_paint_begin( pQTC->qWnd->qConsole->image );
            hb_bm_color( iRight );
            painter.setBrush( QBrush( QTC_NUM2RGB( iRight ) ) );
            painter.drawEllipse( QPoint( iLeft, iTop ), iBottom, iBottom );
            hb_bm_paint_end();
            hb_gfx_update( iLeft - iBottom, iTop - iBottom, iLeft + iBottom, iTop + iBottom );
            break;

         case HB_GFX_ELLIPSE:
            hb_bm_paint_begin( pQTC->qWnd->qConsole->image );
            hb_bm_color( iColor );
            painter.setBrush( Qt::NoBrush );
            painter.drawEllipse( QPoint( iLeft, iTop ), iRight, iBottom );
            hb_bm_paint_end();
            hb_gfx_update( iLeft - iRight, iTop - iBottom, iLeft + iRight, iTop + iBottom );
            break;

         case HB_GFX_FILLEDELLIPSE:
            hb_bm_paint_begin( pQTC->qWnd->qConsole->image );
            hb_bm_color( iColor );
            painter.setBrush( QBrush( QTC_NUM2RGB( iColor ) ) );
            painter.drawEllipse( QPoint( iLeft, iTop ), iRight, iBottom );
            hb_bm_paint_end();
            hb_gfx_update( iLeft - iRight, iTop - iBottom, iLeft + iRight, iTop + iBottom );
            break;
      }
   }

   return iRet;
}

/* *********************************************************************** */

static HB_BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_FuncInit(%p)", pFuncTable ) );

   pFuncTable->Init                 = hb_gt_qtc_Init;
   pFuncTable->Exit                 = hb_gt_qtc_Exit;
   pFuncTable->SetMode              = hb_gt_qtc_SetMode;
   pFuncTable->Redraw               = hb_gt_qtc_Redraw;
   pFuncTable->Refresh              = hb_gt_qtc_Refresh;
   pFuncTable->Version              = hb_gt_qtc_Version;
   pFuncTable->Tone                 = hb_gt_qtc_Tone;
   pFuncTable->Info                 = hb_gt_qtc_Info;
   pFuncTable->ReadKey              = hb_gt_qtc_ReadKey;

   pFuncTable->MouseIsPresent       = hb_gt_qtc_mouse_IsPresent;
   pFuncTable->MouseGetPos          = hb_gt_qtc_mouse_GetPos;
   pFuncTable->MouseButtonState     = hb_gt_qtc_mouse_ButtonState;
   pFuncTable->MouseCountButton     = hb_gt_qtc_mouse_CountButton;

   pFuncTable->GfxPrimitive         = hb_gt_qtc_gfx_Primitive;

   return HB_TRUE;
}

/* *********************************************************************** */

static const HB_GT_INIT gtInit = { HB_GT_DRVNAME( HB_GT_NAME ),
                                   hb_gt_FuncInit,
                                   HB_GTSUPER,
                                   HB_GTID_PTR };

HB_GT_ANNOUNCE( HB_GT_NAME )

HB_CALL_ON_STARTUP_BEGIN( _hb_startup_gt_Init_ )
   hb_gtRegister( &gtInit );
HB_CALL_ON_STARTUP_END( _hb_startup_gt_Init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_startup_gt_Init_
#elif defined( HB_MSC_STARTUP )
   #if defined( HB_OS_WIN_64 )
      #pragma section( HB_MSC_START_SEGMENT, long, read )
   #endif
   #pragma data_seg( HB_MSC_START_SEGMENT )
   static HB_$INITSYM hb_vm_auto__hb_startup_gt_Init_ = _hb_startup_gt_Init_;
   #pragma data_seg()
#endif

/* *********************************************************************** */

QTConsole::QTConsole( PHB_GTQTC pStructQTC, QWidget *parent ) : QWidget( parent )
{
   pQTC = pStructQTC;

   resizeMode = false;
   selectMode = false;
   image = NULL;
   timer = NULL;

   setAttribute( Qt::WA_StaticContents );
   setAttribute( Qt::WA_PaintOnScreen );
   setAttribute( Qt::WA_OpaquePaintEvent );
   setAttribute( Qt::WA_KeyCompression );

   /* Qt::WA_InputMethodEnabled disables support for
    * national characters in few European countries
    * (f.e. Polish characters with ALT in MacOSX)
    * If some Asian users needs it then we will have
    * to enable it optionally [druzus]
    */
   /* setAttribute( Qt::WA_InputMethodEnabled ); */

   setFocusPolicy( Qt::StrongFocus );
   setMouseTracking( true );

   image = new QImage();

   timer = new QBasicTimer();
   if( QApplication::cursorFlashTime() / 2 > 0 )
      timer->start( QApplication::cursorFlashTime() / 2, this );
}

QTConsole::~QTConsole( void )
{
   delete image;

   if( timer->isActive() )
      timer->stop();
   delete timer;
}

void QTConsole::resetWindowSize( void )
{
   font = QFont( *pQTC->fontName );
   if( pQTC->fontWeight )
      font.setWeight( pQTC->fontWeight == HB_GTI_FONTW_THIN ? QFont::Light :
                      ( pQTC->fontWeight == HB_GTI_FONTW_BOLD ? QFont::Bold :
                        QFont::Normal ) ) ;
   font.setFixedPitch( true );
   font.setKerning( false );
   setFontSize( pQTC->fontHeight, pQTC->fontWidth );
   pQTC->qWnd->setWindowSize();
}

void QTConsole::setFontSize( int iFH, int iFW )
{
   int iDec = 0, iDir, iHeight, iWidth, iAscent;
   bool bStretch = ( pQTC->fontAttribute & HB_GTI_FONTA_NOSTRETCH ) == 0;
   bool bMaxSize = ( pQTC->qWnd->windowState() &
                     ( Qt::WindowMaximized | Qt::WindowFullScreen ) ) != 0;

   if( iFH < 4 )
      iFH = 4;

   font.setStretch( 100 );
   do
   {
      font.setPixelSize( iFH - iDec );
      QFontMetrics fm( font );
      iHeight = fm.height();
      iWidth  = fm.averageCharWidth();
      iAscent = fm.ascent();
      if( iHeight <= iFH && ( bStretch || ! bMaxSize || iWidth <= iFW ) )
         break;
   }
   while( iFH - ++iDec >= 4 );

   if( iFW > 0 && bStretch )
   {
      if( iFW < 2 )
         iFW = 2;

      if( iWidth != iFW )
      {
         iDec = ( iFW * 100 ) / iWidth;
         if( iDec < 1 )
            iDec = 1;
         else if( iDec >= 4000 )
            iDec = 3999;
         iDir = iDec;
         do
         {
            font.setStretch( iDec );
            QFontMetrics fm( font );
            iHeight = fm.height();
            iWidth  = fm.averageCharWidth();
            iAscent = fm.ascent();

            if( iWidth == iFW )
               break;

            if( iWidth < iFW )
            {
               if( iDir > iDec )
                  break;
               ++iDec;
            }
            else /* iWidth > iFW */
            {
               if( --iDec <= iDir )
                  iDir = iDec + 1;
            }
         }
         while( iDec > ( iDir >> 1 ) && iDec < ( iDir << 1 ) && iDec < 4000 );
      }
   }

   if( ( iHeight < iFH || iWidth < iFW ) &&
       ( pQTC->fontAttribute & HB_GTI_FONTA_CLRBKG ) != 0 &&
       ( pQTC->fontAttribute & HB_GTI_FONTA_DRAWBOX ) != 0 &&
       ( pQTC->fontAttribute & HB_GTI_FONTA_FIXMETRIC ) != 0 )
   {
      if( iHeight < iFH )
         iHeight = iFH;
      if( iWidth < iFW )
         iWidth = iFW;
   }

   pQTC->fontHeight = iHeight;
   pQTC->fontWidth  = iWidth;
   pQTC->fontAscent = iAscent;

   pQTC->fRepaint = pQTC->cellX != pQTC->fontWidth ||
                    pQTC->cellY != pQTC->fontHeight;

   pQTC->cellX = pQTC->fontWidth;
   pQTC->cellY = pQTC->fontHeight;

   if( pQTC->fRepaint )
      hb_gt_qtc_resetBoxCharBitmaps( pQTC );
   setImageSize();
}

void QTConsole::setImageSize( void )
{
   int iWidth  = pQTC->cellX * pQTC->iCols;
   int iHeight = pQTC->cellY * pQTC->iRows;

   if( iWidth != image->width() || iHeight != image->height() )
   {
      delete image;
      image = new QImage( iWidth, iHeight, QImage::Format_RGB32 );
      pQTC->fRepaint = HB_TRUE;
   }
   if( pQTC->fRepaint )
   {
      image->fill( BLACK );
      repaintChars( image->rect() );
      pQTC->fRepaint = HB_FALSE;
   }
}

void QTConsole::resizeEvent( QResizeEvent * event )
{
   int iWidth  = width();
   int iHeight = height();

   if( iWidth != image->width() || iHeight != image->height() )
   {
      resizeMode = true;

      if( pQTC->iResizeMode == HB_GTI_RESIZEMODE_ROWS )
      {
         int iRows  = iHeight / pQTC->cellY;
         int iCols  = iWidth / pQTC->cellX;

         if( hb_gt_qtc_setWindowSize( pQTC, iRows, iCols ) )
            setImageSize();
      }
      else
         setFontSize( iHeight / pQTC->iRows, iWidth / pQTC->iCols );

      update();
   }
   else
      QWidget::resizeEvent( event );
}

static QRect hb_gt_qtc_cellToPixel( PHB_GTQTC pQTC, const QRect & rc )
{
   return QRect( rc.left()   * pQTC->cellX,
                 rc.top()    * pQTC->cellY,
                 rc.width()  * pQTC->cellX,
                 rc.height() * pQTC->cellY );
}

static QRect hb_gt_qtc_pixelToCell( PHB_GTQTC pQTC, const QRect & rx )
{
   QRect rc;
   rc.setCoords( rx.left()   / pQTC->cellX,
                 rx.top()    / pQTC->cellY,
                 rx.right()  / pQTC->cellX,
                 rx.bottom() / pQTC->cellY );
   return rc;
}

static QRect hb_gt_qtc_mapRect( PHB_GTQTC pQTC, const QImage * image, const QRect & rx )
{
   return hb_gt_qtc_pixelToCell( pQTC,
             ( rx.normalized() & QRect( pQTC->marginLeft, pQTC->marginTop,
                                        image->width(), image->height() ) )
             .translated( -pQTC->marginLeft, -pQTC->marginTop ) );
}

static QRect hb_gt_qtc_unmapRect( PHB_GTQTC pQTC, const QRect & rc )
{
   return hb_gt_qtc_cellToPixel( pQTC, rc ).translated( pQTC->marginLeft,
                                                        pQTC->marginTop );
}

void QTConsole::copySelection( void )
{
   const QRect rc = hb_gt_qtc_mapRect( pQTC, image, selectRect );
   int iRow, iCol;
   QString qStrEol( hb_conNewLine() );
   QString qStr( "" );

   qStr.reserve( rc.height() * ( rc.width() + qStrEol.size() ) );

   selectMode = false;
   update( hb_gt_qtc_unmapRect( pQTC, rc ) );

   for( iRow = rc.top(); iRow <= rc.bottom(); ++iRow )
   {
      for( iCol = rc.left(); iCol <= rc.right(); ++iCol )
      {
         int iColor;
         HB_BYTE bAttr;
         HB_USHORT usChar;

         if( !HB_GTSELF_GETSCRCHAR( pQTC->pGT, iRow, iCol, &iColor, &bAttr, &usChar ) )
            break;
         qStr += ( QChar ) usChar;
      }
      if( rc.height() > 1 )
         qStr += qStrEol;
   }

   QApplication::clipboard()->setText( qStr );
}

void QTConsole::repaintChars( const QRect & rx )
{
   int       iCol, iRow, iStartCol, iLen, iColor, iTextColor = 0;
   HB_BYTE   bAttr;
   HB_USHORT usChar;
   bool      bClrBkg = ( pQTC->fontAttribute & HB_GTI_FONTA_CLRBKG ) != 0;
   bool      bFixMetric = ( pQTC->fontAttribute & HB_GTI_FONTA_FIXMETRIC ) != 0;
   QRect     rc = hb_gt_qtc_pixelToCell( pQTC, rx );

   QPainter painter( image );
   painter.setFont( QFont( font, painter.device() ) );
   if( bClrBkg )
      painter.setBackgroundMode( Qt::TransparentMode );
   else
      painter.setBackgroundMode( Qt::OpaqueMode );

   for( iRow = rc.top(); iRow <= rc.bottom(); ++iRow )
   {
      iCol = iStartCol = rc.left();
      iLen = 0;

      while( iCol <= rc.right() )
      {
         if( !HB_GTSELF_GETSCRCHAR( pQTC->pGT, iRow, iCol, &iColor, &bAttr, &usChar ) )
            break;

         if( ( pQTC->fontAttribute & HB_GTI_FONTA_CTRLCHARS ) == 0 )
            usChar = hb_cdpGetU16Ctrl( usChar );
         iColor &= 0xff;

         QBitmap * qBitMap = hb_gt_qtc_getBoxChar( pQTC, &usChar );

         if( iLen > 0 && ( iColor != iTextColor || bFixMetric || qBitMap ) )
         {
            if( bClrBkg )
               painter.fillRect( iStartCol * pQTC->cellX, iRow * pQTC->cellY,
                                 iLen * pQTC->cellX, pQTC->cellY,
                                 pQTC->colors[ iTextColor >> 4 ] );
            else
               painter.setBackground( QBrush( pQTC->colors[ iTextColor >> 4 ] ) );
            painter.setPen( pQTC->colors[ iTextColor & 0x0F ] );
            painter.drawText( iStartCol * pQTC->cellX,
                              ( iRow * pQTC->cellY ) + pQTC->fontAscent,
                              QString( pQTC->textLine, iLen ) );
            iLen = 0;
         }
         if( qBitMap )
         {
            painter.setBackgroundMode( Qt::OpaqueMode );
            painter.setBackground( QBrush( pQTC->colors[ iColor >> 4 ] ) );
            painter.setPen( pQTC->colors[ iColor & 0x0F ] );
            painter.drawPixmap( iCol * pQTC->cellX, iRow * pQTC->cellY, *qBitMap );
            if( bClrBkg )
               painter.setBackgroundMode( Qt::TransparentMode );
         }
         else
         {
            if( iLen == 0 )
            {
               iTextColor = iColor;
               iStartCol  = iCol;
            }
            pQTC->textLine[ iLen++ ] = usChar;
         }
         iCol++;
      }
      if( iLen > 0 )
      {
         if( bClrBkg )
            painter.fillRect( iStartCol * pQTC->cellX, iRow * pQTC->cellY,
                              iLen * pQTC->cellX, pQTC->cellY,
                              pQTC->colors[ iTextColor >> 4 ] );
         else
            painter.setBackground( QBrush( pQTC->colors[ iTextColor >> 4 ] ) );
         painter.setPen( pQTC->colors[ iTextColor & 0x0F ] );
         painter.drawText( iStartCol * pQTC->cellX,
                           ( iRow * pQTC->cellY ) + pQTC->fontAscent,
                           QString( pQTC->textLine, iLen ) );
      }
   }

   update( rx.translated( pQTC->marginLeft, pQTC->marginTop ) );
}

void QTConsole::paintEvent( QPaintEvent * event )
{
   QPainter painter( this );
   QRect rEvt = event->rect();

   if( rEvt.left() < pQTC->marginLeft )
   {
      QRect rc = rEvt;
      rc.setRight( pQTC->marginLeft );
      painter.fillRect( rc, QBrush( BLACK ) );
      rEvt.setLeft( pQTC->marginLeft );
   }
   if( rEvt.top() < pQTC->marginTop )
   {
      QRect rc = rEvt;
      rc.setBottom( pQTC->marginTop );
      painter.fillRect( rc, QBrush( BLACK ) );
      rEvt.setTop( pQTC->marginTop );
   }
   if( rEvt.right() > pQTC->marginLeft + image->width() )
   {
      QRect rc = rEvt;
      rc.setLeft( pQTC->marginLeft + image->width() );
      painter.fillRect( rc, QBrush( BLACK ) );
      rEvt.setRight( pQTC->marginLeft + image->width() - 1 );
   }
   if( rEvt.bottom() > pQTC->marginTop + image->height() )
   {
      QRect rc = rEvt;
      rc.setTop( pQTC->marginTop + image->height() );
      painter.fillRect( rc, QBrush( BLACK ) );
      rEvt.setBottom( pQTC->marginTop + image->height() - 1 );
   }

   painter.drawImage( rEvt, *image, rEvt.translated( -pQTC->marginLeft, -pQTC->marginTop ) );

   if( selectMode )
   {
      /* Display selection */
      QRect rSel = hb_gt_qtc_unmapRect( pQTC, hb_gt_qtc_mapRect( pQTC, image, selectRect ) );
      if( rSel.intersects( rEvt ) )
      {
#if defined( HB_OS_DARWIN )
         /* RasterOp operations are not supported in MacOSX */
         rEvt &= rSel;
         image->invertPixels();
         painter.drawImage( rEvt, *image, rEvt.translated( -pQTC->marginLeft, -pQTC->marginTop ) );
         image->invertPixels();
#else
         painter.setCompositionMode( QPainter::RasterOp_SourceXorDestination );
         painter.fillRect( rSel & rEvt, Qt::color0 );
#endif
      }
   }
   else if( pQTC->cursorType != SC_NONE )
   {
      /* Display cursor */
      QRect rCrs( pQTC->cursorCol * pQTC->cellX + pQTC->marginLeft,
                  pQTC->cursorRow * pQTC->cellY + pQTC->marginTop + pQTC->cursorOffset,
                  pQTC->cellX, pQTC->cursorSize );
      if( rEvt.intersects( rCrs ) )
      {
#if defined( HB_OS_DARWIN )
         /* RasterOp operations are not supported in MacOSX,
          * use foreground cell color like hardware VGA cursor
          */
         HB_BYTE   bAttr;
         HB_USHORT usChar;
         int       iColor;

         if( HB_GTSELF_GETSCRCHAR( pQTC->pGT, pQTC->cursorRow, pQTC->cursorCol,
                                   &iColor, &bAttr, &usChar ) )
         {
            painter.fillRect( rCrs, pQTC->colors[ iColor & 0x0F ] );
         }
#else
         painter.setCompositionMode( QPainter::RasterOp_SourceXorDestination );
         /* TODO? use foreground cell color like hardware VGA cursor ? */
         painter.fillRect( rCrs, Qt::color0 );
#endif
      }
   }
}

void QTConsole::timerEvent( QTimerEvent * event )
{
   if( event->timerId() == timer->timerId() )
   {
      if( hasFocus() )
      {
         pQTC->cursorVisible = !pQTC->cursorVisible;
         hb_gt_qtc_updateCursor( pQTC );
      }
   }
   else
      QWidget::timerEvent( event );
}

void QTConsole::focusInEvent( QFocusEvent * event )
{
   hb_gt_qtc_addKeyToInputQueue( pQTC, HB_K_GOTFOCUS );
   QWidget::focusInEvent( event );
}

void QTConsole::focusOutEvent( QFocusEvent * event )
{
   hb_gt_qtc_addKeyToInputQueue( pQTC, HB_K_LOSTFOCUS );
   QWidget::focusOutEvent( event );
}

void QTConsole::mouseMoveEvent( QMouseEvent * event )
{
   if( pQTC->fSelectCopy &&
       ( event->buttons() & Qt::LeftButton ) &&
       ( event->modifiers() & Qt::ShiftModifier ) )
   {
      if( !selectMode )
      {
         selectMode = true;
         selectRect.setCoords( event->x(), event->y(), event->x(), event->y() );
         update( hb_gt_qtc_unmapRect( pQTC, hb_gt_qtc_mapRect( pQTC, image, selectRect ) ) );
      }
      else
      {
         QRect rSel1 = hb_gt_qtc_unmapRect( pQTC, hb_gt_qtc_mapRect( pQTC, image, selectRect ) );
         selectRect.setBottomRight( event->pos() );
         QRect rSel2 = hb_gt_qtc_unmapRect( pQTC, hb_gt_qtc_mapRect( pQTC, image, selectRect ) );
         if( rSel1 != rSel2 )
            update( QRegion( rSel1 ).xored( QRegion( rSel2 ) ) );
      }
   }

   hb_gt_qtc_setMouseKey( pQTC, event->x(), event->y(), 0, event->modifiers() );
}

void QTConsole::wheelEvent( QWheelEvent * event )
{
   int iKey;

   switch( event->orientation() )
   {
      case Qt::Vertical:
         if( event->delta() < 0 )
            iKey = K_MWBACKWARD;
         else
            iKey = K_MWFORWARD;
         break;

      case Qt::Horizontal:
         /* TODO? add support for horizontal wheels */
      default:
         QWidget::wheelEvent( event );
         return;
   }

   hb_gt_qtc_setMouseKey( pQTC, event->x(), event->y(), iKey, event->modifiers() );
}

void QTConsole::mouseDoubleClickEvent( QMouseEvent * event )
{
   int iKey;

   switch( event->button() )
   {
      case Qt::LeftButton:
         iKey = K_LDBLCLK;
         break;

      case Qt::RightButton:
         iKey = K_RDBLCLK;;
         break;

      case Qt::MidButton:
         iKey = K_MDBLCLK;;
         break;

      default:
         QWidget::mouseDoubleClickEvent( event );
         return;
   }

   hb_gt_qtc_setMouseKey( pQTC, event->x(), event->y(), iKey, event->modifiers() );
}

void QTConsole::mousePressEvent( QMouseEvent * event )
{
   int iKey;

   switch( event->button() )
   {
      case Qt::LeftButton:
         iKey = K_LBUTTONDOWN;
         break;

      case Qt::RightButton:
         iKey = K_RBUTTONDOWN;
         break;

      case Qt::MidButton:
         iKey = K_MBUTTONDOWN;
         break;

      default:
         QWidget::mousePressEvent( event );
         return;
   }

   hb_gt_qtc_setMouseKey( pQTC, event->x(), event->y(), iKey, event->modifiers() );
}

void QTConsole::mouseReleaseEvent( QMouseEvent * event )
{
   int iKey;

   switch( event->button() )
   {
      case Qt::LeftButton:
         iKey = K_LBUTTONUP;
         break;

      case Qt::RightButton:
         iKey = K_RBUTTONUP;
         break;

      case Qt::MidButton:
         iKey = K_MBUTTONUP;
         break;

      default:
         QWidget::mouseReleaseEvent( event );
         return;
   }

   hb_gt_qtc_setMouseKey( pQTC, event->x(), event->y(), iKey, event->modifiers() );
}

bool QTConsole::event( QEvent * event )
{
   if( resizeMode )
   {
      switch( event->type() )
      {
         case QEvent::Enter:
         case QEvent::Leave:
         case QEvent::MouseMove:
         case QEvent::FocusIn:
         case QEvent::FocusOut:
         case QEvent::ChildRemoved:
         case QEvent::UpdateRequest:
            resizeMode = false;
            update();
            pQTC->qWnd->setWindowSize();
            break;
         default:
            break;
      }
   }

   return QWidget::event( event );
}

void QTConsole::keyReleaseEvent( QKeyEvent * event )
{
   if( selectMode && ( event->modifiers() & Qt::ShiftModifier ) == 0 )
      copySelection();

   QWidget::keyReleaseEvent( event );
}

void QTConsole::keyPressEvent( QKeyEvent * event )
{
   int iKey = 0, iFlags = hb_gt_qtc_getKeyFlags( event->modifiers() ),
       iSize, i;

   /* support for national characters */
   if( ( iSize = event->text().size() ) > 0 )
   {
      QString qStr = event->text();
      HB_WCHAR wc = qStr[ 0 ].unicode();

      if( iSize > 1 || ( wc >= 32 && wc != 127 ) )
      {
         if( ( iFlags & HB_KF_CTRL ) != 0 && ( iFlags & HB_KF_ALT ) != 0 )
            /* workaround for AltGR and German keyboard */
            iFlags &= ~( HB_KF_CTRL | HB_KF_ALT );

         for( i = 0; i < iSize; ++i )
         {
            wc = qStr[ i ].unicode();
            hb_gt_qtc_addKeyToInputQueue( pQTC, HB_INKEY_NEW_UNICODEF( wc, iFlags ) );
         }
         return;
      }
   }

   switch( event->key() )
   {
      case Qt::Key_F1:
         iKey = HB_KX_F1;
         break;
      case Qt::Key_F2:
         iKey = HB_KX_F2;
         break;
      case Qt::Key_F3:
         iKey = HB_KX_F3;
         break;
      case Qt::Key_F4:
         iKey = HB_KX_F4;
         break;
      case Qt::Key_F5:
         iKey = HB_KX_F5;
         break;
      case Qt::Key_F6:
         iKey = HB_KX_F6;
         break;
      case Qt::Key_F7:
         iKey = HB_KX_F7;
         break;
      case Qt::Key_F8:
         iKey = HB_KX_F8;
         break;
      case Qt::Key_F9:
         iKey = HB_KX_F9;
         break;
      case Qt::Key_F10:
         iKey = HB_KX_F10;
         break;
      case Qt::Key_F11:
         iKey = HB_KX_F11;
         break;
      case Qt::Key_F12:
         iKey = HB_KX_F12;
         break;
      case Qt::Key_Up:
         iKey = HB_KX_UP;
         break;
      case Qt::Key_Down:
         iKey = HB_KX_DOWN;
         break;
      case Qt::Key_Left:
         iKey = HB_KX_LEFT;
         break;
      case Qt::Key_Right:
         iKey = HB_KX_RIGHT;
         break;
      case Qt::Key_Home:
         iKey = HB_KX_HOME;
         break;
      case Qt::Key_End:
         iKey = HB_KX_END;
         break;
      case Qt::Key_PageUp:
         iKey = HB_KX_PGUP;
         break;
      case Qt::Key_PageDown:
         iKey = HB_KX_PGDN;
         break;
      case Qt::Key_Insert:
         iKey = HB_KX_INS;
         break;
      case Qt::Key_Delete:
         iKey = HB_KX_DEL;
         break;
      case Qt::Key_Backspace:
         iKey = HB_KX_BS;
         break;
      case Qt::Key_Tab:
         iKey = HB_KX_TAB;
         break;
      case Qt::Key_Backtab:
         iKey = HB_KX_TAB;
         iFlags |= HB_KF_SHIFT;
         break;
      case Qt::Key_Escape:
         iKey = HB_KX_ESC;
         break;
      case Qt::Key_Return:
      case Qt::Key_Enter:
         if( pQTC->fAltEnter && ( iFlags & HB_KF_ALT ) != 0 &&
                                ( iFlags & HB_KF_KEYPAD ) == 0 )
         {
            pQTC->fFullScreen = ( pQTC->qWnd->windowState() & Qt::WindowFullScreen ) == 0;
            hb_gt_qtc_setWindowState( pQTC, Qt::WindowFullScreen, pQTC->fFullScreen );
            return;
         }
         iKey = HB_KX_ENTER;
         break;
      case Qt::Key_Clear:
         iKey = HB_KX_CENTER;
         break;
      case Qt::Key_Print:
         iKey = HB_KX_PRTSCR;
         break;
      case Qt::Key_Pause:
         iKey = HB_KX_PAUSE;
         break;
      case Qt::Key_Space:
         iKey = ' ';
         break;
      case Qt::Key_Exclam:
         iKey = '!';
         break;
      case Qt::Key_QuoteDbl:
         iKey = '"';
         break;
      case Qt::Key_NumberSign:
         iKey = '#';
         break;
      case Qt::Key_Dollar:
         iKey = '$';
         break;
      case Qt::Key_Percent:
         iKey = '%';
         break;
      case Qt::Key_Ampersand:
         iKey = '&';
         break;
      case Qt::Key_Apostrophe:
         iKey = '\'';
         break;
      case Qt::Key_ParenLeft:
         iKey = '(';
         break;
      case Qt::Key_ParenRight:
         iKey = ')';
         break;
      case Qt::Key_Asterisk:
         iKey = '*';
         break;
      case Qt::Key_Plus:
         iKey = '+';
         break;
      case Qt::Key_Comma:
         iKey = ',';
         break;
      case Qt::Key_Minus:
         iKey = '-';
         break;
      case Qt::Key_Period:
         iKey = '.';
         break;
      case Qt::Key_Slash:
         iKey = '/';
         break;
      case Qt::Key_0:
         iKey = '0';
         break;
      case Qt::Key_1:
         iKey = '1';
         break;
      case Qt::Key_2:
         iKey = '2';
         break;
      case Qt::Key_3:
         iKey = '3';
         break;
      case Qt::Key_4:
         iKey = '4';
         break;
      case Qt::Key_5:
         iKey = '5';
         break;
      case Qt::Key_6:
         iKey = '6';
         break;
      case Qt::Key_7:
         iKey = '7';
         break;
      case Qt::Key_8:
         iKey = '8';
         break;
      case Qt::Key_9:
         iKey = '9';
         break;
      case Qt::Key_Colon:
         iKey = ':';
         break;
      case Qt::Key_Semicolon:
         iKey = ';';
         break;
      case Qt::Key_Less:
         iKey = '<';
         break;
      case Qt::Key_Equal:
         iKey = '=';
         break;
      case Qt::Key_Greater:
         iKey = '>';
         break;
      case Qt::Key_Question:
         iKey = '?';
         break;
      case Qt::Key_At:
         iKey = '@';
         break;
      case Qt::Key_A:
         iKey = 'A';
         break;
      case Qt::Key_B:
         iKey = 'B';
         break;
      case Qt::Key_C:
         iKey = 'C';
         break;
      case Qt::Key_D:
         iKey = 'D';
         break;
      case Qt::Key_E:
         iKey = 'E';
         break;
      case Qt::Key_F:
         iKey = 'F';
         break;
      case Qt::Key_G:
         iKey = 'G';
         break;
      case Qt::Key_H:
         iKey = 'H';
         break;
      case Qt::Key_I:
         iKey = 'I';
         break;
      case Qt::Key_J:
         iKey = 'J';
         break;
      case Qt::Key_K:
         iKey = 'K';
         break;
      case Qt::Key_L:
         iKey = 'L';
         break;
      case Qt::Key_M:
         iKey = 'M';
         break;
      case Qt::Key_N:
         iKey = 'N';
         break;
      case Qt::Key_O:
         iKey = 'O';
         break;
      case Qt::Key_P:
         iKey = 'P';
         break;
      case Qt::Key_Q:
         iKey = 'Q';
         break;
      case Qt::Key_R:
         iKey = 'R';
         break;
      case Qt::Key_S:
         iKey = 'S';
         break;
      case Qt::Key_T:
         iKey = 'T';
         break;
      case Qt::Key_U:
         iKey = 'U';
         break;
      case Qt::Key_V:
         iKey = 'V';
         break;
      case Qt::Key_W:
         iKey = 'W';
         break;
      case Qt::Key_X:
         iKey = 'X';
         break;
      case Qt::Key_Y:
         iKey = 'Y';
         break;
      case Qt::Key_Z:
         iKey = 'Z';
         break;
      case Qt::Key_BracketLeft:
         iKey = '[';
         break;
      case Qt::Key_Backslash:
         iKey = '\\';
         break;
      case Qt::Key_BracketRight:
         iKey = ']';
         break;
      case Qt::Key_AsciiCircum:
         iKey = '^';
         break;
      case Qt::Key_Underscore:
         iKey = '_';
         break;
      case Qt::Key_QuoteLeft:
         iKey = '`';
         break;
      case Qt::Key_BraceLeft:
         iKey = '{';
         break;
      case Qt::Key_Bar:
         iKey = '|';
         break;
      case Qt::Key_BraceRight:
         iKey = '}';
         break;
      case Qt::Key_AsciiTilde:
         iKey = '~';
         break;
      default:
         break;
   }

   if( iKey != 0 )
      hb_gt_qtc_addKeyToInputQueue( pQTC, HB_INKEY_NEW_KEY( iKey, iFlags ) );
   else
      QWidget::keyPressEvent( event );
}

/* *********************************************************************** */

QTCWindow::QTCWindow( PHB_GTQTC pQTC )
{
   Qt::WindowFlags flags = ( windowFlags() & Qt::WindowType_Mask ) |
                           Qt::CustomizeWindowHint                 |
                           Qt::WindowMinimizeButtonHint            |
                           Qt::WindowSystemMenuHint                |
                           Qt::WindowTitleHint                     |
                           Qt::Window;
   if( pQTC->iCloseMode < 2 )
      flags |= Qt::WindowCloseButtonHint;
   if( pQTC->fResizable )
      flags |= Qt::WindowMaximizeButtonHint;

   setWindowFlags( flags );

   /* set default size used when leaving initial
    * fullscreen or maximized mode [druzus]
    */
   resize( pQTC->cellX * pQTC->iCols, pQTC->cellY * pQTC->iRows );

   if( pQTC->fMaximized )
      setWindowState( windowState() | Qt::WindowMaximized );
   if( pQTC->fFullScreen )
      setWindowState( windowState() | Qt::WindowFullScreen );

   if( pQTC->qIcon )
      setWindowIcon( *pQTC->qIcon );
   setWindowTitle( *pQTC->wndTitle );

   qConsole = new QTConsole( pQTC );
   setCentralWidget( qConsole );
   setFocusProxy( qConsole );
   setFocusPolicy( Qt::StrongFocus );
   /* In windows it helps to keep focus in fullscreen or maximized
    * windows [druzus]
    */
   setFocus( Qt::OtherFocusReason );
}

QTCWindow::~QTCWindow( void )
{
   delete qConsole;
}

void QTCWindow::closeEvent( QCloseEvent * event )
{
   if( qConsole->pQTC->iCloseMode == 0 )
   {
      PHB_ITEM pItem = hb_itemPutL( NULL, HB_TRUE );
      hb_setSetItem( HB_SET_CANCEL, pItem );
      hb_itemRelease( pItem );
      hb_vmRequestCancel();
   }
   else
      hb_gt_qtc_addKeyToInputQueue( qConsole->pQTC, HB_K_CLOSE );

   event->ignore();
}

void QTCWindow::setWindowSize( void )
{
   if( ( windowState() & ( Qt::WindowMaximized | Qt::WindowFullScreen ) ) != 0 )
   {
      qConsole->pQTC->marginLeft = width() - qConsole->image->width();
      if( qConsole->pQTC->marginLeft > 0 )
         qConsole->pQTC->marginLeft >>= 1;
      else
         qConsole->pQTC->marginLeft = 0;

      qConsole->pQTC->marginTop = height() - qConsole->image->height();
      if( qConsole->pQTC->marginTop > 0 )
         qConsole->pQTC->marginTop >>= 1;
      else
         qConsole->pQTC->marginTop = 0;
   }
   else
   {
      qConsole->pQTC->marginLeft = qConsole->pQTC->marginTop = 0;
      resize( qConsole->image->size() );
   }
   setResizing();
}

void QTCWindow::setResizing( void )
{
   if( qConsole->pQTC->fResizable )
   {
      setMaximumSize( QApplication::desktop()->screenGeometry().size() );

      if( qConsole->pQTC->iResizeMode == HB_GTI_RESIZEMODE_ROWS )
      {
         setMinimumSize( qConsole->pQTC->cellX << 1, qConsole->pQTC->cellY << 1 );
         if( !qConsole->pQTC->fResizeInc || ( windowState() & Qt::WindowMaximized ) != 0 )
            setSizeIncrement( 0, 0 );
         else
            setSizeIncrement( qConsole->pQTC->cellX, qConsole->pQTC->cellY );
      }
      else
      {
         setMinimumSize( qConsole->pQTC->iCols << 1, qConsole->pQTC->iRows << 2 );
         if( !qConsole->pQTC->fResizeInc || ( windowState() & Qt::WindowMaximized ) != 0 )
            setSizeIncrement( 0, 0 );
         else
            setSizeIncrement( qConsole->pQTC->iCols, qConsole->pQTC->iRows );
      }
   }
   else
   {
      setFixedSize( size() );
      setSizeIncrement( 0, 0 );
   }
}

/* *********************************************************************** */

#ifdef HB_XLIB_NEEDLOCKS

#include <X11/Xlib.h>

static void hb_gt_qtc_InitMT( void )
{
   if( hb_vmIsMt() )
   {
      if( ! XInitThreads() )
         hb_errInternal( 10002, "XInitThreads() failed !!!", NULL, NULL );
   }
}

#else

static void hb_gt_qtc_InitMT( void ) { }

#endif

/* *********************************************************************** */
