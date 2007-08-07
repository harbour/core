/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Clipper Tools like window system
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.h"
#include "hbapigt.h"
#include "hbgtcore.h"
#include "ctwin.h"

HB_FUNC( CTWINIT )
{
   hb_retl( hb_ctw_Init() );
}

HB_FUNC( GETCLEARA )
{
   hb_retni( hb_gt_GetClearColor() );
}

HB_FUNC( SETCLEARA )
{
   if( ISNUM( 1 ) )
      hb_gt_SetClearColor( hb_parni( 1 ) & 0xff );
   else if( hb_parclen( 1 ) > 0 )
      hb_gt_SetClearColor( hb_gtColorToN( hb_parc( 1 ) ) );

   hb_retc( NULL );
}

HB_FUNC( SETCLEARB )
{
   int iNew;

   if( ISNUM( 1 ) )
      iNew = hb_parni( 1 );
   else if( ISCHAR( 1 ) )
      iNew = hb_parc( 1 )[0];
   else
      iNew = 255;

   hb_gt_SetClearChar( iNew & 0xff );

   hb_retc( NULL );
}

HB_FUNC( GETCLEARB )
{
   hb_retni( hb_gt_GetClearChar() );
}

HB_FUNC( WSETSHADOW )
{
   hb_retni( hb_ctw_SetShadowAttr( ISNUM( 1 ) ? hb_parni( 1 ) : -2 ) );
}

HB_FUNC( WSETMOVE )
{
   hb_retl( hb_ctw_SetMoveMode( ISLOG( 1 ) ? hb_parl( 1 ) : -1 ) != 0 );
}

HB_FUNC( WSTEP )
{
   if( ISNUM( 1 ) && ISNUM( 2 ) )
      hb_retni( hb_ctw_SetMoveStep( hb_parni( 1 ), hb_parni( 2 ) ) );
   else
      hb_retni( -1 );
}

HB_FUNC( WMODE )
{
   hb_retni( hb_ctw_SetBorderMode( ISLOG( 1 ) ? ( hb_parl( 1 ) ? 1 : 0 ) : -1,
                                   ISLOG( 2 ) ? ( hb_parl( 2 ) ? 1 : 0 ) : -1,
                                   ISLOG( 3 ) ? ( hb_parl( 3 ) ? 1 : 0 ) : -1,
                                   ISLOG( 4 ) ? ( hb_parl( 4 ) ? 1 : 0 ) : -1 ) );
}

HB_FUNC( WBOARD )
{
   int iResult;

   iResult = hb_ctw_SetWindowBoard( hb_parni( 1 ), hb_parni( 2 ),
                                    ISNUM( 3 ) ? hb_parni( 3 ) : hb_gtMaxRow(),
                                    ISNUM( 4 ) ? hb_parni( 4 ) : hb_gtMaxCol() );
   hb_gt_Flush();
   hb_retni( iResult );
}

HB_FUNC( WOPEN )
{
   int iResult, iColor;

   iColor = ISCHAR( 6 ) ? hb_gtColorToN( hb_parc( 6 ) ) : hb_parni( 6 );
   iResult = hb_ctw_CreateWindow( hb_parni( 1 ), hb_parni( 2 ),
                                  hb_parni( 3 ), hb_parni( 4 ),
                                  hb_parl( 5 ), iColor );
   hb_gt_Flush();
   hb_retni( iResult );
}

HB_FUNC( WCLOSE )
{
   int iResult;

   iResult = hb_ctw_CloseWindow( hb_ctw_CurrentWindow() );
   hb_gt_Flush();
   hb_retni( iResult );
}

HB_FUNC( WACLOSE )
{
   int iResult;

   iResult = hb_ctw_CloseAllWindows();
   hb_gt_Flush();
   hb_retni( iResult );
}

HB_FUNC( WSELECT )
{
   int iResult;

   if( ISNUM( 1 ) )
      iResult = hb_ctw_SelectWindow( hb_parni( 1 ) );
   else
      iResult =  hb_ctw_CurrentWindow();
   hb_gt_Flush();
   hb_retni( iResult );
}

HB_FUNC( WNUM )
{
   hb_retni( hb_ctw_MaxWindow() );
}

HB_FUNC( WBOX )
{
   static const char * pWBoxFrames[] = {
            _B_DOUBLE,        /* 0  WB_DOUBLE_CLEAR */
            _B_SINGLE,        /* 1  WB_SINGLE_CLEAR */
            _B_DOUBLE_SINGLE, /* 2  WB_DOUBLE_SINGLE_CLEAR */
            _B_SINGLE_DOUBLE, /* 3  WB_SINGLE_DOUBLE_CLEAR */

            _B_DOUBLE,        /* 4  WB_DOUBLE */
            _B_SINGLE,        /* 5  WB_SINGLE */
            _B_DOUBLE_SINGLE, /* 6  WB_DOUBLE_SINGLE */
            _B_SINGLE_DOUBLE, /* 7  WB_SINGLE_DOUBLE */

            "лплллмлл",       /* 8  WB_HALF_FULL_CLEAR */
            "опнннмоо",       /* 9  WB_HALF_CLEAR */
            "олнннлоо",       /* 10 WB_FULL_HALF_CLEAR */
            "лллллллл",       /* 11 WB_FULL_CLEAR */

            "лплллмлл",       /* 12 WB_HALF_FULL */
            "опнннмоо",       /* 13 WB_HALF */
            "олнннлоо",       /* 14 WB_FULL_HALF */
            "лллллллл"  };    /* 15 WB_FULL */

   BYTE * szBox, szBoxBuf[ 10 ];
   int iResult, iColor;

   if( ISCHAR( 1 ) )
   {
      szBox = ( BYTE * ) hb_parc( 1 );
   }
   else
   {
      int iFrame = hb_parni( 1 );

      if( iFrame < 0 || iFrame > 15 )
         iFrame = 0;
      memcpy( szBoxBuf, pWBoxFrames[ iFrame ], 9 );
      if( ( iFrame & 4 ) == 0 )
      {
         szBoxBuf[ 8 ] = hb_gt_GetClearChar();
      }
      szBoxBuf[ 9 ] = '0';
      szBox = szBoxBuf;
   }

   iColor = ISCHAR( 2 ) ? hb_gtColorToN( hb_parc( 2 ) ) : hb_parni( 2 );
   iResult = hb_ctw_AddWindowBox( hb_ctw_CurrentWindow(), szBox, iColor );
   hb_gt_Flush();
   hb_retni( iResult );
}

HB_FUNC( WFORMAT )
{
   hb_retni( hb_ctw_ChangeMargins( hb_ctw_CurrentWindow(),
                                   hb_parni( 1 ), hb_parni( 2 ),
                                   hb_parni( 3 ), hb_parni( 4 ) ) );
}

HB_FUNC( WROW )
{
   int iTop, iLeft, iBottom, iRight;

   hb_ctw_GetWindowCords( hb_ctw_CurrentWindow(), hb_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   hb_retni( iTop );
}

HB_FUNC( WCOL )
{
   int iTop, iLeft, iBottom, iRight;

   hb_ctw_GetWindowCords( hb_ctw_CurrentWindow(), hb_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   hb_retni( iLeft );
}

HB_FUNC( WLASTROW )
{
   int iTop, iLeft, iBottom, iRight;

   hb_ctw_GetWindowCords( hb_ctw_CurrentWindow(), hb_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   hb_retni( iBottom );
}

HB_FUNC( WLASTCOL )
{
   int iTop, iLeft, iBottom, iRight;

   hb_ctw_GetWindowCords( hb_ctw_CurrentWindow(), hb_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   hb_retni( iRight );
}

HB_FUNC( WFROW )
{
   int iTop, iLeft, iBottom, iRight;

   hb_ctw_GetFormatCords( hb_ctw_CurrentWindow(), hb_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   hb_retni( iTop );
}

HB_FUNC( WFCOL )
{
   int iTop, iLeft, iBottom, iRight;

   hb_ctw_GetFormatCords( hb_ctw_CurrentWindow(), hb_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   hb_retni( iLeft );
}

HB_FUNC( WFLASTROW )
{
   int iTop, iLeft, iBottom, iRight;

   hb_ctw_GetFormatCords( hb_ctw_CurrentWindow(), hb_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   hb_retni( iBottom );
}

HB_FUNC( WFLASTCOL )
{
   int iTop, iLeft, iBottom, iRight;

   hb_ctw_GetFormatCords( hb_ctw_CurrentWindow(), hb_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   hb_retni( iRight );
}

HB_FUNC( WCENTER )
{
   hb_retni( hb_ctw_CenterWindow( hb_ctw_CurrentWindow(), hb_parl( 1 ) ) );
}

HB_FUNC( WMOVE )
{
   hb_retni( hb_ctw_MoveWindow( hb_ctw_CurrentWindow(),
                                hb_parni( 1 ), hb_parni( 2 ) ) );
}
