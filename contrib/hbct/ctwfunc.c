/*
 * Harbour Project source code:
 * Clipper Tools like window system
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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
#include "hbapistr.h"
#include "ctwin.h"

static int hb_ctColorParam( int iParam, int iDefault )
{
   int iColor;

   if( HB_ISNUM( iParam ) )
      iColor = hb_parni( iParam );
   else if( hb_parclen( iParam ) > 0 )
   {
      iColor = hb_gtColorToN( hb_parc( iParam ) );
      if( iColor == -1 )
         iColor = iDefault;
   }
   else
      iColor = iDefault;

   return iColor;
}

HB_FUNC( CTWINIT )
{
   hb_retl( hb_ctwInit() );
}

HB_FUNC( GETCLEARA )
{
   hb_retni( hb_gtGetClearColor() );
}

HB_FUNC( SETCLEARA )
{
   int iColor = hb_ctColorParam( 1, -1 );

   if( iColor >= 0 )
      hb_gtSetClearColor( iColor );

   hb_retc_null();
}

HB_FUNC( SETCLEARB )
{
   HB_USHORT usNew;

   if( HB_ISNUM( 1 ) )
   {
      int iChar = hb_parni( 1 );
      PHB_CODEPAGE cdp = hb_vmCDP();
      if( ! HB_CDP_ISCHARUNI( cdp ) )
         iChar = hb_cdpGetU16( cdp, ( HB_UCHAR ) iChar );
      usNew = ( HB_USHORT ) iChar;
   }
   else if( HB_ISCHAR( 1 ) )
      usNew = hb_cdpTextGetU16( hb_vmCDP(), hb_parc( 1 ), hb_parclen( 1 ) );
   else
      usNew = ' ';  /* CT uses 255 => U+00A0 in CP437 */

   hb_gtSetClearChar( usNew );

   hb_retc_null();
}

HB_FUNC( GETCLEARB )
{
   int iChar = hb_gtGetClearChar();
   PHB_CODEPAGE cdp = hb_vmCDP();

   if( ! HB_CDP_ISCHARUNI( cdp ) )
   {
      HB_UCHAR uc = hb_cdpGetUC( cdp, ( HB_WCHAR ) iChar, 0 );
      if( uc )
         iChar = uc;
   }

   hb_retni( iChar );
}

HB_FUNC( WSETSHADOW )
{
   hb_retni( hb_ctwSetShadowAttr( hb_ctColorParam( 1, HB_CTW_SHADOW_UNDEF ) ) );
}

HB_FUNC( WSETMOVE )
{
   hb_retl( hb_ctwSetMoveMode( hb_parldef( 1, -1 ) ) != 0 );
}

HB_FUNC( WSTEP )
{
   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
      hb_retni( hb_ctwSetMoveStep( hb_parni( 1 ), hb_parni( 2 ) ) );
   else
      hb_retni( -1 );
}

HB_FUNC( WMODE )
{
   hb_retni( hb_ctwSetBorderMode( hb_parldef( 1, -1 ),
                                  hb_parldef( 2, -1 ),
                                  hb_parldef( 3, -1 ),
                                  hb_parldef( 4, -1 ) ) );
}

HB_FUNC( WBOARD )
{
   hb_retni( hb_ctwSetWindowBoard( hb_parni( 1 ), hb_parni( 2 ),
                                   HB_ISNUM( 3 ) ? hb_parni( 3 ) : hb_gtMaxRow(),
                                   HB_ISNUM( 4 ) ? hb_parni( 4 ) : hb_gtMaxCol() ) );
}

HB_FUNC( WOPEN )
{
   int iColor;

   /* 6-th (color) and 7-th (lVisible) parameters are Harbour extensions */
   iColor = hb_ctColorParam( 6, -1 );   /* Harbour extension */ /* HB_EXTENSION */
   hb_retni( hb_ctwCreateWindow( hb_parni( 1 ), hb_parni( 2 ),
                                 hb_parni( 3 ), hb_parni( 4 ),
                                 hb_parl( 5 ), iColor,
                                 hb_parldef( 7, 1 ) ) ); /* HB_EXTENSION */
}

HB_FUNC( WCLOSE )
{
   /* 1-st parameter (window handle) is Harbour extension */
   hb_retni( hb_ctwCloseWindow( HB_ISNUM( 1 ) ? hb_parni( 1 ) : /* HB_EXTENSION */
                                             hb_ctwCurrentWindow() ) );
}

HB_FUNC( WACLOSE )
{
   hb_retni( hb_ctwCloseAllWindows() );
}

HB_FUNC( WSELECT )
{
   /* 2-nd parameter (fBringToTop) is Harbour extension */
   hb_retni( HB_ISNUM( 1 ) ? hb_ctwSelectWindow( hb_parni( 1 ),
                                                 hb_parldef( 2, 1 ) ) : /* HB_EXTENSION */
                          hb_ctwCurrentWindow() );
}

HB_FUNC( WNUM )
{
   hb_retni( hb_ctwMaxWindow() );
}

HB_FUNC( WBOX )
{
   static const HB_WCHAR s_pWBoxFrames[ 16 ][ 9 ] = {
      HB_B_DOUBLE_W,          /* 0  WB_DOUBLE_CLEAR */
      HB_B_SINGLE_W,          /* 1  WB_SINGLE_CLEAR */
      HB_B_DOUBLE_SINGLE_W,   /* 2  WB_DOUBLE_SINGLE_CLEAR */
      HB_B_SINGLE_DOUBLE_W,   /* 3  WB_SINGLE_DOUBLE_CLEAR */

      HB_B_DOUBLE_W,          /* 4  WB_DOUBLE */
      HB_B_SINGLE_W,          /* 5  WB_SINGLE */
      HB_B_DOUBLE_SINGLE_W,   /* 6  WB_DOUBLE_SINGLE */
      HB_B_SINGLE_DOUBLE_W,   /* 7  WB_SINGLE_DOUBLE */

      HB_B_HALF_FULL_W,       /* 8  WB_HALF_FULL_CLEAR */
      HB_B_HALF_W,            /* 9  WB_HALF_CLEAR */
      HB_B_FULL_HALF_W,       /* 10 WB_FULL_HALF_CLEAR */
      HB_B_FULL_W,            /* 11 WB_FULL_CLEAR */

      HB_B_HALF_FULL_W,       /* 12 WB_HALF_FULL */
      HB_B_HALF_W,            /* 13 WB_HALF */
      HB_B_FULL_HALF_W,       /* 14 WB_FULL_HALF */
      HB_B_FULL_W
   };                         /* 15 WB_FULL */

   HB_WCHAR szBoxBuf[ 10 ], wc;
   const char * pszBoxFrame = hb_parc( 1 );
   int iColor;

   if( pszBoxFrame )
   {
      HB_SIZE nLen = hb_parclen( 1 ), nIndex = 0, nSize = 0;
      PHB_CODEPAGE cdp = hb_gtBoxCP();

      while( nSize < HB_SIZEOFARRAY( szBoxBuf ) - 1 &&
             HB_CDPCHAR_GET( cdp, pszBoxFrame, nLen, &nIndex, &wc ) )
         szBoxBuf[ nSize++ ] = wc;
      szBoxBuf[ nSize ] = 0;
   }
   else
   {
      int iFrame = hb_parni( 1 );

      if( iFrame < 0 || iFrame > 15 )
         iFrame = 0;
      memcpy( szBoxBuf, s_pWBoxFrames[ iFrame ], 9 * sizeof( HB_WCHAR ) );
      if( ( iFrame & 4 ) == 0 )
         szBoxBuf[ 8 ] = hb_gtGetClearChar();
      szBoxBuf[ 9 ] = '\0';
   }

   iColor = hb_ctColorParam( 2, -1 );   /* Harbour extension */ /* HB_EXTENSION */
   hb_retni( hb_ctwAddWindowBox( hb_ctwCurrentWindow(), szBoxBuf, iColor ) );
}

HB_FUNC( WFORMAT )
{
   int iWindow = hb_ctwCurrentWindow();
   int iTop, iLeft, iBottom, iRight;

   if( hb_pcount() == 0 )
   {
      hb_ctwGetFormatCords( iWindow, HB_TRUE, &iTop, &iLeft, &iBottom, &iRight );
      iTop    = -iTop;
      iLeft   = -iLeft;
      iBottom = -iBottom;
      iRight  = -iRight;
   }
   else
   {
      iTop    = hb_parni( 1 );
      iLeft   = hb_parni( 2 );
      iBottom = hb_parni( 3 );
      iRight  = hb_parni( 4 );
   }
   hb_retni( hb_ctwChangeMargins( iWindow, iTop, iLeft, iBottom, iRight ) );
}

HB_FUNC( WROW )
{
   int iTop, iLeft, iBottom, iRight;

   hb_ctwGetWindowCords( hb_ctwCurrentWindow(), hb_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   hb_retni( iTop );
}

HB_FUNC( WCOL )
{
   int iTop, iLeft, iBottom, iRight;

   hb_ctwGetWindowCords( hb_ctwCurrentWindow(), hb_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   hb_retni( iLeft );
}

HB_FUNC( WLASTROW )
{
   int iTop, iLeft, iBottom, iRight;

   hb_ctwGetWindowCords( hb_ctwCurrentWindow(), hb_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   hb_retni( iBottom );
}

HB_FUNC( WLASTCOL )
{
   int iTop, iLeft, iBottom, iRight;

   hb_ctwGetWindowCords( hb_ctwCurrentWindow(), hb_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   hb_retni( iRight );
}

HB_FUNC( WFROW )
{
   int iTop, iLeft, iBottom, iRight;

   hb_ctwGetFormatCords( hb_ctwCurrentWindow(), hb_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   hb_retni( iTop );
}

HB_FUNC( WFCOL )
{
   int iTop, iLeft, iBottom, iRight;

   hb_ctwGetFormatCords( hb_ctwCurrentWindow(), hb_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   hb_retni( iLeft );
}

HB_FUNC( WFLASTROW )
{
   int iTop, iLeft, iBottom, iRight;

   hb_ctwGetFormatCords( hb_ctwCurrentWindow(), hb_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   hb_retni( iBottom );
}

HB_FUNC( WFLASTCOL )
{
   int iTop, iLeft, iBottom, iRight;

   hb_ctwGetFormatCords( hb_ctwCurrentWindow(), hb_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   hb_retni( iRight );
}

HB_FUNC( WCENTER )
{
   hb_retni( hb_ctwCenterWindow( hb_ctwCurrentWindow(), hb_parl( 1 ) ) );
}

HB_FUNC( WMOVE )
{
   hb_retni( hb_ctwMoveWindow( hb_ctwCurrentWindow(),
                               hb_parni( 1 ), hb_parni( 2 ) ) );
}

HB_FUNC( CTWLASTKEY )
{
   if( HB_ISNUM( 1 ) )
   {
      int iNewKey = hb_parni( 1 );
      hb_retni( hb_ctwLastKey( &iNewKey ) );
   }
   else
      hb_retni( hb_ctwLastKey( NULL ) );
}

/* NOTE: These two functions are emulating the MaxRow()/MaxCol() core functions
         "overloaded" by the CT3 library. */

HB_FUNC( HBCT_MAXROW ) /* Return the maximum screen/window row number (zero origin) */
{
   if( hb_parl( 1 ) )
   {
      int iRows, iCols;
      hb_gtScrDim( &iRows, &iCols );
      hb_retni( iRows - 1 );
   }
   else
      hb_retni( hb_gtMaxRow() );
}

HB_FUNC( HBCT_MAXCOL ) /* Return the maximum screen/window column number (zero origin) */
{
   if( hb_parl( 1 ) )
   {
      int iRows, iCols;
      hb_gtScrDim( &iRows, &iCols );
      hb_retni( iCols - 1 );
   }
   else
      hb_retni( hb_gtMaxCol() );
}

/* Undocumented CT3 window functions
 */

/*
   WAlias( <nHandle> ) -> <nHandle> | -1
   change current window handle to <nHandle>
   if <nHandle> is not used by other window
   or is current window.
 */
HB_FUNC( WALIAS )
{
   int iWindow = hb_parnidef( 1, -1 );

   /* 255 is original CT3 limit,
    * harbour CTWIN does not have such intenral limits
    */
   if( iWindow >= 0 && iWindow <= 255 )
      iWindow = hb_ctwChangeWindowHandle( iWindow );
   else
      iWindow = -1;

   hb_retni( iWindow );
}

/*
   WList() -> <cHandleList>
   _WStack() -> <cHandleList>
   return string with window handles in each character,
   the last character is the top window.

   Warning: this is compatibility only function
            which works correctly only for 255 windows.
 */

HB_FUNC( WLIST )
{
   const int * piStack;
   int iWindows, iFrom, i;

   iWindows = hb_ctwGetWindowStack( &piStack );
   if( iWindows < 0 )
      hb_retc_null();
   else if( iWindows == 0 )
      hb_retclen( "\000", 1 );
   else
   {
      char * pszWindows = ( char * ) hb_xgrab( iWindows + 2 );

      iFrom = 0;
      if( hb_ctwCurrentWindow() == 0 )
         pszWindows[ iWindows ] = 0;
      else
         pszWindows[ iFrom++ ] = 0;

      for( i = 0; i < iWindows; ++i )
         pszWindows[ iFrom + i ] = ( char ) piStack[ i ];

      hb_retclen_buffer( pszWindows, iWindows + 1 );
   }
}

HB_FUNC_TRANSLATE( _WSTACK, WLIST )

/* Temporary Harbour extensions to test some extended CTW functionality
 */

HB_FUNC( WHIDE ) /* HB_EXTENSION */
{
   hb_ctwVisible( HB_ISNUM( 1 ) ? hb_parni( 1 ) : hb_ctwCurrentWindow(),
                  HB_CTW_HIDDEN );
}

HB_FUNC( WSHOW ) /* HB_EXTENSION */
{
   hb_ctwVisible( HB_ISNUM( 1 ) ? hb_parni( 1 ) : hb_ctwCurrentWindow(),
                  HB_CTW_VISIBLE );
}

HB_FUNC( WSHADOW ) /* HB_EXTENSION */
{
   hb_retni( hb_ctwSetWindowShadow( hb_ctwCurrentWindow(),
                                    hb_parnidef( 1, HB_CTW_SHADOW_UNDEF ) /* nAttr */ ) );
}

HB_FUNC( WLEVEL ) /* HB_EXTENSION */
{
   hb_retni( hb_ctwSetWindowLevel( hb_ctwCurrentWindow(),
                                   hb_parnidef( 1, HB_CTW_UNDEF ) /* nLevel */ ) );
}
