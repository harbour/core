/*
 * $Id$
 */

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
      usNew = ( HB_USHORT ) hb_parni( 1 );
   else if( HB_ISCHAR( 1 ) )
      usNew = ( HB_USHORT ) hb_parc( 1 )[0];
   else
      usNew = 255;

   hb_gtSetClearChar( usNew );

   hb_retc_null();
}

HB_FUNC( GETCLEARB )
{
   hb_retni( hb_gtGetClearChar() );
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
   iColor = hb_ctColorParam( 6, -1 );   /* Harbour extension */
   hb_retni( hb_ctwCreateWindow( hb_parni( 1 ), hb_parni( 2 ),
                                 hb_parni( 3 ), hb_parni( 4 ),
                                 hb_parl( 5 ), iColor,
                                 hb_parldef( 7, 1 ) ) );
}

HB_FUNC( WCLOSE )
{
   /* 1-st parameter (window handle) is Harbour extension */
   hb_retni( hb_ctwCloseWindow( HB_ISNUM( 1 ) ? hb_parni( 1 ) :
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
                                                 hb_parldef( 2, 1 ) ) :
                          hb_ctwCurrentWindow() );
}

HB_FUNC( WNUM )
{
   hb_retni( hb_ctwMaxWindow() );
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

   const char * szBox;
   char szBoxBuf[ 10 ];
   int iColor;

   if( HB_ISCHAR( 1 ) )
   {
      szBox = hb_parc( 1 );
   }
   else
   {
      int iFrame = hb_parni( 1 );

      if( iFrame < 0 || iFrame > 15 )
         iFrame = 0;
      memcpy( szBoxBuf, pWBoxFrames[ iFrame ], 9 );
      if( ( iFrame & 4 ) == 0 )
      {
         szBoxBuf[ 8 ] = ( char ) hb_gtGetClearChar();
      }
      szBoxBuf[ 9 ] = '\0';
      szBox = szBoxBuf;
   }

   iColor = hb_ctColorParam( 2, -1 );   /* Harbour extension */
   hb_retni( hb_ctwAddWindowBox( hb_ctwCurrentWindow(), szBox, iColor ) );
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

/* Temporary Harbour extensions to test some extended CTW functionality
 */

/* Harbour extension */
HB_FUNC( WHIDE )
{
   hb_ctwVisible( HB_ISNUM( 1 ) ? hb_parni( 1 ) : hb_ctwCurrentWindow(),
                  HB_CTW_HIDDEN );
}

/* Harbour extension */
HB_FUNC( WSHOW )
{
   hb_ctwVisible( HB_ISNUM( 1 ) ? hb_parni( 1 ) : hb_ctwCurrentWindow(),
                  HB_CTW_VISIBLE );
}

/* Harbour extension */
HB_FUNC( WSHADOW )
{
   hb_retni( hb_ctwSetWindowShadow( hb_ctwCurrentWindow(),
                                    hb_parnidef( 1, HB_CTW_SHADOW_UNDEF ) /* nAttr */ ) );
}

/* Harbour extension */
HB_FUNC( WLEVEL )
{
   hb_retni( hb_ctwSetWindowLevel( hb_ctwCurrentWindow(),
                                   hb_parnidef( 1, HB_CTW_UNDEF ) /* nLevel */ ) );
}
