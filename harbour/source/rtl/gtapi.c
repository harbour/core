/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Terminal API
 *
 * Copyright 1999 Bil Simser <bsimser@home.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Paul Tucker <ptucker@sympatico.ca>
 *    hb_gtInit()
 *    hb_gtExit()
 *    hb_gtDispBegin()
 *    hb_gtDispEnd()
 *    hb_gtPreExt()
 *    hb_gtPostExt()
 *    hb_gtGetColorStr()
 *    hb_gtSetColorStr()
 *    hb_gtSetMode()
 *
 * Copyright 1999-2001 Viktor Szakats (harbour.01 syenar.hu)
 *    hb_gtDrawShadow()
 *
 * Copyright 2006 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
 *    The body of these functions which were usable in new GT API
 *    have been moved to hbgtcore.c to hb_gt_def_*() functions
 *    some of my modificaations.
 *
 * See COPYING for licensing terms.
 *
 */

#include "hbgtcore.h"
#include "hbset.h"

/* gt API functions */

HB_ERRCODE hb_gtInit( HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtInit()"));

   hb_gtStartupInit();

   pGT = hb_gt_Base();
   if( !pGT )
      return HB_FAILURE;

   HB_GTSELF_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );
   HB_GTSELF_SETCOLORSTR( pGT, hb_setGetColor() );
   HB_GTSELF_SETCURSORSTYLE( pGT, SC_NORMAL );
   HB_GTSELF_FLUSH( pGT );
   hb_gt_BaseFree( pGT );

   return HB_SUCCESS;
}

HB_ERRCODE hb_gtExit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtExit()"));

   hb_gtRelease( NULL );

   /* clear internal clipboard data */
   hb_gt_setClipboard( NULL, 0 );

   return HB_SUCCESS;
}

HB_ERRCODE hb_gtLock( void )
{
   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtLock()"));

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_LOCK( pGT ) )
         errCode = HB_SUCCESS;
      hb_gt_BaseFree( pGT );
   }
   return errCode;
}

HB_ERRCODE hb_gtUnlock( void )
{
   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtUnlock()"));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_UNLOCK( pGT );
      errCode = HB_SUCCESS;
      hb_gt_BaseFree( pGT );
   }
   return errCode;
}

int hb_gtReadKey( int iEventMask )
{
   int iKey = 0;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtReadKey(%d)", iEventMask));

   pGT = hb_gt_Base();
   if( pGT )
   {
      iKey = HB_GTSELF_READKEY( pGT, iEventMask );
      hb_gt_BaseFree( pGT );
   }
   return iKey;
}

HB_ERRCODE hb_gtBox( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, const BYTE * pbyFrame )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtBox(%hd, %hd, %hd, %hd, %p)", Top, Left, Bottom, Right, pbyFrame));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_BOX( pGT, Top, Left, Bottom, Right, pbyFrame, HB_GTSELF_GETCOLOR( pGT ) );
      HB_GTSELF_SETPOS( pGT, Top + 1, Left + 1 );
      HB_GTSELF_FLUSH( pGT );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtBoxD( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtBoxD(%hd, %hd, %hd, %hd)", Top, Left, Bottom, Right));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_BOXD( pGT, Top, Left, Bottom, Right, ( const BYTE * ) _B_DOUBLE, HB_GTSELF_GETCOLOR( pGT ) );
      HB_GTSELF_SETPOS( pGT, Top + 1, Left + 1 );
      HB_GTSELF_FLUSH( pGT );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtBoxS( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtBoxS(%hd, %hd, %hd, %hd)", Top, Left, Bottom, Right));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_BOXD( pGT, Top, Left, Bottom, Right, ( const BYTE * ) _B_SINGLE, HB_GTSELF_GETCOLOR( pGT ) );
      HB_GTSELF_SETPOS( pGT, Top + 1, Left + 1 );
      HB_GTSELF_FLUSH( pGT );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtDrawBox( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, const BYTE * pbyFrame, int iColor )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtDrawBox(%hd, %hd, %hd, %hd, %p, %d)", Top, Left, Bottom, Right, pbyFrame, iColor));

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( iColor == -1 )
         iColor = HB_GTSELF_GETCOLOR( pGT );

      HB_GTSELF_BOX( pGT, Top, Left, Bottom, Right, pbyFrame, iColor );
      HB_GTSELF_FLUSH( pGT );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtColorSelect( USHORT uiColorIndex )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtColorSelect(%hu)", uiColorIndex));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_COLORSELECT( pGT, uiColorIndex );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtDispBegin( void )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtDispBegin()"));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_DISPBEGIN( pGT );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

USHORT hb_gtDispCount( void )
{
   USHORT uiCount = 0;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtDispCount()"));

   pGT = hb_gt_Base();
   if( pGT )
   {
      uiCount = HB_GTSELF_DISPCOUNT( pGT );
      hb_gt_BaseFree( pGT );
   }
   return uiCount;
}

HB_ERRCODE hb_gtDispEnd( void )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtDispEnd()"));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_DISPEND( pGT );
      HB_GTSELF_FLUSH( pGT );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtPreExt( void )
{
   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtPreExt()"));

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_PREEXT( pGT ) )
         errCode = HB_SUCCESS;
      hb_gt_BaseFree( pGT );
   }
   return errCode;
}

HB_ERRCODE hb_gtPostExt( void )
{
   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtPostExt()"));

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_POSTEXT( pGT ) )
         errCode = HB_SUCCESS;
      hb_gt_BaseFree( pGT );
   }
   return errCode;
}

/* NOTE: szColorString must be at least HB_CLRSTR_LEN wide by the NG. It seems
         that CA-Cl*pper SETCOLOR() will return string lengths up to 131+EOF.
         That seems like a 127+1 buffer size, plus lazy overflow checking.
         [vszakats] */

HB_ERRCODE hb_gtGetColorStr( char * pszColorString )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetColorStr(%s)", pszColorString));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_GETCOLORSTR( pGT, pszColorString );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   pszColorString[ 0 ] = '\0';
   return HB_FAILURE;
}

int hb_gtColorToN( const char * szColorString )
{
   int iColor = 0;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtColorToN(%s)", szColorString));

   pGT = hb_gt_Base();
   if( pGT )
   {
      iColor = HB_GTSELF_COLORNUM( pGT, szColorString );
      hb_gt_BaseFree( pGT );
   }
   return iColor;
}

HB_ERRCODE hb_gtColorsToString( int * pColors, int iColorCount, char * pszColorString, int iBufSize )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtColorsToString(%p, %d, %p, %d)", pColors, iColorCount, pszColorString, iBufSize));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_COLORSTOSTRING( pGT, pColors, iColorCount, pszColorString, iBufSize );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   pszColorString[ 0 ] = '\0';
   return HB_FAILURE;
}

HB_ERRCODE hb_gtSetColorStr( const char * szColorString )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetColorStr(%s)", szColorString));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_SETCOLORSTR( pGT, szColorString );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtGetCursor( USHORT * uipCursorStyle )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetCursor(%p)", uipCursorStyle));

   pGT = hb_gt_Base();
   if( pGT )
   {
      *uipCursorStyle = HB_GTSELF_GETCURSORSTYLE( pGT );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   *uipCursorStyle = SC_NONE;
   return HB_FAILURE;
}

HB_ERRCODE hb_gtSetCursor( USHORT uiCursorStyle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetCursor(%hu)", uiCursorStyle));

   if( uiCursorStyle <= SC_SPECIAL2 )
   {
      PHB_GT pGT = hb_gt_Base();
      if( pGT )
      {
         HB_GTSELF_SETCURSORSTYLE( pGT, uiCursorStyle );
         HB_GTSELF_FLUSH( pGT );
         hb_gt_BaseFree( pGT );
         return HB_SUCCESS;
      }
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtGetPos( SHORT * piRow, SHORT * piCol )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetPos(%p, %p)", piRow, piCol));

   pGT = hb_gt_Base();
   if( pGT )
   {
      int iRow, iCol;

      HB_GTSELF_GETPOS( pGT, &iRow, &iCol );
      *piRow = ( SHORT ) iRow;
      *piCol = ( SHORT ) iCol;
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   *piRow = *piCol = 0;
   return HB_FAILURE;
}

/* NOTE: Should be exactly the same as hb_gtSetPosContext(), but without the
         additional third parameter. */

HB_ERRCODE hb_gtSetPos( SHORT iRow, SHORT iCol )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetPos(%hd, %hd)", iRow, iCol));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_SETPOS( pGT, iRow, iCol );
      HB_GTSELF_FLUSH( pGT );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

USHORT hb_gtMaxCol( void )
{
   USHORT uiMaxCol = 79;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtMaxCol()"));

   pGT = hb_gt_Base();
   if( pGT )
   {
      uiMaxCol = HB_GTSELF_MAXCOL( pGT );
      hb_gt_BaseFree( pGT );
   }
   return uiMaxCol;
}

USHORT hb_gtMaxRow( void )
{
   USHORT uiMaxRow = 24;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtMaxRow()"));

   pGT = hb_gt_Base();
   if( pGT )
   {
      uiMaxRow = HB_GTSELF_MAXROW( pGT );
      hb_gt_BaseFree( pGT );
   }
   return uiMaxRow;
}

HB_ERRCODE hb_gtScrDim( USHORT * uipHeight, USHORT * uipWidth )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtScrDim(%p, %p)", uipHeight, uipWidth));

   pGT = hb_gt_Base();
   if( pGT )
   {
      int iHeight, iWidth;

      HB_GTSELF_GETSIZE( pGT, &iHeight, &iWidth );
      *uipHeight = ( USHORT ) iHeight;
      *uipWidth  = ( USHORT ) iWidth;
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   *uipHeight = *uipWidth = 0;
   return HB_FAILURE;
}

HB_ERRCODE hb_gtSetSnowFlag( BOOL fNoSnow )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetSnowFlag(%d)", (int) fNoSnow));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_SETSNOWFLAG( pGT, fNoSnow );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtRectSize( int iTop, int iLeft, int iBottom, int iRight, ULONG * pulBuffSize )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtRectSize(%d, %d, %d, %d, %p)", iTop, iLeft, iBottom, iRight, pulBuffSize));

   pGT = hb_gt_Base();
   if( pGT )
   {
      *pulBuffSize = HB_GTSELF_RECTSIZE( pGT, iTop, iLeft, iBottom, iRight );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   *pulBuffSize = 0;
   return HB_FAILURE;
}

BOOL hb_gtIsColor( void )
{
   BOOL fColor = TRUE;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtIsColor()"));

   pGT = hb_gt_Base();
   if( pGT )
   {
      fColor = HB_GTSELF_ISCOLOR( pGT );
      hb_gt_BaseFree( pGT );
   }
   return fColor;
}

HB_ERRCODE hb_gtRepChar( USHORT uiRow, USHORT uiCol, BYTE byChar, USHORT uiCount )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtRepChar(%hu, %hu, %d, %hu)", uiRow, uiCol, (int) byChar, uiCount));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_REPLICATE( pGT, uiRow, uiCol, HB_GTSELF_GETCOLOR( pGT ), 0,
                           byChar, uiCount );
      HB_GTSELF_FLUSH( pGT );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtSave( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, void * pScrBuff )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtSave(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pScrBuff));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_SAVE( pGT, uiTop, uiLeft, uiBottom, uiRight, ( BYTE * ) pScrBuff );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtRest( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, const void * pScrBuff )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtRest(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pScrBuff));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_REST( pGT, uiTop, uiLeft, uiBottom, uiRight, ( const BYTE * ) pScrBuff );
      HB_GTSELF_FLUSH( pGT );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtGetChar( USHORT uiRow, USHORT uiCol, BYTE * pbColor, BYTE * pbAttr, USHORT * pusChar )
{
   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetChar(%hu, %hu, %p, %p, %p)", uiRow, uiCol, pbColor, pbAttr, pusChar));

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_GETCHAR( pGT, uiRow, uiCol, pbColor, pbAttr, pusChar ) )
         errCode = HB_SUCCESS;
      hb_gt_BaseFree( pGT );
   }
   return errCode;
}

HB_ERRCODE hb_gtPutChar( USHORT uiRow, USHORT uiCol, BYTE bColor, BYTE bAttr, USHORT usChar )
{
   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtPutChar(%hu, %hu, %hu, %hu, %hu)", uiRow, uiCol, bColor, bAttr, usChar));

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_PUTCHAR( pGT, uiRow, uiCol, bColor, bAttr, usChar ) )
         errCode = HB_SUCCESS;
      hb_gt_BaseFree( pGT );
   }
   return errCode;
}

HB_ERRCODE hb_gtBeginWrite( void )
{
   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtBeginWrite()"));

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_LOCK( pGT ) )
         errCode = HB_SUCCESS;
      hb_gt_BaseFree( pGT );
   }

   return errCode;
}

HB_ERRCODE hb_gtEndWrite( void )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtEndWrite()"));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_FLUSH( pGT );
      HB_GTSELF_UNLOCK( pGT );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtGetBlink( BOOL * bpBlink )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetBlink(%p)", bpBlink));

   pGT = hb_gt_Base();
   if( pGT )
   {
      *bpBlink = HB_GTSELF_GETBLINK( pGT );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   *bpBlink = 0;
   return HB_FAILURE;
}

HB_ERRCODE hb_gtSetBlink( BOOL fBlink )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetBlink(%d)", (int) fBlink));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_SETBLINK( pGT, fBlink );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtSetMode( USHORT uiRows, USHORT uiCols )
{
   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetMode(%hu, %hu)", uiRows, uiCols));

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_SETMODE( pGT, uiRows, uiCols ) )
         errCode = HB_SUCCESS;
      hb_gt_BaseFree( pGT );
   }
   return errCode;
}

HB_ERRCODE hb_gtPutText( USHORT uiRow, USHORT uiCol,
                                const BYTE * pStr, ULONG ulLength,
                                int iColor )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtPutText(%hu, %hu, %p, %lu, %d)", uiRow, uiCol, pStr, ulLength, iColor));

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( iColor == -1 )
         iColor = HB_GTSELF_GETCOLOR( pGT );

      HB_GTSELF_PUTTEXT( pGT, uiRow, uiCol, iColor, pStr, ulLength );
      HB_GTSELF_FLUSH( pGT );

      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtWriteAt( USHORT uiRow, USHORT uiCol, const BYTE * pStr, ULONG ulLength )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtWriteAt(%hu, %hu, %p, %lu)", uiRow, uiCol, pStr, ulLength));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_WRITEAT( pGT, uiRow, uiCol, pStr, ulLength );
      HB_GTSELF_FLUSH( pGT );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtWrite( const BYTE * pStr, ULONG ulLength )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtWrite(%p, %lu)", pStr, ulLength));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_WRITE( pGT, pStr, ulLength );
      HB_GTSELF_FLUSH( pGT );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtWriteCon( const BYTE * pStr, ULONG ulLength )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtWriteCon(%p, %lu)", pStr, ulLength));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_WRITECON( pGT, pStr, ulLength );
      HB_GTSELF_FLUSH( pGT );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtScroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, SHORT iRows, SHORT iCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtScroll(%hu, %hu, %hu, %hu, %hd, %hd)", uiTop, uiLeft, uiBottom, uiRight, iRows, iCols));

   if( uiTop <= uiBottom && uiLeft <= uiRight )
   {
      PHB_GT pGT = hb_gt_Base();
      if( pGT )
      {
         HB_GTSELF_SCROLL( pGT, uiTop, uiLeft, uiBottom, uiRight,
                           HB_GTSELF_GETCOLOR( pGT ), ' ', iRows, iCols );
         HB_GTSELF_FLUSH( pGT );
         hb_gt_BaseFree( pGT );
         return HB_SUCCESS;
      }
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtScrollUp( USHORT uiRows )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtScrollUp(%hd)", uiRows));

   if( uiRows != 0 )
   {
      PHB_GT pGT = hb_gt_Base();
      if( pGT )
      {
         HB_GTSELF_SCROLLUP( pGT, uiRows, HB_GTSELF_GETCOLOR( pGT ), ' ' );
         HB_GTSELF_FLUSH( pGT );
         hb_gt_BaseFree( pGT );
         return HB_SUCCESS;
      }
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtDrawShadow( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtDrawShadow(%hu, %hu, %hu, %hu, %d)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_DRAWSHADOW( pGT, uiTop, uiLeft, uiBottom, uiRight, byAttr );
      HB_GTSELF_FLUSH( pGT );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtTone( double dFrequency, double dDuration )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtTone(%lf, %lf)", dFrequency, dDuration));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_TONE( pGT, dFrequency, dDuration );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

const char * hb_gtVersion( int iType )
{
   const char * szVersion = "";
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtVersion(%d)",iType));

   pGT = hb_gt_Base();
   if( pGT )
   {
      szVersion = HB_GTSELF_VERSION( pGT, iType );
      hb_gt_BaseFree( pGT );
   }
   return szVersion;
}

HB_ERRCODE hb_gtSetAttribute( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetAttribute(%hu, %hu, %hu, %hu, %d)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_SETATTRIBUTE( pGT, uiTop, uiLeft, uiBottom, uiRight, byAttr );
      HB_GTSELF_FLUSH( pGT );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

/* prepare the terminal for system call */
HB_ERRCODE hb_gtSuspend( void )
{
   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtSuspend()"));

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_SUSPEND( pGT ) )
         errCode = HB_SUCCESS;
      hb_gt_BaseFree( pGT );
   }
   return errCode;
}

HB_ERRCODE hb_gtResume( void )
{
   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtResume()"));

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_RESUME( pGT ) )
         errCode = HB_SUCCESS;
      hb_gt_BaseFree( pGT );
   }
   return errCode;
}

HB_ERRCODE hb_gtOutStd( const BYTE * pbyStr, ULONG ulLen )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtOutStd(%p, %lu)", pbyStr, ulLen));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_OUTSTD( pGT, pbyStr, ulLen );
      hb_gt_BaseFree( pGT );
   }
   else
      hb_fsWriteLarge( ( HB_FHANDLE ) HB_STDOUT_HANDLE, pbyStr, ulLen );

   return HB_SUCCESS;
}

HB_ERRCODE hb_gtOutErr( const BYTE * pbyStr, ULONG ulLen )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtOutErr(%p, %lu)", pbyStr, ulLen));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_OUTERR( pGT, pbyStr, ulLen );
      hb_gt_BaseFree( pGT );
   }
   else
      hb_fsWriteLarge( ( HB_FHANDLE ) HB_STDERR_HANDLE, pbyStr, ulLen );

   return HB_SUCCESS;
}

HB_ERRCODE hb_gtSetDispCP( const char * pszTermCDP, const char * pszHostCDP, BOOL fBox )
{
   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetDispCP(%s, %s, %d)", pszTermCDP, pszHostCDP, fBox));

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_SETDISPCP( pGT, pszTermCDP, pszHostCDP, fBox ) )
         errCode = HB_SUCCESS;
      hb_gt_BaseFree( pGT );
   }
   return errCode;
}

HB_ERRCODE hb_gtSetKeyCP( const char * pszTermCDP, const char * pszHostCDP )
{
   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetKeyCP(%s, %s)", pszTermCDP, pszHostCDP));

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_SETKEYCP( pGT, pszTermCDP, pszHostCDP ) )
         errCode = HB_SUCCESS;
      hb_gt_BaseFree( pGT );
   }
   return errCode;
}

HB_ERRCODE hb_gtInfo( int iType, PHB_GT_INFO pInfo )
{
   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtInfo(%d, %p)", iType, pInfo));

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_INFO( pGT, iType, pInfo ) )
         errCode = HB_SUCCESS;
      hb_gt_BaseFree( pGT );
   }
   return errCode;
}

int hb_gtAlert( PHB_ITEM pMessage, PHB_ITEM pOptions,
                          int iClrNorm, int iClrHigh, double dDelay )
{
   int iResult = 0;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtAlert(%p, %p, %d, %d, %f)", pMessage, pOptions, iClrNorm, iClrHigh, dDelay));

   pGT = hb_gt_Base();
   if( pGT )
   {
      iResult = HB_GTSELF_ALERT( pGT, pMessage, pOptions, iClrNorm,
                                 iClrHigh, dDelay );
      hb_gt_BaseFree( pGT );
   }
   return iResult;
}

int hb_gtSetFlag( int iType, int iNewValue )
{
   int iFlag = 0;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetFlag(%d, %d)", iType, iNewValue));

   pGT = hb_gt_Base();
   if( pGT )
   {
      iFlag = HB_GTSELF_SETFLAG( pGT, iType, iNewValue );
      hb_gt_BaseFree( pGT );
   }
   return iFlag;
}

int hb_gtGetCurrColor( void )
{
   int iColor = 0x07;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetCurrColor()"));

   pGT = hb_gt_Base();
   if( pGT )
   {
      iColor = HB_GTSELF_GETCOLOR( pGT );
      hb_gt_BaseFree( pGT );
   }
   return iColor;
}

int hb_gtGetClearColor( void )
{
   int iColor = 0x07;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetClearColor()"));

   pGT = hb_gt_Base();
   if( pGT )
   {
      iColor = HB_GTSELF_GETCLEARCOLOR( pGT );
      hb_gt_BaseFree( pGT );
   }
   return iColor;
}

HB_ERRCODE hb_gtSetClearColor( int iColor )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetClearColor(%d)", iColor));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_SETCLEARCOLOR( pGT, iColor );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

int hb_gtGetClearChar( void )
{
   int iChar = ' ';
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetClearChar()"));

   pGT = hb_gt_Base();
   if( pGT )
   {
      iChar = HB_GTSELF_GETCLEARCHAR( pGT );
      hb_gt_BaseFree( pGT );
   }
   return iChar;
}

HB_ERRCODE hb_gtSetClearChar( int iChar )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetClearChar(%d)", iChar));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_SETCLEARCHAR( pGT, iChar );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtGetScrChar( int iRow, int iCol, BYTE * pbColor, BYTE * pbAttr, USHORT * pusChar )
{
   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtScrGetChar(%d, %d, %p, %p, %p)", iRow, iCol, pbColor, pbAttr, pusChar));

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_GETSCRCHAR( pGT, iRow, iCol, pbColor, pbAttr, pusChar ) )
         errCode = HB_SUCCESS;
      hb_gt_BaseFree( pGT );
   }
   return errCode;
}

HB_ERRCODE hb_gtPutScrChar( int iRow, int iCol, BYTE bColor, BYTE bAttr, USHORT usChar )
{
   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtPutScrChar(%d, %d, %d, %d, %hu)", iRow, iCol, bColor, bAttr, usChar));

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_PUTSCRCHAR( pGT, iRow, iCol, bColor, bAttr, usChar ) )
         errCode = HB_SUCCESS;
      hb_gt_BaseFree( pGT );
   }
   return errCode;
}

HB_ERRCODE hb_gtFlush( void )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtFlush()"));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_FLUSH( pGT );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtGetPosEx( int * piRow, int * piCol )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetPosEx(%p, %p)", piRow, piCol));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_GETPOS( pGT, piRow, piCol );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   *piRow = *piCol = 0;
   return HB_FAILURE;
}

HB_ERRCODE hb_gtScrollEx( int iTop, int iLeft, int iBottom, int iRight, int iColor, int iChar, int iRows, int iCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtScrollEx(%d, %d, %d, %d, %d, %hd, %d, %d)", iTop, iLeft, iBottom, iRight, iColor, iChar, iRows, iCols));

   if( iTop <= iBottom && iLeft <= iRight )
   {
      PHB_GT pGT = hb_gt_Base();
      if( pGT )
      {
         if( iColor == -1 )
            iColor = HB_GTSELF_GETCOLOR( pGT );
         if( iChar < 0 )
            iChar = HB_GTSELF_GETCLEARCHAR( pGT );
         HB_GTSELF_SCROLL( pGT, iTop, iLeft, iBottom, iRight,
                           iColor, iChar, iRows, iCols );
         HB_GTSELF_FLUSH( pGT );
         hb_gt_BaseFree( pGT );
         return HB_SUCCESS;
      }
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtBoxEx( int iTop, int iLeft, int iBottom, int iRight, const BYTE * pbyFrame, int iColor )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtBoxEx(%d, %d, %d, %d, %p, %d)", iTop, iLeft, iBottom, iRight, pbyFrame, iColor));

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( iColor == -1 )
         iColor = HB_GTSELF_GETCOLOR( pGT );
      HB_GTSELF_BOX( pGT, iTop, iLeft, iBottom, iRight, pbyFrame, iColor );
      HB_GTSELF_SETPOS( pGT, iTop + 1, iLeft + 1 );
      HB_GTSELF_FLUSH( pGT );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

int hb_gtGfxPrimitive( int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
   PHB_GT pGT;
   int iResult = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtGfxText(%d, %d, %d, %d, %d, %d)", iType, iTop, iLeft, iBottom, iRight, iColor));

   pGT = hb_gt_Base();
   if( pGT )
   {
      iResult = HB_GTSELF_GFXPRIMITIVE( pGT, iType, iTop, iLeft, iBottom, iRight, iColor );
      HB_GTSELF_FLUSH( pGT );
      hb_gt_BaseFree( pGT );
   }
   return iResult;
}

HB_ERRCODE hb_gtGfxText( int iTop, int iLeft, const char * cBuf, int iColor, int iSize, int iWidth )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtGfxText(%d, %d, %s, %d, %d, %d)", iTop, iLeft, cBuf, iColor, iSize, iWidth));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_GFXTEXT( pGT, iTop, iLeft, cBuf, iColor, iSize, iWidth );
      HB_GTSELF_FLUSH( pGT );
      hb_gt_BaseFree( pGT );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}
