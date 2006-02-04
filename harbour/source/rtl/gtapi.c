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
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_gtDrawShadow()
 *
 * Copyright 2006 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
 *    The body of these functions which were usable in new GT API
 *    have been moved to hbgtcore.c to hb_gt_def_*() functions
 *    some of my modificaations.
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include <ctype.h>
#include "hbgtcore.h"
#include "hbset.h"

static BOOL   s_bInit = FALSE;

/* gt API functions */

HB_EXPORT ERRCODE hb_gtInit( FHANDLE hFilenoStdin, FHANDLE hFilenoStdout, FHANDLE hFilenoStderr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtInit()"));

   hb_gtStartupInit();

   hb_gt_Init( hFilenoStdin, hFilenoStdout, hFilenoStderr );
   hb_gt_SetColorStr( hb_set.HB_SET_COLOR );
   hb_gt_SetCursorStyle( SC_NORMAL );
   s_bInit = TRUE;

   if( hb_cmdargCheck( "INFO" ) )
   {
      hb_conOutErr( hb_gt_Version( 1 ), 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
   }
   hb_gt_Flush();

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtExit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtExit()"));

   if( s_bInit )
   {
      while( hb_gt_DispCount() )
         hb_gt_DispEnd();

      hb_gt_Flush();
      hb_gt_Exit();

      s_bInit = FALSE;

      hb_gtUnLoad();
   }

   return SUCCESS;
}

HB_EXPORT int hb_gtReadKey( int iEventMask )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtReadKey(%d)", iEventMask));

   return hb_gt_ReadKey( iEventMask );
}

HB_EXPORT ERRCODE hb_gtBox( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtBox(%hd, %hd, %hd, %hd, %p)", Top, Left, Bottom, Right, pbyFrame));

   hb_gt_Box( Top, Left, Bottom, Right, pbyFrame, ( BYTE ) hb_gt_GetColor() );
   hb_gt_SetPos( Top + 1, Left + 1 );
   hb_gt_Flush();

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtBoxD( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtBoxD(%hd, %hd, %hd, %hd)", Top, Left, Bottom, Right));

   hb_gt_Box( Top, Left, Bottom, Right, ( BYTE * ) _B_DOUBLE, ( BYTE ) hb_gt_GetColor() );
   hb_gt_SetPos( Top + 1, Left + 1 );
   hb_gt_Flush();

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtBoxS( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtBoxS(%hd, %hd, %hd, %hd)", Top, Left, Bottom, Right));

   hb_gt_Box( Top, Left, Bottom, Right, ( BYTE * ) _B_SINGLE, ( BYTE ) hb_gt_GetColor() );
   hb_gt_SetPos( Top + 1, Left + 1 );
   hb_gt_Flush();

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtColorSelect( USHORT uiColorIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtColorSelect(%hu)", uiColorIndex));

   hb_gt_ColorSelect( uiColorIndex );

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtDispBegin( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtDispBegin()"));

   hb_gt_DispBegin();

   return SUCCESS;
}

HB_EXPORT USHORT hb_gtDispCount( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtDispCount()"));

   return hb_gt_DispCount();
}

HB_EXPORT ERRCODE hb_gtDispEnd( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtDispEnd()"));

   hb_gt_DispEnd();
   hb_gt_Flush();

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtPreExt( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtPreExt()"));

   hb_gt_PreExt();

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtPostExt( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtPostExt()"));

   hb_gt_PostExt();

   return SUCCESS;
}

/* NOTE: szColorString must be at least CLR_STRLEN wide by the NG. It seems
         that CA-Cl*pper SETCOLOR() will return string lengths up to 131+EOF.
         That seems like a 127+1 buffer size, plus lazy overflow checking.
         [vszakats] */

HB_EXPORT ERRCODE hb_gtGetColorStr( char * pszColorString )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetColorStr(%s)", pszColorString));

   hb_gt_GetColorStr( pszColorString );

   return SUCCESS;
}

HB_EXPORT USHORT hb_gtColorToN( char * szColorString )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtColorToN(%s)", szColorString));

   return hb_gt_ColorNum( szColorString );
}

HB_EXPORT ERRCODE hb_gtSetColorStr( const char * szColorString )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetColorStr(%s)", szColorString));

   hb_gt_SetColorStr( szColorString );

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtGetCursor( USHORT * uipCursorStyle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetCursor(%p)", uipCursorStyle));

   *uipCursorStyle = hb_gt_GetCursorStyle();
   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtSetCursor( USHORT uiCursorStyle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetCursor(%hu)", uiCursorStyle));

   if( uiCursorStyle <= SC_SPECIAL2 )
   {
      hb_gt_SetCursorStyle( uiCursorStyle );
      hb_gt_Flush();

      return SUCCESS;
   }

   return FAILURE;
}

HB_EXPORT ERRCODE hb_gtGetPos( SHORT * piRow, SHORT * piCol )
{
   int iRow, iCol;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetPos(%p, %p)", piRow, piCol));

   hb_gt_GetPos( &iRow, &iCol );
   *piRow = ( SHORT ) iRow;
   *piCol = ( SHORT ) iCol;

   return SUCCESS;
}

/* NOTE: Should be exactly the same as hb_gtSetPosContext(), but without the
         additional third parameter. */

HB_EXPORT ERRCODE hb_gtSetPos( SHORT iRow, SHORT iCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetPos(%hd, %hd)", iRow, iCol));

   hb_gt_SetPos( iRow, iCol );
   hb_gt_Flush();

   return SUCCESS;
}

HB_EXPORT USHORT hb_gtMaxCol( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtMaxCol()"));

   return hb_gt_MaxCol();
}

HB_EXPORT USHORT hb_gtMaxRow( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtMaxRow()"));

   return hb_gt_MaxRow();
}

HB_EXPORT ERRCODE hb_gtScrDim( USHORT * uipHeight, USHORT * uipWidth )
{
   int iHeight, iWidth;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtScrDim(%p, %p)", uipHeight, uipWidth));

   hb_gt_GetSize( &iHeight, &iWidth );
   *uipHeight = ( USHORT ) iHeight;
   *uipWidth  = ( USHORT ) iWidth;

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtSetSnowFlag( BOOL fNoSnow )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetSnowFlag(%d)", (int) fNoSnow));

   hb_gt_SetSnowFlag( fNoSnow );

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtSetCompatBuffer( BOOL fCompat )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetCompatBuffer(%d)", (int) fCompat));

   hb_gt_SetCompatBuffer( fCompat );

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtRectSize( int iTop, int iLeft, int iBottom, int iRight, ULONG * pulBuffSize )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtRectSize(%d, %d, %d, %d, %p)", iTop, iLeft, iBottom, iRight, pulBuffSize));

   *pulBuffSize = hb_gt_RectSize( iTop, iLeft, iBottom, iRight );

   return SUCCESS;
}

HB_EXPORT BOOL hb_gtIsColor( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtIsColor()"));

   return hb_gt_IsColor();
}

HB_EXPORT ERRCODE hb_gtRepChar( USHORT uiRow, USHORT uiCol, BYTE byChar, USHORT uiCount )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtRepChar(%hu, %hu, %d, %hu)", uiRow, uiCol, (int) byChar, uiCount));

   hb_gt_Replicate( uiRow, uiCol, ( BYTE ) hb_gt_GetColor(), 0,
                    byChar, uiCount );
   hb_gt_Flush();

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtSave( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, void * pScrBuff )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSave(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pScrBuff));

   hb_gt_Save( uiTop, uiLeft, uiBottom, uiRight, ( BYTE * ) pScrBuff );

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtRest( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, void * pScrBuff )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtRest(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pScrBuff));

   hb_gt_Rest( uiTop, uiLeft, uiBottom, uiRight, ( BYTE * ) pScrBuff );
   hb_gt_Flush();

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtGetChar( USHORT uiRow, USHORT uiCol, BYTE * pbColor, BYTE * pbAttr, USHORT * pusChar )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetChar(%hu, %hu, %p, %p, %p)", uiRow, uiCol, pbColor, pbAttr, pusChar));

   hb_gt_GetChar( uiRow, uiCol, pbColor, pbAttr, pusChar );

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtPutChar( USHORT uiRow, USHORT uiCol, BYTE bColor, BYTE bAttr, USHORT usChar )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtPutChar(%hu, %hu, %hu, %hu, %hu)", uiRow, uiCol, bColor, bAttr, usChar));

   hb_gt_PutChar( uiRow, uiCol, bColor, bAttr, usChar );

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtBeginWrite( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtBeginWrite()"));

   /* Do nothing in Harbour */

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtEndWrite( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtBeginWrite()"));

   hb_gt_Flush();

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtGetBlink( BOOL * bpBlink )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetBlink(%p)", bpBlink));

   *bpBlink = hb_gt_GetBlink();

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtSetBlink( BOOL bBlink )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetBlink(%d)", (int) bBlink));

   hb_gt_SetBlink( bBlink );

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtSetMode( USHORT uiRows, USHORT uiCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetMode(%hu, %hu)", uiRows, uiCols));

   if( !hb_gt_SetMode( uiRows, uiCols ) )
      return FAILURE;

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtWriteAt( USHORT uiRow, USHORT uiCol, BYTE * pStr, ULONG ulLength )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtWriteAt(%hu, %hu, %p, %lu)", uiRow, uiCol, pStr, ulLength));

   hb_gt_WriteAt( uiRow, uiCol, pStr, ulLength );
   hb_gt_Flush();

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtWrite( BYTE * pStr, ULONG ulLength )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtWrite(%p, %lu)", pStr, ulLength));

   hb_gt_Write( pStr, ulLength );
   hb_gt_Flush();

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtWriteCon( BYTE * pStr, ULONG ulLength )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtWriteCon(%p, %lu)", pStr, ulLength));

   hb_gt_WriteCon( pStr, ulLength );
   hb_gt_Flush();

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtScroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, SHORT iRows, SHORT iCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtScroll(%hu, %hu, %hu, %hu, %hd, %hd)", uiTop, uiLeft, uiBottom, uiRight, iRows, iCols));

   if( uiTop > uiBottom || uiLeft > uiRight )
      return FAILURE;

   hb_gt_Scroll( uiTop, uiLeft, uiBottom, uiRight, ( BYTE ) hb_gt_GetColor(), ' ', iRows, iCols );
   hb_gt_Flush();

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtScrollUp( USHORT uiRows )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtScrollUp(%hd)", uiRows));

   if( uiRows == 0 )
      return FAILURE;

   hb_gt_ScrollUp( uiRows, ( BYTE ) hb_gt_GetColor(), ' ' );
   hb_gt_Flush();

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtDrawShadow( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtDrawShadow(%hu, %hu, %hu, %hu, %d)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr));

   hb_gt_DrawShadow( uiTop, uiLeft, uiBottom, uiRight, byAttr );
   hb_gt_Flush();

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtTone( double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtTone(%lf, %lf)", dFrequency, dDuration));

   hb_gt_Tone( dFrequency, dDuration );

   return SUCCESS;
}

HB_EXPORT char * hb_gtVersion( int iType )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtVersion(%d)",iType));

   return hb_gt_Version( iType );
}

HB_EXPORT ERRCODE hb_gtSetAttribute( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetAttribute(%hu, %hu, %hu, %hu, %d)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr));

   hb_gt_SetAttribute( uiTop, uiLeft, uiBottom, uiRight, byAttr );
   hb_gt_Flush();

   return SUCCESS;
}

/* prepare the terminal for system call */
HB_EXPORT ERRCODE hb_gtSuspend( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSuspend()"));

   if( !hb_gt_Suspend() )
      return FAILURE;

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtResume( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtResume()"));

   if( !hb_gt_Resume() )
      return FAILURE;

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtOutStd( BYTE * pbyStr, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtOutStd(%p, %lu)", pbyStr, ulLen));

   hb_gt_OutStd( pbyStr, ulLen );

   return SUCCESS;
}

HB_EXPORT ERRCODE hb_gtOutErr( BYTE * pbyStr, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtOutErr(%p, %lu)", pbyStr, ulLen));

   hb_gt_OutErr( pbyStr, ulLen );

   return SUCCESS;
}

ERRCODE hb_gtSetDispCP( char * pszTermCDP, char * pszHostCDP, BOOL fBox )
{
   if( hb_gt_SetDispCP( pszTermCDP, pszHostCDP, fBox ) )
      return SUCCESS;
   else
      return FAILURE;
}

ERRCODE hb_gtSetKeyCP( char * pszTermCDP, char * pszHostCDP )
{
   if( hb_gt_SetKeyCP( pszTermCDP, pszHostCDP ) )
      return SUCCESS;
   else
      return FAILURE;
}

ERRCODE hb_gtInfo( int iType, PHB_GT_INFO pInfo )
{
   if( hb_gt_Info( iType, pInfo ) )
      return SUCCESS;
   else
      return FAILURE;
}
