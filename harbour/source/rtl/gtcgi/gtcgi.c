/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for plain ANSI C stream IO
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
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

/* NOTE: User programs should never call this layer directly! */

#define HB_GT_NAME	CGI

/*
 * HB_GT_CGI_RAWOUTPUT controls the behavior of GTCGI output
 * When not set GT driver output data in Refresh()/Redraw() methods
 * from super GT memory buffer - it causes that columns are wrapped
 * after last col.
 * It changes the previous behavior though it gives some improvements
 * like enabling DispBegin()/DispEnd() for output buffering.
 * If it's wrong and someone will prefer the old GTCGI behavior then
 * please uncomment HB_GT_CGI_RAWOUTPUT macro what will cause that
 * WriteCon(), Write(), WriteAt() method will be overload by GTCGI
 * and output data send directly without any buffering and line wrapping.
 */


#define HB_GT_CGI_RAWOUTPUT

#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapifs.h"
#include "hbapicdp.h"
#include "hbdate.h"

static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER (&SuperTable)

static FHANDLE s_hFilenoStdin;
static FHANDLE s_hFilenoStdout;
static FHANDLE s_hFilenoStderr;
static int     s_iRow;
static int     s_iCol;
static int     s_iLastCol;
static int     s_iLineBufSize;
static BYTE *  s_sLineBuf;
static BYTE *  s_szCrLf;
static ULONG   s_ulCrLf;
static BYTE    s_szBell[] = { HB_CHAR_BEL, 0 };
static BOOL    s_fDispTrans;
static PHB_CODEPAGE  s_cdpTerm;
static PHB_CODEPAGE  s_cdpHost;

static void hb_gt_cgi_termOut( BYTE * pStr, ULONG ulLen )
{
   hb_fsWriteLarge( s_hFilenoStdout, pStr, ulLen );
}

static void hb_gt_cgi_newLine( void )
{
   hb_gt_cgi_termOut( s_szCrLf, s_ulCrLf );
}

static void hb_gt_cgi_Init( FHANDLE hFilenoStdin, FHANDLE hFilenoStdout, FHANDLE hFilenoStderr )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_cgi_Init(%p,%p,%p)", hFilenoStdin, hFilenoStdout, hFilenoStderr ) );

   s_hFilenoStdin  = hFilenoStdin;
   s_hFilenoStdout = hFilenoStdout;
   s_hFilenoStderr = hFilenoStderr;

   s_iRow = s_iCol = s_iLastCol = s_iLineBufSize = 0;
   s_cdpTerm = s_cdpHost = NULL;
   s_fDispTrans = FALSE;

   s_szCrLf = (BYTE *) hb_conNewLine();
   s_ulCrLf = strlen( (char *) s_szCrLf );

   hb_fsSetDevMode( s_hFilenoStdout, FD_BINARY );

   HB_GTSUPER_INIT( hFilenoStdin, hFilenoStdout, hFilenoStderr );
   hb_gt_SetFlag( GTI_STDOUTCON, TRUE );
}

static void hb_gt_cgi_Exit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_cgi_Exit()"));

   hb_gt_Refresh();

   /* update cursor position on exit */
   if( s_iLastCol > 0 )
      hb_gt_cgi_newLine();

   HB_GTSUPER_EXIT();

   if( s_iLineBufSize > 0 )
   {
      hb_xfree( s_sLineBuf );
      s_iLineBufSize = 0;
   }
}

static int hb_gt_cgi_ReadKey( int iEventMask )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_cgi_ReadKey(%d)", iEventMask));

   HB_SYMBOL_UNUSED( iEventMask );

   return 13;
}

static BOOL hb_gt_cgi_IsColor( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_cgi_IsColor()"));

   return FALSE;
}

static void hb_gt_cgi_Bell( void )
{
   HB_TRACE(HB_TR_DEBUG, ( "hb_gt_cgi_Bell()" ) );

   hb_gt_cgi_termOut( s_szBell, 1 );
}

static char * hb_gt_cgi_Version( int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_cgi_Version(%d)", iType ) );

   if( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: Raw stream console";
}

static void hb_gt_cgi_Scroll( int iTop, int iLeft, int iBottom, int iRight,
                              BYTE bColor, BYTE bChar, int iRows, int iCols )
{
   int iHeight, iWidth;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_cgi_Scroll(%d,%d,%d,%d,%d,%d,%d,%d)", iTop, iLeft, iBottom, iRight, bColor, bChar, iRows, iCols ) );

   /* Provide some basic scroll support for full screen */
   hb_gt_GetSize( &iHeight, &iWidth );
   if( iCols == 0 && iRows > 0 &&
       iTop == 0 && iLeft == 0 &&
       iBottom >= iHeight - 1 && iRight >= iWidth - 1 )
   {
      /* scroll up the internal screen buffer */
      HB_GTSUPER_SCROLLUP( iRows, bColor, bChar );
      /* update our internal row position */
      s_iRow -= iRows;
      if( s_iRow < 0 )
         s_iRow = 0;
      s_iLastCol = s_iCol = 0;
   }
   else
      HB_GTSUPER_SCROLL( iTop, iLeft, iBottom, iRight, bColor, bChar, iRows, iCols );
}

static BOOL hb_gt_cgi_SetDispCP( char *pszTermCDP, char *pszHostCDP, BOOL fBox )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_cgi_SetDispCP(%s,%s,%d)", pszTermCDP, pszHostCDP, (int) fBox ) );

   HB_GTSUPER_SETDISPCP( pszTermCDP, pszHostCDP, fBox );

#ifndef HB_CDP_SUPPORT_OFF
   if( !pszHostCDP )
      pszHostCDP = hb_cdp_page->id;
   if( !pszTermCDP )
      pszTermCDP = pszHostCDP;

   if( pszTermCDP && pszHostCDP )
   {
      s_cdpTerm = hb_cdpFind( pszTermCDP );
      s_cdpHost = hb_cdpFind( pszHostCDP );
      s_fDispTrans = s_cdpTerm && s_cdpHost && s_cdpTerm != s_cdpHost;
      return TRUE;
   }
#endif

   return FALSE;
}

#ifdef HB_GT_CGI_RAWOUTPUT

static void hb_gt_cgi_WriteCon( BYTE * pText, ULONG ulLength )
{
   BYTE * buffer = NULL;

#ifndef HB_CDP_SUPPORT_OFF
   if( s_fDispTrans )
   {
      buffer = ( BYTE * ) hb_xgrab( ulLength );
      memcpy( buffer, pText, ulLength );
      hb_cdpnTranslate( ( char * ) buffer, s_cdpHost, s_cdpTerm, ulLength );
      pText = buffer;
   }
#endif

   hb_gt_cgi_termOut( pText, ulLength );
   while( ulLength-- )
   {
      switch( *pText++ )
      {
         case HB_CHAR_BEL:
            break;

         case HB_CHAR_BS:
            if( s_iCol )
               s_iCol--;
            break;

         case HB_CHAR_LF:
            s_iRow++;
            break;

         case HB_CHAR_CR:
            s_iCol = 0;
            break;

         default:
            ++s_iCol;
      }
   }
   HB_GTSUPER_SETPOS( s_iRow, s_iCol );

   if( buffer )
      hb_xfree( buffer );
}

static void hb_gt_cgi_WriteAt( int iRow, int iCol, BYTE * pText, ULONG ulLength )
{
   int iLineFeed = 0, iSpace = 0;

   if( s_iRow != iRow )
   {
      iLineFeed = s_iRow < iRow ? iRow - s_iRow : 1;
   }
   else if( s_iCol > iCol )
   {
      iLineFeed = 1;
      iSpace = iCol;
   }
   else if( s_iCol < iCol )
   {
      iSpace = iCol - s_iCol;
   }

   if( iSpace > 0 )
   {
      BYTE * buffer = ( BYTE * ) hb_xgrab( iSpace );
      memset( buffer, ' ', iSpace );
      hb_gt_cgi_termOut( buffer, iSpace );
      hb_xfree( buffer );
   }
   while( --iLineFeed >= 0 )
      hb_gt_cgi_newLine();
   s_iRow = iRow;
   s_iCol = iCol;

   hb_gt_cgi_WriteCon( pText, ulLength );
}

#else /* HB_GT_CGI_RAWOUTPUT */

static void hb_gt_cgi_Redraw( int iRow, int iCol, int iSize )
{
   BYTE bColor, bAttr;
   USHORT usChar;
   int iLineFeed, iHeight, iWidth, iLen;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_cgi_Redraw(%d, %d, %d)", iRow, iCol, iSize ) );

   hb_gt_GetSize( &iHeight, &iWidth );
   iLineFeed = iLen = 0;

   if( s_iRow != iRow )
   {
      iLineFeed = s_iRow < iRow ? iRow - s_iRow : 1;
      s_iLastCol = s_iCol = iCol = 0;
      iSize = iWidth;
   }
   else if( s_iCol < iCol )
   {
      iSize += iCol - s_iCol;
      iCol = s_iCol;
   }
   else if( s_iCol > iCol )
   {
      iLineFeed = 1;
      iCol = s_iCol = s_iLastCol = 0;
      iSize = iWidth;
   }

   while( iSize > 0 && iCol + iSize > s_iLastCol &&
          hb_gt_GetScrChar( iRow, iCol + iSize - 1, &bColor, &bAttr, &usChar ) )
   {
      if( usChar != ' ' )
         break;
      --iSize;
   }

   if( iSize > 0 )
   {
      while( --iLineFeed >= 0 )
         hb_gt_cgi_newLine();
      s_iRow = iRow;

      while( iLen < iSize )
      {
         if( !hb_gt_GetScrChar( iRow, iCol, &bColor, &bAttr, &usChar ) )
            break;
         s_sLineBuf[ iLen++ ] = ( BYTE ) usChar;
         ++iCol;
      }
      if( iLen )
      {
#ifndef HB_CDP_SUPPORT_OFF
         if( s_fDispTrans )
            hb_cdpnTranslate( ( char * ) s_sLineBuf, s_cdpHost, s_cdpTerm, iLen );
#endif
         hb_gt_cgi_termOut( s_sLineBuf, iLen );
         s_iCol = iCol;
         if( s_iCol > s_iLastCol )
            s_iLastCol = s_iCol;
      }
   }
}

static void hb_gt_cgi_Refresh( void )
{
   int iHeight, iWidth;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_cgi_Refresh()" ) );

   hb_gt_GetSize( &iHeight, &iWidth );
   if( s_iLineBufSize == 0 )
   {
      s_sLineBuf = ( BYTE * ) hb_xgrab( iWidth );
      s_iLineBufSize = iWidth;
   }
   else if( s_iLineBufSize < iWidth )
   {
      s_sLineBuf = ( BYTE * ) hb_xrealloc( s_sLineBuf, iWidth );
      s_iLineBufSize = iWidth;
   }
   HB_GTSUPER_REFRESH();
}

#endif /* HB_GT_CGI_RAWOUTPUT */

/* *********************************************************************** */

static BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", pFuncTable));

   pFuncTable->Init                       = hb_gt_cgi_Init;
   pFuncTable->Exit                       = hb_gt_cgi_Exit;
   pFuncTable->IsColor                    = hb_gt_cgi_IsColor;
#ifdef HB_GT_CGI_RAWOUTPUT
   pFuncTable->WriteCon                   = hb_gt_cgi_WriteCon;
   pFuncTable->Write                      = hb_gt_cgi_WriteCon;
   pFuncTable->WriteAt                    = hb_gt_cgi_WriteAt;
#else
   pFuncTable->Redraw                     = hb_gt_cgi_Redraw;
   pFuncTable->Refresh                    = hb_gt_cgi_Refresh;
#endif
   pFuncTable->Scroll                     = hb_gt_cgi_Scroll;
   pFuncTable->Version                    = hb_gt_cgi_Version;
   pFuncTable->SetDispCP                  = hb_gt_cgi_SetDispCP;
   pFuncTable->Bell                       = hb_gt_cgi_Bell;

   pFuncTable->ReadKey                    = hb_gt_cgi_ReadKey;

   return TRUE;
}

/* ********************************************************************** */

static HB_GT_INIT gtInit = { HB_GT_DRVNAME( HB_GT_NAME ),
                             hb_gt_FuncInit,
                             HB_GTSUPER };

HB_GT_ANNOUNCE( HB_GT_NAME )

HB_CALL_ON_STARTUP_BEGIN( _hb_startup_gt_Init_ )
   hb_gtRegister( &gtInit );
HB_CALL_ON_STARTUP_END( _hb_startup_gt_Init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_startup_gt_Init_
#elif defined(HB_MSC_STARTUP)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto__hb_startup_gt_Init_ = _hb_startup_gt_Init_;
   #pragma data_seg()
#endif

/* *********************************************************************** */
