/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for Win32 compilers ver.2
 * Copyright 2002 Przemys³aw Czerpak <druzus@polbox.com>
 *
 * based on
 *   Bcc ConIO Video subsystem by
 *     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
 *     Copyright 2002 Przemys³aw Czerpak <druzus@polbox.com>
 *   Video subsystem for Win32 compilers
 *     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
 *
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_gt_CtrlHandler()
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_Tone()
 *    hb_gt_ReadKey()
 *
 * See doc/license.txt for licensing terms.
 *
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.   If not, write to
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
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* NOTE: User programs should never call this layer directly! */

/* TODO: include any standard headers here */

/* *********************************************************************** */

#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "hbapigt.h"
#include "hbapifs.h"
#include "hbset.h"
#include "hbvm.h"
#include "hbinkey.ch"
#include "inkey.ch"

#include <string.h>
#include <time.h>
#include <io.h>

#if defined( _MSC_VER )
   #include <conio.h>
#endif

/* *********************************************************************** */

#if defined(__IBMCPP__)
   #undef WORD                            /* 2 bytes unsigned */
   typedef unsigned short int WORD;
#else
   #if ! defined(HB_DONT_DEFINE_BASIC_TYPES)
      #undef WORD                            /* 2 bytes unsigned */
      typedef unsigned short int WORD;

      #undef DWORD                           /* 4 bytes unsigned */
      typedef unsigned long DWORD;
   #endif
#endif

#if ! defined(__GNUC__) && defined(__CYGWIN__)
   typedef WORD far * LPWORD;
#endif

#if defined(__RSXNT__)
   #ifndef FROM_LEFT_1ST_BUTTON_PRESSED
      #define FROM_LEFT_1ST_BUTTON_PRESSED    0x0001
   #endif
   #ifndef RIGHTMOST_BUTTON_PRESSED
      #define RIGHTMOST_BUTTON_PRESSED        0x0002
   #endif
   #ifndef MOUSE_MOVED
      #define MOUSE_MOVED                     0x0001
   #endif
   #ifndef DOUBLE_CLICK
      #define DOUBLE_CLICK                    0x0002
   #endif
#endif

/* *********************************************************************** */

#define MK_SCREEN_UPDATE() hb_gt_ScreenUpdate()

static BOOL s_bBreak;            /* Used to signal Ctrl+Break to hb_inkeyPoll() */
static USHORT s_uiDispCount;
static USHORT s_usCursorStyle;
static USHORT s_usOldCurStyle;
static SHORT s_sCurRow;
static SHORT s_sCurCol;
static USHORT s_usUpdtTop;
static USHORT s_usUpdtBottom;
static USHORT s_usUpdtLeft;
static USHORT s_usUpdtRight;
static CHAR_INFO * s_pCharInfoScreen = NULL;

static int s_iStdIn, s_iStdOut, s_iStdErr;

static HANDLE s_HInput  = INVALID_HANDLE_VALUE;
static HANDLE s_HOutput = INVALID_HANDLE_VALUE;
static CONSOLE_SCREEN_BUFFER_INFO s_csbi,     /* active screen mode */
                                  s_origCsbi; /* to restore screen mode on exit */

#define INPUT_BUFFER_LEN 128

static DWORD        s_cNumRead;   /* Ok to use DWORD here, because this is specific... */
static DWORD        s_cNumIndex;  /* ...to the Windows API, which defines DWORD, etc.  */
static INPUT_RECORD s_irInBuf[ INPUT_BUFFER_LEN ];
static int          s_mouseLast;  /* Last mouse button to be pressed                     */

extern int hb_mouse_iCol;
extern int hb_mouse_iRow;


/* *********************************************************************** */

/* *********************************************************************** */

static void hb_gt_xSetCursorPos( void )
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_xSetCursorPos()"));

    s_csbi.dwCursorPosition.Y = s_sCurRow;
    s_csbi.dwCursorPosition.X = s_sCurCol;
    SetConsoleCursorPosition( s_HOutput, s_csbi.dwCursorPosition );
}

/* *********************************************************************** */

static void hb_gt_xSetCursorStyle( void )
{
    CONSOLE_CURSOR_INFO cci;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_xSetCursorStyle(%hu)", s_usCursorStyle));

    switch( s_usCursorStyle )
    {
    case SC_NONE:
        cci.bVisible = FALSE;
        cci.dwSize = 25;
        break;

    case SC_INSERT:
        cci.bVisible = TRUE;
        cci.dwSize = 50;
        break;

    case SC_SPECIAL1:
        cci.bVisible = TRUE;
        cci.dwSize = 99;
        break;

    case SC_SPECIAL2:
        cci.bVisible = TRUE;
        cci.dwSize = 66;
        /* In their infinite wisdom, MS doesn't support cursors that
           don't start at the bottom of the cell */
        break;

    case SC_NORMAL:
    default:
        cci.bVisible = TRUE;
        cci.dwSize = 12;  /* this was 12, but when used in full screen dos window
                             cursor state is erratic  - doesn't turn off, etc.
			     09-10-2002 druzus: I hope now it's OK. */
        break;
    }
    s_usOldCurStyle = s_usCursorStyle;
    SetConsoleCursorInfo( s_HOutput, &cci );
}

/* *********************************************************************** */

static void hb_gt_xScreenUpdate( void )
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_xScreenUpdate()"));

    if ( s_pCharInfoScreen != NULL )
    {
        if ( s_uiDispCount == 0 && s_usUpdtTop <= s_usUpdtBottom )
        {
            COORD coDest, coSize;
            SMALL_RECT srWin;

            coSize.Y = s_csbi.dwSize.Y;
            coSize.X = s_csbi.dwSize.X;
            coDest.Y = s_usUpdtTop;
            coDest.X = s_usUpdtLeft;
            srWin.Top    = ( SHORT ) s_usUpdtTop;
            srWin.Left   = ( SHORT ) s_usUpdtLeft;
            srWin.Bottom = ( SHORT ) s_usUpdtBottom;
            srWin.Right  = ( SHORT ) s_usUpdtRight;

            WriteConsoleOutput( s_HOutput,         /* output handle */
                                s_pCharInfoScreen, /* data to write */
                                coSize,            /* col/row size of source buffer */
                                coDest,            /* upper-left cell to write data from in src */
                                &srWin );          /* screen buffer rect to write data to */

            s_usUpdtTop = s_csbi.dwSize.Y;
            s_usUpdtLeft = s_csbi.dwSize.X;
            s_usUpdtBottom = s_usUpdtRight = 0;
        }

        if ( s_usOldCurStyle != s_usCursorStyle &&
             ( s_uiDispCount == 0 || s_usCursorStyle == SC_NONE ) )
            hb_gt_xSetCursorStyle();

        if ( s_usCursorStyle != SC_NONE && s_uiDispCount == 0 &&
             ( s_csbi.dwCursorPosition.Y != s_sCurRow ||
               s_csbi.dwCursorPosition.X != s_sCurCol ) )
            hb_gt_xSetCursorPos();

    }
}

/* *********************************************************************** */

static void hb_gt_xUpdtSet( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight )
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_xUpdtSet(%hu, %hu, %hu, %hu)", usTop, usLeft, usBottom, usRight));

    if ( usTop < s_usUpdtTop )
        s_usUpdtTop = usTop;
    if ( usLeft < s_usUpdtLeft )
        s_usUpdtLeft = usLeft;
    if ( usBottom > s_usUpdtBottom )
        s_usUpdtBottom = HB_MIN( usBottom, s_csbi.dwSize.Y - 1);
    if ( usRight > s_usUpdtRight )
        s_usUpdtRight = HB_MIN( usRight, s_csbi.dwSize.X - 1);;
}

/* *********************************************************************** */

void hb_gt_ScreenUpdate( void )
{
    hb_gt_xScreenUpdate();
}

/* *********************************************************************** */

/* *********************************************************************** */

static BOOL WINAPI hb_gt_CtrlHandler( DWORD dwCtrlType )
{
   BOOL bHandled;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_CtrlHandler(%lu)", (unsigned long) dwCtrlType));

   switch( dwCtrlType )
   {
   case CTRL_C_EVENT:
      bHandled = FALSE;
      break;

   case CTRL_BREAK_EVENT:
      s_bBreak = TRUE;
      bHandled = TRUE;
      break;

   case CTRL_CLOSE_EVENT:
   case CTRL_LOGOFF_EVENT:
   case CTRL_SHUTDOWN_EVENT:
   default:
      bHandled = FALSE;
   }

   return bHandled;
}

/* *********************************************************************** */

static void hb_gt_xInitScreenParam( void )
{
    COORD coDest = { 0, 0 };

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_xInitScreenParam()"));

    if ( s_pCharInfoScreen != NULL )
        hb_xfree( s_pCharInfoScreen );

    if (GetConsoleScreenBufferInfo( s_HOutput, &s_csbi ))
        s_pCharInfoScreen = ( CHAR_INFO * ) hb_xgrab( s_csbi.dwSize.X *
                                                      s_csbi.dwSize.Y *
                                                      sizeof( CHAR_INFO ) );
    s_sCurRow = s_csbi.dwCursorPosition.Y;
    s_sCurCol = s_csbi.dwCursorPosition.X;
    s_usUpdtTop = s_csbi.dwSize.Y;
    s_usUpdtLeft = s_csbi.dwSize.X;
    s_usUpdtBottom = s_usUpdtRight = 0;

    /* read the screen rectangle into the buffer */
    if ( s_pCharInfoScreen != NULL )
        ReadConsoleOutput( s_HOutput,          /* screen handle */
                           s_pCharInfoScreen,  /* transfer area */
                           s_csbi.dwSize,      /* size of destination buffer */
                           coDest,             /* upper-left cell to write data to */
                           &s_csbi.srWindow);  /* screen buffer rectangle to read from */
}

/* *********************************************************************** */

void hb_gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr )
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

    /* stdin && stdout && stderr */
    s_iStdIn  = iFilenoStdin;
    s_iStdOut = iFilenoStdout;
    s_iStdErr = iFilenoStderr;

    s_bBreak = FALSE;
    s_cNumRead = 0;
    s_cNumIndex = 0;
    s_uiDispCount = 0;
    s_usOldCurStyle = s_usCursorStyle = SC_NORMAL;

    /* Add Ctrl+Break handler [vszakats] */
    SetConsoleCtrlHandler( hb_gt_CtrlHandler, TRUE );

    if( ( s_HInput = GetStdHandle( STD_INPUT_HANDLE ) ) == INVALID_HANDLE_VALUE )
    {
        if( hb_dynsymFindName( "__DBGENTRY" ) ) /* the debugger is linked */
        {
            AllocConsole(); /* It is a Windows app without a console, so we create one */
            s_HInput = GetStdHandle( STD_INPUT_HANDLE );
        }
    }

    s_HOutput = CreateFile( "CONOUT$",     /* filename    */
                     GENERIC_READ    | GENERIC_WRITE,       /* Access flag */
                     FILE_SHARE_READ | FILE_SHARE_WRITE,    /* share mode  */
                     NULL,                                  /* security attributes */
                     OPEN_EXISTING,                         /* create mode */
                     0, 0 );

    if( s_HOutput != INVALID_HANDLE_VALUE )
    {
        GetConsoleScreenBufferInfo( s_HOutput, &s_csbi );

        /* save screen info to restore on exit */
        memcpy( &s_origCsbi, &s_csbi, sizeof( s_csbi ) );

        s_csbi.srWindow.Top = s_csbi.srWindow.Left = 0;
        s_csbi.srWindow.Right = HB_MIN( s_csbi.srWindow.Right, s_csbi.dwSize.X-1 );
        s_csbi.srWindow.Bottom = HB_MIN( s_csbi.srWindow.Bottom, s_csbi.dwSize.Y-1 );

        SetConsoleWindowInfo( s_HOutput, TRUE,  &s_csbi.srWindow );
        SetConsoleScreenBufferSize( s_HOutput, s_csbi.dwSize );

        hb_gt_xInitScreenParam();
    }

    if( s_HInput != INVALID_HANDLE_VALUE )
    {
        SetConsoleMode( s_HInput, ENABLE_MOUSE_INPUT );
        hb_mouse_Init();
    }
}

/* *********************************************************************** */

void hb_gt_Exit( void )
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));

    if ( s_uiDispCount > 0 )
    {
        s_uiDispCount = 0;
        MK_SCREEN_UPDATE();
    }

    if ( s_pCharInfoScreen != NULL )
    {
        hb_xfree( s_pCharInfoScreen );
        s_pCharInfoScreen = NULL;
    }

    if( s_HOutput != INVALID_HANDLE_VALUE )
    {
        SetConsoleScreenBufferSize( s_HOutput, s_origCsbi.dwSize );
        SetConsoleWindowInfo( s_HOutput, FALSE, &s_origCsbi.srWindow );
        CloseHandle( s_HOutput );
    }
    /* Remove Ctrl+Break handler */
    SetConsoleCtrlHandler( hb_gt_CtrlHandler, FALSE );

    hb_mouse_Exit();
}

/* *********************************************************************** */

USHORT hb_gt_GetScreenWidth( void )
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));

    return s_csbi.dwSize.X;
}

/* *********************************************************************** */

USHORT hb_gt_GetScreenHeight( void )
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));

    return s_csbi.dwSize.Y;
}

/* *********************************************************************** */

SHORT hb_gt_Col( void )
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));

    return s_sCurCol;
}

/* *********************************************************************** */

SHORT hb_gt_Row( void )
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));

    return s_sCurRow;
}

/* *********************************************************************** */

void hb_gt_SetPos( SHORT sRow, SHORT sCol, SHORT sMethod )
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd, %hd)", sRow, sCol, sMethod));

    s_sCurRow = sRow;
    s_sCurCol = sCol;

    HB_SYMBOL_UNUSED( sMethod );

    MK_SCREEN_UPDATE();
}

/* *********************************************************************** */

BOOL hb_gt_AdjustPos( BYTE * pStr, ULONG ulLen )
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_AdjustPos(%s, %lu)", pStr, ulLen ));

    HB_SYMBOL_UNUSED( pStr );
    HB_SYMBOL_UNUSED( ulLen );

    return TRUE;
}

/* *********************************************************************** */

BOOL hb_gt_IsColor( void )
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));

    /* TODO: need to call something to do this instead of returning TRUE */
    return TRUE;
}

/* *********************************************************************** */

USHORT hb_gt_GetCursorStyle( void )
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));

    return s_usCursorStyle;
}

/* *********************************************************************** */

void hb_gt_SetCursorStyle( USHORT usStyle )
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", usStyle));

    switch( usStyle )
    {
    case SC_NONE:
    case SC_NORMAL:
    case SC_INSERT:
    case SC_SPECIAL1:
    case SC_SPECIAL2:
        s_usCursorStyle = usStyle;
        break;

    default:
        break;
    }

    MK_SCREEN_UPDATE();
}

/* *********************************************************************** */

void hb_gt_DispBegin( void )
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispBegin()"));

    ++s_uiDispCount;
}

/* *********************************************************************** */

void hb_gt_DispEnd()
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));

    if ( s_uiDispCount > 0 )
        --s_uiDispCount;

    MK_SCREEN_UPDATE();
}

/* *********************************************************************** */

USHORT hb_gt_DispCount()
{
    return s_uiDispCount;
}

/* *********************************************************************** */

void hb_gt_Puts( USHORT usRow, USHORT usCol, BYTE byAttr, BYTE *pbyStr, ULONG ulLen )
{
    int i, j; USHORT l, r, u;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", usRow, usCol, (int) byAttr, pbyStr, ulLen));

    if( ulLen > 0 && s_pCharInfoScreen != NULL )
    {
        i = ( int ) ulLen;
        j = ( int ) ( usRow * s_csbi.dwSize.X + usCol );

        if ( i > s_csbi.dwSize.Y * s_csbi.dwSize.X - j )
            i = s_csbi.dwSize.Y * s_csbi.dwSize.X - j;

        if ( i > 0 )
        {
            u = usRow + ( i + usCol - 1 ) / s_csbi.dwSize.X;
            if ( u > usRow )
            {
                l = 0;
                r = s_csbi.dwSize.X - 1;
            }
            else
            {
                l = usCol;
                r = i + usCol - 1;
            }
            while( i-- )
            {
                s_pCharInfoScreen[j].Char.AsciiChar = ( CHAR ) *pbyStr++;
                s_pCharInfoScreen[j].Attributes = ( WORD )( byAttr & 0xFF );
                ++j;
            }
            hb_gt_xUpdtSet( usRow, l, u, r );
            MK_SCREEN_UPDATE();
        }
    }
}

/* *********************************************************************** */

void hb_gt_Replicate( USHORT usRow, USHORT usCol, BYTE byAttr, BYTE byChar, ULONG ulLen )
{
    int i, j; USHORT l, r, u;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%hu, %hu, %i, %i, %lu)", usRow, usCol, byAttr, byChar, ulLen));

    if( ulLen > 0 && s_pCharInfoScreen != NULL )
    {
        i = ( int ) ulLen;
        j = ( int ) ( usRow * s_csbi.dwSize.X + usCol );

        if ( i > s_csbi.dwSize.Y * s_csbi.dwSize.X - j )
            i = s_csbi.dwSize.Y * s_csbi.dwSize.X - j;

        if ( i > 0 )
        {
            u = usRow + ( i + usCol - 1 ) / s_csbi.dwSize.X;
            if ( u > usRow )
            {
                l = 0;
                r = s_csbi.dwSize.X - 1;
            }
            else
            {
                l = usCol;
                r = i + usCol - 1;
            }
            while( i-- )
            {
                s_pCharInfoScreen[j].Char.AsciiChar = ( CHAR ) byChar;
                s_pCharInfoScreen[j].Attributes = ( WORD )( byAttr & 0xFF );
                ++j;
            }
            hb_gt_xUpdtSet( usRow, l, u, r );
            MK_SCREEN_UPDATE();
        }
    }
}

/* *********************************************************************** */

int hb_gt_RectSize( USHORT rows, USHORT cols )
{
    return rows * cols * 2;
}

/* *********************************************************************** */

void hb_gt_GetText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE * pbyDst )
{
    USHORT x,y;
    ULONG  i;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p)", usTop, usLeft, usBottom, usRight, pbyDst));

    if ( s_pCharInfoScreen != NULL )
    {
        if ( usBottom >= s_csbi.dwSize.Y )
            usBottom = s_csbi.dwSize.Y - 1;

        if ( usRight >= s_csbi.dwSize.X )
            usRight = s_csbi.dwSize.X - 1;

        for( y = usTop; y <= usBottom; y++ )
        {
            i = y * s_csbi.dwSize.X;
            for( x = usLeft; x <= usRight; x++ )
            {
                *(pbyDst++) = (BYTE) s_pCharInfoScreen[i+x].Char.AsciiChar;
                *(pbyDst++) = (BYTE) s_pCharInfoScreen[i+x].Attributes;
            }
        }
    }
}

/* *********************************************************************** */

void hb_gt_PutText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE * pbySrc )
{
    USHORT x,y;
    ULONG  i;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", usTop, usLeft, usBottom, usRight, pbySrc));

    if ( s_pCharInfoScreen != NULL )
    {
        if ( usBottom >= s_csbi.dwSize.Y )
            usBottom = s_csbi.dwSize.Y - 1;

        if ( usRight >= s_csbi.dwSize.X )
            usRight = s_csbi.dwSize.X - 1;

        if ( usTop <= usBottom && usLeft <= usRight )
        {
            for( y = usTop; y <= usBottom; y++ )
            {
                i = y * s_csbi.dwSize.X;
                for( x = usLeft; x <= usRight; x++ )
                {
                    s_pCharInfoScreen[i+x].Char.AsciiChar = ( CHAR ) *(pbySrc++);
                    s_pCharInfoScreen[i+x].Attributes = ( WORD ) *(pbySrc++);
                }
            }
        }
        hb_gt_xUpdtSet( usTop, usLeft, usBottom, usRight );
        MK_SCREEN_UPDATE();
    }
}

/* *********************************************************************** */

void hb_gt_SetAttribute( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr )
{
    USHORT x, y;
    ULONG i;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d", usTop, usLeft, usBottom, usRight, (int) attr));

    if ( s_pCharInfoScreen != NULL )
    {
        if ( usBottom >= s_csbi.dwSize.Y )
            usBottom = s_csbi.dwSize.Y - 1;

        if ( usRight >= s_csbi.dwSize.X )
            usRight = s_csbi.dwSize.X - 1;

        if ( usTop <= usBottom && usLeft <= usRight )
        {
            for( y = usTop; y <= usBottom; y++ )
            {
                i = y * s_csbi.dwSize.X;
                for( x = usLeft; x <= usRight; x++ )
                    s_pCharInfoScreen[i+x].Attributes = ( WORD )( attr & 0xFF );
            }
            hb_gt_xUpdtSet( usTop, usLeft, usBottom, usRight );
            MK_SCREEN_UPDATE();
        }
    }
}

/* *********************************************************************** */

void hb_gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE byAttr, SHORT iRows, SHORT iCols )
{
    SHORT usSaveRow, usSaveCol;
    UINT uiSize;

    int iLength = ( usRight - usLeft ) + 1;
    int iCount, iColOld, iColNew, iColSize;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", usTop, usLeft, usBottom, usRight, (int) byAttr, iRows, iCols));

    if( hb_gtRectSize( usTop, usLeft, usBottom, usRight, &uiSize ) == 0 )
    {
        unsigned char * fpBlank = ( unsigned char * ) hb_xgrab( iLength );
        unsigned char * fpBuff = ( unsigned char * ) hb_xgrab( iLength * 2 );

        memset( fpBlank, ' ', iLength );

        iColOld = iColNew = usLeft;
        iColSize = iLength -1;
        if( iCols >= 0 )
        {
            iColOld += iCols;
            iColSize -= iCols;
        }
        else
        {
            iColNew -= iCols;
            iColSize += iCols;
        }

        /* this is probably not compatible with Clipper */
        hb_gt_DispBegin();

        hb_gtGetPos( &usSaveRow, &usSaveCol );

        for( iCount = ( iRows >= 0 ? usTop : usBottom );
             ( iRows >= 0 ? iCount <= usBottom : iCount >= usTop );
             ( iRows >= 0 ? iCount++ : iCount-- ) )
        {
            int iRowPos = iCount + iRows;

            /* Read the text to be scrolled into the current row */
            if( ( iRows || iCols ) && iRowPos <= usBottom && iRowPos >= usTop )
                hb_gt_GetText( iRowPos, iColOld, iRowPos, iColOld + iColSize, fpBuff );

            /* Blank the scroll region in the current row */
            hb_gt_Puts( iCount, usLeft, byAttr, fpBlank, iLength );

            /* Write the scrolled text to the current row */
            if( ( iRows || iCols ) && iRowPos <= usBottom && iRowPos >= usTop )
                hb_gt_PutText( iCount, iColNew, iCount, iColNew + iColSize, fpBuff );
        }

        hb_xfree( fpBlank );
        hb_xfree( fpBuff );

        hb_gtSetPos( usSaveRow, usSaveCol );
        /* hb_gt_SetPos( usSaveRow, usSaveCol, HB_GT_SET_POS_AFTER ); */

        /* this is probably not compatible with Clipper */
        hb_gt_DispEnd();
        MK_SCREEN_UPDATE();
    }
}

/* *********************************************************************** */

BOOL hb_gt_SetMode( USHORT usRows, USHORT usCols )
{
    BOOL Ret = FALSE;
    SMALL_RECT srWin;
    COORD coBuf;
    USHORT uiDispCount = s_uiDispCount;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", usRows, usCols));

    if( s_HOutput != INVALID_HANDLE_VALUE )
    {
        while( s_uiDispCount )
            hb_gt_DispEnd();

        coBuf = GetLargestConsoleWindowSize( s_HOutput );
        if ( usRows > coBuf.Y )
            usRows = coBuf.Y;
        else
            coBuf.Y = usRows;  /* Thx to Peter Rees */

        if ( usCols > coBuf.X )
            usCols = coBuf.X;
        else
            coBuf.X = usCols;

        /* new console window size and scroll position */
        srWin.Top    = srWin.Left = 0;
        srWin.Bottom = ( SHORT ) ( usRows - 1 );
        srWin.Right  = ( SHORT ) ( usCols - 1 );

        /* if the current buffer is larger than what we want, resize the */
        /* console window first, then the buffer */
        if( ( DWORD ) s_csbi.dwSize.X * s_csbi.dwSize.Y > ( DWORD ) usCols * usRows )
        {
            if ( SetConsoleWindowInfo( s_HOutput, TRUE, &srWin ) )
            {
                SetConsoleScreenBufferSize( s_HOutput, coBuf );
                Ret = TRUE;
            }
        }
        else
        {
            if ( SetConsoleScreenBufferSize( s_HOutput, coBuf ) )
            {
                SetConsoleWindowInfo( s_HOutput, TRUE, &srWin );
                Ret = TRUE;
            }
        }

        if ( Ret )
            hb_gt_xInitScreenParam();

        while( s_uiDispCount < uiDispCount )
            hb_gt_DispBegin();
    }
    return ( Ret );
}

/* *********************************************************************** */

BOOL hb_gt_GetBlink()
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));

    /* TODO */
    return TRUE;
}

/* *********************************************************************** */

void hb_gt_SetBlink( BOOL bBlink )
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink));

    /* TODO: set the bit if it's supported */
    HB_SYMBOL_UNUSED( bBlink );
}

/* *********************************************************************** */

char * hb_gt_Version( void )
{
    return "Harbour Terminal: Win32 buffered console";
}

/* *********************************************************************** */

static void hb_gt_xPutch( USHORT usRow, USHORT usCol, BYTE byAttr, BYTE byChar )
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_xPutch(%hu, %hu, %d, %i)", usRow, usCol, (int) byAttr, byChar));

    if ( s_pCharInfoScreen != NULL &&
         usRow < s_csbi.dwSize.Y && usCol < s_csbi.dwSize.X )
    {
        int i = ( int ) ( usRow * s_csbi.dwSize.X + usCol );

        s_pCharInfoScreen[i].Char.AsciiChar = ( CHAR ) byChar;
        s_pCharInfoScreen[i].Attributes = ( WORD )( byAttr & 0xFF );

        hb_gt_xUpdtSet( usRow, usCol, usRow, usCol );
    }
}

/* *********************************************************************** */

USHORT hb_gt_Box( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right,
                  BYTE * szBox, BYTE byAttr )
{
    USHORT ret = 1;
    SHORT Row;
    SHORT Col;
    SHORT Height;
    SHORT Width;
    USHORT sWidth = hb_gt_GetScreenWidth(),
          sHeight = hb_gt_GetScreenHeight();

    if( ( Left   >= 0 && Left   < sWidth  ) ||
        ( Right  >= 0 && Right  < sWidth  ) ||
        ( Top    >= 0 && Top    < sHeight ) ||
        ( Bottom >= 0 && Bottom < sHeight ) )
    {
        /* Ensure that box is drawn from top left to bottom right. */
        if( Top > Bottom )
        {
            Row = Top;
            Top = Bottom;
            Bottom = Row;
        }
        if( Left > Right )
        {
            Row = Left;
            Left = Right;
            Right = Row;
        }

        /* Draw the box or line as specified */
        Height = Bottom - Top + 1;
        Width  = Right - Left + 1;

        hb_gt_DispBegin();

        if( Height > 1 && Width > 1 &&
               Top >= 0 && Top < sHeight &&
              Left >= 0 && Left < sWidth )
            hb_gt_xPutch( Top, Left, byAttr, szBox[ 0 ] ); /* Upper left corner */

        Col = ( Height > 1 ? Left + 1 : Left );
        if( Col < 0 )
        {
            Width += Col;
            Col = 0;
        }
        if( Right >= sWidth )
        {
            Width -= Right - sWidth;
        }

        if( Col <= Right && Col < sWidth &&
                Top >= 0 && Top < sHeight )
            hb_gt_Replicate( Top, Col, byAttr, szBox[ 1 ], Width + ( (Right - Left) > 1 ? -2 : 0 ) ); /* Top line */

        if( Height > 1 &&
               (Right - Left) > 1 && Right < sWidth &&
               Top >= 0 && Top < sHeight )
            hb_gt_xPutch( Top, Right, byAttr, szBox[ 2 ] ); /* Upper right corner */

        if( szBox[ 8 ] && Height > 2 && Width > 2 )
        {
            for( Row = Top + 1; Row < Bottom; Row++ )
            {
                if( Row >= 0 && Row < sHeight )
                {
                    Col = Left;
                    if( Col < 0 )
                        Col = 0; /* The width was corrected earlier. */
                    else
                        hb_gt_xPutch( Row, Col++, byAttr, szBox[ 7 ] ); /* Left side */
                    hb_gt_Replicate( Row, Col, byAttr, szBox[ 8 ], Width - 2 ); /* Fill */
                    if( Right < sWidth )
                        hb_gt_xPutch( Row, Right, byAttr, szBox[ 3 ] ); /* Right side */
                }
            }
        }
        else
        {
            for( Row = ( Width > 1 ? Top + 1 : Top ); Row < ( (Right - Left ) > 1 ? Bottom : Bottom + 1 ); Row++ )
            {
                if( Row >= 0 && Row < sHeight )
                {
                    if( Left >= 0 && Left < sWidth )
                        hb_gt_xPutch( Row, Left, byAttr, szBox[ 7 ] ); /* Left side */
                    if( ( Width > 1 || Left < 0 ) && Right < sWidth )
                        hb_gt_xPutch( Row, Right, byAttr, szBox[ 3 ] ); /* Right side */
                }
            }
        }

        if( Height > 1 && Width > 1 )
        {
            if( Left >= 0 && Bottom < sHeight )
                hb_gt_xPutch( Bottom, Left, byAttr, szBox[ 6 ] ); /* Bottom left corner */

            Col = Left + 1;
            if( Col < 0 )
                Col = 0; /* The width was corrected earlier. */

            if( Col <= Right && Bottom < sHeight )
                hb_gt_Replicate( Bottom, Col, byAttr, szBox[ 5 ], Width - 2 ); /* Bottom line */

            if( Right < sWidth && Bottom < sHeight )
                hb_gt_xPutch( Bottom, Right, byAttr, szBox[ 4 ] ); /* Bottom right corner */
        }

        hb_gt_DispEnd();
        MK_SCREEN_UPDATE();
        ret = 0;
    }

    return ret;
}

/* *********************************************************************** */

USHORT hb_gt_BoxD( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr )
{
    return hb_gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr );
}

/* *********************************************************************** */

USHORT hb_gt_BoxS( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr )
{
    return hb_gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr );
}

/* *********************************************************************** */

USHORT hb_gt_HorizLine( SHORT Row, SHORT Left, SHORT Right, BYTE byChar, BYTE byAttr )
{
    USHORT ret = 1;
    if( Row >= 0 && Row < hb_gt_GetScreenHeight() )
    {
        if( Left < 0 )
            Left = 0;
        else if( Left >= hb_gt_GetScreenWidth() )
            Left = hb_gt_GetScreenWidth() - 1;

        if( Right < 0 )
            Right = 0;
        else if( Right >= hb_gt_GetScreenWidth() )
            Right = hb_gt_GetScreenWidth() - 1;

        if( Left < Right )
            hb_gt_Replicate( Row, Left, byAttr, byChar, Right - Left + 1 );
        else
            hb_gt_Replicate( Row, Right, byAttr, byChar, Left - Right + 1 );
        ret = 0;
    }
    return ret;
}

/* *********************************************************************** */

USHORT hb_gt_VertLine( SHORT Col, SHORT Top, SHORT Bottom, BYTE byChar, BYTE byAttr )
{
    USHORT ret = 1;
    SHORT Row;

    if( Col >= 0 && Col < hb_gt_GetScreenWidth() )
    {
        if( Top < 0 )
            Top = 0;
        else if( Top >= hb_gt_GetScreenHeight() )
            Top = hb_gt_GetScreenHeight() - 1;

        if( Bottom < 0 )
            Bottom = 0;
        else if( Bottom >= hb_gt_GetScreenHeight() )
            Bottom = hb_gt_GetScreenHeight() - 1;

        if( Top <= Bottom )
            Row = Top;
        else
        {
            Row = Bottom;
            Bottom = Top;
        }

        hb_gt_DispBegin();

        while( Row <= Bottom )
            hb_gt_xPutch( Row++, Col, byAttr, byChar );

        hb_gt_DispEnd();

        MK_SCREEN_UPDATE();
        ret = 0;
    }
    return ret;
}

/* *********************************************************************** */

BOOL hb_gt_Suspend()
{
    return TRUE;
}

/* *********************************************************************** */

BOOL hb_gt_Resume()
{
    return TRUE;
}

/* *********************************************************************** */

BOOL hb_gt_PreExt()
{
    return TRUE;
}

/* *********************************************************************** */

BOOL hb_gt_PostExt()
{
    return TRUE;
}

/* *********************************************************************** */

void hb_gt_OutStd( BYTE * pbyStr, ULONG ulLen )
{
    hb_fsWriteLarge( s_iStdOut, ( BYTE * ) pbyStr, ulLen );
}

/* *********************************************************************** */

void hb_gt_OutErr( BYTE * pbyStr, ULONG ulLen )
{
    hb_fsWriteLarge( s_iStdErr, ( BYTE * ) pbyStr, ulLen );
}

/* *********************************************************************** */

int hb_gt_ExtendedKeySupport()
{
    return 1;
}

/* *********************************************************************** */

static int StdFnKeys( WORD wKey, BOOL bEnhanced )
{
   int ch;
   /* Normal function key */
   ch = wKey + HB_INKEY_NONE;
   if( bEnhanced ) ch += HB_INKEY_ENHANCED;
   return ch;
}

/* *********************************************************************** */

static int IgnoreKeyCodes( int wKey )
{
   int ignore = 0;
   switch( wKey )
   {
      /* Virtual scan codes to ignore */
      case 29: /* Ctrl */
      case 40: /* Circle Accent */
      case 41: /* Tick Accent */
      case 42: /* Left Shift */
      case 43: /* Reverse Tick Accent */
      case 54: /* Right Shift */
      case 56: /* Alt */
      case 58: /* Caps Lock */
      case 69: /* Num Lock */
      case 70: /* Pause or Scroll Lock */
         ignore = -1;
   }
   return ignore;
}

/* *********************************************************************** */

int hb_gt_ReadKey( HB_inkey_enum eventmask )
{
    int ch = 0, extended = 0;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));

    /* First check for Ctrl+Break, which is handled by gt/gtwin.c */
    if( s_bBreak )
    {
       /* Reset the global Ctrl+Break flag */
       s_bBreak = FALSE;
       ch = HB_BREAK_FLAG; /* Indicate that Ctrl+Break was pressed */
    }
    /* Check for events only when the event buffer is exhausted. */
    else if( s_cNumRead <= s_cNumIndex )
    {
       /* Check for keyboard input */
       s_cNumRead = 0;
       GetNumberOfConsoleInputEvents( s_HInput, &s_cNumRead );
       if( s_cNumRead )
       {
          /* Read keyboard input */
          ReadConsoleInput(
             s_HInput,         /* input buffer handle    */
             s_irInBuf,        /* buffer to read into    */
             INPUT_BUFFER_LEN, /* size of read buffer    */
             &s_cNumRead);     /* number of records read */
          /* Set up to process the first input event */
          s_cNumIndex = 0;
       }
    }
    /* Only process one keyboard event at a time. */
    if( s_cNumRead > s_cNumIndex )
    {
       if( s_irInBuf[ s_cNumIndex ].EventType == KEY_EVENT )
       {
          /* Only process key down events */
          if( s_irInBuf[ s_cNumIndex ].Event.KeyEvent.bKeyDown )
          {
             /* Save the keyboard state and ASCII key code */
             DWORD dwState = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.dwControlKeyState;
             WORD wChar = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualKeyCode;
             WORD wKey = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualScanCode;
             ch = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.uChar.AsciiChar;
             #ifdef HB_DEBUG_KEYBOARD
                /* if( dwState & ENHANCED_KEY ) ch = -32; */
                fprintf( stdout, "\n\nhb_gt_ReadKey(): dwState is %ld, wChar is %d, wKey is %d, ch is %d", dwState, wChar, wKey, ch );
                if( dwState & CAPSLOCK_ON ) fprintf( stdout, " CL" );
                if( dwState & ENHANCED_KEY ) fprintf( stdout, " EK" );
                if( dwState & LEFT_ALT_PRESSED ) fprintf( stdout, " LA" );
                if( dwState & RIGHT_ALT_PRESSED ) fprintf( stdout, " RA" );
                if( dwState & LEFT_CTRL_PRESSED ) fprintf( stdout, " LC" );
                if( dwState & RIGHT_CTRL_PRESSED ) fprintf( stdout, " RC" );
                if( dwState & NUMLOCK_ON ) fprintf( stdout, " NL" );
                if( dwState & SCROLLLOCK_ON ) fprintf( stdout, " SL" );
                if( dwState & SHIFT_PRESSED ) fprintf( stdout, " SH" );
                fprintf( stdout, " " );
             #endif
             if( ch == 224 )
             {
                /* Strip extended key lead-in codes */
                ch = 0;
                #ifdef HB_DEBUG_KEYBOARD
                   fprintf( stdout, "-" );
                #endif
             }
             else if( ch < 0  && ch != -32 && ch != -16 /* Hopefully all "dead" keys generate ch = 0 when used alone... && !IgnoreKeyCodes( wKey ) */ )
             {
                /* Process international key codes */
                ch += 256;
                #ifdef HB_DEBUG_KEYBOARD
                   fprintf( stdout, "+" );
                #endif
             }
             else if( ch < 0 && ch != -32 && ch != -16 )
             {
                /* Ignore any negative character codes that didn't get handled
                   by the international keyboard processing and don't signify
                   extended key codes */
                ch = 0;
             }
             else
             {
                #ifdef HB_DEBUG_KEYBOARD
                   fprintf( stdout, "0" );
                #endif
                if( wChar == 27 )
                {
                   /* Fix for escape key problem with some international
                      keyboards and/or international versions of Windows */
                      ch = 27;
                }
                if( ( ( ch == 0 || ch == -32 || ch == -16 ) && ( dwState & ( SHIFT_PRESSED | LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED ) ) )
                || ( ( dwState & ( ENHANCED_KEY | LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED ) ) ) )
                {
                   extended = 1;
                   #ifdef HB_DEBUG_KEYBOARD
                      fprintf( stdout, "1" );
                   #endif
                }
                else if( ch == 0 )
                {
                   if( eventmask & INKEY_RAW )
                   {
                      extended = 1;
                      #ifdef HB_DEBUG_KEYBOARD
                         fprintf( stdout, "2" );
                      #endif
                   }
                   else if( IgnoreKeyCodes( wKey ) )
                   {
                      #ifdef HB_DEBUG_KEYBOARD
                         fprintf( stdout, "!" );
                      #endif
                   }
                   else
                   {
                      ch = StdFnKeys( wKey, 0 );
                      #ifdef HB_DEBUG_KEYBOARD
                         fprintf( stdout, "3" );
                      #endif
                   }
                }
                else if( ch == 9 && wKey == 15 && ( dwState & SHIFT_PRESSED ) )
                {
                   #ifdef HB_DEBUG_KEYBOARD
                      fprintf( stdout, "@" );
                   #endif
                   ch = wKey + 256;   /* Shift+TAB */
                }
             }
             if( extended )
             {
                #ifdef HB_DEBUG_KEYBOARD
                   fprintf( stdout, "4" );
                #endif
                /* Process non-ASCII key codes */
                if( eventmask & INKEY_RAW ) wKey = wChar;
                /* Discard standalone state key presses for normal mode only */
                if( ( eventmask & INKEY_RAW ) == 0 && IgnoreKeyCodes( wKey ) )
                {
                      wKey = 0;
                      #ifdef HB_DEBUG_KEYBOARD
                         fprintf( stdout, "5" );
                      #endif
                }
                if( wKey == 0 ) ch = 0;
                else
                {
                   #ifdef HB_DEBUG_KEYBOARD
                      fprintf( stdout, "6" );
                   #endif
                   if( eventmask & INKEY_RAW )
                   {
                      /* Pass along all virtual key codes with all
                         enhanced and state indicators accounted for */
                      wKey += 256;
                      if( dwState & ENHANCED_KEY ) wKey += 512;
                      if( dwState & SHIFT_PRESSED ) wKey += 1024;
                      if( dwState & LEFT_CTRL_PRESSED ) wKey += 2048;
                      if( dwState & RIGHT_CTRL_PRESSED ) wKey += 4096;
                      if( dwState & LEFT_ALT_PRESSED ) wKey += 8192;
                      if( dwState & RIGHT_ALT_PRESSED ) wKey += 16384;
                      ch = wKey;
                      #ifdef HB_DEBUG_KEYBOARD
                         fprintf( stdout, "7" );
                      #endif
                   }
                   else
                   {
                      /* Translate virtual scan codes to Clipper codes */
                      BOOL bAlt = dwState & ( LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED );
                      BOOL bCtrl = dwState & ( LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED );
                      BOOL bShift = dwState & SHIFT_PRESSED;
                      BOOL bAltGr = ( dwState & ( LEFT_CTRL_PRESSED | RIGHT_ALT_PRESSED ) ) == ( LEFT_CTRL_PRESSED | RIGHT_ALT_PRESSED );
                      BOOL bEnhanced = dwState & ENHANCED_KEY;
                      #ifdef HB_DEBUG_KEYBOARD
                         fprintf( stdout, "8" );
                      #endif

                      HB_TRACE(HB_TR_INFO, ("hb_gt_ReadKey(): wKey is %d, dwState is %d, ch is %d", wKey, dwState, ch));

                      if( bAlt )
                      {
                         #ifdef HB_DEBUG_KEYBOARD
                            fprintf( stdout, "9" );
                         #endif
                         /* Alt key held */
                         if( bAltGr && ch ) { /* It's actually Alt+Gr */ }
                         else
                         {
                            #ifdef HB_DEBUG_KEYBOARD
                               fprintf( stdout, "a" );
                            #endif
                            ch = wKey + HB_INKEY_ALT;
                            if( bEnhanced ) ch += HB_INKEY_ENHANCED;
                         }
                      }
                      else if( bCtrl )
                      {
                         #ifdef HB_DEBUG_KEYBOARD
                            fprintf( stdout, "b" );
                         #endif
                         /* Ctrl key held */
                         if( ch == 0 || bEnhanced ) ch = wKey + HB_INKEY_CTRL;
                         if( bEnhanced ) ch += HB_INKEY_ENHANCED;
                      }
                      else if( bShift )
                      {
                         #ifdef HB_DEBUG_KEYBOARD
                            fprintf( stdout, "c" );
                         #endif
                         /* Shift key held */
                         if( ch == 0 || bEnhanced ) ch = wKey + HB_INKEY_SHIFT;
                         if( bEnhanced ) ch += HB_INKEY_ENHANCED;
                      }
                      else
                      {
                         #ifdef HB_DEBUG_KEYBOARD
                            fprintf( stdout, "d" );
                         #endif
                         ch = StdFnKeys( wKey, bEnhanced );
                      }
                   }
                }
             }

#if 0
             /* Debug code: */
             else
             {
                WORD wKey;
                if( eventmask & INKEY_RAW )
                   wKey = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualKeyCode;
                else
                   wKey = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualScanCode;
                HB_TRACE(HB_TR_INFO, ("hb_gt_ReadKey(): wKey is %d", wKey));
             }
#endif
          }
       }
       else if( eventmask & ~( INKEY_KEYBOARD | INKEY_RAW )
                            && s_irInBuf[ s_cNumIndex ].EventType == MOUSE_EVENT )
       {

          hb_mouse_iCol = s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwMousePosition.X;
          hb_mouse_iRow = s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwMousePosition.Y;

          if( eventmask & INKEY_MOVE && s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwEventFlags == MOUSE_MOVED )
             ch = K_MOUSEMOVE;

          else if( eventmask & INKEY_LDOWN && s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwButtonState &
                   FROM_LEFT_1ST_BUTTON_PRESSED )
          {
             if( s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwEventFlags == DOUBLE_CLICK )
                ch = K_LDBLCLK;
             else
                ch = K_LBUTTONDOWN;

             s_mouseLast = K_LBUTTONDOWN;
          }
          else if( eventmask & INKEY_RDOWN && s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwButtonState &
                   RIGHTMOST_BUTTON_PRESSED )
          {
             if( s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwEventFlags == DOUBLE_CLICK )
                ch = K_RDBLCLK;
             else
                ch = K_RBUTTONDOWN;

             s_mouseLast = K_RBUTTONDOWN;
          }
          else if( s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwEventFlags == 0 &&
                   s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwButtonState == 0 )
          {
             if( eventmask & INKEY_LUP && s_mouseLast == K_LBUTTONDOWN )
                ch = K_LBUTTONUP;
             else if( eventmask & INKEY_RUP && s_mouseLast == K_RBUTTONDOWN )
                ch = K_RBUTTONUP;
          }
       }
       /* Set up to process the next input event (if any) */
       s_cNumIndex++;
    }

    return ch;
}

/* *********************************************************************** */

#if defined(__BORLANDC__) || defined(_MSC_VER)
static int hb_Inp9x( USHORT usPort )
{
  USHORT usVal;

    HB_TRACE(HB_TR_DEBUG, ("hb_Inp9x(%hu)", usPort));

    #if defined(__BORLANDC__)
       _DX = usPort;
       __emit__(0xEC);        /* ASM  IN AL, DX */
       __emit__(0x32,0xE4);   /* ASM XOR AH, AH */
       usVal = _AX;
    #else
       usVal = _inp( usPort );
    #endif

    return usVal;
}

/* *********************************************************************** */

static int hb_Outp9x( USHORT usPort, USHORT usVal )
{
    HB_TRACE(HB_TR_DEBUG, ("hb_Outp9x(%hu, %hu)", usPort, usVal));

    #if defined(__BORLANDC__)
      _DX = usPort;
      _AL = usVal;
      __emit__(0xEE);        /* ASM OUT DX, AL */
      __emit__(0x32,0xE4);   /* ASM XOR AH, AH */
      usVal = _AX;
    #else
       usVal = _outp( usPort, usVal );
    #endif

    return usVal;
}

/* *********************************************************************** */

static void hb_gt_w9xTone( double dFreq, double dDurat, double dTick )
{
  INT uLSB,uMSB;
  UINT uiValue;
  ULONG lAdjFreq;
  clock_t end_clock;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_w9xtone(%lf, %lf, %lf)", dFreq, dDurat, dTick));

    /* Clipper ignores Tone() requests if Frequency is less than  
       < 20 hz (and so should we) to maintain compatibility .. */

    if ( dFreq > 20.0 )
    {

      /* Setup Sound Control Port Registers.. */

      /* select timer channel 2 */

      hb_Outp9x(67, 182) ;

      lAdjFreq = (ULONG)( 1193180 / dFreq ) ;

      if( lAdjFreq < 0 ) 
         uLSB = lAdjFreq + 65536;
      else
         uLSB = lAdjFreq % 256;

      if( lAdjFreq < 0 ) 
         uMSB = lAdjFreq + 65536;
      else
         uMSB = lAdjFreq / 256;


      /* set the frequency (LSB,MSB) */

      hb_Outp9x(66, uLSB);
      hb_Outp9x(66, uMSB);

      /* Get current Port setting */

      uiValue = hb_Inp9x( 97 );

      /* enable Speaker Data & Timer gate bits */

      uiValue = uiValue | 3;  /* 00000011B is bitmask to enable sound */

      /* Turn on Speaker - sound Tone for duration.. */

      hb_Outp9x(97, uiValue);

      end_clock = clock() + ( clock_t ) ( dDurat );
      while( clock() < end_clock )
      {
        hb_idleState();
      }
      hb_idleReset();

      /* Read back current Port value for Reset */

      uiValue = hb_Inp9x( 97 );

      /* disable Speaker Data & Timer gate bits */
      uiValue = uiValue & 0xFC ;

      /* Turn off the Speaker ! */

      hb_Outp9x(97, uiValue);

    }

    /* Delay (1) clock tick, just like Clipper .. */

    end_clock = clock() + ( clock_t ) ( dTick );
    while( clock() < end_clock )
    {
      hb_idleState();
    }
    hb_idleReset();

}
#endif

/* *********************************************************************** */

static void hb_gt_wNtTone( double dFreq, double dDurat, double dTick )
{
  clock_t end_clock;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_wNtTone(%lf, %lf, %lf)", dFreq, dDurat, dTick));

    /* Clipper ignores Tone() requests if Frequency is less than  
       < 20 hz (and so should we) to maintain compatibility .. */

    if ( dFreq > 20.0 )
    {
       Beep( (ULONG) dFreq, (ULONG) dDurat );
    }

    /* Delay (1) clock tick, just like Clipper .. */

    end_clock = clock() + ( clock_t ) ( dTick );
    while( clock() < end_clock )
    {
      hb_idleState();
    }
    hb_idleReset();

}

/* *********************************************************************** */


void hb_gt_Tone( double dFrequency, double dDuration )
{
    double dMillisecs;
    OSVERSIONINFO osv;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));

    /* The conversion from Clipper timer tick units to
       milliseconds is * 1000.0 / 18.2. */

    dDuration = HB_MIN( HB_MAX( 0, dDuration ), ULONG_MAX );

    if( dDuration > 0 ) 
    {
      #if defined( _MSC_VER )
         double dTick = (double) ( 1000.0 / CLOCKS_PER_SEC );
      #else
         double dTick = (double) ( CLOCKS_PER_SEC / 18.2 );
      #endif

      dMillisecs = dDuration * dTick;   /* milliseconds */

      /* What version of Windows are you running? */
      osv.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
      GetVersionEx(&osv);

      /* If Windows 95 or 98, use w9xTone for BCC32, MSVC */
      if (osv.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS)
      {
         #if defined(__BORLANDC__) || defined( _MSC_VER )
            hb_gt_w9xTone( HB_MIN( HB_MAX( 0.0, dFrequency ), 32767.0 ),
                           dMillisecs, dTick );
         #else
            hb_gt_wNtTone( HB_MIN( HB_MAX( 0.0, dFrequency ), 32767.0 ),
                           dMillisecs, dTick );
         #endif
      }

      /* If Windows NT or NT2k, use wNtTone, which provides TONE()
         reset sequence support (new) */
      else if (osv.dwPlatformId == VER_PLATFORM_WIN32_NT)
      {
        /* We pass the Millisecond converted value here .. */
        hb_gt_wNtTone( HB_MIN( HB_MAX( 0.0, dFrequency ), 32767.0 ),
              dMillisecs, dTick );
      }
    }
}


/* *********************************************************************** */
