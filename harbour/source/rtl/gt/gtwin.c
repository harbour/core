/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for Win32 compilers
 *
 * Copyright 1999 Paul Tucker <ptucker@sympatico.ca> (functions marked ptucker)
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 *    hb_gt_CtrlHandler()
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_Tone()
 *
 * See doc/license.txt for licensing terms.
 *
 */

/*
 *  Portions of this module are based (somewhat) on VIDMGR by
 *   Andrew Clarke and modified for the Harbour project
 */

/* NOTE: User programs should never call this layer directly! */

#include <stdlib.h>
#include <string.h>

#define HB_OS_WIN_32_USED

#include "hbapigt.h"
#include "hbset.h" /* For Ctrl+Break handling */
#include "hbvm.h" /* For Ctrl+Break handling */

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

static HANDLE s_HOsave;
/* static HANDLE s_HSsave; */
static HANDLE s_HDOutput   = INVALID_HANDLE_VALUE;
/* static HANDLE s_HDStealth = INVALID_HANDLE_VALUE; */

static HANDLE s_HOriginal  = INVALID_HANDLE_VALUE;
static HANDLE s_HOutput    = INVALID_HANDLE_VALUE;
static HANDLE s_HActive    = INVALID_HANDLE_VALUE;
static HANDLE s_HInactive  = INVALID_HANDLE_VALUE;
static BOOL   s_bOldCursor = TRUE;

HANDLE hb_gtHInput = INVALID_HANDLE_VALUE;
BOOL   hb_gtBreak  = FALSE; /* Used to signal Ctrl+Break to hb_inkeyPoll() */

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
      hb_gtBreak = TRUE;
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

void hb_gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init(): %d, %d, %d", iFilenoStdin, iFilenoStdout, iFilenoStderr));
   HB_SYMBOL_UNUSED( iFilenoStdin );
   HB_SYMBOL_UNUSED( iFilenoStdout );
   HB_SYMBOL_UNUSED( iFilenoStderr );

   /* Add Ctrl+Break handler [vszakats] */
   SetConsoleCtrlHandler( hb_gt_CtrlHandler, TRUE );

   if( ( hb_gtHInput = GetStdHandle( STD_INPUT_HANDLE ) ) == INVALID_HANDLE_VALUE )
   {
      if( hb_dynsymFindName( "__DBGENTRY" ) ) /* the debugger is linked */
      {
         AllocConsole(); /* It is a Windows app without a console, so we create one */
         hb_gtHInput = GetStdHandle( STD_INPUT_HANDLE );
      }
   }

   if( hb_gtHInput != INVALID_HANDLE_VALUE )
      SetConsoleMode( hb_gtHInput, ENABLE_MOUSE_INPUT );

   /* ptucker */
   s_HOriginal = CreateFile( "CONOUT$",     /* filename    */
                       GENERIC_READ    | GENERIC_WRITE,       /* Access flag */
                       FILE_SHARE_READ | FILE_SHARE_WRITE,    /* share mode  */
                       NULL,                                  /* security attributes */
                       OPEN_EXISTING,                         /* create mode */
                       0, 0 );

   s_HOutput = s_HOriginal;
   s_HActive = s_HOutput;

   {
      CONSOLE_SCREEN_BUFFER_INFO csbi;
      SMALL_RECT srWin;

      s_HInactive = CreateConsoleScreenBuffer(
                 GENERIC_READ    | GENERIC_WRITE,    /* Access flag        */
                 FILE_SHARE_READ | FILE_SHARE_WRITE, /* Buffer share mode  */
                 NULL,                               /* Security attribute */
                 CONSOLE_TEXTMODE_BUFFER,            /* Type of buffer     */
                 NULL );                             /* reserved           */

      GetConsoleScreenBufferInfo( s_HOriginal, &csbi );

      /* new console window size and scroll position */
      srWin.Top    = srWin.Left = 0;
      srWin.Bottom = csbi.dwSize.Y - 1;
      srWin.Right  = csbi.dwSize.X - 1;

      SetConsoleScreenBufferSize( s_HInactive, csbi.dwSize );
      SetConsoleWindowInfo( s_HInactive, TRUE,  &csbi.srWindow );
      SetConsoleWindowInfo( s_HInactive, FALSE, &srWin );
   }

/*
   SetConsoleActiveScreenBuffer( s_HActive );
*/
}

void hb_gt_Done( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Done()"));

   if( s_HOutput != s_HOriginal )
   {
      /* ptucker */
      /* because the current screen may not be the one that was active
         when the app started, we need to restore that screen and update
         it with the current image before quitting.
       */
      /* easy fix ;-) */

      /* TOFIX: Violation of API calling rules! */
      hb_gtDispBegin();  /* must use these versions ! */
      hb_gtDispEnd();
   }
/* NOTE: There's no need to close these explicitly, moreover if we close them
         functions using stdout will not show anything.
   CloseHandle( hb_gtHInput );
   hb_gtHInput = INVALID_HANDLE_VALUE;
   CloseHandle( s_HOutput );
   s_HOutput = INVALID_HANDLE_VALUE;
*/
   /* detected using NuMega BoundsChecker */
   CloseHandle( s_HOriginal );
   s_HOriginal = INVALID_HANDLE_VALUE;

   CloseHandle( s_HInactive );
   s_HInactive = INVALID_HANDLE_VALUE;

   /* Remove Ctrl+Break handler [vszakats] */
   SetConsoleCtrlHandler( hb_gt_CtrlHandler, FALSE );
}

int hb_gt_ReadKey( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey()")); 

   /* TODO: */

   return 0;
}

BOOL hb_gt_AdjustPos( BYTE * pStr, ULONG ulLen )
{
   CONSOLE_SCREEN_BUFFER_INFO csbi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_AdjustPos(%s, %lu)", pStr, ulLen ));

   HB_SYMBOL_UNUSED( pStr );
   HB_SYMBOL_UNUSED( ulLen );

   GetConsoleScreenBufferInfo( s_HActive, &csbi );

   hb_gtSetPos( csbi.dwCursorPosition.Y, csbi.dwCursorPosition.X );
   
   return TRUE;
}

BOOL hb_gt_IsColor( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));

   /* TODO: need to call something to do this instead of returning TRUE */
   return TRUE;
}

USHORT hb_gt_GetScreenWidth( void )
{
   CONSOLE_SCREEN_BUFFER_INFO csbi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));

   GetConsoleScreenBufferInfo( s_HOutput, &csbi );
/* return csbi.dwMaximumWindowSize.X; */
/* return max( csbi.srWindow.Right - csbi.srWindow.Left + 1, 40 ); */
   return max( csbi.dwSize.X, 40 );
}

USHORT hb_gt_GetScreenHeight( void )
{
   CONSOLE_SCREEN_BUFFER_INFO csbi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));

   GetConsoleScreenBufferInfo( s_HOutput, &csbi );
/* return csbi.dwMaximumWindowSize.Y; */
/* return max( csbi.srWindow.Bottom - csbi.srWindow.Top + 1, 25 ); */
   return max( csbi.dwSize.Y, 25 );
}

void hb_gt_SetPos( SHORT iRow, SHORT iCol )
{
   COORD dwCursorPosition;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd)", iRow, iCol));

   dwCursorPosition.X = iCol;
   dwCursorPosition.Y = iRow;

   SetConsoleCursorPosition( s_HActive, dwCursorPosition );
}

USHORT hb_gt_GetCursorStyle( void )
{
   CONSOLE_CURSOR_INFO cci;
   USHORT uiCursorShape;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));

   GetConsoleCursorInfo( s_HActive, &cci );

   if( ! cci.bVisible )
   {
      uiCursorShape = SC_NONE;
   }
   else
   {
      switch( cci.dwSize )
      {
         case 50:
            uiCursorShape = SC_INSERT;   /* half block in clipper */
            break;

         case 99:
            uiCursorShape = SC_SPECIAL1; /* full block in clipper */
            break;

         case 66:
            uiCursorShape = SC_SPECIAL2; /* upper half block in clipper */
            break;
            /* TODO: cannot tell if the block is upper or lower for cursor */
            /* Answer: Supposed to be upper third, but ms don't support it. */

         default:
            uiCursorShape = SC_NORMAL;  /* anything else, we'll call it normal */
            break;
      }
   }

   return uiCursorShape;
}

void hb_gt_SetCursorStyle( USHORT style )
{
   CONSOLE_CURSOR_INFO cci;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", style));

   GetConsoleCursorInfo( s_HActive, &cci );

   switch( style )
   {
      case SC_NONE:
         cci.bVisible = FALSE;
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
      default:            /* traps for invalid values */
         cci.bVisible = TRUE;
         cci.dwSize = 25; /* this was 12, but when used in full screen dos window
                             cursor state is erratic  - doesn't turn off, etc. */
         break;
   }

   s_bOldCursor = cci.bVisible;

   SetConsoleCursorInfo( s_HActive, &cci );
}

void hb_gt_Puts( USHORT uiRow, USHORT uiCol, BYTE attr, BYTE * str, ULONG len )
{
   DWORD dwWritten;
   COORD coord;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", uiRow, uiCol, (int) attr, str, len));

   coord.X = ( SHORT ) uiCol;
   coord.Y = ( SHORT ) uiRow;

   FillConsoleOutputAttribute( s_HOutput, ( WORD )( attr & 0xFF ), ( DWORD ) len, coord, &dwWritten );
   WriteConsoleOutputCharacterA( s_HOutput, ( char * ) str, ( DWORD ) len, coord, &dwWritten );
}

void hb_gt_GetText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * dest )
{
   LPWORD pwattr;
   BYTE * pstr;
   USHORT width;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, dest));

   width = ( uiRight - uiLeft + 1 );
   pwattr = ( LPWORD ) hb_xgrab( width * sizeof( *pwattr ) );
   pstr = ( BYTE * ) hb_xgrab( width );

   for( ; uiTop <= uiBottom; uiTop++ )
   {
      COORD coord;
      USHORT i;
      DWORD dwWritten;

      coord.X = ( SHORT ) uiLeft;
      coord.Y = ( SHORT ) uiTop;
      ReadConsoleOutputCharacterA( s_HOutput, ( char * ) pstr, width, coord, &dwWritten );
      ReadConsoleOutputAttribute( s_HOutput, pwattr, width, coord, &dwWritten );
      for( i = 0; i < width; i++ )
      {
         *dest = *( pstr + i );
         dest++;
         *dest = ( BYTE ) *( pwattr + i ) & 0xFF;
         dest++;
      }
   }

   hb_xfree( pstr );
   hb_xfree( pwattr );
}

void hb_gt_PutText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * srce )
{
   LPWORD pwattr;
   BYTE * pstr;
   USHORT width;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, srce));

   width = ( uiRight - uiLeft + 1 );
   pwattr = ( LPWORD ) hb_xgrab( width * sizeof( *pwattr ) );
   pstr = ( BYTE * ) hb_xgrab( width );

   for( ; uiTop <= uiBottom; uiTop++ )
   {
      COORD coord;
      USHORT i;
      DWORD dwWritten;

      for( i = 0; i < width; i++ )
      {
         *( pstr + i ) = *srce;
         srce++;
         *( pwattr + i ) = ( ( WORD )( ( BYTE ) *srce ) & 0xFF );
         srce++;
      }
      coord.X = ( SHORT ) uiLeft;
      coord.Y = ( SHORT ) uiTop;
      WriteConsoleOutputAttribute( s_HOutput, pwattr, width, coord, &dwWritten );
      WriteConsoleOutputCharacterA( s_HOutput, ( char * ) pstr, width, coord, &dwWritten );
   }

   hb_xfree( pstr );
   hb_xfree( pwattr );
}

void hb_gt_SetAttribute( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE attr )
{
/* ptucker */

   COORD coord;
   USHORT width;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d)", uiTop, uiLeft, uiBottom, uiRight, (int) attr));

   width = uiRight - uiLeft + 1;

   coord.X = ( SHORT ) uiLeft;

   for( ; uiTop <= uiBottom; uiTop++ )
   {
      DWORD dwWritten;

      coord.Y = uiTop;
      FillConsoleOutputAttribute( s_HOutput, ( WORD )( attr & 0xFF ), width, coord, &dwWritten );
   }
}

SHORT hb_gt_Col( void )
{
   CONSOLE_SCREEN_BUFFER_INFO csbi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));

   GetConsoleScreenBufferInfo( s_HActive, &csbi );

   return csbi.dwCursorPosition.X;
}

SHORT hb_gt_Row( void )
{
   CONSOLE_SCREEN_BUFFER_INFO csbi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));

   GetConsoleScreenBufferInfo( s_HActive, &csbi );

   return csbi.dwCursorPosition.Y;
}

void hb_gt_Scroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE attr, SHORT iVert, SHORT iHoriz )
{
/* ptucker */

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", uiTop, uiLeft, uiBottom, uiRight, (int) attr, iVert, iHoriz));

   if( ( iHoriz | iVert ) == 0 ) /* both zero? */
   {
      COORD coord;
      USHORT width = uiRight - uiLeft + 1;

      coord.X = ( SHORT ) uiLeft;

      for( ; uiTop <= uiBottom; uiTop++ )
      {
         DWORD dwWritten;

         coord.Y = uiTop;
         FillConsoleOutputAttribute( s_HOutput, ( WORD )( attr & 0xFF ), width, coord, &dwWritten );
         FillConsoleOutputCharacter( s_HOutput, ' ', width, coord, &dwWritten );
      }
   }
   else
   {
      SMALL_RECT Source, Clip;
      COORD      Target;
      CHAR_INFO  FillChar;

      Source.Top    = uiTop;
      Source.Left   = uiLeft;
      Source.Bottom = uiBottom;
      Source.Right  = uiRight;

      memcpy( &Clip, &Source, sizeof( SMALL_RECT ) );

      Target.Y = uiTop - iVert;
      Target.X = uiLeft - iHoriz;

      FillChar.Char.AsciiChar = ' ';
      FillChar.Attributes = ( WORD )( attr & 0xFF );

      ScrollConsoleScreenBuffer( s_HOutput, &Source, &Clip, Target, &FillChar );
   }
}

void hb_gt_DispBegin( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispBegin()"));

/* ptucker */

   /* TOFIX: Violation of API calling rules! */
   if( hb_gtDispCount() == 1 )
   {
      COORD coDest = { 0, 0 };
      COORD coBuf;                    /* the size of the buffer to read into */
      CHAR_INFO * pCharInfo;  /* buffer to store info from ReadConsoleOutput */
      SMALL_RECT srWin;                     /* source rectangle to read from */
      CONSOLE_SCREEN_BUFFER_INFO csbi;

      GetConsoleScreenBufferInfo( s_HOutput, &csbi );
      srWin.Top    = srWin.Left = 0;
      srWin.Bottom = ( coBuf.Y = csbi.dwSize.Y ) - 1;
      srWin.Right  = ( coBuf.X = csbi.dwSize.X ) - 1;

      /* allocate a buffer for the screen rectangle */
      pCharInfo = ( CHAR_INFO * ) hb_xgrab( coBuf.Y * coBuf.X * sizeof( CHAR_INFO ) );

      /* read the screen rectangle into the buffer */
      ReadConsoleOutput( s_HOutput,    /* current screen handle  */
                   pCharInfo,          /* transfer area          */
                   coBuf,              /* size of destination buffer */
                   coDest,             /* upper-left cell to write data to   */
                   &srWin );           /* screen buffer rectangle to read from */

      WriteConsoleOutput( s_HInactive, /* output handle */
                   pCharInfo,          /* data to write */
                   coBuf,              /* col/row size of source buffer */
                   coDest,             /* upper-left cell to write data from in src */
                   &srWin );           /* screen buffer rect to write data to */

      s_HOutput = s_HInactive;

      {
         CONSOLE_CURSOR_INFO cci;

         GetConsoleCursorInfo( s_HActive, &cci );
         s_bOldCursor = cci.bVisible;
         cci.bVisible = FALSE;
         SetConsoleCursorInfo( s_HActive, &cci );
      }

      hb_xfree( pCharInfo );
   }
}

void hb_gt_DispEnd( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));

/* ptucker */

   /* TOFIX: Violation of API calling rules! */
   if( hb_gtDispCount() == 1 )
   {
      s_HOutput = s_HInactive;
      s_HInactive = s_HActive;
      s_HActive = s_HOutput;
      SetConsoleActiveScreenBuffer( s_HActive );

      {
         CONSOLE_CURSOR_INFO cci;

         GetConsoleCursorInfo( s_HActive, &cci );
         cci.bVisible = s_bOldCursor;
         SetConsoleCursorInfo( s_HActive, &cci );
      }
   }
}

BOOL hb_gt_SetMode( USHORT uiRows, USHORT uiCols )
{
/* ptucker */
   BOOL bRetVal = TRUE;
   CONSOLE_SCREEN_BUFFER_INFO csbi;
   SMALL_RECT srWin;
   COORD coBuf;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", uiRows, uiCols));

   GetConsoleScreenBufferInfo( s_HOutput, &csbi );
   coBuf = GetLargestConsoleWindowSize( s_HOutput );

   /* new console window size and scroll position */
   srWin.Top    = srWin.Left = 0;
   srWin.Bottom = ( SHORT ) ( min( uiRows, coBuf.Y ) - 1 );
   srWin.Right  = ( SHORT ) ( min( uiCols, coBuf.X ) - 1 );

   /* new console buffer size */
   coBuf.Y = uiRows;
   coBuf.X = uiCols;

   /* if the current buffer is larger than what we want, resize the */
   /* console window first, then the buffer */
   if( ( DWORD ) csbi.dwSize.X * csbi.dwSize.Y > ( DWORD ) uiCols * uiRows )
   {
      if( !SetConsoleWindowInfo( s_HOutput, TRUE, &srWin ) ||
          !SetConsoleScreenBufferSize( s_HOutput, coBuf ) )
         bRetVal = FALSE;
   }
   else if( ( DWORD ) csbi.dwSize.X * csbi.dwSize.Y < ( DWORD ) uiCols * uiRows )
   {
      if( !SetConsoleScreenBufferSize( s_HOutput, coBuf ) ||
          !SetConsoleWindowInfo( s_HOutput, TRUE, &srWin ) )
         bRetVal = FALSE;
   }

   return bRetVal;
}

void hb_gt_Replicate( BYTE c, ULONG ulLength )
{

/* ptucker */
   COORD coBuf = { 0, 0 };
   DWORD dwWritten;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%d, %lu)", (int) c, ulLength));

/* TODO: This is not used and may be eliminated after further review */
   FillConsoleOutputCharacter(
           s_HOutput,                    /* handle to screen buffer        */
           c,                            /* character to write             */
           ( DWORD ) ulLength,           /* number of cells to write       */
           coBuf,                        /* coordinates of first cell      */
           &dwWritten                    /* receives actual number written */
           );

}

BOOL hb_gt_GetBlink()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));

   /* TODO */
   return FALSE;
}

void hb_gt_SetBlink( BOOL bBlink )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink));

   /* TODO: set the bit if it's supported */
   HB_SYMBOL_UNUSED( bBlink );
}

void hb_gt_DebugScreen( BOOL bActivate )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DebugScreen(%d)", (int) bActivate));

   /* ptucker */
   /* TODO: This is not used and is still a work in progress */
   if( bActivate )
   {
      if( s_HDOutput == INVALID_HANDLE_VALUE )
      {
         CONSOLE_SCREEN_BUFFER_INFO csbi;
         SMALL_RECT srWin;

         s_HDOutput = CreateConsoleScreenBuffer(
                    GENERIC_READ    | GENERIC_WRITE,    /* Access flag        */
                    FILE_SHARE_READ | FILE_SHARE_WRITE, /* Buffer share mode  */
                    NULL,                               /* Security attribute */
                    CONSOLE_TEXTMODE_BUFFER,            /* Type of buffer     */
                    NULL );                             /* reserved           */


         GetConsoleScreenBufferInfo( s_HOutput, &csbi );

         /* new console window size and scroll position */
         srWin.Top    = srWin.Left = 0;
         srWin.Bottom = csbi.dwSize.Y - 1;
         srWin.Right  = csbi.dwSize.X - 1;

         SetConsoleScreenBufferSize( s_HDOutput, csbi.dwSize );
         SetConsoleWindowInfo( s_HDOutput, TRUE,  &csbi.srWindow );
         SetConsoleWindowInfo( s_HDOutput, FALSE, &srWin );
      }
      s_HOsave = s_HOutput;
      s_HOutput = s_HActive = s_HDOutput;

      /* TOFIX: Violation of API calling rules! */
      hb_gtDispBegin();
      hb_gtDispEnd();
   }
   else
   {
      s_HOutput = s_HOsave;
      s_HActive = s_HOriginal;
   }
   SetConsoleActiveScreenBuffer( s_HOutput );
}

void hb_gt_Tone( double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));

   /* The conversion from Clipper timer tick units to
      milliseconds is * 1000.0 / 18.2. */

   dFrequency = HB_MIN_( HB_MAX_( 0.0, dFrequency ), 32767.0 );
   dDuration = dDuration * 1000.0 / 18.2; /* milliseconds */

   while( dDuration > 0.0 )
   {
      ULONG temp = ( ULONG ) HB_MIN_( HB_MAX_( 0, dDuration ), ULONG_MAX );

      dDuration -= temp;
      if( temp <= 0 )
      {
         /* Ensure that the loop gets terminated when
            only a fraction of the delay time remains. */
         dDuration = -1.0;
      }
      else
      {
         /* Bad news for non-NT Windows platforms: Beep() ignores
            both parameters and either generates the default sound
            event or the standard system beep. */
         Beep( ( ULONG ) dFrequency, temp );
      }
   }
}

