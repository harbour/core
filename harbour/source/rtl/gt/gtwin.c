/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for Win32 compilers
 *
 * Copyright 1999 Paul Tucker <ptucker@sympatico.ca> (functions marked ptucker)
 *
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
 *  Portions of this module are based (somewhat) on VIDMGR by
 *   Andrew Clarke and modified for the Harbour project
 */

/* NOTE: User programs should never call this layer directly! */

#include <stdlib.h>
#include <string.h>

#define WIN32_LEAN_AND_MEAN

#if defined(__GNUC__)
#define HB_DONT_DEFINE_BASIC_TYPES
#endif /* __GNUC__ */

#include <windows.h>
#include "gtapi.h"

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

#if ! defined(__GNUC__)
#ifdef __CYGWIN__
typedef WORD far * LPWORD;
#endif
#endif /* __GNUC__ */

static BOOL hb_gt_SetScreenBuffer( HANDLE HNew, HANDLE HOld );

static HANDLE HOsave;
/* static HANDLE HSsave; */
static HANDLE HDOutput  = INVALID_HANDLE_VALUE;
/* static HANDLE HDStealth = INVALID_HANDLE_VALUE; */
       HANDLE hb_gtHInput = INVALID_HANDLE_VALUE;
static HANDLE HOutput   = INVALID_HANDLE_VALUE;
static HANDLE HStealth  = INVALID_HANDLE_VALUE; /* DispBegin buffer */
static HANDLE HOriginal;                      /* used to restore before quit */
static HANDLE HCursor;  /* When DispBegin is in effect, all cursor related
                           functions must refer to the active handle!
                           Otherwise turds are left on the screen when
                           running in a window. This handle will always
                           refer to the currently _active_ buffer which could
                           be different than the one being written to.
                         */

void hb_gt_Init( void )
{
   HB_TRACE(("hb_gt_Init()"));

   if( ( hb_gtHInput = GetStdHandle( STD_INPUT_HANDLE ) ) == INVALID_HANDLE_VALUE )
   {
      if( hb_dynsymFindName( "__DBGENTRY" ) ) /* the debugger is linked */
      {
         AllocConsole(); /* It is a Windows app without a console, so we create one */
         hb_gtHInput = GetStdHandle( STD_INPUT_HANDLE );
      }
   }

   SetConsoleMode( hb_gtHInput, 0 );
   /* ptucker */
   HOriginal = HOutput = HCursor = CreateFile( "CONOUT$",     /* filename    */
                       GENERIC_READ    | GENERIC_WRITE,       /* Access flag */
                       FILE_SHARE_READ | FILE_SHARE_WRITE,    /* share mode  */
                       NULL,                                  /* security attributes */
                       OPEN_EXISTING,                         /* create mode */
                       0, 0 );
}

void hb_gt_Done( void )
{
   HB_TRACE(("hb_gt_Done()"));

   if( HOutput != HOriginal )
   {
      /* ptucker */
      /* because the current screen may not be the one that was active
         when the app started, we need to restore that screen and update
         it with the current image before quitting.
       */
      /* easy fix ;-) */
      hb_gtDispBegin();  /* must use these versions ! */
      hb_gtDispEnd();
   }
/* NOTE: There's no need to close these explicitly, moreover if we close them
         functions using stdout will not show anything.
   CloseHandle( hb_gtHInput );
   hb_gtHInput = INVALID_HANDLE_VALUE;
   CloseHandle( HOutput );
   HOutput = INVALID_HANDLE_VALUE;
*/
   if( HStealth != INVALID_HANDLE_VALUE )
   {
      CloseHandle( HStealth );
      HStealth = INVALID_HANDLE_VALUE;
   }
}

BOOL hb_gt_IsColor( void )
{
   HB_TRACE(("hb_gt_IsColor()"));

   /* TODO: need to call something to do this instead of returning TRUE */
   return TRUE;
}

USHORT hb_gt_GetScreenWidth( void )
{
   CONSOLE_SCREEN_BUFFER_INFO csbi;

   HB_TRACE(("hb_gt_GetScreenWidth()"));

   GetConsoleScreenBufferInfo( HOutput, &csbi );
/* return csbi.dwMaximumWindowSize.X; */
/* return max( csbi.srWindow.Right - csbi.srWindow.Left + 1, 40 ); */
   return max( csbi.dwSize.X, 40 );
}

USHORT hb_gt_GetScreenHeight( void )
{
   CONSOLE_SCREEN_BUFFER_INFO csbi;

   HB_TRACE(("hb_gt_GetScreenHeight()"));

   GetConsoleScreenBufferInfo( HOutput, &csbi );
/* return csbi.dwMaximumWindowSize.Y; */
/* return max( csbi.srWindow.Bottom - csbi.srWindow.Top + 1, 25 ); */
   return max( csbi.dwSize.Y, 25 );
}

void hb_gt_SetPos( USHORT uiRow, USHORT uiCol )
{
   COORD dwCursorPosition;

   HB_TRACE(("hb_gt_SetPos(%hu, %hu)", uiRow, uiCol));

   dwCursorPosition.X = ( SHORT ) uiCol;
   dwCursorPosition.Y = ( SHORT ) uiRow;

   SetConsoleCursorPosition( HCursor, dwCursorPosition );
}

USHORT hb_gt_GetCursorStyle( void )
{
   CONSOLE_CURSOR_INFO cci;
   USHORT uiCursorShape;

   HB_TRACE(("hb_gt_GetCursorStyle()"));

   GetConsoleCursorInfo( HCursor, &cci );

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

   HB_TRACE(("hb_gt_SetCursorStyle(%hu)", style));

   GetConsoleCursorInfo( HCursor, &cci );

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

   SetConsoleCursorInfo( HCursor, &cci );
}

void hb_gt_Puts( USHORT uiRow, USHORT uiCol, BYTE attr, BYTE * str, ULONG len )
{
   DWORD dwWritten;
   COORD coord;

   HB_TRACE(("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", uiRow, uiCol, (int) attr, str, len));

   coord.X = ( DWORD ) uiCol;
   coord.Y = ( DWORD ) uiRow;

   FillConsoleOutputAttribute( HOutput, ( WORD )( attr & 0xFF ), ( DWORD ) len, coord, &dwWritten );
   WriteConsoleOutputCharacterA( HOutput, ( char * ) str, ( DWORD ) len, coord, &dwWritten );
}

void hb_gt_GetText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * dest )
{
   LPWORD pwattr;
   BYTE * pstr;
   USHORT width;

   HB_TRACE(("hb_gt_GetText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, dest));

   width = ( uiRight - uiLeft + 1 );
   pwattr = ( LPWORD ) hb_xgrab( width * sizeof( *pwattr ) );
   pstr = ( BYTE * ) hb_xgrab( width );

   for( ; uiTop <= uiBottom; uiTop++ )
   {
      COORD coord;
      USHORT i;
      DWORD dwWritten;

      coord.X = ( DWORD ) uiLeft;
      coord.Y = ( DWORD ) uiTop;
      ReadConsoleOutputCharacterA( HOutput, ( char * ) pstr, width, coord, &dwWritten );
      ReadConsoleOutputAttribute( HOutput, pwattr, width, coord, &dwWritten );
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

   HB_TRACE(("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, srce));

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
      coord.X = ( DWORD ) uiLeft;
      coord.Y = ( DWORD ) uiTop;
      WriteConsoleOutputAttribute( HOutput, pwattr, width, coord, &dwWritten );
      WriteConsoleOutputCharacterA( HOutput, ( char * ) pstr, width, coord, &dwWritten );
   }

   hb_xfree( pstr );
   hb_xfree( pwattr );
}

void hb_gt_SetAttribute( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE attr )
{
/* ptucker */

   COORD coord;
   USHORT width;

   HB_TRACE(("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d)", uiTop, uiLeft, uiBottom, uiRight, (int) attr));

   width = uiRight - uiLeft + 1;

   coord.X = ( DWORD ) uiLeft;

   for( ; uiTop <= uiBottom; uiTop++ )
   {
      DWORD dwWritten;

      coord.Y = uiTop;
      FillConsoleOutputAttribute( HOutput, ( WORD )( attr & 0xFF ), width, coord, &dwWritten );
   }
}

USHORT hb_gt_Col( void )
{
   CONSOLE_SCREEN_BUFFER_INFO csbi;

   HB_TRACE(("hb_gt_Col()"));

   GetConsoleScreenBufferInfo( HCursor, &csbi );

   return csbi.dwCursorPosition.X;
}

USHORT hb_gt_Row( void )
{
   CONSOLE_SCREEN_BUFFER_INFO csbi;

   HB_TRACE(("hb_gt_Row()"));

   GetConsoleScreenBufferInfo( HCursor, &csbi );

   return csbi.dwCursorPosition.Y;
}

void hb_gt_Scroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE attr, SHORT iVert, SHORT iHoriz )
{
/* ptucker */

   HB_TRACE(("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", uiTop, uiLeft, uiBottom, uiRight, (int) attr, iVert, iHoriz));

   if( ( iHoriz | iVert ) == 0 ) /* both zero? */
   {
      COORD coord;
      USHORT width = uiRight - uiLeft + 1;

      coord.X = ( DWORD ) uiLeft;

      for( ; uiTop <= uiBottom; uiTop++ )
      {
         DWORD dwWritten;

         coord.Y = uiTop;
         FillConsoleOutputAttribute( HOutput, ( WORD )( attr & 0xFF ), width, coord, &dwWritten );
         FillConsoleOutputCharacter( HOutput, ' ', width, coord, &dwWritten );
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

      ScrollConsoleScreenBuffer( HOutput, &Source, &Clip, Target, &FillChar );
   }
}

void hb_gt_DispBegin( void )
{
   HB_TRACE(("hb_gt_DispBegin()"));

/* ptucker */
   if( hb_gtDispCount() == 1 )
   {
      COORD coDest = { 0, 0 };
      COORD coBuf;                    /* the size of the buffer to read into */
      CHAR_INFO * pCharInfo;  /* buffer to store info from ReadConsoleOutput */
      SMALL_RECT srWin;                     /* source rectangle to read from */
      CONSOLE_SCREEN_BUFFER_INFO csbi;

      GetConsoleScreenBufferInfo( HCursor, &csbi );
      srWin.Top    = srWin.Left = 0;
      srWin.Bottom = ( coBuf.Y = csbi.dwSize.Y ) - 1;
      srWin.Right  = ( coBuf.X = csbi.dwSize.X ) - 1;

      /* allocate a buffer for the screen rectangle */
      pCharInfo = ( CHAR_INFO * ) hb_xgrab( coBuf.Y * coBuf.X * sizeof( CHAR_INFO ) );

      /* read the screen rectangle into the buffer */
      ReadConsoleOutput( HOutput,    /* current screen handle  */
                   pCharInfo,        /* transfer area          */
                   coBuf,            /* size of destination buffer */
                   coDest,           /* upper-left cell to write data to   */
                   &srWin );         /* screen buffer rectangle to read from */

      if( HStealth == INVALID_HANDLE_VALUE )
      {
         HStealth = CreateConsoleScreenBuffer(
                    GENERIC_READ    | GENERIC_WRITE,    /* Access flag        */
                    FILE_SHARE_READ | FILE_SHARE_WRITE, /* Buffer share mode  */
                    NULL,                               /* Security attribute */
                    CONSOLE_TEXTMODE_BUFFER,            /* Type of buffer     */
                    NULL );                             /* reserved           */

      }

      hb_gt_SetScreenBuffer( HStealth, HOutput );

      HOutput = HStealth;
      WriteConsoleOutput( HOutput,    /* output handle */
                   pCharInfo,         /* data to write */
                   coBuf,             /* col/row size of source buffer */
                   coDest,            /* upper-left cell to write data from in src */
                   &srWin );          /* screen buffer rect to write data to */

      hb_xfree( pCharInfo );
   }
}

void hb_gt_DispEnd( void )
{
   HB_TRACE(("hb_gt_DispEnd()"));

/* ptucker */

   if( hb_gtDispCount() == 1 )
   {
      HANDLE htmp = HStealth;

      HStealth = HCursor;
      hb_gt_DispBegin();
      HStealth = htmp;
   }
}

static BOOL hb_gt_SetScreenBuffer( HANDLE HNew, HANDLE HOld )
{
/* ptucker */

/* set a new buffer to have the same characteristics as an existing buffer */
   CONSOLE_SCREEN_BUFFER_INFO csbi;
   SMALL_RECT srWin;

   HB_TRACE(("hb_gt_SetScreenBuffer(%p, %p)", HNew, HOld));

   GetConsoleScreenBufferInfo( HOld, &csbi );

   /* new console window size and scroll position */
   srWin.Top    = srWin.Left = 0;
   srWin.Bottom = csbi.dwSize.Y - 1;
   srWin.Right  = csbi.dwSize.X - 1;

   SetConsoleScreenBufferSize( HNew, csbi.dwSize );
   SetConsoleWindowInfo( HNew, TRUE,  &csbi.srWindow );
   SetConsoleWindowInfo( HNew, FALSE, &srWin );

   return TRUE;
}

BOOL hb_gt_SetMode( USHORT uiRows, USHORT uiCols )
{
/* ptucker */
   BOOL bRetVal = TRUE;
   CONSOLE_SCREEN_BUFFER_INFO csbi;
   SMALL_RECT srWin;
   COORD coBuf;

   HB_TRACE(("hb_gt_SetMode(%hu, %hu)", uiRows, uiCols));

   GetConsoleScreenBufferInfo( HOutput, &csbi );
   coBuf = GetLargestConsoleWindowSize( HOutput );

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
      if( !SetConsoleWindowInfo( HOutput, TRUE, &srWin ) ||
          !SetConsoleScreenBufferSize( HOutput, coBuf ) )
         bRetVal = FALSE;
   }
   else if( ( DWORD ) csbi.dwSize.X * csbi.dwSize.Y < ( DWORD ) uiCols * uiRows )
   {
      if( !SetConsoleScreenBufferSize( HOutput, coBuf ) ||
          !SetConsoleWindowInfo( HOutput, TRUE, &srWin ) )
         bRetVal = FALSE;
   }

   return bRetVal;
}

void hb_gt_Replicate( BYTE c, ULONG ulLength )
{

/* ptucker */
   COORD coBuf = { 0, 0 };
   DWORD dwWritten;

   HB_TRACE(("hb_gt_Replicate(%d, %lu)", (int) c, ulLength));

/* TODO: This is not used and may be eliminated after further review */
   FillConsoleOutputCharacter(
           HOutput,                      /* handle to screen buffer        */
           c,                            /* character to write             */
           ( DWORD ) ulLength,           /* number of cells to write       */
           coBuf,                        /* coordinates of first cell      */
           &dwWritten                    /* receives actual number written */
           );

}

BOOL hb_gt_GetBlink()
{
   HB_TRACE(("hb_gt_GetBlink()"));

   /* TODO */
   return FALSE;
}

void hb_gt_SetBlink( BOOL bBlink )
{
   HB_TRACE(("hb_gt_SetBlink(%d)", (int) bBlink));

  /* TODO: set the bit if it's supported */
   HB_SYMBOL_UNUSED( bBlink );
}

void hb_gt_DebugScreen( BOOL bActivate )
{
   HB_TRACE(("hb_gt_DebugScreen(%d)", (int) bActivate));

   /* ptucker */
   /* TODO: This is not used and is still a work in progress */
   if( bActivate )
   {
      if( HDOutput == INVALID_HANDLE_VALUE )
      {
         HDOutput = CreateConsoleScreenBuffer(
                    GENERIC_READ    | GENERIC_WRITE,    /* Access flag        */
                    FILE_SHARE_READ | FILE_SHARE_WRITE, /* Buffer share mode  */
                    NULL,                               /* Security attribute */
                    CONSOLE_TEXTMODE_BUFFER,            /* Type of buffer     */
                    NULL );                             /* reserved           */

         hb_gt_SetScreenBuffer( HDOutput, HOutput );
      }
      HOsave = HOutput;
      HOutput = HCursor = HDOutput;
      hb_gtDispBegin();
      hb_gtDispEnd();
   }
   else
   {
      HOutput = HOsave;
      HCursor = HOriginal;
   }
   SetConsoleActiveScreenBuffer( HOutput );
}
