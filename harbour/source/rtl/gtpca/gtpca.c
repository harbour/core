/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for ANSI terminals
 *
 * Copyright 2000 David G. Holm <dholm@jsd-llc.com>
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
 *  This module is partially based on VIDMGR by Andrew Clarke and modified
 *  for the Harbour project
 */

/* NOTE: User programs should never call this layer directly! */

#if defined(__GNUC__) && ! defined(__MINGW32__)
   #include <unistd.h>
   #if defined(__DJGPP__) || defined(__CYGWIN__) || defined(HARBOUR_GCC_OS2)
      #include <io.h>
   #endif
#else
   #include <io.h>
#endif
#include <ctype.h>
#include <string.h>

#include "hbapigt.h"
#include "hbset.h"
#include "inkey.ch"

static USHORT s_usRow, s_usCol, s_usMaxRow, s_usMaxCol;
static int s_iFilenoStdin, s_iFilenoStdout, s_iFilenoStderr;
static int s_iAttribute;
static BOOL s_bColor;
static char s_szSpaces[] = "                                                                                    "; /* 84 spaces */

static void hb_gt_AnsiGetCurPos( USHORT * row, USHORT * col );

static USHORT s_uiDispCount;

void hb_gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));
   s_iAttribute = 0;
   s_iFilenoStdin = iFilenoStdin;
   s_iFilenoStdout = iFilenoStdout;
   s_iFilenoStderr = iFilenoStderr;
   s_usRow = s_usCol = 0;
   hb_gt_AnsiGetCurPos( &s_usRow, &s_usCol );
#ifdef OS_UNIX_COMPATIBLE
   s_usMaxRow = 23;
   s_bColor = FALSE;
#else
   s_usMaxRow = 24;
   s_bColor = TRUE;
#endif
   s_usMaxCol = 79;
   fprintf( stdout, "\x1B[=7h" ); /* Enable line wrap (for OUTSTD() and OUTERR()) */
}

void hb_gt_Done( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Done()"));
   /* TODO: */
}

static void hb_gt_AnsiGetCurPos( USHORT * row, USHORT * col )
{
   if( isatty( s_iFilenoStdin ) && isatty( s_iFilenoStdout ) )
   {
      USHORT ch, value = 0, index = 0;
      fprintf( stdout, "\x1B[6n" );
      do
      {
        ch = getc( stdin );
        if( isdigit( ch ) )
        {
           value = ( value * 10 ) + ( ch - '0' );
        }
        else if( ch == ';' )
        {
           *row = value - 1;
           value = 0;
        }
      }
      while( ch != 'R' && index < 10 );
      *col = value - 1;
   }
}

static void hb_gt_AnsiSetAttributes( BYTE attr )
{
   static const int color[] = { 0, 4, 2, 6, 1, 5, 3, 7 };
   int bg_color = 40 + color[ ( attr & 0x70 ) >> 4 ];
   int fg_color = 30 + color[ attr & 0x07 ];
   int special = 0;
   if( attr & 0x08 ) special = 1;
   else if( attr & 0x80 )
   {
      if( hb_set.HB_SET_INTENSITY ) special = 1;
      else special = 5;
   }
   fprintf( stdout, "\x1B[%d;%d;%dm", special, fg_color, bg_color );
}

BOOL hb_gt_AdjustPos( BYTE * pStr, ULONG ulLen )
{
   USHORT row = s_usRow;
   USHORT col = s_usCol;
   ULONG ulCount;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_AdjustPos(%s, %lu)", pStr, ulLen ));

   for( ulCount = 0; ulCount < ulLen; ulCount++ )
   {
      switch( *pStr++  )
      {
         case HB_CHAR_BEL:
            break;

         case HB_CHAR_BS:
            if( col )
               col--;
            else
            {
               col = s_usMaxCol;
               if( row )
                  row--;
            }
            break;

         case HB_CHAR_LF:
            if( row < s_usMaxRow )
               row++;
            break;

         case HB_CHAR_CR:
            col = 0;
            break;

         default:
            if( col < s_usMaxCol )
               col++;
            else
            {
               col = 0;
               if( row < s_usMaxRow )
                  row++;
            }
      }
   }
   hb_gt_SetPos( row, col );
   return TRUE;
}

#ifdef HARBOUR_GCC_OS2
   #include "..\..\kbdos2.gcc"
#else
int hb_gt_ReadKey( HB_inkey_enum eventmask )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));
   HB_SYMBOL_UNUSED( eventmask );
   /* TODO: */
   return 0;
}
#endif

BOOL hb_gt_IsColor( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()")); 
   /* TODO: */
   return s_bColor;
}

USHORT hb_gt_GetScreenWidth( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()")); 
   /* TODO: */
   return s_usMaxCol + 1;
}

USHORT hb_gt_GetScreenHeight( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()")); 
   /* TODO: */
   return s_usMaxRow + 1;
}

void hb_gt_SetPos( SHORT iRow, SHORT iCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd)", iRow, iCol));
   if( iRow < 0 ) iRow = 0;
   else if( iRow > s_usMaxRow ) iRow = s_usMaxRow;
   if( iCol < 0 ) iCol = 0;
   else if( iCol > s_usMaxCol ) iCol = s_usMaxCol;
   s_usRow = iRow;
   s_usCol = iCol;
   fprintf( stdout, "\x1B[%d;%dH", s_usRow + 1, s_usCol + 1 );
}

SHORT hb_gt_Row( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));
   return s_usRow;
}

SHORT hb_gt_Col( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));
   return s_usCol;
}


void hb_gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr, SHORT sVert, SHORT sHoriz )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", usTop, usLeft, usBottom, usRight, (int) attr, sVert, sHoriz));
   HB_SYMBOL_UNUSED( attr );
   if( sVert == 0 && sHoriz == 0 )
   {
      /* Clear */
      if( usTop == 0 && usLeft == 0 && usBottom >= s_usMaxRow && usRight >= s_usMaxCol )
      {
         /* Clear the entire screen */
         fprintf( stdout, "\x1B[2J" );
      }
      else
      {
         /* Clear a screen region */
         USHORT i;
         for( i = usTop; i <= usBottom; i++ )
         {
            hb_gt_Puts( i, usLeft, s_iAttribute, ( BYTE * )s_szSpaces, (usRight - usLeft ) + 1 );
         }
      }
   }
   else
   {
      if( usTop == 0 && usLeft == 0 && usBottom >= s_usMaxRow && usRight >= s_usMaxCol )
      {
         if( sVert > 0 && sHoriz == 0 )
         {
            /* Scroll the entire screen up */
            fprintf( stdout, "\x1B[25;80" );
            while( sVert-- ) fputc( '\n', stdout );
         }
         else
         {
            /* TODO: Scroll the entire screen any direction other than up */
         }
      }
      else
      {
         /* TODO: Scroll a screen region */
      }
   }
}

USHORT hb_gt_GetCursorStyle( void )
{
   /* TODO: What shape is the cursor? */
   USHORT uiStyle = 0;
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));
   return uiStyle;
}

void hb_gt_SetCursorStyle( USHORT style )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", style));
   HB_SYMBOL_UNUSED( style );
}

void hb_gt_Puts( USHORT usRow, USHORT usCol, BYTE attr, BYTE * str, ULONG len )
{
   /* Because Clipper strings don't have to be null terminated, add a null
      terminating character after saving what used to be at the termination
      position, because it might not even be part of the string object */
   char save = str[ len ];
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu, %s)", usRow, usCol, (int) attr, str, len, str));
   str[ len ] = '\0';
   /* Disable line wrap, set the new cursor position, send the string, then
      enable line wrap (for OUTSTD() and OUTERR() ) */
   hb_gt_AnsiSetAttributes( attr );
   fprintf( stdout, "\x1B[=7l\x1B[%d;%dH%s\x1B[=7h", usRow + 1, usCol + 1, str );
   /* Restore whatever used to be at the termination position */
   str[ len ] = save;
   /* Update the cursor position */
   s_usRow = usRow;
   s_usCol = usCol + ( USHORT )len;
   if( s_usCol > s_usMaxCol ) s_usCol = s_usMaxCol;
}

void hb_gt_GetText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE *dest )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p)", usTop, usLeft, usBottom, usRight, dest));
   HB_SYMBOL_UNUSED( usTop );
   HB_SYMBOL_UNUSED( usLeft );
   HB_SYMBOL_UNUSED( usBottom );
   HB_SYMBOL_UNUSED( usRight );
   HB_SYMBOL_UNUSED( dest );
   /* TODO: */
}

void hb_gt_PutText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE *src )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", usTop, usLeft, usBottom, usRight, srce) );
   HB_SYMBOL_UNUSED( usTop );
   HB_SYMBOL_UNUSED( usLeft );
   HB_SYMBOL_UNUSED( usBottom );
   HB_SYMBOL_UNUSED( usRight );
   HB_SYMBOL_UNUSED( src );
   /* TODO: */
}

void hb_gt_SetAttribute( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d)", usTop, usLeft, usBottom, usRight, (int) attr) );
   HB_SYMBOL_UNUSED( usTop );
   HB_SYMBOL_UNUSED( usLeft );
   HB_SYMBOL_UNUSED( usBottom );
   HB_SYMBOL_UNUSED( usRight );
   HB_SYMBOL_UNUSED( attr );
   /* TODO: */
   s_iAttribute = attr;
}

void hb_gt_DispBegin( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispBegin()"));

   ++s_uiDispCount;

   /* TODO: */
}

void hb_gt_DispEnd( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));

   --s_uiDispCount;

   /* TODO: here we flush the buffer, and restore normal screen writes */
}

BOOL hb_gt_SetMode( USHORT uiRows, USHORT uiCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", uiRows, uiCols) );
   HB_SYMBOL_UNUSED( uiRows );
   HB_SYMBOL_UNUSED( uiCols );
   /* TODO: */
   return 0;   /* 0 = Ok, other = Fail */
}

void hb_gt_Replicate( BYTE c, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%d, %lu)", (int) c, ulLen) );
   HB_SYMBOL_UNUSED( c );
   HB_SYMBOL_UNUSED( ulLen );
  /* TODO: this will write character c nlength times to the screen.
           Note that it is not used yet
           If there is no native function that supports this, it is
           already handled in a generic way by higher level functions.
  */
}

BOOL hb_gt_GetBlink()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));
   /* TODO: */
   return 1;               /* 0 = blink, 1 = intens      */
}

void hb_gt_SetBlink( BOOL bBlink )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink) );
   HB_SYMBOL_UNUSED( bBlink );
   /* TODO: */
}

void hb_gt_Tone( double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));

   /* TODO: Implement this */

   HB_SYMBOL_UNUSED( dFrequency );
   HB_SYMBOL_UNUSED( dDuration );
}

char * hb_gt_Version( void )
{
   return "Harbour Terminal: PC ANSI";
}

USHORT hb_gt_DispCount()
{
   return s_uiDispCount;
}
