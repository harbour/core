/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for DOS compilers
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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
 *  This module is based on VIDMGR by Andrew Clarke and modified for
 *  the Harbour project
 */

/* NOTE: User programs should never call this layer directly! */

#include <string.h>
#include <dos.h>
#include "gtapi.h"

#if defined(__POWERC) || (defined(__TURBOC__) && !defined(__BORLANDC__)) || \
   (defined(__ZTC__) && !defined(__SC__))
   #define FAR far
#elif (defined(__MSDOS__) || defined(MSDOS) || defined(DOS)) && !defined(__DJGPP__)
   #define FAR _far
#else
   #define FAR
#endif

#ifdef __DJGPP__
   #include <conio.h>
   #include <sys/farptr.h>
#else
   #ifndef MK_FP
      #define MK_FP( seg, off ) \
         ((void FAR *)(((unsigned long)(seg) << 16)|(unsigned)(off)))
   #endif
#endif

static void hb_gt_xGetXY( USHORT cRow, USHORT cCol, BYTE * attr, BYTE * ch );
static void hb_gt_xPutch( USHORT cRow, USHORT cCol, BYTE attr, BYTE ch );

static char hb_gt_GetScreenMode( void );
static void hb_gt_SetCursorSize( char start, char end );
static void hb_gt_GetCursorSize( char * start, char * end );

#if defined(__WATCOMC__) && defined(__386__)
   #define FAR
#endif
#ifndef __DJGPP__
   static char FAR * scrnPtr;
   static char FAR * scrnStealth;
   static char FAR * hb_gt_ScreenAddress( void );
#endif

void hb_gt_Init( void )
{
#ifdef __DJGPP__
   gppconio_init();
#else
   scrnStealth = ( char * ) -1;
   scrnPtr = hb_gt_ScreenAddress();
#endif
}

void hb_gt_Done( void )
{
}

BOOL hb_gt_IsColor( void )
{
   return hb_gt_GetScreenMode() != 7;
}

#ifndef __DJGPP__
static char FAR * hb_gt_ScreenAddress()
{
   char FAR * ptr;

   #if defined(__WATCOMC__) && defined(__386__)
      if( hb_gt_IsColor() )
      {
         ptr = ( char * ) ( 0xB800 << 4 );
      }
      else
      {
         ptr = ( char * )( 0xB000 << 4 );
      }
   #else
      if( hb_gt_IsColor() )
      {
         ptr = ( char FAR * ) MK_FP( 0xB800, 0x0000 );
      }
      else
      {
         ptr = ( char FAR * ) MK_FP( 0xB000, 0x0000 );
      }
   #endif

   return ptr;
}
#endif

#ifndef __DJGPP__
char FAR * hb_gt_ScreenPtr( USHORT cRow, USHORT cCol )
{
   return scrnPtr + ( cRow * hb_gt_GetScreenWidth() * 2 ) + ( cCol * 2 );
}
#endif

static char hb_gt_GetScreenMode( void )
{
#if defined(__WATCOMC__) && defined(__386__)
   return *( ( char * ) 0x0449 );
#elif defined(__DJGPP__)
   return _farpeekb( 0x0040, 0x0049 );
#else
   return *( ( char FAR * ) MK_FP( 0x0040, 0x0049 ) );
#endif
}

USHORT hb_gt_GetScreenWidth( void )
{
#if defined(__WATCOMC__) && defined(__386__)
   return ( USHORT ) *( ( char * ) 0x044A );
#elif defined(__DJGPP__)
   return ( USHORT ) _farpeekb( 0x0040, 0x004A );
#else
   return ( USHORT ) *( ( char FAR * ) MK_FP( 0x0040, 0x004A ) );
#endif
}

USHORT hb_gt_GetScreenHeight( void )
{
#if defined(__WATCOMC__) && defined(__386__)
   return ( USHORT ) ( char ) ( *( ( char * ) 0x0484 ) + 1 );
#elif defined(__DJGPP__)
   return ( USHORT ) _farpeekb( 0x0040, 0x0084 ) + 1;
#else
   return ( USHORT ) ( ( char ) ( *( ( char FAR * ) MK_FP( 0x0040, 0x0084 ) ) + 1 ) );
#endif
}

void hb_gt_SetPos( USHORT usRow, USHORT usCol )
{
#if defined(__TURBOC__)
   BYTE cRow, cCol;

   cRow = ( BYTE ) usRow;
   cCol = ( BYTE ) usCol;

   _AH = 0x02;
   _BH = 0;
   _DH = cRow;
   _DL = cCol;
   geninterrupt( 0x10 );
#else
   union REGS regs;
   regs.h.ah = 0x02;
   regs.h.bh = 0;
   regs.h.dh = ( BYTE ) usRow;
   regs.h.dl = ( BYTE ) usCol;
#if defined(__WATCOMC__) && defined(__386__)
   int386( 0x10, &regs, &regs );
#else
   int86( 0x10, &regs, &regs );
#endif
#endif
}

static void hb_gt_SetCursorSize( char start, char end )
{
#if defined(__TURBOC__)
   _AH = 0x01;
   _CH = start;
   _CL = end;
   geninterrupt( 0x10 );
#else
   union REGS regs;
   regs.h.ah = 0x01;
   regs.h.ch = start;
   regs.h.cl = end;
#if defined(__WATCOMC__) && defined(__386__)
   int386( 0x10, &regs, &regs );
#else
   int86( 0x10, &regs, &regs );
#endif
#endif
}

static void hb_gt_GetCursorSize( char * start, char *end )
{
#if defined(__TURBOC__)
   _AH = 0x03;
   _BH = 0;
   geninterrupt( 0x10 );
   *start = _CH;
   *end = _CL;
#else
   union REGS regs;
   regs.h.ah = 0x03;
   regs.h.bh = 0;
#if defined(__WATCOMC__) && defined(__386__)
   int386( 0x10, &regs, &regs );
#else
   int86( 0x10, &regs, &regs );
#endif
   *start = regs.h.ch;
   *end = regs.h.cl;
#endif
}

USHORT hb_gt_GetCursorStyle( void )
{
   char start, end;
   int rc;

   hb_gt_GetCursorSize( &start, &end );

   if( ( start == 32 ) && ( end == 32 ) )
      rc = SC_NONE;

   else if( ( start == 6 ) && ( end == 7 ) )
      rc = SC_NORMAL;

   else if( ( start == 4 ) && ( end == 7 ) )
      rc = SC_INSERT;

   else if( ( start == 0 ) && ( end == 7 ) )
      rc = SC_SPECIAL1;

   else if( ( start == 0 ) && ( end == 3 ) )
      rc = SC_SPECIAL2;

   else
      rc = SC_NONE;

   return rc;
}

void hb_gt_SetCursorStyle( USHORT style )
{
   switch( style )
   {
   case SC_NONE:
      hb_gt_SetCursorSize( 32, 32 );
      break;

   case SC_NORMAL:
      hb_gt_SetCursorSize( 6, 7 );
      break;

   case SC_INSERT:
      hb_gt_SetCursorSize( 4, 7 );
      break;

   case SC_SPECIAL1:
      hb_gt_SetCursorSize( 0, 7 );
      break;

   case SC_SPECIAL2:
      hb_gt_SetCursorSize( 0, 3 );
      break;

   default:
      break;
   }
}

static void hb_gt_xGetXY( USHORT cRow, USHORT cCol, BYTE * attr, BYTE * ch )
{
#ifdef __DJGPP__
   short ch_attr;
   gettext( cCol + 1, cRow + 1, cCol + 1, cRow + 1, &ch_attr );
   *ch = ch_attr >> 8;
   *attr = ch_attr & 0xFF;

/* printf("\r\nhb_gt_xGetXY(): row == %d, col = %d, char = %d, attr = %d", cRow, cCol, *ch, *attr ); */
#else
   char FAR *p;
   p = hb_gt_ScreenPtr( cRow, cCol );
   *ch = *p;
   *attr = *( p + 1 );
#endif
}

void hb_gt_xPutch( USHORT cRow, USHORT cCol, BYTE attr, BYTE ch )
{
#ifdef __DJGPP__
   long ch_attr = ( ch << 8 ) | attr;
/* printf("\r\nhb_gt_xPutch(): row == %d, col = %d, char = %d, attr = %d", cRow, cCol, ch, attr ); */
   puttext( cCol + 1, cRow + 1, cCol + 1, cRow + 1, &ch_attr );
#else
   char FAR * p;
   p = hb_gt_ScreenPtr( cRow, cCol );
   *p = ch;
   *( p + 1 ) = attr;
#endif
}

void hb_gt_Puts( USHORT cRow, USHORT cCol, BYTE attr, BYTE *str, ULONG len )
{
#ifdef __DJGPP__
   int i = ( int ) len;
   int bottom, left = cCol, right, top = cRow;
   int width = hb_gt_GetScreenWidth();
   BYTE * ch_attr, * ptr;
/* printf("\r\nhb_gt_Puts(): row == %d, col = %d, attr = %d, len = %d", cRow, cCol, attr, len ); */
   ptr = ch_attr = hb_xgrab( i * 2 );
   while( i-- )
   {
/* printf("+"); */
      *ptr++ = *str++;
/* printf("-"); */
      *ptr++ = attr;
   }
   i = len - 1; /* We want end position, not next cursor position */
   right = left;
   bottom = top;
   if( right + i > width - 1 )
   {
      /* Calculate end row position and the remainder size for the end column adjust */
      bottom += ( i / width );
      i = i % width;
   }
   right += i;
   if( right > width - 1 )
   {
      /* Column movement overflows into next row */
      bottom++;
      right -= width;
   }
/* printf("\r\nhb_gt_Puts(): puttext( %d,%d, %d,%d )", left + 1, top + 1, right + 1, bottom + 1 ); */
   puttext( left + 1, top + 1, right + 1, bottom + 1, ch_attr );
   hb_xfree( ch_attr );
#else
   char FAR *p;
   int i;
   p = hb_gt_ScreenPtr( cRow, cCol );
   for( i = 0; i < len; i++ )
   {
      *p++ = *str++;
      *p++ = attr;
   }
#endif
}

void hb_gt_GetText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE * dest )
{
#ifdef __DJGPP__
   gettext( usLeft + 1, usTop + 1, usRight + 1, usBottom + 1, dest );
#else
   USHORT x, y;

   for( y = usTop; y <= usBottom; y++ )
   {
      for( x = usLeft; x <= usRight; x++ )
      {
         hb_gt_xGetXY( y, x, dest + 1, dest );
         dest += 2;
      }
   }
#endif
}

void hb_gt_PutText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE * srce )
{
#ifdef __DJGPP__
   puttext( usLeft + 1, usTop + 1, usRight + 1, usBottom + 1, srce );
#else
   USHORT x, y;

   for( y = usTop; y <= usBottom; y++ )
   {
      for( x = usLeft; x <= usRight; x++ )
      {
         hb_gt_xPutch( y, x, *( srce + 1 ), *srce );
         srce += 2;
      }
   }
#endif
}

void hb_gt_SetAttribute( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr )
{
   USHORT x, y;

   for( y = usTop; y <= usBottom; y++ )
   {
      BYTE scratchattr;
      BYTE ch;

      for( x = usLeft; x <= usRight; x++ )
      {
         hb_gt_xGetXY( y, x, &scratchattr, &ch );
         hb_gt_xPutch( y, x, attr, ch );
      }
   }
}

USHORT hb_gt_Col( void )
{
#if defined(__TURBOC__)
   _AH = 0x03;
   _BH = 0;
   geninterrupt( 0x10 );
   return _DL;
#else
   union REGS regs;
   regs.h.ah = 0x03;
   regs.h.bh = 0;
#if defined(__WATCOMC__) && defined(__386__)
   int386( 0x10, &regs, &regs );
#else
   int86( 0x10, &regs, &regs );
#endif
   return regs.h.dl;
#endif
}

USHORT hb_gt_Row( void )
{
#if defined(__TURBOC__)
   _AH = 0x03;
   _BH = 0;
   geninterrupt( 0x10 );
   return _DH;
#else
   union REGS regs;
   regs.h.ah = 0x03;
   regs.h.bh = 0;
#if defined(__WATCOMC__) && defined(__386__)
   int386( 0x10, &regs, &regs );
#else
   int86( 0x10, &regs, &regs );
#endif
   return regs.h.dh;
#endif
}

void hb_gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr, SHORT sVert, SHORT sHoriz )
{
   int iRows = sVert, iCols = sHoriz;

   USHORT usRow, usCol;
   USHORT uiSize;   /* gtRectSize returns int */
   int iLength = ( usRight - usLeft ) + 1;
   int iCount, iColOld, iColNew, iColSize;

   hb_gtGetPos( &usRow, &usCol );

   if( hb_gtRectSize( usTop, usLeft, usBottom, usRight, &uiSize ) == 0 )
   {
      char * fpBlank = ( char * ) hb_xgrab( iLength );
      char * fpBuff = ( char * ) hb_xgrab( iLength * 2 );

      memset( fpBlank, ' ', iLength );

      iColOld = iColNew = usLeft;
      if( iCols >= 0 )
      {
         iColOld += iCols;
         iColSize = ( int ) ( usRight - usLeft );
         iColSize -= iCols;
      }
      else
      {
         iColNew -= iCols;
         iColSize = ( int ) ( usRight - usLeft );
         iColSize += iCols;
      }

      for( iCount = ( iRows >= 0 ? usTop : usBottom );
           ( iRows >= 0 ? iCount <= usBottom : iCount >= usTop );
           ( iRows >= 0 ? iCount++ : iCount-- ) )
      {
         int iRowPos = iCount + iRows;

         /* Blank the scroll region in the current row */
         hb_gt_Puts( iCount, usLeft, attr, fpBlank, iLength );

         if( ( iRows || iCols ) && iRowPos <= usBottom && iRowPos >= usTop )
         {
            /* Read the text to be scrolled into the current row */
            hb_gt_GetText( iRowPos, iColOld, iRowPos, iColOld + iColSize, fpBuff );

            /* Write the scrolled text to the current row */
            hb_gt_PutText( iCount, iColNew, iCount, iColNew + iColSize, fpBuff );
         }
      }

      hb_xfree( fpBlank );
      hb_xfree( fpBuff );
   }

   hb_gtSetPos( usRow, usCol );
}

void hb_gt_DispBegin( void )
{
/* ptucker */
#ifndef __DJGPP__
   if( hb_gtDispCount() == 1 )
   {
      char FAR * ptr;
      ULONG nSize;

      nSize = hb_gt_GetScreenWidth() * hb_gt_GetScreenHeight() * 2;

      ptr = scrnPtr;
      if( ( scrnPtr = scrnStealth ) == ( char * ) -1 )
         scrnPtr = ( char FAR * ) hb_xgrab( nSize );
      scrnStealth = ptr;
      memcpy( ( void * ) scrnPtr, ( void * ) ptr, nSize );
   }
#endif
}

void hb_gt_DispEnd( void )
{
/* ptucker */
#ifndef __DJGPP__
   if( hb_gtDispCount() == 1 )
   {
      char FAR * ptr;
      ULONG nSize;

      nSize = hb_gt_GetScreenWidth() * hb_gt_GetScreenHeight() * 2;

      ptr = scrnPtr;
      scrnPtr = scrnStealth;
      scrnStealth = ptr;
      memcpy( ( void * ) scrnPtr, ( void * )ptr, nSize );
   }
#endif
}

BOOL hb_gt_SetMode( USHORT usRows, USHORT usCols )
{
   /* TODO: Implement this function. */

   HB_SYMBOL_UNUSED( usRows );
   HB_SYMBOL_UNUSED( usCols );

   return 0;
}

void hb_gt_Replicate( BYTE c, ULONG nLength )
{
   c = ' ';
   nLength = 0;
}

BOOL hb_gt_GetBlink()
{
#if defined(__WATCOMC__) && defined(__386__)
   return *( ( char * ) 0x0465 ) & 0x10;
#elif defined(__DJGPP__)
   return _farpeekb( 0x0040, 0x0065 ) & 0x10;
#else
   return *( ( char FAR * ) MK_FP( 0x0040, 0x0065 ) ) &0x10;
#endif
}

void hb_gt_SetBlink( BOOL bBlink )
{
#if defined(__TURBOC__)
   _AX = 0x1003;
   _BX = bBlink;
   geninterrupt( 0x10 );
   return;
#else
   union REGS regs;
   regs.h.ah = 0x10;
   regs.h.al = 0x03;
   regs.h.bh = 0;
   regs.h.bl = bBlink;
#if defined(__WATCOMC__) && defined(__386__)
   int386( 0x10, &regs, &regs );
#else
   int86( 0x10, &regs, &regs );
#endif
#endif
}
