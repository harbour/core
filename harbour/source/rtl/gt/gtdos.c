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
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
 *    hb_gt_CtrlBrkHandler()
 *    hb_gt_CtrlBrkRestore()
 *
 * See doc/license.txt for licensing terms.
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
#include "set.h" /* For Ctrl+Break handling */
#include "ctoharb.h" /* For Ctrl+Break handling */

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
   #include <sys/exceptn.h>
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

#if defined(__WATCOMC__)
   #if defined(__386__)
      #define FAR
   #endif
   #include <signal.h>
#endif
#ifndef __DJGPP__
   static char FAR * scrnPtr;
   static char FAR * scrnStealth = NULL;
   static char FAR * hb_gt_ScreenAddress( void );
#endif

#ifndef __DJGPP__
BOOL hb_gtBreak = FALSE; /* Used to signal Ctrl+Break to hb_inkeyPoll() */
#if defined(__WATCOMC__)
static void hb_gt_Watcom_CtrlBreak_Handler( int iSignal )
{
   /* Ctrl-Break was pressed */
   /* NOTE: the layout of this function is forced by the Watcom compiler
    */
   HB_SYMBOL_UNUSED( iSignal );
   hb_gtBreak = TRUE;
}
#else
static int s_iOldCtrlBreak = 0;

static int hb_gt_CtrlBrkHandler( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_CtrlBrkHandler()"));
   hb_gtBreak = TRUE;
   return 1;
}
#endif

static void hb_gt_CtrlBrkRestore( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_CtrlBrkRestore()"));
   #if defined(__WATCOMC__)
      signal( SIGBREAK, SIG_DFL);
   #else
      setcbrk( s_iOldCtrlBreak );
   #endif
}
#endif

void hb_gt_Init( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

#ifdef __DJGPP__
   gppconio_init();
   __djgpp_hwint_flags |= 2;     /* Count Ctrl+Break instead of killing program */
   __djgpp_set_ctrl_c( 0 );      /* Disable Ctrl+C */
   __djgpp_set_sigquit_key( 0 ); /* Disable Ctrl+\ */

#else
   /* Set the Ctrl+Break handler [vszel] */

   #if defined(__WATCOMC__)
      signal( SIGBREAK, hb_gt_Watcom_CtrlBreak_Handler );
   #else
      ctrlbrk( hb_gt_CtrlBrkHandler );
      s_iOldCtrlBreak = getcbrk();
      setcbrk( 1 );
   #endif
   atexit( hb_gt_CtrlBrkRestore );

   /* */

   scrnStealth = ( char * ) -1;
   scrnPtr = hb_gt_ScreenAddress();
#endif
}

void hb_gt_Done( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Done()"));

#ifndef __DJGPP__
  if( scrnStealth != ( char * ) -1 )
     hb_xfree( scrnStealth );
#endif
}

BOOL hb_gt_IsColor( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));

   return hb_gt_GetScreenMode() != 7;
}

#ifndef __DJGPP__
static char FAR * hb_gt_ScreenAddress()
{
   char FAR * ptr;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ScreenAddress()"));

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
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ScreenPtr(%hu, %hu)", cRow, cCol));

   return scrnPtr + ( cRow * hb_gt_GetScreenWidth() * 2 ) + ( cCol * 2 );
}
#endif

static char hb_gt_GetScreenMode( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenMode()"));

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
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));

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
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeigth()"));

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
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hu, %hu)", usRow, usCol));

#if defined(__TURBOC__)
   {
     BYTE cRow, cCol;
     cRow = ( BYTE ) usRow;
     cCol = ( BYTE ) usCol;

     _AH = 0x02;
     _BH = 0;
     _DH = cRow;
     _DL = cCol;
     geninterrupt( 0x10 );
   }
#else
   {
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
   }
#endif
}

static void hb_gt_SetCursorSize( char start, char end )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorSize(%d, %d)", (int) start, (int) end));

#if defined(__TURBOC__)
   {
     _AH = 0x01;
     _CH = start;
     _CL = end;
     geninterrupt( 0x10 );
   }
#else
   {
     union REGS regs;
     regs.h.ah = 0x01;
     regs.h.ch = start;
     regs.h.cl = end;
#if defined(__WATCOMC__) && defined(__386__)
     int386( 0x10, &regs, &regs );
#else
     int86( 0x10, &regs, &regs );
#endif
   }
#endif
}

static void hb_gt_GetCursorSize( char * start, char *end )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorSize(%p, %p)", start, end));

#if defined(__TURBOC__)
   {
     _AH = 0x03;
     _BH = 0;
     geninterrupt( 0x10 );
     *start = _CH;
     *end = _CL;
   }
#else
   {
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
   }
#endif
}

USHORT hb_gt_GetCursorStyle( void )
{
   char start, end;
   int rc;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));

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
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", style));

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
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_xGetXY(%hu, %hu, %p, %p", cRow, cCol, ch, attr));

#ifdef __DJGPP__
   {
     short ch_attr;
     gettext( cCol + 1, cRow + 1, cCol + 1, cRow + 1, &ch_attr );
     *ch = ch_attr >> 8;
     *attr = ch_attr & 0xFF;
   }
#else
   {
     char FAR *p;
     p = hb_gt_ScreenPtr( cRow, cCol );
     *ch = *p;
     *attr = *( p + 1 );
   }
#endif
}

void hb_gt_xPutch( USHORT cRow, USHORT cCol, BYTE attr, BYTE ch )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_xPutch(%hu, %hu, %d, %d", cRow, cCol, (int) attr, (int) ch));

#ifdef __DJGPP__
   {
     long ch_attr;
     ch_attr = ( ch << 8 ) | attr;
     puttext( cCol + 1, cRow + 1, cCol + 1, cRow + 1, &ch_attr );
   }
#else
   {
     char FAR * p;
     p = hb_gt_ScreenPtr( cRow, cCol );
     *p = ch;
     *( p + 1 ) = attr;
   }
#endif
}

void hb_gt_Puts( USHORT cRow, USHORT cCol, BYTE attr, BYTE *str, ULONG len )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu", cRow, cCol, (int) attr, str, len));

#ifdef __DJGPP__
   {
     int i;
     int bottom, left, right, top;
     int width;
     BYTE * ch_attr;
     BYTE * ptr;

     i = ( int ) len;
     left = cCol;
     top = cRow;
     width = hb_gt_GetScreenWidth();
     ptr = ch_attr = hb_xgrab( i * 2 );
     while( i-- )
       {
	 *ptr++ = *str++;
	 *ptr++ = attr;
       }
     i = len - 1; /* We want end position, not next cursor position */
     right = left;
     bottom = top;
     if( right + i > width - 1 )
       {
	 /*
	  * Calculate end row position and the remainder size for the
	  * end column adjust.
	  */
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
     puttext( left + 1, top + 1, right + 1, bottom + 1, ch_attr );
     hb_xfree( ch_attr );
   }
#else
   {
     char FAR *p;
     int i;

     p = hb_gt_ScreenPtr( cRow, cCol );
     for( i = 0; i < len; i++ )
       {
	 *p++ = *str++;
	 *p++ = attr;
       }
   }
#endif
}

void hb_gt_GetText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE * dest )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p", usTop, usLeft, usBottom, usRight, dest));

#ifdef __DJGPP__
   {
     gettext( usLeft + 1, usTop + 1, usRight + 1, usBottom + 1, dest );
   }
#else
   {
     USHORT x, y;

     for( y = usTop; y <= usBottom; y++ )
       {
	 for( x = usLeft; x <= usRight; x++ )
	   {
	     hb_gt_xGetXY( y, x, dest + 1, dest );
	     dest += 2;
	   }
       }
   }
#endif
}

void hb_gt_PutText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE * srce )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p", usTop, usLeft, usBottom, usRight, srce));

#ifdef __DJGPP__
   {
     puttext( usLeft + 1, usTop + 1, usRight + 1, usBottom + 1, srce );
   }
#else
   {
     USHORT x, y;

     for( y = usTop; y <= usBottom; y++ )
       {
	 for( x = usLeft; x <= usRight; x++ )
	   {
	     hb_gt_xPutch( y, x, *( srce + 1 ), *srce );
	     srce += 2;
	   }
       }
   }
#endif
}

void hb_gt_SetAttribute( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr )
{
   USHORT x, y;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d", usTop, usLeft, usBottom, usRight, (int) attr));

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
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));

#if defined(__TURBOC__)
   {
     _AH = 0x03;
     _BH = 0;
     geninterrupt( 0x10 );
     return _DL;
   }
#else
   {
     union REGS regs;
     regs.h.ah = 0x03;
     regs.h.bh = 0;
#if defined(__WATCOMC__) && defined(__386__)
     int386( 0x10, &regs, &regs );
#else
     int86( 0x10, &regs, &regs );
#endif
     return regs.h.dl;
   }
#endif
}

USHORT hb_gt_Row( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));

#if defined(__TURBOC__)
   {
     _AH = 0x03;
     _BH = 0;
     geninterrupt( 0x10 );
     return _DH;
   }
#else
   {
     union REGS regs;
     regs.h.ah = 0x03;
     regs.h.bh = 0;
#if defined(__WATCOMC__) && defined(__386__)
     int386( 0x10, &regs, &regs );
#else
     int86( 0x10, &regs, &regs );
#endif
     return regs.h.dh;
   }
#endif
}

void hb_gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr, SHORT sVert, SHORT sHoriz )
{
   int iRows = sVert, iCols = sHoriz;

   /* NOTE: 'SHORT' is used intentionally to correctly compile
   *  with C++ compilers
   */
   SHORT usRow, usCol;
   USHORT uiSize;   /* gtRectSize returns int */
   int iLength = ( usRight - usLeft ) + 1;
   int iCount, iColOld, iColNew, iColSize;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", usTop, usLeft, usBottom, usRight, (int) attr, sVert, sHoriz));

   hb_gtGetPos( &usRow, &usCol );

   if( hb_gtRectSize( usTop, usLeft, usBottom, usRight, &uiSize ) == 0 )
   {
      /* NOTE: 'unsigned' is used intentionally to correctly compile
       * with C++ compilers
       */
      unsigned char * fpBlank = ( unsigned char * ) hb_xgrab( iLength );
      unsigned char * fpBuff = ( unsigned char * ) hb_xgrab( iLength * 2 );

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
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispBegin()"));

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
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));

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
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", usRows, usCols));

   /* TODO: Implement this function. */

   HB_SYMBOL_UNUSED( usRows );
   HB_SYMBOL_UNUSED( usCols );

   return 0;
}

void hb_gt_Replicate( BYTE c, ULONG nLength )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%d, %lu)", (int) c, nLength));

   c = ' ';
   nLength = 0;
}

BOOL hb_gt_GetBlink()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));

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
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink));

#if defined(__TURBOC__)
   {
     _AX = 0x1003;
     _BX = bBlink;
     geninterrupt( 0x10 );
   }
#else
   {
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
   }
#endif
}
