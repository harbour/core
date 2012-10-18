/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for DOS compilers
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
 *    hb_gt_CtrlBrkHandler()
 *    hb_gt_CtrlBrkRestore()
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_ReadKey()
 *
 * Copyright 2000 Alejandro de Garate <alex_degarate@hotmail.com>
 *    vmode12x40()
 *    vmode25x40()
 *    vmode28x40()
 *    vmode50x40()
 *    vmode12x80()
 *    vmode25x80()
 *    vmode28x80()
 *    vmode43x80()
 *    vmode50x80()
 *    hb_gt_SetMode()
 *    hb_gt_GetDisplay()
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *                Luiz Rafael Culik <Culik@sl.conex.net>
 *    Harbour Mouse Subsystem for DOS
 *
 * See COPYING for licensing terms.
 *
 */

/*
 * This module is based on VIDMGR by Andrew Clarke and modified for Harbour.
 */

/* NOTE: User programs should never call this layer directly! */

#define HB_GT_NAME      DOS

#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapicdp.h"
#include "hbapiitm.h"
#include "inkey.ch"

#include <dos.h>

#include <string.h>
#include <time.h>
#include <conio.h>


/*
 * use mouse driver save/restore state functions,
 * add other compilers for which calling real mode
 * interrupts with memory pointer is implemented.
 */
#if defined( __DJGPP__ )
   #define HB_MOUSE_SAVE
#endif

#if defined( __DJGPP__ )
   #include <pc.h>
   #include <sys/exceptn.h>
   #include <sys/farptr.h>
   #include <dpmi.h>
#elif defined( _MSC_VER ) || defined( __WATCOMC__ )
   #include <signal.h>
#endif

/* For screen support */
#if defined( __POWERC ) || ( defined( __TURBOC__ ) && !defined( __BORLANDC__ ) ) || ( defined( __ZTC__ ) && !defined( __SC__ ) )
   #define FAR far
#elif defined( HB_OS_DOS ) && !defined( __DJGPP__ ) && !defined( __RSX32__ ) && !defined( __WATCOMC__ )
   #define FAR _far
#else
   #define FAR
#endif

#if !defined( __DJGPP__ )
   #ifndef MK_FP
      #define MK_FP( seg, off ) \
         ((void FAR *)(((unsigned long)(seg) << 4)|(unsigned)(off)))
   #endif
   static unsigned char FAR * s_pScreenAddres;
#endif

#if defined( __WATCOMC__ ) && defined( __386__ )
   #define HB_PEEK_BYTE(s,o)     ( *( ( HB_UCHAR * ) ( ( (s) << 4 ) | (o) ) ) )
   #define HB_POKE_BYTE(s,o,b)   ( *( ( HB_UCHAR * ) ( ( (s) << 4 ) | (o) ) ) = ( HB_UCHAR ) (b) )
#elif defined( __DJGPP__ )
   #define HB_PEEK_BYTE(s,o)     _farpeekb( (s), (o) )
   #define HB_POKE_BYTE(s,o,b)   _farpokeb( (s), (o), (b) )
#else
   #define HB_PEEK_BYTE(s,o)     ( *( ( HB_UCHAR FAR * ) MK_FP( (s), (o) ) ) )
   #define HB_POKE_BYTE(s,o,b)   ( *( ( HB_UCHAR FAR * ) MK_FP( (s), (o) ) ) = ( HB_UCHAR ) (b) )
#endif

static int           s_GtId;
static HB_GT_FUNCS   SuperTable;
#define HB_GTSUPER   (&SuperTable)
#define HB_GTID_PTR  (&s_GtId)

static int  s_iRows;
static int  s_iCols;
static int  s_iCurRow;
static int  s_iCurCol;
static int  s_iCursorStyle;
static int  s_iScreenMode;

static HB_BOOL s_bBreak; /* Used to signal Ctrl+Break to hb_inkeyPoll() */

#if defined( __RSX32__ )
static int kbhit( void )
{
   union REGS regs;

   regs.h.ah = 0x0B;
   HB_DOS_INT86( 0x21, &regs, &regs );

   return regs.HB_XREGS.ax;
}
#endif

#if !defined( __DJGPP__ ) && !defined( __RSX32__ )
#if defined( __WATCOMC__ ) || defined( _MSC_VER )
static void hb_gt_dos_CtrlBreak_Handler( int iSignal )
{
   /* Ctrl-Break was pressed */
   /* NOTE: the layout of this function is forced by the compiler
    */
   HB_SYMBOL_UNUSED( iSignal );
   s_bBreak = HB_TRUE;
}
#else
static int s_iOldCtrlBreak = 0;
static int hb_gt_dos_CtrlBrkHandler( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_CtrlBrkHandler()"));
   s_bBreak = HB_TRUE;
   return 1;
}
#endif

static void hb_gt_dos_CtrlBrkRestore( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_CtrlBrkRestore()"));

#if defined( __WATCOMC__ )
   signal( SIGBREAK, SIG_DFL );
#elif defined( _MSC_VER )
   signal( SIGINT, SIG_DFL );
#else
   setcbrk( s_iOldCtrlBreak );
#endif
}
#endif

static int hb_gt_dos_GetScreenMode( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_GetScreenMode()"));

   return ( int ) HB_PEEK_BYTE( 0x0040, 0x0049 );
}

static void hb_gt_dos_GetScreenSize( int * piRows, int * piCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_GetScreenSize(%p, %p)", piRows, piCols));

   *piRows = ( int ) HB_PEEK_BYTE( 0x0040, 0x0084 ) + 1;
   *piCols = ( int ) HB_PEEK_BYTE( 0x0040, 0x004A );
}

#if !defined( __DJGPP__ )
static HB_BYTE FAR * hb_gt_dos_ScreenAddress( PHB_GT pGT )
{
   HB_BYTE FAR * ptr;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_ScreenAddress(%p)", pGT));

   #if defined( __WATCOMC__ ) && defined( __386__ )
      if( HB_GTSELF_ISCOLOR( pGT ) )
         ptr = ( HB_BYTE * )( 0xB800 << 4 );
      else
         ptr = ( HB_BYTE * )( 0xB000 << 4 );
   #else
      if( HB_GTSELF_ISCOLOR( pGT ) )
         ptr = ( HB_BYTE FAR * ) MK_FP( 0xB800, 0x0000 );
      else
         ptr = ( HB_BYTE FAR * ) MK_FP( 0xB000, 0x0000 );
   #endif

   return ptr;
}

HB_BYTE FAR * hb_gt_dos_ScreenPtr( int iRow, int iCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_ScreenPtr(%d, %d)", iRow, iCol));

   return s_pScreenAddres + ( ( ( iRow * s_iCols ) + iCol ) << 1 );
}
#endif

static void hb_gt_dos_GetScreenContents( PHB_GT pGT )
{
   PHB_CODEPAGE cdp;
   int iRow, iCol;
   HB_BYTE bAttr, bChar, bxAttr;
   HB_USHORT usChar;
#if !defined( __DJGPP__ )
   HB_BYTE * pScreenPtr = s_pScreenAddres;
#endif

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_GetScreenContents(%p)", pGT));

   bxAttr = 0;
   cdp = HB_GTSELF_CPTERM( pGT );
   if( !cdp )
   {
      cdp = HB_GTSELF_CPBOX( pGT );
      if( cdp )
         bxAttr = HB_GT_ATTR_BOX;
      else
         cdp = HB_GTSELF_HOSTCP( pGT );
   }

   for( iRow = 0; iRow < s_iRows; ++iRow )
   {
      for( iCol = 0; iCol < s_iCols; ++iCol )
      {
#if defined( __DJGPP__TEXT )
         short ch_attr;
         gettext( iCol + 1, iRow + 1, iCol + 1, iRow + 1, &ch_attr );
         bChar = ch_attr & 0xFF;
         bAttr = ch_attr >> 8;
#elif defined( __DJGPP__ )
         int iChar, iAttr;
         ScreenGetChar( &iChar, &iAttr, iCol, iRow );
         bAttr = iAttr;
         bChar = iChar;
#else
         bChar = *pScreenPtr;
         bAttr = *( pScreenPtr + 1 );
         pScreenPtr += 2;
#endif
         usChar = hb_cdpGetU16( cdp, bChar );
         HB_GTSELF_PUTSCRCHAR( pGT, iRow, iCol, bAttr, bxAttr, usChar );
      }
   }
   HB_GTSELF_COLDAREA( pGT, 0, 0, s_iRows, s_iCols );
}

static void hb_gt_dos_GetCursorPosition( int * piRow, int * piCol )
{
   union REGS regs;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_GetCursorPosition(%p, %p)", piRow, piCol));

   regs.h.ah = 0x03;
   regs.h.bh = 0;
   HB_DOS_INT86( 0x10, &regs, &regs );
   *piRow = regs.h.dh;
   *piCol = regs.h.dl;
}

static void hb_gt_dos_SetCursorPosition( int iRow, int iCol )
{
   union REGS regs;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_SetCursorPosition(%d, %d)", iRow, iCol));

   if( s_iCurRow != iRow || s_iCurCol != iCol )
   {
      regs.h.ah = 0x02;
      regs.h.bh = 0;
      regs.h.dh = ( HB_BYTE ) iRow;
      regs.h.dl = ( HB_BYTE ) iCol;
      HB_DOS_INT86( 0x10, &regs, &regs );
      s_iCurRow = iRow;
      s_iCurCol = iCol;
   }
}

static void hb_gt_dos_SetCursorSize( unsigned char start, unsigned char end )
{
   union REGS regs;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_SetCursorSize(%d, %d)", (int) start, (int) end));

   regs.h.ah = 0x01;
   regs.h.ch = start;
   regs.h.cl = end;
   HB_DOS_INT86( 0x10, &regs, &regs );
}

static void hb_gt_dos_GetCursorSize( unsigned char * start, unsigned char *end )
{
   union REGS regs;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_GetCursorSize(%p, %p)", start, end));

   regs.h.ah = 0x03;
   regs.h.bh = 0;
   HB_DOS_INT86( 0x10, &regs, &regs );
   *start = regs.h.ch;
   *end = regs.h.cl;
}

static int hb_gt_dos_GetCursorStyle( void )
{
   unsigned char start, end;
   int iStyle;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_GetCursorStyle()"));

   hb_gt_dos_GetCursorSize( &start, &end );

   if( start == 32 && end == 32 )
      iStyle = SC_NONE;

   else if( start == 6 && end == 7 )
      iStyle = SC_NORMAL;

   else if( start == 4 && end == 7 )
      iStyle = SC_INSERT;

   else if( start == 0 && end == 7 )
      iStyle = SC_SPECIAL1;

   else if( start == 0 && end == 3 )
      iStyle = SC_SPECIAL2;

   else
      iStyle = -1;

   return iStyle;
}

static void hb_gt_dos_SetCursorStyle( int iStyle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_SetCursorStyle(%d)", iStyle));

   if( iStyle != s_iCursorStyle )
   {
      switch( iStyle )
      {
         case SC_NONE:
            hb_gt_dos_SetCursorSize( 32, 32 );
            break;

         case SC_NORMAL:
            hb_gt_dos_SetCursorSize( 6, 7 );
            break;

         case SC_INSERT:
            hb_gt_dos_SetCursorSize( 4, 7 );
            break;

         case SC_SPECIAL1:
            hb_gt_dos_SetCursorSize( 0, 7 );
            break;

         case SC_SPECIAL2:
            hb_gt_dos_SetCursorSize( 0, 3 );
            break;

         default:
            return;
      }
      s_iCursorStyle = iStyle;
   }
}

/* *********************************************************************** */

static HB_BOOL s_fMousePresent = HB_FALSE;   /* Is there a mouse ? */
static HB_BOOL s_fMouseVisible = HB_FALSE;   /* Is mouse cursor visible ? */
static int     s_iMouseButtons = 0;          /* Mouse buttons */
static int     s_iMouseInitCol = 0;          /* Init mouse pos */
static int     s_iMouseInitRow = 0;          /* Init mouse pos */
static HB_BOOL s_fMouseBound;
static int     s_iMouseTop;
static int     s_iMouseLeft;
static int     s_iMouseBottom;
static int     s_iMouseRight;

#ifdef HB_MOUSE_SAVE
   static int  s_iMouseStorageSize = 0;      /* size of mouse storage buffer */
#endif

static void hb_gt_dos_mouse_Init( PHB_GT pGT )
{
   union REGS regs;

   regs.HB_XREGS.ax = 0;
   HB_DOS_INT86( 0x33, &regs, &regs );
   s_fMousePresent = regs.HB_XREGS.ax;
   if( s_fMousePresent )
   {
      s_iMouseButtons = regs.HB_XREGS.bx;
      if( s_iMouseButtons == 0 )
         s_iMouseButtons = 3;
      if( s_iMouseButtons == 0xffff )
         s_iMouseButtons = 2;

      s_iMouseInitCol = HB_GTSELF_MOUSECOL( pGT );
      s_iMouseInitRow = HB_GTSELF_MOUSEROW( pGT );
   }
}

static void hb_gt_dos_mouse_Exit( PHB_GT pGT )
{
   if( s_fMousePresent )
   {
      int iHeight, iWidth;

      HB_GTSELF_GETSIZE( pGT, &iHeight, &iWidth );
      HB_GTSELF_MOUSESETPOS( pGT, s_iMouseInitRow, s_iMouseInitCol );
      HB_GTSELF_MOUSESETBOUNDS( pGT, 0, 0, iHeight - 1, iWidth - 1 );
      s_fMousePresent = HB_FALSE;
   }
}

static HB_BOOL hb_gt_dos_mouse_IsPresent( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   return s_fMousePresent;
}

static void hb_gt_dos_mouse_Show( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   if( s_fMousePresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 1;
      HB_DOS_INT86( 0x33, &regs, &regs );

      s_fMouseVisible = HB_TRUE;
   }
}

static void hb_gt_dos_mouse_Hide( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   if( s_fMousePresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 2;
      HB_DOS_INT86( 0x33, &regs, &regs );

      s_fMouseVisible = HB_FALSE;
   }
}

static void hb_gt_dos_mouse_GetPos( PHB_GT pGT, int * piRow, int * piCol )
{
   HB_SYMBOL_UNUSED( pGT );

   if( s_fMousePresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 3;
      HB_DOS_INT86( 0x33, &regs, &regs );

      *piRow = regs.HB_XREGS.dx >> 3;
      *piCol = regs.HB_XREGS.cx >> 3;
   }
   else
      *piRow = *piCol = 0;
}

static void hb_gt_dos_mouse_SetPos( PHB_GT pGT, int iRow, int iCol )
{
   HB_SYMBOL_UNUSED( pGT );

   if( s_fMousePresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 4;
      regs.HB_XREGS.dx = iRow << 3;
      regs.HB_XREGS.cx = iCol << 3;
      HB_DOS_INT86( 0x33, &regs, &regs );
   }
}

static int hb_gt_dos_mouse_CountButton( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   if( s_fMousePresent )
      return s_iMouseButtons;
   else
      return 0;
}

static HB_BOOL hb_gt_dos_mouse_ButtonState( PHB_GT pGT, int iButton )
{
   HB_SYMBOL_UNUSED( pGT );

   if( s_fMousePresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 3;
      HB_DOS_INT86( 0x33, &regs, &regs );

      if( regs.HB_XREGS.bx & ( 1 << iButton ) )
         return HB_TRUE;
   }

   return HB_FALSE;
}

static HB_BOOL hb_gt_dos_mouse_ButtonPressed( PHB_GT pGT, int iButton, int * piRow, int * piCol )
{
   HB_SYMBOL_UNUSED( pGT );

   if( s_fMousePresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 5;
      regs.HB_XREGS.bx = iButton;
      HB_DOS_INT86( 0x33, &regs, &regs );

      if( regs.HB_XREGS.bx )
      {
         *piRow = regs.HB_XREGS.dx >> 3;
         *piCol = regs.HB_XREGS.cx >> 3;
         return HB_TRUE;
      }
   }
   *piRow = *piCol = 0;

   return HB_FALSE;
}

static HB_BOOL hb_gt_dos_mouse_ButtonReleased( PHB_GT pGT, int iButton, int * piRow, int * piCol )
{
   HB_SYMBOL_UNUSED( pGT );

   if( s_fMousePresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 6;
      regs.HB_XREGS.bx = iButton;
      HB_DOS_INT86( 0x33, &regs, &regs );

      if( regs.HB_XREGS.bx )
      {
         *piRow = regs.HB_XREGS.dx >> 3;
         *piCol = regs.HB_XREGS.cx >> 3;
         return HB_TRUE;
      }
   }
   *piRow = *piCol = 0;

   return HB_FALSE;
}

#ifdef HB_MOUSE_SAVE
static int hb_gt_dos_mouse_StorageSize( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   int iSize = 0;

   if( s_fMousePresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 0x15;
      HB_DOS_INT86( 0x33, &regs, &regs );
      s_iMouseStorageSize = regs.HB_XREGS.bx;
      if( s_iMouseStorageSize )
         iSize = s_iMouseStorageSize + 1;
   }
   return iSize;
}

static void hb_gt_dos_mouse_SaveState( PHB_GT pGT, void * pBuffer )
{
   if( s_fMousePresent )
   {
      union REGS regs;
      struct SREGS sregs;

      memset( &sregs, 0, sizeof( sregs ) );

#if defined( __DJGPP__ )
{
      _go32_dpmi_seginfo info;

      info.size = ( s_iMouseStorageSize + 15 ) >> 4;
      _go32_dpmi_allocate_dos_memory( &info );

      regs.HB_XREGS.ax = 0x16;
      regs.HB_XREGS.bx = s_iMouseStorageSize;
      regs.HB_XREGS.dx = 0;
      sregs.es = info.rm_segment;

      HB_DOS_INT86X( 0x33, &regs, &regs, &sregs );

      dosmemget( info.rm_segment << 4, s_iMouseStorageSize, pBuffer );
      _go32_dpmi_free_dos_memory(&info);
}
#else
      regs.HB_XREGS.ax = 0x16;
      regs.HB_XREGS.bx = s_iMouseStorageSize;
      regs.HB_XREGS.dx = FP_OFF( pBuffer );
      sregs.es = FP_SEG( pBuffer );
      HB_DOS_INT86X( 0x33, &regs, &regs, &sregs );
#endif
      ( ( HB_BYTE * ) pBuffer )[ s_iMouseStorageSize ] = HB_GTSELF_MOUSEGETCURSOR( pGT ) ? 1 : 0;
   }
}

static void hb_gt_dos_mouse_RestoreState( PHB_GT pGT, const void * pBuffer )
{
   if( s_fMousePresent )
   {
      union REGS regs;
      struct SREGS sregs;

      memset( &sregs, 0, sizeof( sregs ) );

      /*
       * Calling hb_mouse_SetCursor is necessary only for
       * synchronization of internal variable s_fMouseVisible
       * because the real mouse cursor state will be also recovered
       * by status restoring
       */
      HB_GTSELF_MOUSESETCURSOR( pGT, ( ( HB_BYTE * ) pBuffer )[ s_iMouseStorageSize ] );

#if defined( __DJGPP__ )
{
      _go32_dpmi_seginfo info;

      info.size = ( s_iMouseStorageSize + 15 ) >> 4;
      _go32_dpmi_allocate_dos_memory( &info );

      regs.HB_XREGS.ax = 0x17;
      regs.HB_XREGS.bx = s_iMouseStorageSize;
      regs.HB_XREGS.dx = 0;
      sregs.es = info.rm_segment;

      HB_DOS_INT86X( 0x33, &regs, &regs, &sregs );

      dosmemput( pBuffer, s_iMouseStorageSize, info.rm_segment << 4 );
      _go32_dpmi_free_dos_memory(&info);
}
#else
      regs.HB_XREGS.ax = 0x17;
      regs.HB_XREGS.bx = s_iMouseStorageSize;
      regs.HB_XREGS.dx = FP_OFF( pBuffer );
      sregs.es = FP_SEG( pBuffer );
      HB_DOS_INT86X( 0x33, &regs, &regs, &sregs );
#endif
   }
}
#endif

static void hb_gt_dos_mouse_SetBounds( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight )
{
   HB_SYMBOL_UNUSED( pGT );

   if( s_fMousePresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 7;
      regs.HB_XREGS.cx = iLeft << 3;
      regs.HB_XREGS.dx = iRight << 3;
      HB_DOS_INT86( 0x33, &regs, &regs );

      regs.HB_XREGS.ax = 8;
      regs.HB_XREGS.cx = iTop << 3;
      regs.HB_XREGS.dx = iBottom << 3;
      HB_DOS_INT86( 0x33, &regs, &regs );

      s_iMouseTop    = iTop;
      s_iMouseLeft   = iLeft;
      s_iMouseBottom = iBottom;
      s_iMouseRight  = iRight;
      s_fMouseBound  = HB_TRUE;
   }
}

static void hb_gt_dos_mouse_GetBounds( PHB_GT pGT, int * piTop, int * piLeft, int * piBottom, int * piRight )
{
   if( s_fMouseBound  )
   {
      *piTop    = s_iMouseTop;
      *piLeft   = s_iMouseLeft;
      *piBottom = s_iMouseBottom;
      *piRight  = s_iMouseRight;
   }
   else
   {
      *piTop = *piLeft = 0;
      HB_GTSELF_GETSIZE( pGT, piBottom, piRight );
      --(*piBottom);
      --(*piRight);
   }
}

/* *********************************************************************** */

static void hb_gt_dos_Init( PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_Init(%p,%d,%d,%d)", pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr));

   s_bBreak = HB_FALSE;

   /* Set the Ctrl+Break handler [vszakats] */

#if defined( __DJGPP__ )

   gppconio_init();
   __djgpp_hwint_flags |= 2;     /* Count Ctrl+Break instead of killing program */
   __djgpp_set_ctrl_c( 0 );      /* Disable Ctrl+C */
   __djgpp_set_sigquit_key( 0 ); /* Disable Ctrl+\ */

#elif defined( __RSX32__ )

   /* TODO */

#elif defined( __WATCOMC__ )

   signal( SIGBREAK, hb_gt_dos_CtrlBreak_Handler );
   atexit( hb_gt_dos_CtrlBrkRestore );

#elif defined( _MSC_VER )

   signal( SIGINT, hb_gt_dos_CtrlBreak_Handler );
   atexit( hb_gt_dos_CtrlBrkRestore );

#else

   ctrlbrk( hb_gt_dos_CtrlBrkHandler );
   s_iOldCtrlBreak = getcbrk();
   setcbrk( 1 );
   atexit( hb_gt_dos_CtrlBrkRestore );

#endif

   s_iScreenMode = hb_gt_dos_GetScreenMode();
#if !defined( __DJGPP__ )
   s_pScreenAddres = hb_gt_dos_ScreenAddress( pGT );
#endif
   hb_gt_dos_GetScreenSize( &s_iRows, &s_iCols );
   hb_gt_dos_GetCursorPosition( &s_iCurRow, &s_iCurCol );
   s_iCursorStyle = hb_gt_dos_GetCursorStyle();
   HB_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );
   HB_GTSELF_RESIZE( pGT, s_iRows, s_iCols );
   hb_gt_dos_GetScreenContents( pGT );
   HB_GTSELF_SETPOS( pGT, s_iCurRow, s_iCurCol );
   if( s_iCursorStyle > 0 )
      HB_GTSELF_SETCURSORSTYLE( pGT, s_iCursorStyle );
}

static void hb_gt_dos_Exit( PHB_GT pGT )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_Exit(%p)", pGT));

   HB_GTSUPER_EXIT( pGT );
}

static int hb_gt_dos_ReadKey( PHB_GT pGT, int iEventMask )
{
   int ch = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_ReadKey(%p,%d)", pGT, iEventMask));

#if defined( __DJGPP__ )
   /* Check to see if Ctrl+Break has been detected */
   if( __djgpp_cbrk_count )
   {
      __djgpp_cbrk_count = 0; /* Indicate that Ctrl+Break has been handled */
      ch = HB_BREAK_FLAG; /* Note that Ctrl+Break was pressed */
   }
#else
   /* First check for Ctrl+Break, which is handled by gt/gtdos.c,
      with the exception of the DJGPP compiler */
   if( s_bBreak )
   {
      s_bBreak = HB_FALSE; /* Indicate that Ctrl+Break has been handled */
      ch = HB_BREAK_FLAG; /* Note that Ctrl+Break was pressed */
   }
#endif
   else if( kbhit() )
   {
      /* A key code is available in the BIOS keyboard buffer, so read it */
#if defined( __DJGPP__ )
      if( iEventMask & HB_INKEY_RAW ) ch = getxkey();
      else ch = getkey();
      if( ch == 256 )
         /* Ignore Ctrl+Break, because it is being handled as soon as it
            happens (see above) rather than waiting for it to show up in
            the keyboard input queue */
         ch = -1;
#else
      /* A key code is available in the BIOS keyboard buffer */
      ch = getch();                  /* Get the key code */
      if( ch == 0 && kbhit() )
      {
         /* It was a function key lead-in code, so read the actual
            function key and then offset it by 256 */
         ch = getch() + 256;
      }
      else if( ch == 224 && kbhit() )
      {
         /* It was an extended function key lead-in code, so read
            the actual function key and then offset it by 256,
            unless extended keyboard events are allowed, in which
            case offset it by 512 */
         if( iEventMask & HB_INKEY_RAW ) ch = getch() + 512;
         else ch = getch() + 256;
      }
#endif
   }

   ch = hb_gt_dos_keyCodeTranslate( ch );

   if( ch == 0 )
      ch = HB_GTSELF_MOUSEREADKEY( pGT, iEventMask );
   else
   {
      int u = HB_GTSELF_KEYTRANS( pGT, ch );
      if( u )
         ch = HB_INKEY_NEW_UNICODE( u );
   }

   return ch;
}

static HB_BOOL hb_gt_dos_IsColor( PHB_GT pGT )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_IsColor(%p)", pGT));

   HB_SYMBOL_UNUSED( pGT );

   return s_iScreenMode != 7;
}

static HB_BOOL hb_gt_dos_GetBlink( PHB_GT pGT )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_GetBlink(%p)", pGT));

   HB_SYMBOL_UNUSED( pGT );

   return ( HB_PEEK_BYTE( 0x0040, 0x0065 ) & 0x10 ) != 0;
}

static void hb_gt_dos_SetBlink( PHB_GT pGT, HB_BOOL fBlink )
{
   union REGS regs;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_SetBlink(%p,%d)", pGT, (int) fBlink));

   HB_SYMBOL_UNUSED( pGT );

   regs.h.ah = 0x10;
   regs.h.al = 0x03;
   regs.h.bh = 0;
   regs.h.bl = fBlink ? 1 : 0;
   HB_DOS_INT86( 0x10, &regs, &regs );
}

static void hb_gt_dos_Tone( PHB_GT pGT, double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_Tone(%p,%lf,%lf)", pGT, dFrequency, dDuration));

   HB_SYMBOL_UNUSED( pGT );

   dFrequency = HB_MIN( HB_MAX( 0.0, dFrequency ), 32767.0 );

#if defined( __BORLANDC__ ) || defined( __WATCOMC__ )
   sound( ( unsigned ) dFrequency );
#elif defined( __DJGPP__ )
   sound( ( int ) dFrequency );
#endif

   /* convert Clipper (DOS) timer tick units to seconds ( x / 18.2 ) */
   hb_idleSleep( dDuration / 18.2 );

#if defined( __BORLANDC__ ) || defined( __WATCOMC__ )
   nosound();
#elif defined( __DJGPP__ )
   sound( 0 );
#endif
}

static const char * hb_gt_dos_Version( PHB_GT pGT, int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_dos_Version(%p,%d)", pGT, iType ) );

   HB_SYMBOL_UNUSED( pGT );

   if( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: DOS console";
}

/* some definitions */
#define INT_VIDEO    0x10

#if defined( __WATCOMC__ )
   #define outportb outp        /* Use correct function name */
   #define outportw outpw       /* Use correct function name */
   #define inportw  inpw        /* Use correct function name */
   #define inportb  inp         /* Use correct function name */
#elif defined( __RSX32__ )
   #define inportb( p ) 0       /* Return 0 */
   #define outportw( p, w )     /* Do nothing */
   #define outportb( p, b )     /* Do nothing */
#endif

static void vmode12x40( void )
{
   union REGS regs;
   regs.HB_XREGS.ax = 0x0001;               /* video mode 40 cols */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs );
   outportb( 0x03D4, 0x09 );         /* update cursor size / pointers */
   regs.h.al = ( inportb( 0x03D5 ) | 0x80 );
   outportb( 0x03D5, regs.h.al );
#if !defined( __DJGPP__ )
   HB_POKE_BYTE( 0x40, 0x84, 11 );   /* 11 rows number update */
#endif
}

static void vmode25x40( void )
{
   union REGS regs;
   regs.HB_XREGS.ax = 0x0001;
   HB_DOS_INT86( INT_VIDEO, &regs, &regs );
}

static void vmode28x40( void )
{
   union REGS regs;
   regs.HB_XREGS.ax = 0x0001;               /* video mode 40 cols */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs );
   regs.HB_XREGS.bx = 0;                    /* load block 0 (BL = 0) */
   regs.HB_XREGS.ax = 0x1111;               /* load 8x8 monochrome char set into RAM */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs );
}

static void vmode50x40( void )
{
   union REGS regs;
   regs.HB_XREGS.ax = 0x0001;
   HB_DOS_INT86( INT_VIDEO, &regs, &regs );
   regs.HB_XREGS.bx = 0;                    /* load block 0 (BL = 0) */
   regs.HB_XREGS.ax = 0x1112;               /* load 8x8 double dot char set into RAM */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs );
   outportw( 0x03D4, 0x060A );
}

static void vmode12x80( void )
{
   union REGS regs;
   regs.HB_XREGS.ax = 0x0003;                  /* mode in AL, if bit 7 is on, No CLS */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs );
   outportb( 0x03D4, 0x09 );            /* update cursor size / pointers */
   regs.h.al = ( inportb( 0x03D5 ) | 0x80 );
   outportb( 0x03D5, regs.h.al );
#if !defined( __DJGPP__ )
   HB_POKE_BYTE( 0x40, 0x84, 11 );      /* 11 rows number update */
#endif
}

static void vmode25x80( void )
{
   union REGS regs;
   regs.HB_XREGS.ax = 0x1202;              /* select 350 scan line mode */
   regs.h.bl = 0x30;
   HB_DOS_INT86( INT_VIDEO, &regs, &regs );
   regs.HB_XREGS.ax = 0x0083;              /* mode in AL, if higher bit is on, No CLS */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs );
   regs.HB_XREGS.bx = 0;                   /* load block 0 (BL = 0) */
   regs.HB_XREGS.ax = 0x1114;              /* load 8x14 VGA char set into RAM */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs );
}

static void vmode28x80( void )
{
   union REGS regs;
   regs.HB_XREGS.ax = 0x0003;              /* mode in AL, if higher bit is on, No CLS */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs );
   regs.HB_XREGS.bx = 0;                   /* load block 0 (BL = 0) */
   regs.HB_XREGS.ax = 0x1111;              /* load 8x8 monochrome char set into RAM */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs );
}

static void vmode43x80( void )
{
   union REGS regs;
   regs.HB_XREGS.ax = 0x1201;              /*  select 350 scan line mode */
   regs.h.bl = 0x30;
   HB_DOS_INT86( INT_VIDEO, &regs, &regs );
   regs.HB_XREGS.ax = 0x0003;              /* mode in AL, if higher bit is on, No CLS */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs );
   regs.h.bh = 0x1;                 /* bytes per character */
   regs.h.bl = 0x0;                 /* load block 0 */
   regs.HB_XREGS.ax = 0x1112;              /* load 8x8 double dot char set into RAM */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs );
   outportw( 0x03D4, 0x060A );      /* update cursor size / pointers */
#if !defined( __DJGPP__ )
   HB_POKE_BYTE( 0x40, 0x84, 42 );  /* 42 rows number update */
#endif
}

static void vmode50x80( void )
{
   union REGS regs;
   regs.HB_XREGS.ax = 0x1202;               /*  select 400 scan line mode */
   regs.h.bl = 0x30;
   HB_DOS_INT86( INT_VIDEO, &regs, &regs );
   regs.HB_XREGS.ax = 0x0003;               /* mode in AL, if bit 7 is on, No CLS */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs );
   regs.HB_XREGS.bx = 0;                    /* load block 0 (BL = 0) */
   regs.HB_XREGS.ax = 0x1112;               /* load 8x8 double dot char set into RAM */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs );
}

/***************************************************************************
 * Return the display combination: monitor + video card
 *
 * INT 10 - VIDEO - GET DISPLAY COMBINATION CODE (PS,VGA/MCGA)
 *         AX = 1A00h
 * Return: AL = 1Ah if function was supported
 *         BL = active display code (see below)
 *         BH = alternate display code
 *
 * Values for display combination code:
 *  00h    no display
 *  01h    monochrome adapter w/ monochrome display
 *  02h    CGA w/ color display
 *  03h    reserved
 *  04h    EGA w/ color display
 *  05h    EGA w/ monochrome display
 *  06h    PGA w/ color display
 *  07h    VGA w/ monochrome analog display
 *  08h    VGA w/ color analog display
 *  09h    reserved
 *  0Ah    MCGA w/ digital color display
 *  0Bh    MCGA w/ monochrome analog display
 *  0Ch    MCGA w/ color analog display
 *  FFh    unknown display type
 ****************************************************************************/

static HB_USHORT hb_gt_dos_GetDisplay( void )
{
   union REGS regs;

   regs.HB_XREGS.ax = 0x1A00;
   HB_DOS_INT86( INT_VIDEO, &regs, &regs );

   return ( regs.h.al == 0x1A ) ? regs.h.bl : 0xFF;
}

static HB_BOOL hb_gt_dos_SetMode( PHB_GT pGT, int iRows, int iCols )
{
   /* HB_GTSELF_ISCOLOR( pGT ) test for color card, we need to know if it is a VGA board...*/
   HB_BOOL bIsVGA, bIsVesa, bSuccess;

   HB_TRACE( HB_TR_DEBUG, ("hb_gt_dos_SetMode(%p,%d,%d)", pGT, iRows, iCols) );

   bIsVGA = ( hb_gt_dos_GetDisplay() == 8 );
   bIsVesa = HB_FALSE;

   /* Available modes in B&N and color screens */
   if( iCols == 40 )
   {
      if( iRows == 12 )
         vmode12x40();
      else if( iRows == 25 )
         vmode25x40();
      else if( iRows == 28 )
         vmode28x40();
      else if( iRows == 50 )
         vmode50x40();
   }

   if( bIsVGA )
   {
      if( iCols == 80)
      {
         if( iRows == 12 )
            vmode12x80();
         else if( iRows == 25 )
            vmode25x80();
         else if( iRows == 28 )
            vmode28x80();
         else if( iRows == 43 )
            vmode43x80();
         else if( iRows == 50 )
            vmode50x80();
      }

      if( iCols > 80 && bIsVesa )
      {
         /* In development process
          * return hb_gt_dos_Modevesa( nMode);
          */
      }
   }

   hb_gt_dos_GetScreenSize( &s_iRows, &s_iCols );

   /* Check for succesful */
   if( s_iRows == iRows && s_iCols == iCols )
   {
      bSuccess = HB_TRUE;
   }
   else
   {
      bSuccess = HB_FALSE;
      vmode25x80();
      hb_gt_dos_GetScreenSize( &s_iRows, &s_iCols );
   }
   s_iScreenMode = hb_gt_dos_GetScreenMode();
#if !defined( __DJGPP__ )
   s_pScreenAddres = hb_gt_dos_ScreenAddress( pGT );
#endif
   hb_gt_dos_GetCursorPosition( &s_iCurRow, &s_iCurCol );
   s_iCursorStyle = hb_gt_dos_GetCursorStyle();
   HB_GTSELF_RESIZE( pGT, s_iRows, s_iCols );
   hb_gt_dos_GetScreenContents( pGT );
   HB_GTSELF_SETPOS( pGT, s_iCurRow, s_iCurCol );
   if( s_iCursorStyle > 0 )
      HB_GTSELF_SETCURSORSTYLE( pGT, s_iCursorStyle );

   return bSuccess;
}

static HB_BOOL hb_gt_dos_PostExt( PHB_GT pGT )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_PostExt(%p)", pGT));

   hb_gt_dos_GetCursorPosition( &s_iCurRow, &s_iCurCol );
   hb_gt_dos_GetScreenContents( pGT );
   HB_GTSELF_SETPOS( pGT, s_iCurRow, s_iCurCol );

   return HB_GTSUPER_POSTEXT( pGT );
}

static HB_BOOL hb_gt_dos_Resume( PHB_GT pGT )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_Resume(%p)", pGT));

   s_iScreenMode = hb_gt_dos_GetScreenMode();
#if !defined( __DJGPP__ )
   s_pScreenAddres = hb_gt_dos_ScreenAddress( pGT );
#endif
   hb_gt_dos_GetScreenSize( &s_iRows, &s_iCols );
   hb_gt_dos_GetCursorPosition( &s_iCurRow, &s_iCurCol );
   s_iCursorStyle = hb_gt_dos_GetCursorStyle();
   HB_GTSELF_RESIZE( pGT, s_iRows, s_iCols );
   hb_gt_dos_GetScreenContents( pGT );
   HB_GTSELF_SETPOS( pGT, s_iCurRow, s_iCurCol );
   if( s_iCursorStyle > 0 )
      HB_GTSELF_SETCURSORSTYLE( pGT, s_iCursorStyle );

   return HB_GTSUPER_RESUME( pGT );
}

/* *********************************************************************** */

static void hb_gt_dos_Redraw( PHB_GT pGT, int iRow, int iCol, int iSize )
{
#if !defined( __DJGPP__ )
   HB_USHORT FAR *pScreenPtr = ( HB_USHORT FAR * ) hb_gt_dos_ScreenPtr( iRow, iCol );
#endif
   int iColor;
   HB_BYTE bAttr;
   HB_UCHAR uc;
   int iLen = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_dos_Redraw(%p,%d,%d,%d)", pGT, iRow, iCol, iSize ) );

   while( iLen < iSize )
   {
      if( !HB_GTSELF_GETSCRUC( pGT, iRow, iCol + iLen, &iColor, &bAttr, &uc, HB_TRUE ) )
         break;

#if defined( __DJGPP__TEXT )
      {
         short ch_attr = ( ( short ) iColor << 8 ) | uc;
         puttext( iCol + iLen + 1, iRow + 1, iCol + iLen  + 1, iRow + 1, &ch_attr );
      }
#elif defined( __DJGPP__ )
      ScreenPutChar( uc, iColor, iCol + iLen, iRow );
#else
      *pScreenPtr++ = ( iColor << 8 ) + uc;
#endif
      iLen++;
   }
}

static void hb_gt_dos_Refresh( PHB_GT pGT )
{
   int iRow, iCol, iStyle;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_dos_Refresh(%p)", pGT ) );

   HB_GTSUPER_REFRESH( pGT );

   HB_GTSELF_GETSCRCURSOR( pGT, &iRow, &iCol, &iStyle );
   if( iStyle != SC_NONE )
   {
      if( iRow >= 0 && iCol >= 0 && iRow < s_iRows && iCol < s_iCols )
         hb_gt_dos_SetCursorPosition( iRow, iCol );
      else
         iStyle = SC_NONE;
   }
   hb_gt_dos_SetCursorStyle( iStyle );
}

#define HB_BIOS_LSHIFT     0x01
#define HB_BIOS_RSHIFT     0x02
#define HB_BIOS_CTRL       0x04
#define HB_BIOS_ALT        0x08
#define HB_BIOS_SHIFT      ( HB_BIOS_LSHIFT | HB_BIOS_RSHIFT )
#define HB_BIOS_SCROLL     0x10
#define HB_BIOS_NUMLOCK    0x20
#define HB_BIOS_CAPSLOCK   0x40
#define HB_BIOS_INSERT     0x80

static int hb_gt_dos_getKbdState( void )
{
   int iKbdState = 0;
   HB_UCHAR ucStat;

   ucStat = HB_PEEK_BYTE( 0x0040, 0x0017 );

   if( ucStat & HB_BIOS_SHIFT    ) iKbdState |= HB_GTI_KBD_SHIFT;
   if( ucStat & HB_BIOS_CTRL     ) iKbdState |= HB_GTI_KBD_CTRL;
   if( ucStat & HB_BIOS_ALT      ) iKbdState |= HB_GTI_KBD_ALT;
   if( ucStat & HB_BIOS_SCROLL   ) iKbdState |= HB_GTI_KBD_SCROLOCK;
   if( ucStat & HB_BIOS_NUMLOCK  ) iKbdState |= HB_GTI_KBD_NUMLOCK;
   if( ucStat & HB_BIOS_CAPSLOCK ) iKbdState |= HB_GTI_KBD_CAPSLOCK;
   if( ucStat & HB_BIOS_INSERT   ) iKbdState |= HB_GTI_KBD_INSERT;

   return iKbdState;
}

static void hb_gt_dos_setKbdState( int iKbdState )
{
   HB_UCHAR ucStat = 0;

   ucStat |= ( iKbdState & HB_GTI_KBD_SHIFT    ) ? HB_BIOS_SHIFT    : 0;
   ucStat |= ( iKbdState & HB_GTI_KBD_CTRL     ) ? HB_BIOS_CTRL     : 0;
   ucStat |= ( iKbdState & HB_GTI_KBD_ALT      ) ? HB_BIOS_ALT      : 0;
   ucStat |= ( iKbdState & HB_GTI_KBD_SCROLOCK ) ? HB_BIOS_SCROLL   : 0;
   ucStat |= ( iKbdState & HB_GTI_KBD_NUMLOCK  ) ? HB_BIOS_NUMLOCK  : 0;
   ucStat |= ( iKbdState & HB_GTI_KBD_CAPSLOCK ) ? HB_BIOS_CAPSLOCK : 0;
   ucStat |= ( iKbdState & HB_GTI_KBD_INSERT   ) ? HB_BIOS_INSERT   : 0;

   HB_POKE_BYTE( 0x0040, 0x0017, ucStat );
}

static HB_BOOL hb_gt_dos_Info( PHB_GT pGT, int iType, PHB_GT_INFO pInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_dos_Info(%p,%d,%p)", pGT, iType, pInfo ) );

   switch( iType )
   {
      case HB_GTI_ISSCREENPOS:
      case HB_GTI_KBDSUPPORT:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_TRUE );
         break;

      case HB_GTI_KBDSHIFTS:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, hb_gt_dos_getKbdState() );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            hb_gt_dos_setKbdState( hb_itemGetNI( pInfo->pNewVal ) );
         break;

      default:
         return HB_GTSUPER_INFO( pGT, iType, pInfo );
   }

   return HB_TRUE;
}

/* *********************************************************************** */

static HB_BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", pFuncTable));

   pFuncTable->Init                       = hb_gt_dos_Init;
   pFuncTable->Exit                       = hb_gt_dos_Exit;
   pFuncTable->IsColor                    = hb_gt_dos_IsColor;
   pFuncTable->SetMode                    = hb_gt_dos_SetMode;
   pFuncTable->Redraw                     = hb_gt_dos_Redraw;
   pFuncTable->Refresh                    = hb_gt_dos_Refresh;
   pFuncTable->SetBlink                   = hb_gt_dos_SetBlink;
   pFuncTable->GetBlink                   = hb_gt_dos_GetBlink;
   pFuncTable->Version                    = hb_gt_dos_Version;
   pFuncTable->Resume                     = hb_gt_dos_Resume;
   pFuncTable->PostExt                    = hb_gt_dos_PostExt;
   pFuncTable->Tone                       = hb_gt_dos_Tone;
   pFuncTable->Info                       = hb_gt_dos_Info;

   pFuncTable->ReadKey                    = hb_gt_dos_ReadKey;

   pFuncTable->MouseInit                  = hb_gt_dos_mouse_Init;
   pFuncTable->MouseExit                  = hb_gt_dos_mouse_Exit;
   pFuncTable->MouseIsPresent             = hb_gt_dos_mouse_IsPresent;
   pFuncTable->MouseShow                  = hb_gt_dos_mouse_Show;
   pFuncTable->MouseHide                  = hb_gt_dos_mouse_Hide;
   pFuncTable->MouseGetPos                = hb_gt_dos_mouse_GetPos;
   pFuncTable->MouseSetPos                = hb_gt_dos_mouse_SetPos;
   pFuncTable->MouseSetBounds             = hb_gt_dos_mouse_SetBounds;
   pFuncTable->MouseGetBounds             = hb_gt_dos_mouse_GetBounds;
   pFuncTable->MouseCountButton           = hb_gt_dos_mouse_CountButton;
   pFuncTable->MouseButtonState           = hb_gt_dos_mouse_ButtonState;
   pFuncTable->MouseButtonPressed         = hb_gt_dos_mouse_ButtonPressed;
   pFuncTable->MouseButtonReleased        = hb_gt_dos_mouse_ButtonReleased;
#ifdef HB_MOUSE_SAVE
   pFuncTable->MouseStorageSize           = hb_gt_dos_mouse_StorageSize;
   pFuncTable->MouseSaveState             = hb_gt_dos_mouse_SaveState;
   pFuncTable->MouseRestoreState          = hb_gt_dos_mouse_RestoreState;
#endif

   return HB_TRUE;
}

/* *********************************************************************** */

#include "hbgtreg.h"

/* *********************************************************************** */
