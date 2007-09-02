/*
 * $Id$
 */

/*
* xHarbour Project source code:
* Allegro based virtual gt with graphic extensions.
*
* Copyright 2004 Mauricio Abre <maurifull@datafull.com>
* www - http://www.xharbour.org
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

#define HB_GT_NAME      ALLEG

#include "ssf.h"

#include "hbgtcore.h"
#include "hbinit.h"
#include "hbvm.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h"

#include "inkey.ch"
#include "hbgfxdef.ch"

static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER (&SuperTable)
static BOOL s_fInit;
static BOOL s_fMakeInit;
static BOOL s_fGtError; 

static int s_iMsButtons, s_iMSBoundTop, s_iMSBoundLeft, s_iMSBoundBottom, s_iMSBoundRight;
static int s_iMSX, s_iMSY;
static BYTE s_byMSButtons;
static int s_iScrWidth = 80, s_iScrHeight = 25;
static int s_iGFXWidth = 0, s_iGFXHeight = 0;
static int s_iGFXUpdTop, s_iGFXUpdLeft, s_iGFXUpdBottom, s_iGFXUpdRight;
static int s_iCTop, s_iCLeft, s_iCBottom, s_iCRight;
static int s_iCurCol, s_iCurRow;
static int s_iCursorStyle;
static int s_pClr[16];
static BYTE s_byFontSize = 16, s_byFontWidth = 8;
static AL_BITMAP * bmp = NULL;

/*
 * I'm not sure of removing these (yet)
 * (they used to be static vars to center gt in hw screen, but now the
 * font size is set based on screen size, so gtAlleg will use about all screen)
 *
 * NOTE: This is only a Linux fb & DOS issue, where we don't have windows
 */
#define s_iHBorder 0
#define s_iVBorder 0

typedef struct {
   int al_key;
   int xhb_key;
} gtAllegKey;

#define GT_KEY_TABLE_SIZE 49

static const gtAllegKey sKeyTable[GT_KEY_TABLE_SIZE] = {
   {AL_KEY_ESC,    K_ESC},
   {AL_KEY_INSERT, K_INS},
   {AL_KEY_HOME,   K_HOME},
   {AL_KEY_PGUP,   K_PGUP},
   {AL_KEY_PGDN,   K_PGDN},
   {AL_KEY_END,    K_END},
   {AL_KEY_DEL,    K_DEL},
   {AL_KEY_UP,     K_UP},
   {AL_KEY_DOWN,   K_DOWN},
   {AL_KEY_LEFT,   K_LEFT},
   {AL_KEY_RIGHT,  K_RIGHT},
   {AL_KEY_A,      K_ALT_A},
   {AL_KEY_B,      K_ALT_B},
   {AL_KEY_C,      K_ALT_C},
   {AL_KEY_D,      K_ALT_D},
   {AL_KEY_E,      K_ALT_E},
   {AL_KEY_F,      K_ALT_F},
   {AL_KEY_G,      K_ALT_G},
   {AL_KEY_H,      K_ALT_H},
   {AL_KEY_I,      K_ALT_I},
   {AL_KEY_J,      K_ALT_J},
   {AL_KEY_K,      K_ALT_K},
   {AL_KEY_L,      K_ALT_L},
   {AL_KEY_M,      K_ALT_M},
   {AL_KEY_N,      K_ALT_N},
   {AL_KEY_O,      K_ALT_O},
   {AL_KEY_P,      K_ALT_P},
   {AL_KEY_Q,      K_ALT_Q},
   {AL_KEY_R,      K_ALT_R},
   {AL_KEY_S,      K_ALT_S},
   {AL_KEY_T,      K_ALT_T},
   {AL_KEY_U,      K_ALT_U},
   {AL_KEY_V,      K_ALT_V},
   {AL_KEY_W,      K_ALT_W},
   {AL_KEY_X,      K_ALT_X},
   {AL_KEY_Y,      K_ALT_Y},
   {AL_KEY_Z,      K_ALT_Z},
   {AL_KEY_F1,     K_F1},
   {AL_KEY_F2,     K_F2},
   {AL_KEY_F3,     K_F3},
   {AL_KEY_F4,     K_F4},
   {AL_KEY_F5,     K_F5},
   {AL_KEY_F6,     K_F6},
   {AL_KEY_F7,     K_F7},
   {AL_KEY_F8,     K_F8},
   {AL_KEY_F9,     K_F9},
   {AL_KEY_F10,    K_F10},
   {AL_KEY_F11,    K_F11},
   {AL_KEY_F12,    K_F12}
};

#define GT_CTRL_TABLE_SIZE 11

static const gtAllegKey sCtrlTable[GT_CTRL_TABLE_SIZE] = {
   {AL_KEY_LEFT,   K_CTRL_LEFT},
   {AL_KEY_RIGHT,  K_CTRL_RIGHT},
   {AL_KEY_UP,     K_CTRL_UP},
   {AL_KEY_DOWN,   K_CTRL_DOWN},
   {AL_KEY_QUOTE,  K_CTRL_PRTSCR},
   {AL_KEY_INSERT, K_CTRL_INS},
   {AL_KEY_DEL,    K_CTRL_DEL},
   {AL_KEY_HOME,   K_CTRL_HOME},
   {AL_KEY_END,    K_CTRL_END},
   {AL_KEY_PGUP,   K_CTRL_PGUP},
   {AL_KEY_PGDN,   K_CTRL_PGDN}
};

#define GT_UPD_GFXRECT(t,l,b,r)  do {if (t<s_iGFXUpdTop) s_iGFXUpdTop=t; if (l<s_iGFXUpdLeft) s_iGFXUpdLeft=l; if (b>s_iGFXUpdBottom) s_iGFXUpdBottom=b; if (r>s_iGFXUpdRight) s_iGFXUpdRight=r;} while(0)
#define GT_SCREENINIT()          do {if (!s_fInit) hb_gt_alleg_InitializeScreen(s_iScrHeight, s_iScrWidth, TRUE);} while(0)
#define MK_GT8BCOLOR(n)          ((n & 0xFF) / 16 | (n & 0xFF00) / 256)


static void hb_gt_alleg_Error( char * szMsg )
{
   s_fGtError = TRUE;
   hb_errInternal( 9997, "%s: %s", szMsg, allegro_error );
}

static BOOL hb_gt_alleg_CursorRect( int iRow, int iCol, int iStyle,
                                    int * piTop, int * piLeft,
                                    int * piBottom, int * piRight )
{
   *piLeft = s_iHBorder + iCol * s_byFontWidth;
   *piRight = *piLeft + s_byFontWidth - 1;
   *piTop = s_iVBorder + iRow * s_byFontSize;
   *piBottom = *piTop;

   switch( iStyle )
   {
      case SC_NORMAL:
         *piBottom += s_byFontSize - 1;
         *piTop = *piBottom - 1;
         break;

      case SC_INSERT:
         *piBottom += s_byFontSize - 1;
         *piTop = *piBottom - ( s_byFontSize / 2 ) + 1;
         break;

      case SC_UNDEF:
      case SC_SPECIAL1:
         *piBottom += s_byFontSize - 1;
         break;

      case SC_SPECIAL2:
         *piBottom += ( s_byFontSize / 2 ) - 1;
         break;

      default:
         return FALSE;
   }

   return TRUE;
}

static void hb_gt_alleg_DoCursor( int iRow, int iCol, int iStyle )
{
   int iTop, iLeft, iBottom, iRight;

   /* Hide the previous cursor */
   if( s_iCursorStyle != SC_NONE &&
       hb_gt_alleg_CursorRect( s_iCurRow, s_iCurCol, s_iCursorStyle,
                               &iTop, &iLeft, &iBottom, &iRight ) )
   {
      al_scare_mouse_area( iLeft, iTop, iRight, iBottom );
      al_blit( bmp, al_screen, iLeft, iTop, iLeft, iTop, iRight - iLeft + 1, iBottom - iTop + 1 );
      al_unscare_mouse();
   }
   /* Show the new one */
   if( iStyle != SC_NONE &&
       hb_gt_alleg_CursorRect( iRow, iCol, iStyle,
                               &iTop, &iLeft, &iBottom, &iRight ) )
   {
      al_drawing_mode( DRAW_MODE_XOR, NULL, 0, 0 );
      al_scare_mouse_area( iLeft, iTop, iRight, iBottom );
      al_draw_rect_fill( al_screen, iLeft, iTop, iRight, iBottom, s_pClr[7] );
      al_unscare_mouse();
      al_drawing_mode( DRAW_MODE_SOLID, NULL, 0, 0 );
   }
   s_iCurRow = iRow;
   s_iCurCol = iCol;
   s_iCursorStyle = iStyle;
}

static void hb_gt_alleg_ScreenUpdate( void )
{
   int iRow, iCol, iStyle;
   BOOL fPix, fCursor;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_alleg_ScreenUpdate()"));

   hb_gt_GetScrCursor( &iRow, &iCol, &iStyle );
   fPix = s_iGFXUpdTop <= s_iGFXUpdBottom && s_iGFXUpdLeft <= s_iGFXUpdRight;
   fCursor = s_iCurRow != iRow || s_iCurCol != iCol || s_iCursorStyle != iStyle;

   if( fPix || fCursor )
   {
      al_acquire_screen();

      if( fPix )
      {
         al_scare_mouse_area( s_iGFXUpdLeft, s_iGFXUpdTop, s_iGFXUpdRight, s_iGFXUpdBottom );
         al_blit( bmp, al_screen, s_iGFXUpdLeft, s_iGFXUpdTop, s_iGFXUpdLeft, s_iGFXUpdTop, s_iGFXUpdRight - s_iGFXUpdLeft + 1, s_iGFXUpdBottom - s_iGFXUpdTop + 1 );
         al_unscare_mouse();
      }
      if( fCursor )
      {
         hb_gt_alleg_DoCursor( iRow, iCol, iStyle );
      }
      al_release_screen();

      s_iGFXUpdTop = s_iScrHeight * s_byFontSize;
      s_iGFXUpdLeft = s_iScrWidth * s_byFontWidth;
      s_iGFXUpdBottom = 0;
      s_iGFXUpdRight = 0;
   }
}

static BOOL hb_gt_alleg_InitializeScreen( int iRows, int iCols, BOOL lClearInit )
{
   PHB_FNAME pFileName;
   int iRet = 1, iWidth, iHeight;  /* Don't remove iRet, ixFP and iyFP initializers! */
   short ixFP = 0, iyFP = 0;
   BOOL lMode = FALSE, lPrev = s_fInit;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_alleg_InitializeScreen(%d, %d, %d)", iRows, iCols, (int) lClearInit ));

   if( s_fGtError )
   {
      return FALSE;
   }

   if ( lPrev )
   {
      al_destroy_bitmap(bmp);
      bmp = NULL;
   }

   if ( s_iGFXWidth != 0 && s_iGFXHeight != 0 )
   {
      iWidth = (int) s_iGFXWidth;
      iHeight = (int) s_iGFXHeight;
   }
   else
   {
      iWidth = s_byFontWidth * iCols;
      iHeight = s_byFontSize * iRows;
   }

   if ( iRows > 11 && iCols > 23 && iRows < 129 && iCols < 257 )
   {
#if defined(AL_GFX_XWINDOWS)
      HB_TRACE(HB_TR_DEBUG, ("trying X DGA2 mode"));
      iRet = al_set_gfx_mode( AL_GFX_XDGA2, iWidth, iHeight, 0, 0 );
      if ( iRet != 0 )
      {
         HB_TRACE(HB_TR_DEBUG, ("trying X DGA mode"));
         iRet = al_set_gfx_mode( AL_GFX_XDGA, iWidth, iHeight, 0, 0 );
      }
      if ( iRet != 0 )
      {
         HB_TRACE(HB_TR_DEBUG, ("trying X Windows mode"));
         iRet = al_set_gfx_mode( AL_GFX_XWINDOWS, iWidth, iHeight, 0, 0 );
      }
#endif
#if defined (ALLEGRO_UNIX) || defined(ALLEGRO_LINUX) || defined(ALLEGRO_DOS)
      if ( iRet != 0 )
      {
         HB_TRACE(HB_TR_DEBUG, ("trying VBE/AF mode"));
         iRet = al_set_gfx_mode( AL_GFX_VBEAF, iWidth, iHeight, 0, 0 );
      }
#endif
#if (defined(ALLEGRO_UNIX) || defined(ALLEGRO_LINUX)) && defined(AL_GFX_FBCON)
      if ( iRet != 0 )
      {
         HB_TRACE(HB_TR_DEBUG, ("trying fb console mode"));
         iRet = al_set_gfx_mode( AL_GFX_FBCON, iWidth, iHeight, 0, 0 );
      }
#endif
      /* Trying safe (slower) modes */
      /* Try a windowed mode first */
      if ( iRet != 0 )
      {
         HB_TRACE(HB_TR_DEBUG, ("trying autodetect windowed mode"));
         iRet = al_set_gfx_mode( AL_GFX_AUTODETECT_WINDOWED, iWidth, iHeight, 0, 0 );
      }
#ifdef ALLEGRO_WINDOWS
      /* GDI is slower, but it is more likely to bring a windowed mode than DirectX */
      if ( iRet != 0 )
      {
         HB_TRACE(HB_TR_DEBUG, ("trying GDI windowed mode"));
         iRet = al_set_gfx_mode( AL_GFX_GDI, iWidth, iHeight, 0, 0 );
      }
      if ( iRet != 0 )
      {
         HB_TRACE(HB_TR_DEBUG, ("trying DirectX windowed mode"));
         iRet = al_set_gfx_mode( AL_GFX_DIRECTX_WIN, iWidth, iHeight, 0, 0 );
      }
#endif
      if ( iRet != 0 )
      {
         HB_TRACE(HB_TR_DEBUG, ("trying autodetect console mode"));
         iRet = al_set_gfx_mode( AL_GFX_AUTODETECT, iWidth, iHeight, 0, 0 );
      }
      if ( iRet != 0 )
      {
      /* If that fails (ie, plain DOS or Linux VESA Framebuffer)
         ensure to get any available gfx mode */
         HB_TRACE(HB_TR_DEBUG, ("trying safe mode"));
         iRet = al_set_gfx_mode(AL_GFX_SAFE, iWidth, iHeight, 0, 0 );
      }
      if ( iRet != 0 )  /* Doh! */
      {
         if ( lPrev )
         {
            iCols = s_iScrWidth;
            iRows = s_iScrHeight;
         }
         else
         {
            hb_gt_alleg_Error( "Could not switch to graphic mode" );
         }
      }
      else
      {
         lMode = TRUE;
      }

      pFileName = hb_fsFNameSplit(hb_cmdargARGV()[0]);
      al_set_window_title(pFileName->szName);
      hb_xfree( pFileName );

      if ( !lPrev )
      {
         al_install_timer();
         al_install_keyboard();
         s_iMsButtons = al_install_mouse();
      }

      s_fInit = TRUE;
      s_iMSBoundLeft = 0;
      s_iMSBoundTop = 0;
      s_iMSBoundRight = AL_SCREEN_W - 1;
      s_iMSBoundBottom = AL_SCREEN_H - 1;
      s_byMSButtons = (BYTE) al_mouse_b;
      al_show_mouse(al_screen);
      s_iScrWidth = iCols;
      s_iScrHeight = iRows;

      /* WAS: Center console in screen if we got a larger resolution than requested
       * NOW: Calculate proper font size
       * eg: Linux vesafb (doesn't support mode switching)
       *     or for DOS, we'll mostly request unavailable resolutions
       */
      if ( AL_SCREEN_W != s_byFontWidth * s_iScrWidth )
      {
         ixFP = (BYTE) (AL_SCREEN_W / s_iScrWidth) * 2;
      }

      if ( AL_SCREEN_H != s_byFontSize * s_iScrHeight )
      {
         iyFP = (BYTE) (AL_SCREEN_H / s_iScrHeight);
         if ( iyFP % 2 == 1 )
         {
            iyFP--;
         }
      }

      if ( ixFP || iyFP )
      {
         if ( !ixFP )
         {
            ixFP = iyFP;
         }
         if ( !iyFP )
         {
            iyFP = ixFP;
         }
         s_byFontSize = ( ixFP < iyFP ? ixFP : iyFP );
         s_byFontWidth = s_byFontSize / 2;
      }

      s_iMSX = al_mouse_x / s_byFontWidth;
      s_iMSY = al_mouse_y / s_byFontSize;
      s_iGFXUpdTop = s_iScrHeight;
      s_iGFXUpdLeft = s_iScrWidth;
      s_iGFXUpdBottom = 0;
      s_iGFXUpdRight = 0;
      s_iCTop = 0;
      s_iCLeft = 0;
      s_iCBottom = AL_SCREEN_H - 1;
      s_iCRight = AL_SCREEN_W - 1;
      s_iCurCol = 0;
      s_iCurRow = 0;
      s_iCursorStyle = SC_NONE;
      ssfSetFontSize(ssfDefaultFont, s_byFontSize);
      s_pClr[ 0] = al_make_color(0x00, 0x00, 0x00);  /* black */
      s_pClr[ 1] = al_make_color(0x00, 0x00, 0xAA);  /* blue */
      s_pClr[ 2] = al_make_color(0x00, 0xAA, 0x00);  /* green */
      s_pClr[ 3] = al_make_color(0x00, 0xAA, 0xAA);  /* cyan */
      s_pClr[ 4] = al_make_color(0xAA, 0x00, 0x00);  /* red */
      s_pClr[ 5] = al_make_color(0xAA, 0x00, 0xAA);  /* magenta */
      s_pClr[ 6] = al_make_color(0xAA, 0x55, 0x00);  /* brown */
      s_pClr[ 7] = al_make_color(0xAA, 0xAA, 0xAA);  /* white */
      s_pClr[ 8] = al_make_color(0x55, 0x55, 0x55);  /* gray */
      s_pClr[ 9] = al_make_color(0x55, 0x55, 0xFF);  /* bright blue */
      s_pClr[10] = al_make_color(0x55, 0xFF, 0x55);  /* bright green */
      s_pClr[11] = al_make_color(0x55, 0xFF, 0xFF);  /* bright cyan */
      s_pClr[12] = al_make_color(0xFF, 0x55, 0x55);  /* bright red */
      s_pClr[13] = al_make_color(0xFF, 0x55, 0xFF);  /* bright magenta */
      s_pClr[14] = al_make_color(0xFF, 0xFF, 0x55);  /* yellow */
      s_pClr[15] = al_make_color(0xFF, 0xFF, 0xFF);  /* bright white */

      bmp = al_create_system_bitmap(AL_SCREEN_W, AL_SCREEN_H);
      if ( bmp == NULL )
      {
         bmp = al_create_bitmap(AL_SCREEN_W, AL_SCREEN_H);
         if ( bmp == NULL )
         {
            hb_gt_alleg_Error( "Could not allocate double buffer bitmap" );
         }
      }

      if ( !lClearInit )
      {
         BYTE bColor = s_pClr[ ( hb_gt_GetClearColor() >> 4 ) & 0x0f ];
         al_clear_to_color( bmp, bColor );
         al_clear_to_color( al_screen, bColor );
      }

      HB_GTSUPER_RESIZE( s_iScrHeight, s_iScrWidth );
      hb_gt_ExposeArea( 0, 0, s_iScrHeight, s_iScrWidth );
      hb_gt_Refresh();
   }

   s_iGFXWidth = 0;
   s_iGFXHeight = 0;

   return lMode;
}

static void hb_gt_alleg_Init( FHANDLE hFilenoStdin, FHANDLE hFilenoStdout, FHANDLE hFilenoStderr )
{
   int iRet;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_alleg_Init(%p,%p,%p)", hFilenoStdin, hFilenoStdout, hFilenoStderr));

   s_iCursorStyle = SC_NONE;
   s_fMakeInit = s_fGtError = FALSE;

   if( allegro_init() != 0 )
   {
      hb_gt_alleg_Error( "Screen driver initialization failure" );
   }

   iRet = al_desktop_color_depth();

   if ( iRet > 0 )
   {
      al_set_color_depth( iRet );
   }

   HB_GTSUPER_INIT( hFilenoStdin, hFilenoStdout, hFilenoStderr );
   HB_GTSUPER_RESIZE( s_iScrHeight, s_iScrWidth );
}

static void hb_gt_alleg_Exit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_alleg_Exit()"));

   HB_GTSUPER_EXIT();

   if ( bmp )
   {
      al_destroy_bitmap( bmp );
      bmp = NULL;
   }
}

static char * hb_gt_alleg_Version( int iType )
{
   if ( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: Multiplatform Allegro graphics console";
}

static BOOL hb_gt_alleg_SetMode( int iRows, int iCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_alleg_SetMode(%d, %d)", iRows, iCols));

   return hb_gt_alleg_InitializeScreen( iRows, iCols, TRUE );
}

static int hb_gt_alleg_ReadKey( int iEventMask )
{
   int nKey = 0;
   int i, iMSX, iMSY;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_alleg_ReadKey(%d)", iEventMask));

   GT_SCREENINIT();

   if ( al_mouse_needs_poll() )
   {
      al_poll_mouse();
   }

   iMSX = al_mouse_x / s_byFontWidth;
   iMSY = al_mouse_y / s_byFontSize;
   if ( iMSX != s_iMSX || iMSY != s_iMSY )
   {
      s_iMSX = iMSX;
      s_iMSY = iMSY;
      if ( iEventMask & INKEY_MOVE )
      {
         nKey = K_MOUSEMOVE;
      }
   }

   if ( ( nKey == 0 ) && ( (BYTE) al_mouse_b != s_byMSButtons ) )
   {
      if ( ( al_mouse_b & 1 ) != ( s_byMSButtons & 1 ) )
      {
         if ( al_mouse_b & 1 )
         {
            if ( iEventMask & INKEY_LDOWN )
            {
               nKey = K_LBUTTONDOWN;
            }
         }
         else
         {
            if ( iEventMask & INKEY_LUP )
            {
               nKey = K_LBUTTONUP;
            }
         }
      } else if ( ( al_mouse_b & 2 ) != ( s_byMSButtons & 2 ) )
      {
         if ( al_mouse_b & 2 )
         {
            if ( iEventMask & INKEY_RDOWN )
            {
               nKey = K_RBUTTONDOWN;
            }
         }
         else
         {
            if ( iEventMask & INKEY_RUP )
            {
               nKey = K_RBUTTONUP;
            }
         }
      } /* else if ( ( al_mouse_b & 4 ) != ( s_byMSButtons & 4 ) )
      {
         if ( al_mouse_b & 4 )
         {
            if ( iEventMask & INKEY_MDOWN )
            {
                  nKey = K_MBUTTONDOWN;
            }
         } else
         {
            if ( iEventMask & INKEY_MUP )
            {
                  nKey = K_MBUTTONUP;
            }
         }
      } */
      /* We need to define INKEY_M* & K_MBUTTON* in inkey.ch ! */
      s_byMSButtons = (BYTE) al_mouse_b;
   }

   if ( ( nKey == 0 ) && ( iEventMask & INKEY_KEYBOARD ) )
   {
      if ( al_keyboard_needs_poll() )
      {
         al_poll_keyboard();
      }

      if ( al_key_pressed() )
      {
         nKey = al_read_key();
      }

#ifdef DEBUG
      if (nKey != 0)
      {
         /* Good standard debuging... */
         printf("gtAlleg: Scancode: %d (0x%0x) ascii: %d (0x%0x) raw: %d (0x%0x)\n", nKey>>8, nKey>>8, nKey&0xff, nKey&0xff, nKey, nKey);
      }
#endif

      if ( ( ( nKey & 255 ) == 2 || ( nKey & 255 ) == 3 )  && ( nKey >> 8 ) > 31 )  /* K_CTRL_ + navigation key */
      {
         for ( i = 0; i < GT_CTRL_TABLE_SIZE; i++ )
         {
            if ( ( nKey >> 8 ) == sCtrlTable[i].al_key )
            {
               nKey = sCtrlTable[i].xhb_key;
               break;
            }
         }
      }
      else if ( ( nKey != 0 ) && ( ( nKey & 255 ) < 32 ) && ( ( nKey & 255 ) == ( nKey >> 8 ) ) )  /* K_CTRL_A .. Z */
      {
#ifdef HB_NEW_KCTRL
        nKey = 512 + ( nKey & 255 );
#else
        nKey = nKey & 255;
#endif
      }
      else if ( ( ( ( nKey & 255 ) == 1 ) || ( ( nKey & 255 ) == 4 ) ) && ( ( ( nKey >> 8 ) >= AL_KEY_F1 ) && ( ( nKey >> 8 ) <= AL_KEY_F12 ) ) )  /* K_SH_F1 .. F12, K_ALT_F1..F12 */
      {
         if ( ( nKey & 255 ) == 1 )
         {
            int iFKeys[12] = {K_SH_F1, K_SH_F2, K_SH_F2, K_SH_F4, K_SH_F5, K_SH_F6,
                              K_SH_F7, K_SH_F8, K_SH_F9, K_SH_F10, K_SH_F11, K_SH_F12};
            nKey   = iFKeys[( nKey >> 8 ) - AL_KEY_F1];
         }
         else
         {
            int iFKeys[12] = {K_ALT_F1, K_ALT_F2, K_ALT_F2, K_ALT_F4, K_ALT_F5, K_ALT_F6,
                              K_ALT_F7, K_ALT_F8, K_ALT_F9, K_ALT_F10, K_ALT_F11, K_ALT_F12};
            nKey   = iFKeys[( nKey >> 8 ) - AL_KEY_F1];
         }
      }
      else if ( nKey & 255 )
      {
         nKey = nKey & 255;
      }
      else if ( nKey != 0 )
      {
         for ( i = 0; i < GT_KEY_TABLE_SIZE; i++ )
         {
            if ( ( nKey >> 8 ) == sKeyTable[i].al_key )
            {
               nKey = sKeyTable[i].xhb_key;
               break;
            }
         }
      }
   }

   return nKey;
}

static BOOL hb_gt_alleg_mouse_IsPresent( void )
{
   return TRUE;
}

static void hb_gt_alleg_mouse_GetPos( int * piRow, int * piCol )
{
   GT_SCREENINIT();

   if ( al_mouse_needs_poll() )
   {
      al_poll_mouse();
   }

   *piRow = al_mouse_y / s_byFontSize;
   *piCol = al_mouse_x / s_byFontWidth;
}

static void hb_gt_alleg_mouse_SetPos( int iRow, int iCol )
{
   GT_SCREENINIT();

   al_position_mouse(iCol * s_byFontWidth, iRow * s_byFontSize);
}

static BOOL hb_gt_alleg_mouse_ButtonState( int iButton )
{
   GT_SCREENINIT();

   if ( al_mouse_needs_poll() )
   {
      al_poll_mouse();
   }

   return ( al_mouse_b & ( 1 << ( iButton - 1 ) ) ) != 0;
}

static int hb_gt_alleg_mouse_CountButton( void )
{
   GT_SCREENINIT();

   return s_iMsButtons;
}

static void hb_gt_alleg_mouse_SetBounds( int iTop, int iLeft, int iBottom, int iRight )
{
   GT_SCREENINIT();

   if ( iTop > -1 && iTop * s_byFontSize < AL_SCREEN_H )
   {
      s_iMSBoundTop = iTop * s_byFontSize;
   }

   if ( iLeft > -1 && iLeft * s_byFontWidth < AL_SCREEN_W )
   {
      s_iMSBoundLeft = iLeft * s_byFontWidth;
   }

   if ( iBottom >= iTop && iBottom * s_byFontSize < AL_SCREEN_H )
   {
      s_iMSBoundBottom = iBottom * s_byFontSize;
   }

   if ( iRight >= iLeft && iRight * s_byFontWidth < AL_SCREEN_W )
   {
      s_iMSBoundRight = iRight * s_byFontWidth;
   }

   al_set_mouse_range( s_iMSBoundLeft, s_iMSBoundTop, s_iMSBoundRight, s_iMSBoundBottom );
}

static void hb_gt_alleg_mouse_GetBounds( int *piTop, int *piLeft, int *piBottom, int *piRight )
{
   GT_SCREENINIT();

   *piTop = s_iMSBoundTop;
   *piLeft = s_iMSBoundLeft;
   *piBottom = s_iMSBoundBottom;
   *piRight = s_iMSBoundRight;
}

static BOOL hb_gt_alleg_Info( int iType, PHB_GT_INFO pInfo )
{
   int iWidth, iHeight, iValue;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_alleg_Info(%d, %p)", iType, pInfo));

   switch( iType )
   {
      case GTI_FULLSCREEN:
      case GTI_KBDSUPPORT:
      case GTI_ISGRAPHIC:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, TRUE );
         break;

      case GTI_INPUTFD:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, -1 );
         break;

      case GTI_SCREENWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, s_fInit ?
                                 AL_SCREEN_W : s_byFontWidth * s_iScrWidth );
         iWidth = hb_itemGetNI( pInfo->pNewVal );
         if( iWidth > 0 )
         {
            s_iGFXWidth = iWidth;
            /* hb_gt_alleg_InitializeScreen(s_iScrHeight, s_iScrWidth, s_fInit); */
         }
         break;

      case GTI_SCREENHEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, s_fInit ?
                                 AL_SCREEN_H : s_byFontSize * s_iScrHeight );
         iHeight = hb_itemGetNI( pInfo->pNewVal );
         if( iHeight > 0 )
         {
            s_iGFXHeight = iHeight;
            hb_gt_alleg_InitializeScreen(s_iScrHeight, s_iScrWidth, s_fInit);
         }
         break;

      case GTI_SCREENDEPTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, s_fInit ?
               al_bitmap_color_depth( al_screen ) : al_desktop_color_depth() );
         iValue = hb_itemGetNI( pInfo->pNewVal );
         if( iValue == 8 || iValue == 15 || iValue == 16 ||
             iValue == 24 || iValue == 32 )
         {
            al_set_color_depth( iValue );
            hb_gt_alleg_InitializeScreen(s_iScrHeight, s_iScrWidth, s_fInit);
         }
         break;

      case GTI_FONTSIZE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, s_byFontSize );
         iValue = hb_itemGetNI( pInfo->pNewVal );
         if ( iValue > 0 && iValue < 256 )
         {
            s_byFontSize = ( BYTE ) iValue;
            s_byFontWidth = s_byFontSize / 2;
            hb_gt_alleg_InitializeScreen(s_iScrHeight, s_iScrWidth, s_fInit);
         }
         break;

      case GTI_FONTWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, s_byFontWidth );
         break;

      case GTI_DESKTOPWIDTH:
         al_get_desktop_resolution( &iWidth, &iHeight );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, iWidth );
         break;

      case GTI_DESKTOPHEIGHT:
         al_get_desktop_resolution( &iWidth, &iHeight );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, iHeight );
         break;

      case GTI_DESKTOPDEPTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, al_desktop_color_depth() );
         break;

      case GTI_KBDSHIFTS:
         if ( al_keyboard_needs_poll() )
         {
            al_poll_keyboard();
         }
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, al_key_shifts );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            al_set_keyboard_leds( hb_itemGetNI( pInfo->pNewVal ) );
         }
         break;

      case GTI_WINTITLE:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            al_set_window_title( hb_itemGetCPtr( pInfo->pNewVal ) );
         }
         break;

      case GTI_VIEWMAXWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, s_iScrWidth );
         break;

      case GTI_VIEWMAXHEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, s_iScrHeight );
         break;

      default:
         return HB_GTSUPER_INFO( iType, pInfo );
   }

   return TRUE;
}

/* ********** Graphics API ********** */

static int hb_gt_alleg_gfx_Primitive( int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
   int iRet = 1;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_alleg_gfx_Primitive(%d, %d, %d, %d, %d, %d)", iType, iTop, iLeft, iBottom, iRight, iColor));

   GT_SCREENINIT();
   hb_gt_Refresh();

   switch (iType)
   {
      case GFX_ACQUIRESCREEN:
         al_acquire_bitmap(bmp);
         break;

      case GFX_RELEASESCREEN:
         al_release_bitmap(bmp);
         break;

      case GFX_MAKECOLOR:
         iRet = al_make_color(iTop, iLeft, iBottom);
         break;

      case GFX_CLIPTOP:
         iRet = s_iCTop;
         break;

      case GFX_CLIPLEFT:
         iRet = s_iCLeft;
         break;

      case GFX_CLIPBOTTOM:
         iRet = s_iCBottom;
         break;

      case GFX_CLIPRIGHT:
         iRet = s_iCRight;
         break;

      case GFX_SETCLIP:
         al_set_clip(bmp, iLeft, iTop, iRight, iBottom);
         s_iCTop = iTop;
         s_iCLeft = iLeft;
         s_iCBottom = iBottom;
         s_iCRight = iRight;
         break;

      case GFX_DRAWINGMODE:
         iRet = GFX_MODE_SOLID;
         break;

      case GFX_GETPIXEL:
         iRet = al_get_pixel(bmp, iLeft, iTop);
         break;

      case GFX_PUTPIXEL:
         al_acquire_bitmap(bmp);
         al_put_pixel(bmp, iLeft, iTop, iBottom);
         al_release_bitmap(bmp);
         GT_UPD_GFXRECT(iTop,iLeft,iTop,iLeft);
         break;

      case GFX_LINE:
         al_acquire_bitmap(bmp);
         if ( iLeft == iRight )
            al_draw_vline(bmp, iLeft, iTop, iBottom, iColor);
         else if ( iTop == iBottom )
            al_draw_hline(bmp, iLeft, iTop, iRight, iColor);
         else
            al_draw_line(bmp, iLeft, iTop, iRight, iBottom, iColor);
         al_release_bitmap(bmp);
         GT_UPD_GFXRECT(iTop,iLeft,iBottom,iRight);
         break;

      case GFX_RECT:
         al_acquire_bitmap(bmp);
         al_draw_rect(bmp, iLeft, iTop, iRight, iBottom, iColor);
         al_release_bitmap(bmp);
         GT_UPD_GFXRECT(iTop,iLeft,iBottom,iRight);
         break;

      case GFX_FILLEDRECT:
         al_acquire_bitmap(bmp);
         al_draw_rect_fill(bmp, iLeft, iTop, iRight, iBottom, iColor);
         al_release_bitmap(bmp);
         GT_UPD_GFXRECT(iTop,iLeft,iBottom,iRight);
         break;

      case GFX_CIRCLE:
         al_acquire_bitmap(bmp);
         al_draw_circle(bmp, iLeft, iTop, iBottom, iRight);
         al_release_bitmap(bmp);
         GT_UPD_GFXRECT(iTop-iBottom,iLeft-iBottom,iTop+iBottom,iLeft+iBottom);
         break;

      case GFX_FILLEDCIRCLE:
         al_acquire_bitmap(bmp);
         al_draw_circle_fill(bmp, iLeft, iTop, iBottom, iRight);
         al_release_bitmap(bmp);
         GT_UPD_GFXRECT(iTop-iBottom,iLeft-iBottom,iTop+iBottom,iLeft+iBottom);
         break;

      case GFX_ELLIPSE:
         al_acquire_bitmap(bmp);
         al_draw_ellipse(bmp, iLeft, iTop, iRight, iBottom, iColor);
         al_release_bitmap(bmp);
         GT_UPD_GFXRECT(iTop-iBottom,iLeft-iRight,iTop+iBottom,iLeft+iRight);
         break;

      case GFX_FILLEDELLIPSE:
         al_acquire_bitmap(bmp);
         al_draw_ellipse_fill(bmp, iLeft, iTop, iRight, iBottom, iColor);
         al_release_bitmap(bmp);
         GT_UPD_GFXRECT(iTop-iBottom,iLeft-iRight,iTop+iBottom,iLeft+iRight);
         break;

      case GFX_FLOODFILL:
         al_acquire_bitmap(bmp);
         al_floodfill(bmp, iLeft, iTop, iBottom);
         al_release_bitmap(bmp);
         GT_UPD_GFXRECT(0,0,s_iScrHeight*s_byFontSize,s_iScrWidth*s_byFontWidth);
         break;

      default:
         return HB_GTSUPER_GFXPRIMITIVE( iType, iTop, iLeft, iBottom, iRight, iColor );
   }

   if ( hb_gt_DispCount() == 0 )
   {
      hb_gt_alleg_ScreenUpdate();
   }

   return iRet;
}

static void hb_gt_alleg_gfx_Text( int iTop, int iLeft, char * cBuf, int iColor, int iSize, int iWidth )
{
   int iBottom, iRight;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_alleg_gfx_Text(%d, %d, %s, %d, %d, %d)", iTop, iLeft, cBuf, iColor, iSize, iWidth));

   HB_SYMBOL_UNUSED( iWidth );

   GT_SCREENINIT();
   hb_gt_Refresh();

   if ( iSize )
   {
      ssfSetFontSize( ssfDefaultFont, (unsigned short) iSize );
   }

   iRight = iLeft + strlen(cBuf) * ( ssfDefaultFont->fsize / 2 ) - 1;
   iBottom = iTop + ssfDefaultFont->fsize - 1;

   al_acquire_bitmap(bmp);
   ssfDrawText( bmp, ssfDefaultFont, cBuf, iLeft, iTop, iColor );
   al_release_bitmap(bmp);
   GT_UPD_GFXRECT( iTop, iLeft, iBottom, iRight );

   if ( iSize )
   {
      ssfSetFontSize( ssfDefaultFont, s_byFontSize );
   }

   if ( hb_gt_DispCount() == 0 )
   {
      hb_gt_alleg_ScreenUpdate();
   }
}

/* ******** Graphics API end ******** */

static void hb_gt_alleg_Redraw( int iRow, int iCol, int iSize )
{
   BYTE bColor, bAttr;
   USHORT usChar;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_alleg_Redraw(%d, %d, %d)", iRow, iCol, iSize ) );

   if( s_fInit )
   {
      int iPosX = iCol * s_byFontWidth, iPosY = iRow * s_byFontSize;

      if( s_iCursorStyle != SC_NONE && s_iCurRow == iRow &&
          s_iCurCol >= iCol && s_iCurCol <= iCol + iSize - 1 )
      {
         s_iCursorStyle = SC_NONE;
      }
      GT_UPD_GFXRECT( iPosY, iPosX, iPosY + s_byFontSize - 1, iPosX + iSize * s_byFontWidth - 1 );

      while( iSize-- )
      {
         if( !hb_gt_GetScrChar( iRow, iCol++, &bColor, &bAttr, &usChar ) )
            break;
         al_draw_rect_fill( bmp,  iPosX, iPosY, iPosX + s_byFontWidth - 1, iPosY + s_byFontSize - 1, s_pClr[bColor >> 4] );
         ssfDrawChar( bmp, ssfDefaultFont, ( BYTE ) usChar, iPosX, iPosY, s_pClr[bColor & 0x0F] );
         iPosX += s_byFontWidth;
      }
   }
   else if( !s_fMakeInit )
   {
      BYTE bDefColor = hb_gt_GetColor();

      while( iSize-- )
      {
         if( !hb_gt_GetScrChar( iRow, iCol++, &bColor, &bAttr, &usChar ) )
            break;

         if( bColor != bDefColor || usChar != ' ' )
         {
            s_fMakeInit = TRUE;
            break;
         }
      }
   }
}

static void hb_gt_alleg_Refresh( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_alleg_Refresh()" ) );

   if( !s_fGtError )
   {
      if( s_fInit )
      {
         al_acquire_bitmap(bmp);
         HB_GTSUPER_REFRESH();
         al_release_bitmap(bmp);
         if ( hb_gt_DispCount() == 0 )
         {
            hb_gt_alleg_ScreenUpdate();
         }
      }
      else
      {
         HB_GTSUPER_REFRESH();
         if( s_fMakeInit )
         {
            s_fMakeInit = FALSE;
            GT_SCREENINIT();
         }
      }
   }
}


/* ******************************************************************* */

static BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", pFuncTable));

   pFuncTable->Init                       = hb_gt_alleg_Init;
   pFuncTable->Exit                       = hb_gt_alleg_Exit;
   pFuncTable->SetMode                    = hb_gt_alleg_SetMode;
   pFuncTable->Redraw                     = hb_gt_alleg_Redraw;
   pFuncTable->Refresh                    = hb_gt_alleg_Refresh;
   pFuncTable->Version                    = hb_gt_alleg_Version;
   pFuncTable->Info                       = hb_gt_alleg_Info;

   pFuncTable->ReadKey                    = hb_gt_alleg_ReadKey;

   pFuncTable->MouseIsPresent             = hb_gt_alleg_mouse_IsPresent;
   pFuncTable->MouseGetPos                = hb_gt_alleg_mouse_GetPos;
   pFuncTable->MouseSetPos                = hb_gt_alleg_mouse_SetPos;
   pFuncTable->MouseSetBounds             = hb_gt_alleg_mouse_SetBounds;
   pFuncTable->MouseGetBounds             = hb_gt_alleg_mouse_GetBounds;
   pFuncTable->MouseCountButton           = hb_gt_alleg_mouse_CountButton;
   pFuncTable->MouseButtonState           = hb_gt_alleg_mouse_ButtonState;
   pFuncTable->GfxPrimitive               = hb_gt_alleg_gfx_Primitive;
   pFuncTable->GfxText                    = hb_gt_alleg_gfx_Text;

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

/* ******************************************************************* */

/*
* this is necessary if you want to link with .so allegro libs
* or when link staticalt and your linker will force to link main()
* from allegro library not the harbour one
*/
int _mangled_main( int argc, char * argv[] )
{
   HB_TRACE(HB_TR_DEBUG, ("_mangled_main(%d, %p)", argc, argv));

   hb_cmdargInit( argc, argv );
   hb_vmInit( TRUE );
   return hb_vmQuit();
}
void * _mangled_main_address = ( void * ) _mangled_main;
