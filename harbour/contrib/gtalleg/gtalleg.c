/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Allegro based virtual GT with graphic extensions.
 *
 * Copyright 2004 Mauricio Abre <maurifull@datafull.com>
 * www - http://www.xharbour.org
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

#define HB_GT_NAME  ALLEG

#include "hbgtcore.h"
#include "hbinit.h"
#include "hbvm.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h"

#include "ssf.h"

#include "inkey.ch"
#include "hbgfxdef.ch"

static int s_GtId;
static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER   ( &SuperTable )
#define HB_GTID_PTR  ( &s_GtId )


static HB_BOOL s_fInit;
static HB_BOOL s_fMakeInit;
static HB_BOOL s_fGtError;

/* font definition */
static ssfFont        s_ssfDefaultFont;
static unsigned short s_byFontSize = 16;
static int s_byFontWidth = 8;

/* mouse parameters */
static int        s_iMsButtons;
static int        s_mouseButtonsState;
static HB_GT_RECT s_mouseBound;
static HB_GT_CORD s_mousePos;

/* screen size in characters */
static int s_iScrWidth  = 80;
static int s_iScrHeight = 25;
/* screen size in pixels */
static int s_iGFXWidth  = 0;
static int s_iGFXHeight = 0;

/* update region in pixels */
static HB_GT_RECT s_GFXUpd;

/* current CLIP region in pixels (modified by HB_GFX_SETCLIP) */
static HB_GT_RECT s_CLIP;

/* cursor position and style */
static int s_iCurCol;
static int s_iCurRow;
static int s_iCursorStyle;

/* color indexes */
static int s_pClr[ 16 ];

/* screen buffer bitmap */
static AL_BITMAP * s_bmp = NULL;


/*
 * I'm not sure of removing these (yet)
 * (they used to be static vars to center gt in hw screen, but now the
 * font size is set based on screen size, so gtAlleg will use about all screen)
 *
 * NOTE: This is only a Linux fb & DOS issue, where we don't have windows
 */
#define s_iHBorder  0
#define s_iVBorder  0


typedef struct
{
   int al_key;
   int xhb_key;
} gtAllegKey;

#define GT_KEY_TABLE_SIZE  49

static const gtAllegKey s_KeyTable[ GT_KEY_TABLE_SIZE ] =
{
   { AL_KEY_ESC,    K_ESC   },
   { AL_KEY_INSERT, K_INS   },
   { AL_KEY_HOME,   K_HOME  },
   { AL_KEY_PGUP,   K_PGUP  },
   { AL_KEY_PGDN,   K_PGDN  },
   { AL_KEY_END,    K_END   },
   { AL_KEY_DEL,    K_DEL   },
   { AL_KEY_UP,     K_UP    },
   { AL_KEY_DOWN,   K_DOWN  },
   { AL_KEY_LEFT,   K_LEFT  },
   { AL_KEY_RIGHT,  K_RIGHT },
   { AL_KEY_A,      K_ALT_A },
   { AL_KEY_B,      K_ALT_B },
   { AL_KEY_C,      K_ALT_C },
   { AL_KEY_D,      K_ALT_D },
   { AL_KEY_E,      K_ALT_E },
   { AL_KEY_F,      K_ALT_F },
   { AL_KEY_G,      K_ALT_G },
   { AL_KEY_H,      K_ALT_H },
   { AL_KEY_I,      K_ALT_I },
   { AL_KEY_J,      K_ALT_J },
   { AL_KEY_K,      K_ALT_K },
   { AL_KEY_L,      K_ALT_L },
   { AL_KEY_M,      K_ALT_M },
   { AL_KEY_N,      K_ALT_N },
   { AL_KEY_O,      K_ALT_O },
   { AL_KEY_P,      K_ALT_P },
   { AL_KEY_Q,      K_ALT_Q },
   { AL_KEY_R,      K_ALT_R },
   { AL_KEY_S,      K_ALT_S },
   { AL_KEY_T,      K_ALT_T },
   { AL_KEY_U,      K_ALT_U },
   { AL_KEY_V,      K_ALT_V },
   { AL_KEY_W,      K_ALT_W },
   { AL_KEY_X,      K_ALT_X },
   { AL_KEY_Y,      K_ALT_Y },
   { AL_KEY_Z,      K_ALT_Z },
   { AL_KEY_F1,     K_F1    },
   { AL_KEY_F2,     K_F2    },
   { AL_KEY_F3,     K_F3    },
   { AL_KEY_F4,     K_F4    },
   { AL_KEY_F5,     K_F5    },
   { AL_KEY_F6,     K_F6    },
   { AL_KEY_F7,     K_F7    },
   { AL_KEY_F8,     K_F8    },
   { AL_KEY_F9,     K_F9    },
   { AL_KEY_F10,    K_F10   },
   { AL_KEY_F11,    K_F11   },
   { AL_KEY_F12,    K_F12   }
};

#define GT_CTRL_TABLE_SIZE  11

static const gtAllegKey s_CtrlTable[ GT_CTRL_TABLE_SIZE ] =
{
   { AL_KEY_LEFT,   K_CTRL_LEFT   },
   { AL_KEY_RIGHT,  K_CTRL_RIGHT  },
   { AL_KEY_UP,     K_CTRL_UP     },
   { AL_KEY_DOWN,   K_CTRL_DOWN   },
   { AL_KEY_QUOTE,  K_CTRL_PRTSCR },
   { AL_KEY_INSERT, K_CTRL_INS    },
   { AL_KEY_DEL,    K_CTRL_DEL    },
   { AL_KEY_HOME,   K_CTRL_HOME   },
   { AL_KEY_END,    K_CTRL_END    },
   { AL_KEY_PGUP,   K_CTRL_PGUP   },
   { AL_KEY_PGDN,   K_CTRL_PGDN   }
};

#define GT_UPD_GFXRECT( t, l, b, r )  do { if( t < s_GFXUpd.iTop ) s_GFXUpd.iTop = t; \
                                           if( l < s_GFXUpd.iLeft ) s_GFXUpd.iLeft = l; \
                                           if( b > s_GFXUpd.iBottom ) s_GFXUpd.iBottom = b; \
                                           if( r > s_GFXUpd.iRight ) s_GFXUpd.iRight = r; \
                                      } while( 0 )
#define GT_SCREENINIT()               do { if( ! s_fInit ) \
                                              hb_gt_alleg_InitializeScreen( pGT, s_iScrHeight, s_iScrWidth, HB_TRUE ); \
                                      } while( 0 )
#define MK_GT8BCOLOR( n )             ( ( n & 0xFF ) / 16 | ( n & 0xFF00 ) / 256 )


static void hb_gt_alleg_Error( const char * szMsg )
{
   s_fGtError = HB_TRUE;
   hb_errInternal( 9997, "%s: %s", szMsg, allegro_error );
}

static HB_BOOL hb_gt_alleg_CursorRect( int iRow, int iCol, int iStyle,
                                       int * piTop, int * piLeft,
                                       int * piBottom, int * piRight )
{
   *piLeft   = s_iHBorder + iCol * s_byFontWidth;
   *piRight  = *piLeft + s_byFontWidth - 1;
   *piTop    = s_iVBorder + iRow * s_byFontSize;
   *piBottom = *piTop;

   switch( iStyle )
   {
      case SC_NORMAL:
         *piBottom += s_byFontSize - 1;
         *piTop     = *piBottom - 1;
         break;

      case SC_INSERT:
         *piBottom += s_byFontSize - 1;
         *piTop     = *piBottom - ( s_byFontSize / 2 ) + 1;
         break;

      case SC_UNDEF:
      case SC_SPECIAL1:
         *piBottom += s_byFontSize - 1;
         break;

      case SC_SPECIAL2:
         *piBottom += ( s_byFontSize / 2 ) - 1;
         break;

      default:
         return HB_FALSE;
   }

   return HB_TRUE;
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
      al_blit( s_bmp, al_screen, iLeft, iTop, iLeft, iTop, iRight - iLeft + 1, iBottom - iTop + 1 );
      al_unscare_mouse();
   }
   /* Show the new one */
   if( iStyle != SC_NONE &&
       hb_gt_alleg_CursorRect( iRow, iCol, iStyle,
                               &iTop, &iLeft, &iBottom, &iRight ) )
   {
      al_drawing_mode( DRAW_MODE_XOR, NULL, 0, 0 );
      al_scare_mouse_area( iLeft, iTop, iRight, iBottom );
      al_draw_rect_fill( al_screen, iLeft, iTop, iRight, iBottom, s_pClr[ 7 ] );
      al_unscare_mouse();
      al_drawing_mode( DRAW_MODE_SOLID, NULL, 0, 0 );
   }
   s_iCurRow      = iRow;
   s_iCurCol      = iCol;
   s_iCursorStyle = iStyle;
}

static void hb_gt_alleg_ScreenUpdate( PHB_GT pGT )
{
   int     iRow, iCol, iStyle;
   HB_BOOL fPix, fCursor;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_alleg_ScreenUpdate(%p)", pGT ) );

   HB_GTSELF_GETSCRCURSOR( pGT, &iRow, &iCol, &iStyle );
   fPix = s_GFXUpd.iTop <= s_GFXUpd.iBottom &&
          s_GFXUpd.iLeft <= s_GFXUpd.iRight;
   fCursor = s_iCurRow != iRow || s_iCurCol != iCol || s_iCursorStyle != iStyle;

   if( fPix || fCursor )
   {
      al_acquire_screen();

      if( fPix )
      {
         al_scare_mouse_area( s_GFXUpd.iLeft, s_GFXUpd.iTop,
                              s_GFXUpd.iRight, s_GFXUpd.iBottom );
         al_blit( s_bmp, al_screen, s_GFXUpd.iLeft, s_GFXUpd.iTop,
                  s_GFXUpd.iLeft, s_GFXUpd.iTop,
                  s_GFXUpd.iRight - s_GFXUpd.iLeft + 1,
                  s_GFXUpd.iBottom - s_GFXUpd.iTop + 1 );
         al_unscare_mouse();
      }
      if( fCursor )
         hb_gt_alleg_DoCursor( iRow, iCol, iStyle );

      al_release_screen();

      s_GFXUpd.iTop    = s_iScrHeight * s_byFontSize;
      s_GFXUpd.iLeft   = s_iScrWidth * s_byFontWidth;
      s_GFXUpd.iBottom = 0;
      s_GFXUpd.iRight  = 0;
   }
}

static HB_BOOL hb_gt_alleg_InitializeScreen( PHB_GT pGT, int iRows, int iCols, HB_BOOL lClearInit )
{
   PHB_FNAME pFileName;
   int       iRet  = 1, iWidth, iHeight; /* Don't remove iRet, ixFP and iyFP initializers! */
   short     ixFP  = 0, iyFP = 0;
   HB_BOOL   lMode = HB_FALSE, lPrev = s_fInit;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_alleg_InitializeScreen(%p,%d,%d,%d)", pGT, iRows, iCols, ( int ) lClearInit ) );

   if( s_fGtError )
      return HB_FALSE;

   if( lPrev )
   {
      al_destroy_bitmap( s_bmp );
      s_bmp = NULL;
   }

   if( s_iGFXWidth != 0 && s_iGFXHeight != 0 )
   {
      iWidth  = ( int ) s_iGFXWidth;
      iHeight = ( int ) s_iGFXHeight;
   }
   else
   {
      iWidth  = s_byFontWidth * iCols;
      iHeight = s_byFontSize * iRows;
   }

   if( iRows > 11 && iCols > 23 && iRows < 129 && iCols < 257 )
   {
#if defined( AL_GFX_XWINDOWS )
      HB_TRACE( HB_TR_DEBUG, ( "trying X DGA2 mode" ) );
      iRet = al_set_gfx_mode( AL_GFX_XDGA2, iWidth, iHeight, 0, 0 );
      if( iRet != 0 )
      {
         HB_TRACE( HB_TR_DEBUG, ( "trying X DGA mode" ) );
         iRet = al_set_gfx_mode( AL_GFX_XDGA, iWidth, iHeight, 0, 0 );
      }
      if( iRet != 0 )
      {
         HB_TRACE( HB_TR_DEBUG, ( "trying X Windows mode" ) );
         iRet = al_set_gfx_mode( AL_GFX_XWINDOWS, iWidth, iHeight, 0, 0 );
      }
#endif
#if defined( ALLEGRO_UNIX ) || defined( ALLEGRO_LINUX ) || defined( ALLEGRO_DOS )
      if( iRet != 0 )
      {
         HB_TRACE( HB_TR_DEBUG, ( "trying VBE/AF mode" ) );
         iRet = al_set_gfx_mode( AL_GFX_VBEAF, iWidth, iHeight, 0, 0 );
      }
#endif
#if ( defined( ALLEGRO_UNIX ) || defined( ALLEGRO_LINUX ) ) && defined( AL_GFX_FBCON )
      if( iRet != 0 )
      {
         HB_TRACE( HB_TR_DEBUG, ( "trying fb console mode" ) );
         iRet = al_set_gfx_mode( AL_GFX_FBCON, iWidth, iHeight, 0, 0 );
      }
#endif
      /* Trying safe (slower) modes */
      /* Try a windowed mode first */
      if( iRet != 0 )
      {
         HB_TRACE( HB_TR_DEBUG, ( "trying autodetect windowed mode" ) );
         iRet = al_set_gfx_mode( AL_GFX_AUTODETECT_WINDOWED, iWidth, iHeight, 0, 0 );
      }
#ifdef ALLEGRO_WINDOWS
      /* GDI is slower, but it is more likely to bring a windowed mode than DirectX */
      if( iRet != 0 )
      {
         HB_TRACE( HB_TR_DEBUG, ( "trying GDI windowed mode" ) );
         iRet = al_set_gfx_mode( AL_GFX_GDI, iWidth, iHeight, 0, 0 );
      }
      if( iRet != 0 )
      {
         HB_TRACE( HB_TR_DEBUG, ( "trying DirectX windowed mode" ) );
         iRet = al_set_gfx_mode( AL_GFX_DIRECTX_WIN, iWidth, iHeight, 0, 0 );
      }
#endif
      if( iRet != 0 )
      {
         HB_TRACE( HB_TR_DEBUG, ( "trying autodetect console mode" ) );
         iRet = al_set_gfx_mode( AL_GFX_AUTODETECT, iWidth, iHeight, 0, 0 );
      }
      if( iRet != 0 )
      {
         /* If that fails (ie, plain DOS or Linux VESA Framebuffer)
            ensure to get any available gfx mode */
         HB_TRACE( HB_TR_DEBUG, ( "trying safe mode" ) );
         iRet = al_set_gfx_mode( AL_GFX_SAFE, iWidth, iHeight, 0, 0 );
      }
      if( iRet != 0 )  /* Doh! */
      {
         if( lPrev )
         {
            iCols = s_iScrWidth;
            iRows = s_iScrHeight;
         }
         else
            hb_gt_alleg_Error( "Could not switch to graphic mode" );
      }
      else
         lMode = HB_TRUE;

      pFileName = hb_fsFNameSplit( hb_cmdargARGV()[ 0 ] );
      al_set_window_title( ( char * ) pFileName->szName );
      hb_xfree( pFileName );

      if( ! lPrev )
      {
         al_install_timer();
         al_install_keyboard();
         s_iMsButtons = al_install_mouse();
      }

      s_fInit = HB_TRUE;
      s_mouseBound.iLeft   = 0;
      s_mouseBound.iTop    = 0;
      s_mouseBound.iRight  = AL_SCREEN_W - 1;
      s_mouseBound.iBottom = AL_SCREEN_H - 1;
      s_mouseButtonsState  = al_mouse_b;
      al_show_mouse( al_screen );
      s_iScrWidth  = iCols;
      s_iScrHeight = iRows;

      /* WAS: Center console in screen if we got a larger resolution than requested
       * NOW: Calculate proper font size
       * eg: Linux vesafb (doesn't support mode switching)
       *     or for DOS, we'll mostly request unavailable resolutions
       */
      if( AL_SCREEN_W != s_byFontWidth * s_iScrWidth )
         ixFP = ( HB_BYTE ) ( AL_SCREEN_W / s_iScrWidth ) * 2;

      if( AL_SCREEN_H != s_byFontSize * s_iScrHeight )
      {
         iyFP = ( HB_BYTE ) ( AL_SCREEN_H / s_iScrHeight );
         if( iyFP & 1 )
            iyFP--;
      }

      if( ixFP || iyFP )
      {
         if( ! ixFP )
            ixFP = iyFP;

         if( ! iyFP )
            iyFP = ixFP;

         s_byFontSize  = ( ixFP < iyFP ? ixFP : iyFP );
         s_byFontWidth = s_byFontSize / 2;
      }

      s_mousePos.iCol  = al_mouse_x / s_byFontWidth;
      s_mousePos.iRow  = al_mouse_y / s_byFontSize;
      s_GFXUpd.iTop    = s_iScrHeight;
      s_GFXUpd.iLeft   = s_iScrWidth;
      s_GFXUpd.iBottom = 0;
      s_GFXUpd.iRight  = 0;
      s_CLIP.iTop      = 0;
      s_CLIP.iLeft     = 0;
      s_CLIP.iBottom   = AL_SCREEN_H - 1;
      s_CLIP.iRight    = AL_SCREEN_W - 1;
      s_iCurCol        = 0;
      s_iCurRow        = 0;
      s_iCursorStyle   = SC_NONE;
      ssfSetFontSize( &s_ssfDefaultFont, s_byFontSize );

      s_pClr[ 0 ]  = al_make_color( 0x00, 0x00, 0x00 );  /* black */
      s_pClr[ 1 ]  = al_make_color( 0x00, 0x00, 0xAA );  /* blue */
      s_pClr[ 2 ]  = al_make_color( 0x00, 0xAA, 0x00 );  /* green */
      s_pClr[ 3 ]  = al_make_color( 0x00, 0xAA, 0xAA );  /* cyan */
      s_pClr[ 4 ]  = al_make_color( 0xAA, 0x00, 0x00 );  /* red */
      s_pClr[ 5 ]  = al_make_color( 0xAA, 0x00, 0xAA );  /* magenta */
      s_pClr[ 6 ]  = al_make_color( 0xAA, 0x55, 0x00 );  /* brown */
      s_pClr[ 7 ]  = al_make_color( 0xAA, 0xAA, 0xAA );  /* white */
      s_pClr[ 8 ]  = al_make_color( 0x55, 0x55, 0x55 );  /* gray */
      s_pClr[ 9 ]  = al_make_color( 0x55, 0x55, 0xFF );  /* bright blue */
      s_pClr[ 10 ] = al_make_color( 0x55, 0xFF, 0x55 );  /* bright green */
      s_pClr[ 11 ] = al_make_color( 0x55, 0xFF, 0xFF );  /* bright cyan */
      s_pClr[ 12 ] = al_make_color( 0xFF, 0x55, 0x55 );  /* bright red */
      s_pClr[ 13 ] = al_make_color( 0xFF, 0x55, 0xFF );  /* bright magenta */
      s_pClr[ 14 ] = al_make_color( 0xFF, 0xFF, 0x55 );  /* yellow */
      s_pClr[ 15 ] = al_make_color( 0xFF, 0xFF, 0xFF );  /* bright white */

      s_bmp = al_create_system_bitmap( AL_SCREEN_W, AL_SCREEN_H );
      if( ! s_bmp )
      {
         s_bmp = al_create_bitmap( AL_SCREEN_W, AL_SCREEN_H );
         if( ! s_bmp )
            hb_gt_alleg_Error( "Could not allocate double buffer bitmap" );
      }

      if( ! lClearInit )
      {
         int iColor = s_pClr[ ( HB_GTSELF_GETCLEARCOLOR( pGT ) >> 4 ) & 0x0f ];
         al_clear_to_color( s_bmp, iColor );
         al_clear_to_color( al_screen, iColor );
      }

      HB_GTSELF_RESIZE( pGT, s_iScrHeight, s_iScrWidth );
      HB_GTSELF_EXPOSEAREA( pGT, 0, 0, s_iScrHeight, s_iScrWidth );
      HB_GTSELF_REFRESH( pGT );
   }

   s_iGFXWidth  = 0;
   s_iGFXHeight = 0;

   return lMode;
}

static void hb_gt_alleg_Init( PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr )
{
   int iRet;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_alleg_Init(%p,%p,%p,%p)", pGT, ( void * ) ( HB_PTRDIFF ) hFilenoStdin, ( void * ) ( HB_PTRDIFF ) hFilenoStdout, ( void * ) ( HB_PTRDIFF ) hFilenoStderr ) );

   ssfCreateThinFont( &s_ssfDefaultFont );

   s_iCursorStyle = SC_NONE;
   s_fMakeInit    = s_fGtError = HB_FALSE;

   if( allegro_init() != 0 )
      hb_gt_alleg_Error( "Screen driver initialization failure" );

   iRet = al_desktop_color_depth();

   if( iRet > 0 )
      al_set_color_depth( iRet );

   HB_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );
   HB_GTSELF_RESIZE( pGT, s_iScrHeight, s_iScrWidth );
}

static void hb_gt_alleg_Exit( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_alleg_Exit(%p)", pGT ) );

   HB_GTSUPER_EXIT( pGT );

   if( s_bmp )
   {
      al_destroy_bitmap( s_bmp );
      s_bmp = NULL;
   }
}

static const char * hb_gt_alleg_Version( PHB_GT pGT, int iType )
{
   HB_SYMBOL_UNUSED( pGT );

   if( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: Portable Allegro GUI console";
}

static HB_BOOL hb_gt_alleg_SetMode( PHB_GT pGT, int iRows, int iCols )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_alleg_SetMode(%p,%d,%d)", pGT, iRows, iCols ) );

   HB_SYMBOL_UNUSED( pGT );

   return hb_gt_alleg_InitializeScreen( pGT, iRows, iCols, HB_TRUE );
}

static int hb_gt_alleg_ReadKey( PHB_GT pGT, int iEventMask )
{
   int nKey = 0;
   int i, iMseCol, iMseRow;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_alleg_ReadKey(%p,%d)", pGT, iEventMask ) );

   HB_SYMBOL_UNUSED( iEventMask );

   GT_SCREENINIT();

   if( al_mouse_needs_poll() )
      al_poll_mouse();

   iMseCol = al_mouse_x / s_byFontWidth;
   iMseRow = al_mouse_y / s_byFontSize;
   if( iMseCol != s_mousePos.iCol || iMseRow != s_mousePos.iRow )
   {
      s_mousePos.iCol = iMseCol;
      s_mousePos.iRow = iMseRow;
      nKey = K_MOUSEMOVE;
   }

   if( nKey == 0 && al_mouse_b != s_mouseButtonsState )
   {
      if( ( al_mouse_b & 1 ) != ( s_mouseButtonsState & 1 ) )
      {
         if( al_mouse_b & 1 )
            nKey = K_LBUTTONDOWN;
         else
            nKey = K_LBUTTONUP;
      }
      else if( ( al_mouse_b & 2 ) != ( s_mouseButtonsState & 2 ) )
      {
         if( al_mouse_b & 2 )
            nKey = K_RBUTTONDOWN;
         else
            nKey = K_RBUTTONUP;
      }
      else if( ( al_mouse_b & 4 ) != ( s_mouseButtonsState & 4 ) )
      {
         if( al_mouse_b & 4 )
            nKey = K_MBUTTONDOWN;
         else
            nKey = K_MBUTTONUP;
      }
      /* We need to define INKEY_M* & K_MBUTTON* in inkey.ch ! */
      s_mouseButtonsState = al_mouse_b;
   }

   if( nKey == 0 )
   {
      if( al_keyboard_needs_poll() )
         al_poll_keyboard();

      if( al_key_pressed() )
         nKey = al_read_key();

#ifdef DEBUG
      if( nKey != 0 )
      {
         /* Good standard debuging... */
         printf( "gtAlleg: Scancode: %d (0x%0x) ascii: %d (0x%0x) raw: %d (0x%0x)\n", nKey >> 8, nKey >> 8, nKey & 0xff, nKey & 0xff, nKey, nKey );
      }
#endif

      if( ( ( nKey & 255 ) == 2 || ( nKey & 255 ) == 3 ) && ( nKey >> 8 ) > 31 )   /* K_CTRL_ + navigation key */
      {
         for( i = 0; i < GT_CTRL_TABLE_SIZE; i++ )
         {
            if( ( nKey >> 8 ) == s_CtrlTable[ i ].al_key )
            {
               nKey = s_CtrlTable[ i ].xhb_key;
               break;
            }
         }
      }
      else if( ( nKey != 0 ) && ( ( nKey & 255 ) < 32 ) && ( ( nKey & 255 ) == ( nKey >> 8 ) ) )  /* K_CTRL_A .. Z */
      {
#ifdef HB_NEW_KCTRL
         nKey = 512 + ( nKey & 255 );
#else
         nKey = nKey & 255;
#endif
      }
      else if( ( ( ( nKey & 255 ) == 1 ) || ( ( nKey & 255 ) == 4 ) ) && ( ( ( nKey >> 8 ) >= AL_KEY_F1 ) && ( ( nKey >> 8 ) <= AL_KEY_F12 ) ) )  /* K_SH_F1 .. F12, K_ALT_F1..F12 */
      {
         if( ( nKey & 255 ) == 1 )
         {
            int iFKeys[ 12 ] = { K_SH_F1, K_SH_F2, K_SH_F2, K_SH_F4,  K_SH_F5,  K_SH_F6,
                                 K_SH_F7, K_SH_F8, K_SH_F9, K_SH_F10, K_SH_F11, K_SH_F12 };
            nKey = iFKeys[ ( nKey >> 8 ) - AL_KEY_F1 ];
         }
         else
         {
            int iFKeys[ 12 ] = { K_ALT_F1, K_ALT_F2, K_ALT_F2, K_ALT_F4,  K_ALT_F5,  K_ALT_F6,
                                 K_ALT_F7, K_ALT_F8, K_ALT_F9, K_ALT_F10, K_ALT_F11, K_ALT_F12 };
            nKey = iFKeys[ ( nKey >> 8 ) - AL_KEY_F1 ];
         }
      }
      else if( nKey & 255 )
      {
         nKey = nKey & 255;
      }
      else if( nKey != 0 )
      {
         for( i = 0; i < GT_KEY_TABLE_SIZE; i++ )
         {
            if( ( nKey >> 8 ) == s_KeyTable[ i ].al_key )
            {
               nKey = s_KeyTable[ i ].xhb_key;
               break;
            }
         }
      }
   }

   return nKey;
}

static HB_BOOL hb_gt_alleg_mouse_IsPresent( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   return HB_TRUE;
}

static void hb_gt_alleg_mouse_GetPos( PHB_GT pGT, int * piRow, int * piCol )
{
   GT_SCREENINIT();

   if( al_mouse_needs_poll() )
      al_poll_mouse();

   *piRow = al_mouse_y / s_byFontSize;
   *piCol = al_mouse_x / s_byFontWidth;
}

static void hb_gt_alleg_mouse_SetPos( PHB_GT pGT, int iRow, int iCol )
{
   GT_SCREENINIT();

   al_position_mouse( iCol * s_byFontWidth, iRow * s_byFontSize );
}

static HB_BOOL hb_gt_alleg_mouse_ButtonState( PHB_GT pGT, int iButton )
{
   GT_SCREENINIT();

   if( al_mouse_needs_poll() )
      al_poll_mouse();

   return ( al_mouse_b & ( 1 << ( iButton - 1 ) ) ) != 0;
}

static int hb_gt_alleg_mouse_CountButton( PHB_GT pGT )
{
   GT_SCREENINIT();

   return s_iMsButtons;
}

static void hb_gt_alleg_mouse_SetBounds( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight )
{
   GT_SCREENINIT();

   if( iTop > -1 && iTop * s_byFontSize < AL_SCREEN_H )
      s_mouseBound.iTop = iTop * s_byFontSize;

   if( iLeft > -1 && iLeft * s_byFontWidth < AL_SCREEN_W )
      s_mouseBound.iLeft = iLeft * s_byFontWidth;

   if( iBottom >= iTop && iBottom * s_byFontSize < AL_SCREEN_H )
      s_mouseBound.iBottom = iBottom * s_byFontSize;

   if( iRight >= iLeft && iRight * s_byFontWidth < AL_SCREEN_W )
      s_mouseBound.iRight = iRight * s_byFontWidth;

   al_set_mouse_range( s_mouseBound.iLeft, s_mouseBound.iTop,
                       s_mouseBound.iRight, s_mouseBound.iBottom );
}

static void hb_gt_alleg_mouse_GetBounds( PHB_GT pGT, int * piTop, int * piLeft, int * piBottom, int * piRight )
{
   GT_SCREENINIT();

   *piTop    = s_mouseBound.iTop;
   *piLeft   = s_mouseBound.iLeft;
   *piBottom = s_mouseBound.iBottom;
   *piRight  = s_mouseBound.iRight;
}

static HB_BOOL hb_gt_alleg_Info( PHB_GT pGT, int iType, PHB_GT_INFO pInfo )
{
   int iWidth, iHeight, iValue;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_alleg_Info(%p,%d,%p)", pGT, iType, pInfo ) );

   switch( iType )
   {
      case HB_GTI_ISSCREENPOS:
      case HB_GTI_KBDSUPPORT:
      case HB_GTI_ISGRAPHIC:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_TRUE );
         break;

      case HB_GTI_INPUTFD:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, -1 );
         break;

      case HB_GTI_SCREENWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, s_fInit ?
                                        AL_SCREEN_W : s_byFontWidth * s_iScrWidth );
         iWidth = hb_itemGetNI( pInfo->pNewVal );
         if( iWidth > 0 )
         {
            s_iGFXWidth = iWidth;
            /* hb_gt_alleg_InitializeScreen( pGT, s_iScrHeight, s_iScrWidth, s_fInit ); */
         }
         break;

      case HB_GTI_SCREENHEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, s_fInit ?
                                        AL_SCREEN_H : s_byFontSize * s_iScrHeight );
         iHeight = hb_itemGetNI( pInfo->pNewVal );
         if( iHeight > 0 )
         {
            s_iGFXHeight = iHeight;
            hb_gt_alleg_InitializeScreen( pGT, s_iScrHeight, s_iScrWidth, s_fInit );
         }
         break;

      case HB_GTI_SCREENDEPTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, s_fInit ?
                                        al_bitmap_color_depth( al_screen ) : al_desktop_color_depth() );
         iValue = hb_itemGetNI( pInfo->pNewVal );
         if( iValue == 8 || iValue == 15 || iValue == 16 ||
             iValue == 24 || iValue == 32 )
         {
            al_set_color_depth( iValue );
            hb_gt_alleg_InitializeScreen( pGT, s_iScrHeight, s_iScrWidth, s_fInit );
         }
         break;

      case HB_GTI_FONTSIZE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, s_byFontSize );
         iValue         = hb_itemGetNI( pInfo->pNewVal );
         if( iValue > 0 && iValue < 256 )
         {
            s_byFontSize  = ( unsigned short ) iValue;
            s_byFontWidth = s_byFontSize / 2;
            hb_gt_alleg_InitializeScreen( pGT, s_iScrHeight, s_iScrWidth, s_fInit );
         }
         break;

      case HB_GTI_FONTWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, s_byFontWidth );
         break;

      case HB_GTI_DESKTOPWIDTH:
         al_get_desktop_resolution( &iWidth, &iHeight );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, iWidth );
         break;

      case HB_GTI_DESKTOPHEIGHT:
         al_get_desktop_resolution( &iWidth, &iHeight );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, iHeight );
         break;

      case HB_GTI_DESKTOPDEPTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, al_desktop_color_depth() );
         break;

      case HB_GTI_KBDSHIFTS:
         if( al_keyboard_needs_poll() )
            al_poll_keyboard();

         pInfo->pResult = hb_itemPutNI( pInfo->pResult, al_key_shifts );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            al_set_keyboard_leds( hb_itemGetNI( pInfo->pNewVal ) );

         break;

      case HB_GTI_WINTITLE:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
            al_set_window_title( ( char * ) hb_itemGetCPtr( pInfo->pNewVal ) );

         break;

      case HB_GTI_VIEWMAXWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, s_iScrWidth );
         break;

      case HB_GTI_VIEWMAXHEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, s_iScrHeight );
         break;

      default:
         return HB_GTSUPER_INFO( pGT, iType, pInfo );
   }

   return HB_TRUE;
}

/* ********** Graphics API ********** */

static int hb_gt_alleg_gfx_Primitive( PHB_GT pGT, int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
   int iRet = 1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_alleg_gfx_Primitive(%p,%d,%d,%d,%d,%d,%d)", pGT, iType, iTop, iLeft, iBottom, iRight, iColor ) );

   GT_SCREENINIT();
   HB_GTSELF_REFRESH( pGT );

   switch( iType )
   {
      case HB_GFX_ACQUIRESCREEN:
         al_acquire_bitmap( s_bmp );
         break;

      case HB_GFX_RELEASESCREEN:
         al_release_bitmap( s_bmp );
         break;

      case HB_GFX_MAKECOLOR:
         iRet = al_make_color( iTop, iLeft, iBottom );
         break;

      case HB_GFX_CLIPTOP:
         iRet = s_CLIP.iTop;
         break;

      case HB_GFX_CLIPLEFT:
         iRet = s_CLIP.iLeft;
         break;

      case HB_GFX_CLIPBOTTOM:
         iRet = s_CLIP.iBottom;
         break;

      case HB_GFX_CLIPRIGHT:
         iRet = s_CLIP.iRight;
         break;

      case HB_GFX_SETCLIP:
         al_set_clip( s_bmp, iLeft, iTop, iRight, iBottom );
         s_CLIP.iTop    = iTop;
         s_CLIP.iLeft   = iLeft;
         s_CLIP.iBottom = iBottom;
         s_CLIP.iRight  = iRight;
         break;

      case HB_GFX_DRAWINGMODE:
         iRet = HB_GFX_MODE_SOLID;
         break;

      case HB_GFX_GETPIXEL:
         iRet = al_get_pixel( s_bmp, iLeft, iTop );
         break;

      case HB_GFX_PUTPIXEL:
         al_acquire_bitmap( s_bmp );
         al_put_pixel( s_bmp, iLeft, iTop, iBottom );
         al_release_bitmap( s_bmp );
         GT_UPD_GFXRECT( iTop, iLeft, iTop, iLeft );
         break;

      case HB_GFX_LINE:
         al_acquire_bitmap( s_bmp );
         if( iLeft == iRight )
            al_draw_vline( s_bmp, iLeft, iTop, iBottom, iColor );
         else if( iTop == iBottom )
            al_draw_hline( s_bmp, iLeft, iTop, iRight, iColor );
         else
            al_draw_line( s_bmp, iLeft, iTop, iRight, iBottom, iColor );
         al_release_bitmap( s_bmp );
         GT_UPD_GFXRECT( iTop, iLeft, iBottom, iRight );
         break;

      case HB_GFX_RECT:
         al_acquire_bitmap( s_bmp );
         al_draw_rect( s_bmp, iLeft, iTop, iRight, iBottom, iColor );
         al_release_bitmap( s_bmp );
         GT_UPD_GFXRECT( iTop, iLeft, iBottom, iRight );
         break;

      case HB_GFX_FILLEDRECT:
         al_acquire_bitmap( s_bmp );
         al_draw_rect_fill( s_bmp, iLeft, iTop, iRight, iBottom, iColor );
         al_release_bitmap( s_bmp );
         GT_UPD_GFXRECT( iTop, iLeft, iBottom, iRight );
         break;

      case HB_GFX_CIRCLE:
         al_acquire_bitmap( s_bmp );
         al_draw_circle( s_bmp, iLeft, iTop, iBottom, iRight );
         al_release_bitmap( s_bmp );
         GT_UPD_GFXRECT( iTop - iBottom, iLeft - iBottom, iTop + iBottom, iLeft + iBottom );
         break;

      case HB_GFX_FILLEDCIRCLE:
         al_acquire_bitmap( s_bmp );
         al_draw_circle_fill( s_bmp, iLeft, iTop, iBottom, iRight );
         al_release_bitmap( s_bmp );
         GT_UPD_GFXRECT( iTop - iBottom, iLeft - iBottom, iTop + iBottom, iLeft + iBottom );
         break;

      case HB_GFX_ELLIPSE:
         al_acquire_bitmap( s_bmp );
         al_draw_ellipse( s_bmp, iLeft, iTop, iRight, iBottom, iColor );
         al_release_bitmap( s_bmp );
         GT_UPD_GFXRECT( iTop - iBottom, iLeft - iRight, iTop + iBottom, iLeft + iRight );
         break;

      case HB_GFX_FILLEDELLIPSE:
         al_acquire_bitmap( s_bmp );
         al_draw_ellipse_fill( s_bmp, iLeft, iTop, iRight, iBottom, iColor );
         al_release_bitmap( s_bmp );
         GT_UPD_GFXRECT( iTop - iBottom, iLeft - iRight, iTop + iBottom, iLeft + iRight );
         break;

      case HB_GFX_FLOODFILL:
         al_acquire_bitmap( s_bmp );
         al_floodfill( s_bmp, iLeft, iTop, iBottom );
         al_release_bitmap( s_bmp );
         GT_UPD_GFXRECT( 0, 0, s_iScrHeight * s_byFontSize, s_iScrWidth * s_byFontWidth );
         break;

      default:
         return HB_GTSUPER_GFXPRIMITIVE( pGT, iType, iTop, iLeft, iBottom, iRight, iColor );
   }

   if( HB_GTSELF_DISPCOUNT( pGT ) == 0 )
      hb_gt_alleg_ScreenUpdate( pGT );

   return iRet;
}

static void hb_gt_alleg_gfx_Text( PHB_GT pGT, int iTop, int iLeft, const char * cBuf, int iColor, int iSize, int iWidth )
{
   int iBottom, iRight;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_alleg_gfx_Text(%p,%d,%d,%s,%d,%d,%d)", pGT, iTop, iLeft, cBuf, iColor, iSize, iWidth ) );

   HB_SYMBOL_UNUSED( iWidth );

   GT_SCREENINIT();
   HB_GTSELF_REFRESH( pGT );

   if( iSize )
      ssfSetFontSize( &s_ssfDefaultFont, ( unsigned short ) iSize );

   iRight  = iLeft + ( int ) strlen( cBuf ) * ( s_ssfDefaultFont.fsize / 2 ) - 1;
   iBottom = iTop + s_ssfDefaultFont.fsize - 1;

   al_acquire_bitmap( s_bmp );
   ssfDrawText( s_bmp, &s_ssfDefaultFont, cBuf, iLeft, iTop, iColor );
   al_release_bitmap( s_bmp );
   GT_UPD_GFXRECT( iTop, iLeft, iBottom, iRight );

   if( iSize )
      ssfSetFontSize( &s_ssfDefaultFont, s_byFontSize );

   if( HB_GTSELF_DISPCOUNT( pGT ) == 0 )
      hb_gt_alleg_ScreenUpdate( pGT );
}

/* ******** Graphics API end ******** */

static void hb_gt_alleg_Redraw( PHB_GT pGT, int iRow, int iCol, int iSize )
{
   int      iColor;
   HB_BYTE  bAttr;
   HB_UCHAR uc;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_alleg_Redraw(%p,%d,%d,%d)", pGT, iRow, iCol, iSize ) );

   if( s_fInit )
   {
      int iPosX = iCol * s_byFontWidth, iPosY = iRow * s_byFontSize;

      if( s_iCursorStyle != SC_NONE && s_iCurRow == iRow &&
          s_iCurCol >= iCol && s_iCurCol <= iCol + iSize - 1 )
         s_iCursorStyle = SC_NONE;

      GT_UPD_GFXRECT( iPosY, iPosX, iPosY + s_byFontSize - 1, iPosX + iSize * s_byFontWidth - 1 );

      while( iSize-- )
      {
         if( ! HB_GTSELF_GETSCRUC( pGT, iRow, iCol++, &iColor, &bAttr, &uc, HB_TRUE ) )
            break;
         al_draw_rect_fill( s_bmp, iPosX, iPosY, iPosX + s_byFontWidth - 1, iPosY + s_byFontSize - 1, s_pClr[ ( iColor >> 4 ) & 0x0F ] );
         ssfDrawChar( s_bmp, &s_ssfDefaultFont, uc, iPosX, iPosY, s_pClr[ iColor & 0x0F ] );
         iPosX += s_byFontWidth;
      }
   }
   else if( ! s_fMakeInit )
   {
      int iDefColor = HB_GTSELF_GETCOLOR( pGT );

      while( iSize-- )
      {
         if( ! HB_GTSELF_GETSCRUC( pGT, iRow, iCol++, &iColor, &bAttr, &uc, HB_TRUE ) )
            break;

         if( iColor != iDefColor || uc != ' ' )
         {
            s_fMakeInit = HB_TRUE;
            break;
         }
      }
   }
}

static void hb_gt_alleg_Refresh( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_alleg_Refresh(%p)", pGT ) );

   if( ! s_fGtError )
   {
      if( s_fInit )
      {
         al_acquire_bitmap( s_bmp );
         HB_GTSUPER_REFRESH( pGT );
         al_release_bitmap( s_bmp );
         if( HB_GTSELF_DISPCOUNT( pGT ) == 0 )
            hb_gt_alleg_ScreenUpdate( pGT );
      }
      else
      {
         HB_GTSUPER_REFRESH( pGT );
         if( s_fMakeInit )
         {
            s_fMakeInit = HB_FALSE;
            GT_SCREENINIT();
         }
      }
   }
}


/* ******************************************************************* */

static HB_BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_FuncInit(%p)", pFuncTable ) );

   pFuncTable->Init    = hb_gt_alleg_Init;
   pFuncTable->Exit    = hb_gt_alleg_Exit;
   pFuncTable->SetMode = hb_gt_alleg_SetMode;
   pFuncTable->Redraw  = hb_gt_alleg_Redraw;
   pFuncTable->Refresh = hb_gt_alleg_Refresh;
   pFuncTable->Version = hb_gt_alleg_Version;
   pFuncTable->Info    = hb_gt_alleg_Info;

   pFuncTable->ReadKey = hb_gt_alleg_ReadKey;

   pFuncTable->MouseIsPresent   = hb_gt_alleg_mouse_IsPresent;
   pFuncTable->MouseGetPos      = hb_gt_alleg_mouse_GetPos;
   pFuncTable->MouseSetPos      = hb_gt_alleg_mouse_SetPos;
   pFuncTable->MouseSetBounds   = hb_gt_alleg_mouse_SetBounds;
   pFuncTable->MouseGetBounds   = hb_gt_alleg_mouse_GetBounds;
   pFuncTable->MouseCountButton = hb_gt_alleg_mouse_CountButton;
   pFuncTable->MouseButtonState = hb_gt_alleg_mouse_ButtonState;
   pFuncTable->GfxPrimitive     = hb_gt_alleg_gfx_Primitive;
   pFuncTable->GfxText = hb_gt_alleg_gfx_Text;

   return HB_TRUE;
}

/* ******************************************************************* */

#include "hbgtreg.h"

/* ******************************************************************* */

/*
 * this is necessary if you want to link with .so Allegro libs
 * or when link statically and your linker will force to link main()
 * from Allegro library not the Harbour one
 */
int _mangled_main( int argc, char * argv[] )
{
   HB_TRACE( HB_TR_DEBUG, ( "_mangled_main(%d, %p)", argc, argv ) );

   hb_cmdargInit( argc, argv );
   hb_vmInit( HB_TRUE );
   return hb_vmQuit();
}

typedef int ( *_hballeg_main_t )( int argc, char * argv[] );

_hballeg_main_t _mangled_main_address = _mangled_main;
