/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Terminal API
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    Keyboard related declarations
 *    Cursor declarations
 * See above for licensing terms.
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 *    Mouse related declarations
 *    Undocumented GT API declarations
 * See doc/license.txt for licensing terms.
 *
 */

#ifndef HB_APIGT_H_
#define HB_APIGT_H_

#include "hbapi.h"

#if defined(HB_EXTERN_C)
extern "C" {
#endif

/* NOTE: The declaration of hb_gtSetPos(), hb_gtGetPos(), hb_gtWrite(),
         hb_gtWriteAt(), hb_gtRepChar() differs in parameter types from the
         original CA-Cl*pper versions. [vszakats] */

/* maximum length of color string */
#define CLR_STRLEN              64

/* attributes for color strings, these are the same as the ones in color.ch
   but prefixed with HB_ to avoid collision. */
#define HB_CLR_STANDARD         0
#define HB_CLR_ENHANCED         1
#define HB_CLR_BORDER           2
#define HB_CLR_BACKGROUND       3
#define HB_CLR_UNSELECTED       4
#define HB_CLR_MAX_             HB_CLR_UNSELECTED

/* strings for borders (same as box.ch, but defined for use by C) */

/* Note. This part will never be used, but is being kept in the source,
         so that if you use code page 437, you can see what the line
         draw characters are supposed to look like.
                                01234567
#define _B_SINGLE              "ÚÄ¿³ÙÄÀ³"
#define _B_DOUBLE              "ÉÍ»º¼ÍÈº"
#define _B_SINGLE_DOUBLE       "ÖÄ·º½ÄÓº"
#define _B_DOUBLE_SINGLE       "ÕÍ¸³¾ÍÔ³"
#define HB_B_SINGLE_V          '³'
#define HB_B_SINGLE_H          'Ä'
#define HB_B_DOUBLE_V          'º'
#define HB_B_DOUBLE_H          'Í'
*/
#define _B_SINGLE              "\xda\xc4\xbf\xb3\xd9\xc4\xc0\xb3"
#define _B_DOUBLE              "\xc9\xcd\xbb\xba\xbc\xcd\xc8\xba"
#define _B_SINGLE_DOUBLE       "\xd6\xc4\xb7\xba\xbd\xc4\xd3\xba"
#define _B_DOUBLE_SINGLE       "\xd5\xcd\xb8\xb3\xbe\xcd\xd4\xb3"
#define HB_B_SINGLE_V          '\xb3'
#define HB_B_SINGLE_H          '\xc4'
#define HB_B_DOUBLE_V          '\xb3'
#define HB_B_DOUBLE_H          '\xc4'

/* Used to tell hb_gt_SetPos() when the cursor position
   is being set. Before or after text is or was displayed.
*/
#define HB_GT_SET_POS_AFTER     1
#define HB_GT_SET_POS_BEFORE    0

/* Keyboard filters */

typedef enum
{
   INKEY_MOVE           = 1,    /* Mouse Events */
   INKEY_LDOWN          = 2,    /* Mouse Left Click Down */
   INKEY_LUP            = 4,    /* Mouse Left Click Up */
   INKEY_RDOWN          = 8,    /* Mouse Right Click Down */
   INKEY_RUP            = 16,   /* Mouse Right Click Up */
   INKEY_KEYBOARD       = 128,  /* Keyboard Events */
   INKEY_ALL            = 159,  /* All Mouse and Keyboard Events */
   INKEY_RAW            = 256   /* Minimally Decoded Keyboard Events */
} HB_inkey_enum;

/* Cursor style constants */

typedef enum
{
   SC_NONE              = 0,    /* None */
   SC_NORMAL            = 1,    /* Underline */
   SC_INSERT            = 2,    /* Lower half block */
   SC_SPECIAL1          = 3,    /* Full block */
   SC_SPECIAL2          = 4     /* Upper half block */
} HB_cursor_enum;

/* Public interface. These should never change, only be added to. */

extern void   hb_gtInit( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr );
extern void   hb_gtExit( void );
extern void   hb_gtAdjustPos( int iHandle, char * pStr, ULONG ulLen );
extern USHORT hb_gtBox( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbyFrame );
extern USHORT hb_gtBoxD( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight );
extern USHORT hb_gtBoxS( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight );
extern USHORT hb_gtColorSelect( USHORT uiColorIndex );
extern USHORT hb_gtColorToN( char * szColorString );
extern USHORT hb_gtDispBegin( void );
extern USHORT hb_gtDispCount( void );
extern USHORT hb_gtDispEnd( void );
extern USHORT hb_gtDrawShadow( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr );
extern USHORT hb_gtGetBlink( BOOL * pbBlink );
extern USHORT hb_gtGetColorStr( char * pszColorString );
extern USHORT hb_gtGetCursor( USHORT * puiCursorShape );
extern USHORT hb_gtGetPos( SHORT * piRow, SHORT * piCol );
extern BOOL   hb_gtIsColor( void );
extern USHORT hb_gtMaxCol( void );
extern USHORT hb_gtMaxRow( void );
extern USHORT hb_gtPostExt( void );
extern USHORT hb_gtPreExt( void );
extern USHORT hb_gtSuspend( void ); /* prepare the reminal for shell output */
extern USHORT hb_gtResume( void ); /* resume the terminal after the shell output */
extern int    hb_gtReadKey( HB_inkey_enum eventmask );
extern USHORT hb_gtRectSize( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, USHORT * puiBuffSize );
extern USHORT hb_gtRepChar( USHORT uiRow, USHORT uiCol, BYTE byChar, USHORT uiCount );
extern USHORT hb_gtRest( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, void * pScrBuff );
extern USHORT hb_gtSave( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, void * pScrBuff );
extern USHORT hb_gtScrDim( USHORT * puiHeight, USHORT * puiWidth );
extern USHORT hb_gtScroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, SHORT iRows, SHORT iCols );
extern USHORT hb_gtSetBlink( BOOL bBlink );
extern USHORT hb_gtSetColorStr( char * pszColorString );
extern USHORT hb_gtSetCursor( USHORT uiCursorShape );
extern USHORT hb_gtSetMode( USHORT uiRows, USHORT uiCols );
extern USHORT hb_gtSetPos( SHORT iRow, SHORT iCol );
extern USHORT hb_gtSetPosContext( SHORT iRow, SHORT iCol, SHORT iMode );
extern USHORT hb_gtSetSnowFlag( BOOL bNoSnow );
extern void   hb_gtTone( double dFrequency, double dDuration );
extern USHORT hb_gtWrite( BYTE * pbyStr, ULONG ulLen );
extern USHORT hb_gtWriteAt( USHORT uiRow, USHORT uiCol, BYTE * pbyStr, ULONG ulLen );
extern USHORT hb_gtWriteCon( BYTE * pbyStr, ULONG ulLen );
extern char * hb_gtVersion( void );

/* Undocumented CA-Clipper 5.x GT API calls */

#define HB_GT_RECT void
#define HB_GT_WND void
#define HB_GT_RGB void
#define HB_GT_SLR void

extern void   hb_gtWCreate( HB_GT_RECT * rect, HB_GT_WND ** wnd );
extern void   hb_gtWDestroy( HB_GT_WND * wnd );
extern BOOL   hb_gtWFlash( void );
extern void   hb_gtWApp( HB_GT_WND ** wnd );
extern void   hb_gtWCurrent( HB_GT_WND * wnd );
extern void   hb_gtWPos( HB_GT_WND * wnd, HB_GT_RECT * rect );
extern BOOL   hb_gtWVis( HB_GT_WND * wnd, USHORT uiStatus );

extern USHORT hb_gtSLR( HB_GT_SLR * pSLR ); /* System Level Request */
extern USHORT hb_gtModalRead( void * );
extern USHORT hb_gtBeginWrite( void );
extern USHORT hb_gtEndWrite( void );
extern USHORT hb_gtFlushCursor( void );
extern USHORT hb_gtSetColor( HB_GT_RGB * color );
extern USHORT hb_gtGetColor( HB_GT_RGB * color );
extern USHORT hb_gtSetBorder( HB_GT_RGB * color );

/* Private interface listed below. these are common to all platforms */

extern void   hb_gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr );
extern void   hb_gt_Exit( void );
extern BOOL   hb_gt_AdjustPos( BYTE * pStr, ULONG ulLen );
extern USHORT hb_gt_Box( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbyFrame, BYTE byAttr );
extern USHORT hb_gt_BoxD( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbyFrame, BYTE byAttr );
extern USHORT hb_gt_BoxS( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbyFrame, BYTE byAttr );
extern SHORT  hb_gt_Col( void );
extern void   hb_gt_DispBegin( void );
extern USHORT hb_gt_DispCount( void );
extern void   hb_gt_DispEnd( void );
extern BOOL   hb_gt_GetBlink( void );
extern USHORT hb_gt_GetCursorStyle( void );
extern USHORT hb_gt_GetScreenHeight( void );
extern USHORT hb_gt_GetScreenWidth( void );
extern void   hb_gt_GetText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbyDst );
extern USHORT hb_gt_HorizLine( USHORT uiRow, USHORT uiLeft, USHORT uiRight, BYTE byChar, BYTE byAttr );
extern BOOL   hb_gt_IsColor( void );
extern BOOL   hb_gt_PreExt( void );
extern BOOL   hb_gt_PostExt( void );
extern BOOL   hb_gt_Suspend( void ); /* suspend the terminal before the shell call */
extern BOOL   hb_gt_Resume( void ); /* resume the terminal after the shell call */
extern void   hb_gt_Puts( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE * pbyStr, ULONG ulLen );
extern void   hb_gt_PutText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbySrc );
extern int    hb_gt_ReadKey( HB_inkey_enum eventmask );
extern int    hb_gt_RectSize( USHORT rows, USHORT cols );
extern void   hb_gt_Replicate( USHORT uiTop, USHORT uiLeft, BYTE byAttr, BYTE byChar, ULONG ulLen );
extern SHORT  hb_gt_Row( void );
extern void   hb_gt_Scroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr, SHORT iRows, SHORT iCols );
extern void   hb_gt_SetAttribute( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr );
extern void   hb_gt_SetBlink( BOOL bBlink );
extern void   hb_gt_SetCursorStyle( USHORT uiCursorShape );
extern BOOL   hb_gt_SetMode( USHORT uiRows, USHORT uiCols );
extern void   hb_gt_SetPos( SHORT iRow, SHORT iCol, SHORT iMethod );
extern void   hb_gt_Tone( double dFrequency, double dDuration );
extern char * hb_gt_Version( void );
extern USHORT hb_gt_VertLine( USHORT uiCol, USHORT uiTop, USHORT uiBottom, BYTE byChar, BYTE byAttr );

/* Keyboard related declarations */

#define HB_BREAK_FLAG 256 /* 256, because that's what DJGPP returns Ctrl+Break as.
                             Clipper has no key code 256, so it may as well be
                             used for all the Harbour builds that need it */

/* Harbour keyboard support functions */
extern int    hb_inkey( BOOL bWait, double dSeconds, HB_inkey_enum event_mask ); /* Wait for keyboard input */
extern int    hb_inkeyGet( void );            /* Extract the next key from the Harbour keyboard buffer */
extern void   hb_inkeyPut( int ch );          /* Inserts an inkey code into the keyboard buffer */
extern int    hb_inkeyLast( void );           /* Return the value of the last key that was extracted */
extern int    hb_inkeyNext( void );           /* Return the next key without extracting it */
extern void   hb_inkeyPoll( void );           /* Poll the console keyboard to stuff the Harbour buffer */
extern void   hb_inkeyReset( BOOL allocate ); /* Reset the Harbour keyboard buffer */

/* Mouse related declarations */

/* Public interface. These should never change, only be added to. */

extern BOOL   hb_mouseIsPresent( void );
extern BOOL   hb_mouseGetCursor( void );
extern void   hb_mouseSetCursor( BOOL bVisible );
extern int    hb_mouseCol( void );
extern int    hb_mouseRow( void );
extern void   hb_mouseSetPos( int iRow, int iCol );
extern BOOL   hb_mouseIsButtonPressed( int iButton );
extern int    hb_mouseCountButton( void );
extern void   hb_mouseSetBounds( int iTop, int iLeft, int iBottom, int iRight );
extern void   hb_mouseGetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight );

/* Private interface listed below. these are common to all platforms */

extern void   hb_mouse_Init( void );
extern void   hb_mouse_Exit( void );
extern BOOL   hb_mouse_IsPresent( void );
extern void   hb_mouse_Show( void );
extern void   hb_mouse_Hide( void );
extern int    hb_mouse_Col( void );
extern int    hb_mouse_Row( void );
extern void   hb_mouse_SetPos( int iRow, int iCol );
extern BOOL   hb_mouse_IsButtonPressed( int iButton );
extern int    hb_mouse_CountButton( void );
extern void   hb_mouse_SetBounds( int iTop, int iLeft, int iBottom, int iRight );
extern void   hb_mouse_GetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight );

/* SetKey related declarations */

/* Public interface. These should never change, only be added to. */

extern void   hb_setkeyInit( void );
extern void   hb_setkeyExit( void );

/* Private interface listed below. these are common to all platforms */

/* none as of yet */

#if defined(HB_EXTERN_C)
}
#endif

#endif /* HB_APIGT_H_ */
