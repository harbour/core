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
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    Keyboard related declarations
 *    Cursor declarations
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 *    Mouse related declarations
 *
 * See doc/license.txt for licensing terms.
 *
 */

#ifndef HB_APIGT_H_
#define HB_APIGT_H_

#include "hbapi.h"
#include "color.ch"

/* NOTE: The declaration of hb_gtSetPos(), hb_gtGetPos(), hb_gtWrite(), 
         hb_gtWriteAt(), hb_gtRepChar() differs in parameter types from the 
         original CA-Cl*pper versions. [vszakats] */

/* maximum length of color string */
#define CLR_STRLEN      64

/* strings for borders (same as box.ch, but defined for use by C) */
                               /*01234567*/
#define _B_SINGLE              "ÚÄ¿³ÙÄÀ³"
#define _B_DOUBLE              "ÉÍ»º¼ÍÈº"
#define _B_SINGLE_DOUBLE       "ÖÄ·º½ÄÓº"
#define _B_DOUBLE_SINGLE       "ÕÍ¸³¾ÍÔ³"

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
extern int    hb_gtReadKey( void );
extern void hb_gtAdjustPos( int iHandle, char * pStr, ULONG ulLen );
extern USHORT hb_gtBox( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbyBoxString );
extern USHORT hb_gtBoxD( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight );
extern USHORT hb_gtBoxS( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight );
extern USHORT hb_gtColorSelect( USHORT uiColorIndex );
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
extern USHORT hb_gtSetSnowFlag( BOOL bNoSnow );
extern USHORT hb_gtWrite( BYTE * pbyStr, ULONG ulLen );
extern USHORT hb_gtWriteAt( USHORT uiRow, USHORT uiCol, BYTE * pbyStr, ULONG ulLen );
extern USHORT hb_gtWriteCon( BYTE * pbyStr, ULONG ulLen );

/* Private interface listed below. these are common to all platforms */

extern void   hb_gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr );
extern BOOL   hb_gt_IsColor( void );
extern void   hb_gt_Done( void );
extern BOOL   hb_gt_AdjustPos( BYTE * pStr, ULONG ulLen );
extern int    hb_gt_ReadKey( void );
extern USHORT hb_gt_GetScreenWidth( void );
extern USHORT hb_gt_GetScreenHeight( void );
extern void   hb_gt_SetPos( SHORT iRow, SHORT iCol );
extern SHORT  hb_gt_Col( void );
extern SHORT  hb_gt_Row( void );
extern void   hb_gt_Scroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr, SHORT iRows, SHORT iCols );
extern void   hb_gt_SetCursorStyle( USHORT uiCursorShape );
extern USHORT hb_gt_GetCursorStyle( void );
extern void   hb_gt_Puts( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE * pbyStr, ULONG ulLen );
extern void   hb_gt_GetText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbyDst );
extern void   hb_gt_PutText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbySrc );
extern void   hb_gt_SetAttribute( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr );
extern void   hb_gt_DispBegin( void );
extern void   hb_gt_DispEnd( void );
extern BOOL   hb_gt_SetMode( USHORT uiRows, USHORT uiCols );
extern BOOL   hb_gt_GetBlink( void );
extern void   hb_gt_SetBlink( BOOL bBlink );
extern void   hb_gt_Replicate( BYTE byChar, ULONG ulLen );

/* Keyboard related declarations */

typedef enum
{
   INKEY_MOVE           = 1,    /* Mouse Events */
   INKEY_LDOWN          = 2,    /* Mouse Left Click Down */
   INKEY_LUP            = 4,    /* Mouse Left Click Up */
   INKEY_RDOWN          = 8,    /* Mouse Right Click Down */
   INKEY_RUP            = 16,   /* Mouse Right Click Up */
   INKEY_KEYBOARD       = 128,  /* Keyboard Events */
   INKEY_ALL            = 159,  /* All Mouse and Keyboard Events */
   INKEY_EXTENDED       = 256   /* Extended Keyboard Events */
} HB_inkey_enum;

/* Harbour keyboard support functions */
extern int    hb_inkey ( double seconds, HB_inkey_enum event_mask, BOOL wait, BOOL forever ); /* Wait for keyboard input */
extern int    hb_inkeyGet( void );            /* Extract the next key from the Harbour keyboard buffer */
extern void   hb_inkeyPut( int ch );          /* Inserts an inkey code into the keyboard buffer */
extern int    hb_inkeyLast( void );           /* Return the value of the last key that was extracted */
extern int    hb_inkeyNext( void );           /* Return the next key without extracting it */
extern void   hb_inkeyPoll( void );           /* Poll the console keyboard to stuff the Harbour buffer */
extern void   hb_inkeyReset( BOOL allocate ); /* Reset the Harbour keyboard buffer */

/* TOFIX: This should go somewhere else. */
extern void   hb_releaseCPU( void );          /* Attempt to release a CPU time slice */

/* Mouse related declarations */

/* Public interface. These should never change, only be added to. */

extern void   hb_mouseInit( void );
extern void   hb_mouseExit( void );
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

#endif /* HB_APIGT_H_ */
