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

#ifndef HB_GTAPI_H_
#define HB_GTAPI_H_

#include "extend.h"
#include "color.ch"
#include "setcurs.ch"
#include "box.h"

/* maximum length of color string */
#define CLR_STRLEN      64

/* Public interface. These should never change, only be added to. */

extern void   hb_gtInit( void );
extern void   hb_gtExit( void );
extern int    hb_gtBox( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, char * fpBoxString );
extern int    hb_gtBoxD( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight );
extern int    hb_gtBoxS( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight );
extern int    hb_gtColorSelect( USHORT uiColorIndex );
extern int    hb_gtDispBegin( void );
extern USHORT hb_gtDispCount( void );
extern int    hb_gtDispEnd( void );
extern int    hb_gtGetBlink( BOOL * bBlink );
extern int    hb_gtGetColorStr( char * fpColorString );
extern int    hb_gtGetCursor( USHORT * uipCursorShape );
extern int    hb_gtGetPos( USHORT * uipRow, USHORT * uipCol );
extern BOOL   hb_gtIsColor( void );
extern USHORT hb_gtMaxCol( void );
extern USHORT hb_gtMaxRow( void );
extern int    hb_gtPostExt( void );
extern int    hb_gtPreExt( void );
extern int    hb_gtRectSize( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, USHORT * uipBuffSize );
extern int    hb_gtRepChar( USHORT uiRow, USHORT uiCol, USHORT uiChar, USHORT uiCount );
extern int    hb_gtRest( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, char * vlpScrBuff );
extern int    hb_gtSave( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, char * vlpScrBuff );
extern int    hb_gtScrDim( USHORT * uipHeight, USHORT * uipWidth );
extern int    hb_gtScroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, SHORT iRows, SHORT iCols );
extern int    hb_gtSetBlink( BOOL bBlink );
extern int    hb_gtSetColorStr( char * fpColorString );
extern int    hb_gtSetCursor( USHORT uiCursorShape );
extern int    hb_gtSetMode( USHORT uiRows, USHORT uiCols );
extern int    hb_gtSetPos( USHORT uiRow, USHORT uiCol );
extern int    hb_gtSetSnowFlag( BOOL bNoSnow );
extern int    hb_gtWrite( char * fpStr, ULONG length );
extern int    hb_gtWriteAt( USHORT uiRow, USHORT uiCol, char * fpStr, ULONG length );
extern int    hb_gtWriteCon( char * fpStr, ULONG length );

/* Private interface listed below. these are common to all platforms */

extern void   hb_gt_Init( void );
extern int    hb_gt_IsColor( void );
extern void   hb_gt_Done( void );
extern char   hb_gt_GetScreenWidth( void );
extern char   hb_gt_GetScreenHeight( void );
extern void   hb_gt_SetPos( char cRow, char cCol );
extern char   hb_gt_Col( void );
extern char   hb_gt_Row( void );
extern void   hb_gt_Scroll( char cTop, char cLeft, char cBottom, char cRight, char attribute, char vert, char horiz );
extern void   hb_gt_SetCursorStyle( int style );
extern int    hb_gt_GetCursorStyle( void );
extern void   hb_gt_Puts( char cRow, char cCol, char attr, char *str, int len );
extern void   hb_gt_GetText( char cTop, char cLeft, char cBottom, char cRight, char *dest );
extern void   hb_gt_PutText( char cTop, char cLeft, char cBottom, char cRight, char *srce );
extern void   hb_gt_SetAttribute( char cTop, char cLeft, char cBottom, char cRight, char attribute );
extern void   hb_gt_DrawShadow( char cTop, char cLeft, char cBottom, char cRight, char attribute );
extern void   hb_gt_DispBegin( void );
extern void   hb_gt_DispEnd( void );
extern BOOL   hb_gt_SetMode( USHORT uiRows, USHORT uiCols );
extern BOOL   hb_gt_GetBlink( void );
extern void   hb_gt_SetBlink( BOOL bBlink );

#endif /* HB_GTAPI_H_ */
