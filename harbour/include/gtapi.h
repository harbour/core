/*
 * $Id$
 */

#ifndef HB_GTAPI_H_
#define HB_GTAPI_H_

#include "extend.h"
#include "color.ch"
#include "setcurs.ch"
#include "box.h"

/* maximum length of color string */
#define CLR_STRLEN      64

/*
 *  GTAPI.H; Screen drawing, cursor and keyboard routines for text mode
 *           16-bit and 32-bit MS-DOS, 16-bit and 32-bit OS/2, and 32-bit
 *           Windows 95/NT applications.
 *
 *  This module is based on VIDMGR by Andrew Clarke and modified for
 *  the Harbour project
 *
 *  GTAPI has been compiled and tested with the following C compilers:
 *
 *    - Turbo C++ (16-bit) for DOS 3.0
 *    - Borland C++ (16-bit) for DOS 3.1
 *    - Borland C++ (16-bit) for DOS 4.5
 *    - Borland C++ (32-bit) for OS/2 1.0
 *    - Cygnus GNU C (32-bit) for Windows 95/NT b14.0
 *    - DJGPP GNU C (32-bit) for DOS 2.0
 *    - EMX GNU C (32-bit) for OS/2 & DOS 0.9b
 *    - IBM VisualAge C/C++ 3.0 (32-bit) for OS/2
 *    - Microsoft C/C++ (16-bit) for OS/2 6.00a
 *    - Microsoft C/C++ (16-bit) for DOS 8.00c
 *    - Microsoft Quick C (16-bit) for DOS 2.50
 *    - Microsoft Visual C/C++ (16-bit) for DOS 1.52
 *    - Microsoft Visual C/C++ (32-bit) for Windows 95/NT 5.0 and 6.0
 *    - WATCOM C/C++ (16-bit & 32-bit) for DOS 9.5
 *    - WATCOM C/C++ (16-bit & 32-bit) for DOS 10.0
 *    - WATCOM C/C++ (32-bit) for OS/2 10.0
 *    - WATCOM C/C++ (32-bit) for Windows 95/NT 10.0
 *    - HI-TECH Pacific C (16-bit) for DOS 7.51
 *    - Symantec C/C++ (16-bit) for DOS 7.0
 *    - Zortech C/C++ (16-bit) for DOS 3.0r4
*/

/* Public interface. These should never change, only be added to. */

void   hb_gtInit(void);
void   hb_gtExit(void);
int    hb_gtBox(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, char * fpBoxString);
int    hb_gtBoxD(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight);
int    hb_gtBoxS(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight);
int    hb_gtColorSelect(USHORT uiColorIndex);
int    hb_gtDispBegin(void);
USHORT hb_gtDispCount(void);
int    hb_gtDispEnd(void);
int    hb_gtGetColorStr(char * fpColorString);
int    hb_gtGetCursor(USHORT * uipCursorShape);
int    hb_gtGetPos(USHORT * uipRow, USHORT * uipCol);
BOOL   hb_gtIsColor(void);
USHORT hb_gtMaxCol(void);
USHORT hb_gtMaxRow(void);
int    hb_gtPostExt(void);
int    hb_gtPreExt(void);
int    hb_gtRectSize(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, USHORT * uipBuffSize);
int    hb_gtRepChar(USHORT uiRow, USHORT uiCol, USHORT uiChar, USHORT uiCount);
int    hb_gtRest(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, char * vlpScrBuff);
int    hb_gtSave(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, char * vlpScrBuff);
int    hb_gtScrDim(USHORT * uipHeight, USHORT * uipWidth);
int    hb_gtScroll(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, SHORT iRows, SHORT iCols);
int    hb_gtSetBlink(BOOL bBlink);
int    hb_gtSetColorStr(char * fpColorString);
int    hb_gtSetCursor(USHORT uiCursorShape);
int    hb_gtSetMode(USHORT uiRows, USHORT uiCols);
int    hb_gtSetPos(USHORT uiRow, USHORT uiCol);
int    hb_gtSetSnowFlag(BOOL bNoSnow);
int    hb_gtWrite(char * fpStr, ULONG length);
int    hb_gtWriteAt(USHORT uiRow, USHORT uiCol, char * fpStr, ULONG length);
int    hb_gtWriteCon(char * fpStr, ULONG length);

#ifndef DOS
#if defined(_QC) || defined(__DOS__) || defined(MSDOS) || defined(__MSDOS__)
#define DOS
#endif
#endif

#ifndef OS2
#if defined(__OS2__) || defined(OS_2)
#define OS2
#endif
#endif

#ifndef EMX
#if defined(__EMX__)
#define EMX
#endif
#endif

#ifndef WINNT
#if defined(__NT__)
#define WINNT
#endif
#endif

/* private interface listed below. these are common to all platforms */

void gtInit(void);
void gtDone(void);
char gtGetScreenWidth(void);
char gtGetScreenHeight(void);
void gtSetPos(char cRow, char cCol);
char gtCol(void);
char gtRow(void);
void gtSetCursorStyle(int style);
int  gtGetCursorStyle(void);
void gtPuts(char cRow, char cCol, char attr, char *str, int len);
void gtGetText(char cTop, char cLeft, char cBottom, char cRight, char *dest);
void gtPutText(char cTop, char cLeft, char cBottom, char cRight, char *srce);
void gtSetAttribute( char cTop, char cLeft, char cBottom, char cRight, char attribute );

#endif /* HB_GTAPI_H_ */
