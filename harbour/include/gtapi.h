/*
 * $Id$
 */

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

#ifndef _GTAPI_H
#define _GTAPI_H

#include <extend.h>

/* Public interface. These should never change, only be added to. */

void hb_gtInit(void);
int hb_gtBox(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, char * fpBoxString);
int hb_gtBoxD(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight);
int hb_gtBoxS(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight);
int hb_gtColorSelect(USHORT uiColorIndex);
int hb_gtDispBegin(void);
USHORT hb_gtDispCount(void);
int hb_gtDispEnd(void);
int hb_gtGetColorStr(char * fpColorString);
int hb_gtGetCursor(USHORT * uipCursorShape);
int hb_gtGetPos(USHORT * uipRow, USHORT * uipCol);
BOOL hb_gtIsColor(void);
USHORT hb_gtMaxCol(void);
USHORT hb_gtMaxRow(void);
int hb_gtPostExt(void);
int hb_gtPreExt(void);
int hb_gtRectSize(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, USHORT * uipBuffSize);
int hb_gtRepChar(USHORT uiRow, USHORT uiCol, USHORT uiChar, USHORT uiCount);
int hb_gtRest(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, char * vlpScrBuff);
int hb_gtSave(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, char * vlpScrBuff);
int hb_gtScrDim(USHORT * uipHeight, USHORT * uipWidth);
int hb_gtScroll(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, SHORT iRows, SHORT iCols);
int hb_gtSetBlink(BOOL bBlink);
int hb_gtSetColorStr(char * fpColorString);
int hb_gtSetCursor(USHORT uiCursorShape);
int hb_gtSetMode(USHORT uiRows, USHORT uiCols);
int hb_gtSetPos(USHORT uiRow, USHORT uiCol);
int hb_gtSetSnowFlag(BOOL bNoSnow);
int hb_gtWrite(char * fpStr, USHORT uiLen);
int hb_gtWriteAt(USHORT uiRow, USHORT uiCol, char * fpStr, USHORT uiLen);
int hb_gtWriteCon(char * fpStr, USHORT uiLen);

/* maximum length of color string */
#define CLR_STRLEN      64

/* cursor types */
#define _SC_NONE        0
#define _SC_NORMAL      1
#define _SC_INSERT      2
#define _SC_SPECIAL1    3
#define _SC_SPECIAL2    4

/* attributes for color strings */
#define _CLR_STANDARD   0
#define _CLR_ENHANCED   1
#define _CLR_BORDER     2
#define _CLR_BACKGROUND 3
#define _CLR_UNSELECTED 4
#define _CLR_LASTCOLOR  _CLR_UNSELECTED

/* strings for borders (same as Clipper/Harbour ones) */
                               /*01234567*/
#define _B_NONE                 "        "
#define _B_SINGLE               "ÚÄ¿³ÙÄÀ³"
#define _B_DOUBLE               "ÉÍ»º¼ÍÈº"
#define _B_SINGLE_DOUBLE        "ÖÄ·º½ÄÓº"
#define _B_DOUBLE_SINGLE        "ÕÍ¸³¾ÍÔ³"

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
void gtGotoXY(char x, char y);
char gtWhereX(void);
char gtWhereY(void);
void gtSetCursorStyle(int style);
int  gtGetCursorStyle(void);
void gtPuts(char x, char y, char attr, char *str, int len);
void gtGetText(char x1, char y1, char x2, char y2, char *dest);
void gtPutText(char x1, char y1, char x2, char y2, char *srce);

#endif /* _GTAPI_H */
