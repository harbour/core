/*
 * $Id$
 */

/*
 *  GTOS2.C: Video subsystem for OS/2 compilers.
 *
 *  This module is based on VIDMGR by Andrew Clarke and modified for
 *  the Harbour project
 */

#define INCL_KBD
#define INCL_VIO
#define INCL_DOSPROCESS

#include <string.h>
#include <os2.h>
#include "gtapi.h"

#ifndef KBDTRF_FINAL_CHAR_IN
#define KBDTRF_FINAL_CHAR_IN  FINAL_CHAR_IN
#endif

static void hb_gt_SetCursorSize(char start, char end);
static void hb_gt_GetCursorSize(char *start, char *end);

void hb_gt_Init(void)
{
}

void hb_gt_Done(void)
{
}

int hb_gt_IsColor(void)
{
    return TRUE;
}

char hb_gt_GetScreenWidth(void)
{
    VIOMODEINFO vi;
    vi.cb = sizeof(VIOMODEINFO);
    VioGetMode(&vi, 0);
    return vi.col;
}

char hb_gt_GetScreenHeight(void)
{
    VIOMODEINFO vi;
    vi.cb = sizeof(VIOMODEINFO);
    VioGetMode(&vi, 0);
    return vi.row;
}

void hb_gt_SetPos(char cRow, char cCol)
{
    VioSetCurPos((USHORT) cRow, (USHORT) cCol, 0);
}

char hb_gt_Row(void)
{
    USHORT x, y;
    VioGetCurPos(&y, &x, 0);
    return x;
}

char hb_gt_Col(void)
{
    USHORT x, y;
    VioGetCurPos(&y, &x, 0);
    return y;
}

static void hb_gt_SetCursorSize(char start, char end)
{
    VIOCURSORINFO vi;
    vi.yStart = start;
    vi.cEnd = end;
    vi.cx = 0;
    vi.attr = 0;
    VioSetCurType(&vi, 0);
}

static void hb_gt_GetCursorSize(char *start, char *end)
{
    VIOCURSORINFO vi;
    VioGetCurType(&vi, 0);
    *start = vi.yStart;
    *end = vi.cEnd;
}

void hb_gt_SetCursorStyle(int style)
{
    /* TODO: need to implement this */
}

int hb_gt_GetCursorStyle(void)
{
    /* TODO: need to implement this */
    int rc=0;

    return(rc);
}

void hb_gt_Puts(char cRow, char cCol, char attr, char *str, int len)
{
    VioWrtCharStrAtt(str, (USHORT) len, (USHORT) cRow, (USHORT) cCol, (PBYTE) &attr, 0);
}

void hb_gt_GetText(char cTop, char cLeft, char cBottom, char cRight, char *dest)
{
    USHORT width;
    char y;
    width = (USHORT) ((cRight - cLeft + 1) * 2);
    for (y = cTop; y <= cBottom; y++)
    {
        VioReadCellStr((PBYTE) dest, &width, (USHORT) cLeft, (USHORT) y, 0);
        dest += width;
    }
}

void hb_gt_PutText(char cTop, char cLeft, char cBottom, char cRight, char *srce)
{
    USHORT width;
    char y;
    width = (USHORT) ((cRight - cLeft + 1) * 2);
    for (y = cTop; y <= cBottom; y++)
    {
        VioWrtCellStr((PBYTE) srce, width, (USHORT) cLeft, (USHORT) y, 0);
        srce += width;
    }
}

void hb_gt_SetAttribute( char cTop, char cLeft, char cBottom, cRight, char attribute )
{
}

void hb_gt_DrawShadow( char cTop, char cLeft, char cBottom, cRight, char attribute )
{
}

void hb_gt_SetMode( USHORT uiRows, USHORT uiCols )
{
   uiRows=uiCols=0;
}

void hb_gt_DispBegin(void)
{
}

void hb_gt_DispEnd(void)
{
}
