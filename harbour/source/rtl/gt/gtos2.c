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

static void gtSetCursorSize(char start, char end);
static void gtGetCursorSize(char *start, char *end);

void gtInit(void)
{
}

void gtDone(void)
{
}

char gtGetScreenWidth(void)
{
    VIOMODEINFO vi;
    vi.cb = sizeof(VIOMODEINFO);
    VioGetMode(&vi, 0);
    return vi.col;
}

char gtGetScreenHeight(void)
{
    VIOMODEINFO vi;
    vi.cb = sizeof(VIOMODEINFO);
    VioGetMode(&vi, 0);
    return vi.row;
}

void gtSetPos(char cRow, char cCol)
{
    VioSetCurPos((USHORT) cRow, (USHORT) cCol, 0);
}

char gtRow(void)
{
    USHORT x, y;
    VioGetCurPos(&y, &x, 0);
    return x;
}

char gtCol(void)
{
    USHORT x, y;
    VioGetCurPos(&y, &x, 0);
    return y;
}

static void gtSetCursorSize(char start, char end)
{
    VIOCURSORINFO vi;
    vi.yStart = start;
    vi.cEnd = end;
    vi.cx = 0;
    vi.attr = 0;
    VioSetCurType(&vi, 0);
}

static void gtGetCursorSize(char *start, char *end)
{
    VIOCURSORINFO vi;
    VioGetCurType(&vi, 0);
    *start = vi.yStart;
    *end = vi.cEnd;
}

void gtSetCursorStyle(int style)
{
    /* TODO: need to implement this */
}

int gtGetCursorStyle(void)
{
    /* TODO: need to implement this */
    int rc=0;

    return(rc);
}

void gtPuts(char cRow, char cCol, char attr, char *str, int len)
{
    VioWrtCharStrAtt(str, (USHORT) len, (USHORT) cRow, (USHORT) cCol, (PBYTE) &attr, 0);
}

void gtGetText(char cTop, char cLeft, char cBottom, char cRight, char *dest)
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

void gtPutText(char cTop, char cLeft, char cBottom, char cRight, char *srce)
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

void gtSetAttribute( char cTop, char cLeft, char cBottom, cRight, char attribute )
{
}
