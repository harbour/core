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
#include <gtapi.h>

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

void gtGotoXY(char x, char y)
{
    VioSetCurPos((USHORT) y, (USHORT) x, 0);
}

char gtWhereX(void)
{
    USHORT x, y;
    VioGetCurPos(&y, &x, 0);
    return x;
}

char gtWhereY()
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

void gtPuts(char x, char y, char attr, char *str, int len)
{
    VioWrtCharStrAtt(str, (USHORT) len, (USHORT) y, (USHORT) x, (PBYTE) &attr, 0);
}

void gtGetText(char x1, char y1, char x2, char y2, char *dest)
{
    USHORT width;
    char y;
    width = (USHORT) ((x2 - x1 + 1) * 2);
    for (y = y1; y <= y2; y++)
    {
        VioReadCellStr((PBYTE) dest, &width, (USHORT) y, (USHORT) x1, 0);
        dest += width;
    }
}

void gtPutText(char x1, char y1, char x2, char y2, char *srce)
{
    USHORT width;
    char y;
    width = (USHORT) ((x2 - x1 + 1) * 2);
    for (y = y1; y <= y2; y++)
    {
        VioWrtCellStr((PBYTE) srce, width, (USHORT) y, (USHORT) x1, 0);
        srce += width;
    }
}
