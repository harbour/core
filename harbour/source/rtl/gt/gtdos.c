/*
 * $Id$
 */

/*
 *  GTDOS.C: Video subsystem for DOS compilers.
 *
 *  This module is based on VIDMGR by Andrew Clarke and modified for
 *  the Harbour project
 */

#include <string.h>
#include <dos.h>
#include "gtapi.h"

#if defined(__POWERC) || (defined(__TURBOC__) && !defined(__BORLANDC__)) || \
  (defined(__ZTC__) && !defined(__SC__))
#define FAR far
#else
#if defined(__MSDOS__) || defined(MSDOS) || defined(DOS)
#define FAR _far
#else
#define FAR
#endif
#endif

#ifndef MK_FP
#define MK_FP(seg,off) \
  ((void FAR *)(((unsigned long)(seg) << 16)|(unsigned)(off)))
#endif

static void hb_gt_xGetXY(char cRow, char cCol, char *attr, char *ch);
static void hb_gt_xPutch(char cRow, char cCol, char attr, char ch);

static char hb_gt_GetScreenMode(void);
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
    return hb_gt_GetScreenMode() != 7;
}

#if defined(__WATCOMC__) && defined(__386__)

char *hb_gt_ScreenPtr(char cRow, char cCol)
{
    char *ptr;
    if (hb_gt_IsColor())
    {
        ptr = (char *)(0xB800 << 4);
    }
    else
    {
        ptr = (char *)(0xB000 << 4);
    }
    return ptr + (cRow * hb_gt_GetScreenWidth() * 2) + (cCol * 2);
}

#else

char FAR *hb_gt_ScreenPtr(char cRow, char cCol)
{
    char FAR *ptr;
    if (hb_gt_IsColor())
    {
        ptr = (char FAR *)MK_FP(0xB800, 0x0000);
    }
    else
    {
        ptr = (char FAR *)MK_FP(0xB000, 0x0000);
    }
    return ptr + (cRow * hb_gt_GetScreenWidth() * 2) + (cCol * 2);
}

#endif

static char hb_gt_GetScreenMode(void)
{
#if defined(__WATCOMC__) && defined(__386__)
    return *((char *)0x0449);
#else
    return *((char FAR *)MK_FP(0x0040, 0x0049));
#endif
}

char hb_gt_GetScreenWidth(void)
{
#if defined(__WATCOMC__) && defined(__386__)
    return *((char *)0x044a);
#else
    return *((char FAR *)MK_FP(0x0040, 0x004a));
#endif
}

char hb_gt_GetScreenHeight(void)
{
#if defined(__WATCOMC__) && defined(__386__)
    return (char)(*((char *)0x0484) + 1);
#else
    return (char)(*((char FAR *)MK_FP(0x0040, 0x0084)) + 1);
#endif
}

void hb_gt_SetPos(char cRow, char cCol)
{
#if defined(__TURBOC__)
    _AH = 0x02;
    _BH = 0;
    _DH = cRow
    _DL = cCol
    geninterrupt(0x10);
#else
    union REGS regs;
    regs.h.ah = 0x02;
    regs.h.bh = 0;
    regs.h.dh = (unsigned char)(cRow);
    regs.h.dl = (unsigned char)(cCol);
#if defined(__WATCOMC__) && defined(__386__)
    int386(0x10, &regs, &regs);
#else
    int86(0x10, &regs, &regs);
#endif
#endif
}

static void hb_gt_SetCursorSize(char start, char end)
{
#if defined(__TURBOC__)
    _AH = 0x01;
    _CH = start;
    _CL = end;
    geninterrupt(0x10);
#else
    union REGS regs;
    regs.h.ah = 0x01;
    regs.h.ch = start;
    regs.h.cl = end;
#if defined(__WATCOMC__) && defined(__386__)
    int386(0x10, &regs, &regs);
#else
    int86(0x10, &regs, &regs);
#endif
#endif
}

static void hb_gt_GetCursorSize(char *start, char *end)
{
   char _ch,_cl;
#if defined(__TURBOC__)
    _AH = 0x03;
    _BH = 0;
    geninterrupt(0x10);
    *start = _CH;
    *end = _CL;
#else
    union REGS regs;
    regs.h.ah = 0x03;
    regs.h.bh = 0;
#if defined(__WATCOMC__) && defined(__386__)
    int386(0x10, &regs, &regs);
#else
    int86(0x10, &regs, &regs);
#endif
    *start = regs.h.ch;
    *end = regs.h.cl;
#endif
}

int hb_gt_GetCursorStyle(void)
{
    char start, end;
    int rc;

    hb_gt_GetCursorSize(&start, &end);

    if((start == 32) && (end == 32))
    {
        rc=SC_NONE;
    }
    else if((start == 6) && (end == 7))
    {
        rc=SC_NORMAL;
    }
    else if((start == 4) && (end == 7))
    {
        rc=SC_INSERT;
    }
    else if((start == 0) && (end == 7))
    {
        rc=SC_SPECIAL1;
    }
    else if((start == 0) && (end == 3))
    {
        rc=SC_SPECIAL2;
    }
    else
    {
        rc=SC_NONE;
    }

    return(rc);
}

void hb_gt_SetCursorStyle(int style)
{
    switch(style)
    {
    case SC_NONE:
        hb_gt_SetCursorSize(32, 32);
        break;

    case SC_NORMAL:
        hb_gt_SetCursorSize(6, 7);
        break;

    case SC_INSERT:
        hb_gt_SetCursorSize(4, 7);
        break;

    case SC_SPECIAL1:
        hb_gt_SetCursorSize(0, 7);
        break;

    case SC_SPECIAL2:
        hb_gt_SetCursorSize(0, 3);
        break;

    default:
        break;
    }
}

static void hb_gt_xGetXY(char cRow, char cCol, char *attr, char *ch)
{
    char FAR *p;
    p = hb_gt_ScreenPtr(cRow, cCol);
    *ch = *p;
    *attr = *(p + 1);
}

void hb_gt_xPutch(char cRow, char cCol, char attr, char ch)
{
    char FAR *p;
    p = hb_gt_ScreenPtr(cRow, cCol);
    *p = ch;
    *(p + 1) = attr;
}

void hb_gt_Puts(char cRow, char cCol, char attr, char *str, int len)
{
    char FAR *p;
    int i;

    p = hb_gt_ScreenPtr( cRow, cCol );
    for(i=0; i<len; i++)
    {
        *p++ = *str++;
        *p++ = attr;
    }
}

void hb_gt_GetText(char cTop, char cLeft, char cBottom, char right, char *dest)
{
    char x, y;
    for (y = cTop; y <= cBottom, y++ )
    {
        for (x = cLeft; x <= cRight; x++)
        {
            hb_gt_xGetXY(y, x, dest + 1, dest);
            dest += 2;
        }
    }
}

void hb_gt_PutText(char cTop, char cLeft, char cBottom, char cRight, char *srce)
{
    char x, y;
    for (y = cTop; y <= cBottom; y++)
    {
        for (x = cLeft; x <= cRight; x++)
        {
            hb_gt_xPutch(y, x, *(srce + 1), *srce);
            srce += 2;
        }
    }
}

void SetAttribute( char cTop, char cLeft, char cBottom, char cRight, char attribute )
{
}

void hb_gt_DrawShadow( char cTop, char cLeft, char cBottom, char cRight, char attribute )
{
}

  /* returns col */
char hb_gt_Col(void)
{
#if defined(__TURBOC__)
    _AH = 0x03;
    _BH = 0;
    geninterrupt(0x10);
    return _DH;
#else
    union REGS regs;
    regs.h.ah = 0x02;
    regs.h.bh = 0;
#if defined(__WATCOMC__) && defined(__386__)
    int386(0x10, &regs, &regs);
#else
    int86(0x10, &regs, &regs);
#endif
    return regs.h.dl;
#endif
}

  /* returns row */
char hb_gt_Row(void)
{
#if defined(__TURBOC__)
    _AH = 0x03;
    _BH = 0;
    geninterrupt(0x10);
    return _DH;
#else
    union REGS regs;
    regs.h.ah = 0x02;
    regs.h.bh = 0;
#if defined(__WATCOMC__) && defined(__386__)
    int386(0x10, &regs, &regs);
#else
    int86(0x10, &regs, &regs);
#endif
    return regs.h.dh;
#endif
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
