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
#include <gtapi.h>

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

static void gtxGetXY(char x, char y, char *attr, char *ch);
static void gtxPutch(char x, char y, char attr, char ch);

static int gtIsColor(void);
static char gtGetScreenMode(void);
static void gtSetCursorSize(char start, char end);
static void gtGetCursorSize(char *start, char *end);

void gtInit(void)
{
}

void gtDone(void)
{
}

static int gtIsColor(void)
{
    return gtGetScreenMode() != 7;
}

#if defined(__WATCOMC__) && defined(__386__)

char *gtScreenPtr(char x, char y)
{
    char *ptr;
    if (gtIsColor())
    {
        ptr = (char *)(0xB800 << 4);
    }
    else
    {
        ptr = (char *)(0xB000 << 4);
    }
    return ptr + (y * gtGetScreenWidth() * 2) + (x * 2);
}

#else

char FAR *gtScreenPtr(char x, char y)
{
    char FAR *ptr;
    if (gtIsColor())
    {
        ptr = (char FAR *)MK_FP(0xB800, 0x0000);
    }
    else
    {
        ptr = (char FAR *)MK_FP(0xB000, 0x0000);
    }
    return ptr + (y * gtGetScreenWidth() * 2) + (x * 2);
}

#endif

static char gtGetScreenMode(void)
{
#if defined(__WATCOMC__) && defined(__386__)
    return *((char *)0x0449);
#else
    return *((char FAR *)MK_FP(0x0040, 0x0049));
#endif
}

char gtGetScreenWidth(void)
{
#if defined(__WATCOMC__) && defined(__386__)
    return *((char *)0x044a);
#else
    return *((char FAR *)MK_FP(0x0040, 0x004a));
#endif
}

char gtGetScreenHeight(void)
{
#if defined(__WATCOMC__) && defined(__386__)
    return (char)(*((char *)0x0484) + 1);
#else
    return (char)(*((char FAR *)MK_FP(0x0040, 0x0084)) + 1);
#endif
}

void gtGotoXY(char x, char y)
{
#if defined(__TURBOC__)
    _AH = 0x02;
    _BH = 0;
    _DH = y;
    _DL = x;
    geninterrupt(0x10);
#else
    union REGS regs;
    regs.h.ah = 0x02;
    regs.h.bh = 0;
    regs.h.dh = (unsigned char)(y);
    regs.h.dl = (unsigned char)(x);
#if defined(__WATCOMC__) && defined(__386__)
    int386(0x10, &regs, &regs);
#else
    int86(0x10, &regs, &regs);
#endif
#endif
}

static void gtSetCursorSize(char start, char end)
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

static void gtGetCursorSize(char *start, char *end)
{
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

int gtGetCursorStyle(void)
{
    char start, end;
    int rc;

    gtGetCursorSize(&start, &end);

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

void gtSetCursorStyle(int style)
{
    switch(style)
    {
    case SC_NONE:
        gtSetCursorSize(32, 32);
        break;

    case SC_NORMAL:
        gtSetCursorSize(6, 7);
        break;

    case SC_INSERT:
        gtSetCursorSize(4, 7);
        break;

    case SC_SPECIAL1:
        gtSetCursorSize(0, 7);
        break;

    case SC_SPECIAL2:
        gtSetCursorSize(0, 3);
        break;

    default:
        break;
    }
}

static void gtxGetXY(char x, char y, char *attr, char *ch)
{
    char FAR *p;
    p = gtScreenPtr((char)(x), (char)(y));
    *ch = *p;
    *attr = *(p + 1);
}

void gtxPutch(char x, char y, char attr, char ch)
{
    char FAR *p;
    p = gtScreenPtr((char)(x), (char)(y));
    *p = ch;
    *(p + 1) = attr;
}

void gtPuts(char x, char y, char attr, char *str, int len)
{
    char FAR *p;
    int i;

    p = gtScreenPtr((char)(x), (char)(y));
    for(i=0; i<len; i++)
    {
        *p++ = *str++;
        *p++ = attr;
    }
}

void gtGetText(char x1, char y1, char x2, char y2, char *dest)
{
    char x, y;
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            gtxGetXY(x, y, dest + 1, dest);
            dest += 2;
        }
    }
}

void gtPutText(char x1, char y1, char x2, char y2, char *srce)
{
    char x, y;
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            gtxPutch(x, y, *(srce + 1), *srce);
            srce += 2;
        }
    }
}

char gtWhereX(void)
{
#if defined(__TURBOC__)
    _AH = 0x03;
    _BH = 0;
    geninterrupt(0x10);
    return _DL;
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

char gtWhereY(void)
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
