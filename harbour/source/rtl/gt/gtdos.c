/*
 * $Id$
 */

/*
 *  GTDOS.C: Video subsystem for DOS compilers.
 *
 *  This module is based on VIDMGR by Andrew Clarke and modified for
 *  the Harbour project
 *
 *  User programs should never call this layer directly!
 */

#include <string.h>
#include <dos.h>
#include "gtapi.h"

#if defined(__POWERC) || (defined(__TURBOC__) && !defined(__BORLANDC__)) || \
  (defined(__ZTC__) && !defined(__SC__))
  #define FAR far
#elif (defined(__MSDOS__) || defined(MSDOS) || defined(DOS)) && !defined(__DJGPP__)
  #define FAR _far
#else
  #define FAR
#endif

#ifdef __DJGPP__
  #include <conio.h>
  #include <sys/farptr.h>
#else
  #ifndef MK_FP
    #define MK_FP(seg,off) \
      ((void FAR *)(((unsigned long)(seg) << 16)|(unsigned)(off)))
  #endif
#endif

static void hb_gt_xGetXY(char cRow, char cCol, char *attr, char *ch);
static void hb_gt_xPutch(char cRow, char cCol, char attr, char ch);

static char hb_gt_GetScreenMode(void);
static void hb_gt_SetCursorSize(char start, char end);
static void hb_gt_GetCursorSize(char *start, char *end);

#if defined(__WATCOMC__) && defined(__386__)
  #define FAR
#endif
#ifndef __DJGPP__
  static char FAR *scrnPtr;
  static char FAR *scrnStealth;
  static char FAR *hb_gt_ScreenAddress(void);
#endif

void hb_gt_Init(void)
{
#ifdef __DJGPP__
   gppconio_init();
#else
   scrnStealth = (char *)-1;
   scrnPtr = hb_gt_ScreenAddress();
#endif
}

void hb_gt_Done(void)
{
}

int hb_gt_IsColor(void)
{
    return hb_gt_GetScreenMode() != 7;
}

#ifndef __DJGPP__
static char FAR *hb_gt_ScreenAddress()
{
    char FAR *ptr;

  #if defined(__WATCOMC__) && defined(__386__)
    if (hb_gt_IsColor())
    {
        ptr = (char *)(0xB800 << 4);
    }
    else
    {
        ptr = (char *)(0xB000 << 4);
    }
  #else
    if (hb_gt_IsColor())
    {
        ptr = (char FAR *)MK_FP(0xB800, 0x0000);
    }
    else
    {
        ptr = (char FAR *)MK_FP(0xB000, 0x0000);
    }
  #endif

    return ptr;
}
#endif

#ifndef __DJGPP__
char FAR *hb_gt_ScreenPtr(char cRow, char cCol)
{
    return scrnPtr + (cRow * hb_gt_GetScreenWidth() * 2) + (cCol * 2);
}
#endif

static char hb_gt_GetScreenMode(void)
{
#if defined(__WATCOMC__) && defined(__386__)
    return *((char *)0x0449);
#elif defined(__DJGPP__)
    return _farpeekb( 0x0040, 0x0049 );
#else
    return *((char FAR *)MK_FP(0x0040, 0x0049));
#endif
}

char hb_gt_GetScreenWidth(void)
{
#if defined(__WATCOMC__) && defined(__386__)
    return *((char *)0x044a);
#elif defined(__DJGPP__)
    return _farpeekb( 0x0040, 0x004a );
#else
    return *((char FAR *)MK_FP(0x0040, 0x004a));
#endif
}

char hb_gt_GetScreenHeight(void)
{
#if defined(__WATCOMC__) && defined(__386__)
    return (char)(*((char *)0x0484) + 1);
#elif defined(__DJGPP__)
    return _farpeekb( 0x0040, 0x0084 ) + 1;
#else
    return (char)(*((char FAR *)MK_FP(0x0040, 0x0084)) + 1);
#endif
}

void hb_gt_SetPos(char cRow, char cCol)
{
#if defined(__TURBOC__)
    _AH = 0x02;
    _BH = 0;
    _DH = cRow;
    _DL = cCol;
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
#ifdef __DJGPP__
    short ch_attr;
    gettext( cCol + 1, cRow + 1, cCol + 1, cRow + 1, &ch_attr );
    *ch = ch_attr >> 8;
    *attr = ch_attr & 0xFF;
///printf("\r\nhb_gt_xGetXY(): row == %d, col = %d, char = %d, attr = %d", cRow, cCol, *ch, *attr );
#else
    char FAR *p;
    p = hb_gt_ScreenPtr(cRow, cCol);
    *ch = *p;
    *attr = *(p + 1);
#endif
}

void hb_gt_xPutch(char cRow, char cCol, char attr, char ch)
{
#ifdef __DJGPP__
    long ch_attr = ( ch << 8 ) | attr;
///printf("\r\nhb_gt_xPutch(): row == %d, col = %d, char = %d, attr = %d", cRow, cCol, ch, attr );
    puttext( cCol + 1, cRow + 1, cCol + 1, cRow + 1, &ch_attr );
#else
    char FAR *p;
    p = hb_gt_ScreenPtr(cRow, cCol);
    *p = ch;
    *(p + 1) = attr;
#endif
}

void hb_gt_Puts(char cRow, char cCol, char attr, char *str, int len)
{
#ifdef __DJGPP__
    int i = len;
    int bottom, left = cCol, right, top = cRow;
    int width = hb_gt_GetScreenWidth();
    char * ch_attr, * ptr;
///printf("\r\nhb_gt_Puts(): row == %d, col = %d, attr = %d, len = %d", cRow, cCol, attr, len );
    ptr = ch_attr = hb_xgrab( i * 2 );
    while( i-- )
    {
///printf("+");
       *ptr++ = *str++;
///printf("-");
       *ptr++ = attr;
    }
    i = len - 1; /* We want end position, not next cursor position */
    right = left;
    bottom = top;
    if (right + i > width - 1)
    {
       /* Calculate end row position and the remainder size for the end column adjust */
       bottom += (i / width);
       i = i % width;
    }
    right += i;
    if (right > width - 1)
    {
       /* Column movement overflows into next row */
       bottom++;
       right -= width;
    }
///printf("\r\nhb_gt_Puts(): puttext( %d,%d, %d,%d )", left + 1, top + 1, right + 1, bottom + 1 );
    puttext( left + 1, top + 1, right + 1, bottom + 1, ch_attr );
    hb_xfree( ch_attr );
#else
    char FAR *p;
    int i;
    p = hb_gt_ScreenPtr( cRow, cCol );
    for(i=0; i<len; i++)
    {
        *p++ = *str++;
        *p++ = attr;
    }
#endif
}

void hb_gt_GetText(char cTop, char cLeft, char cBottom, char cRight, char *dest)
{
#ifdef __DJGPP__
    gettext( cLeft + 1, cTop + 1, cRight + 1, cBottom + 1, dest );
#else
    char x, y;

    for (y = cTop; y <= cBottom; y++ )
    {
        for (x = cLeft; x <= cRight; x++)
        {
            hb_gt_xGetXY(y, x, dest + 1, dest);
            dest += 2;
        }
    }
#endif
}

void hb_gt_PutText(char cTop, char cLeft, char cBottom, char cRight, char *srce)
{
#ifdef __DJGPP__
    puttext( cLeft + 1, cTop + 1, cRight + 1, cBottom + 1, srce );
#else
    char x, y;

    for (y = cTop; y <= cBottom; y++)
    {
        for (x = cLeft; x <= cRight; x++)
        {
            hb_gt_xPutch(y, x, *(srce + 1), *srce);
            srce += 2;
        }
    }
#endif
}

void hb_gt_SetAttribute( char cTop, char cLeft, char cBottom, char cRight, char attribute )
{
    char x, y;
    char attr, ch;

    for (y = cTop; y <= cBottom; y++)
    {
        for (x = cLeft; x <= cRight; x++)
        {
            hb_gt_xGetXY( y, x, &attr, &ch );
            hb_gt_xPutch( y, x, attribute, ch);
        }
    }
}

void hb_gt_DrawShadow( char cTop, char cLeft, char cBottom, char cRight, char attribute )
{
    char x, y;
    char attr, ch;

    for (y = cTop; y <= cBottom; y++)
    {

        hb_gt_xGetXY( y, cRight, &attr, &ch );
        hb_gt_xPutch( y, cRight, attribute, ch);
       
        if( y == cBottom )
           for (x = cLeft; x <= cRight; x++)
           {
               hb_gt_xGetXY( y, x, &attr, &ch );
               hb_gt_xPutch( y, x, attribute, ch );
           }

    }
}

char hb_gt_Col(void)
{
#if defined(__TURBOC__)
    _AH = 0x03;
    _BH = 0;
    geninterrupt(0x10);
    return _DL;
#else
    union REGS regs;
    regs.h.ah = 0x03;
    regs.h.bh = 0;
#if defined(__WATCOMC__) && defined(__386__)
    int386(0x10, &regs, &regs);
#else
    int86(0x10, &regs, &regs);
#endif
    return regs.h.dl;
#endif
}

char hb_gt_Row(void)
{
#if defined(__TURBOC__)
    _AH = 0x03;
    _BH = 0;
    geninterrupt(0x10);
    return _DH;
#else
    union REGS regs;
    regs.h.ah = 0x03;
    regs.h.bh = 0;
#if defined(__WATCOMC__) && defined(__386__)
    int386(0x10, &regs, &regs);
#else
    int86(0x10, &regs, &regs);
#endif
    return regs.h.dh;
#endif
}

void hb_gt_Scroll( char cTop, char cLeft, char cBottom, char cRight, char attr, char vert, char horiz )
{
   /* Convert the "low-level" parameters back to the same types they
      had when the following code used to be in gtapi.c */
   USHORT uiTop = cTop, uiLeft = cLeft, uiBottom = cBottom, uiRight = cRight;
   int iRows = vert, iCols = horiz;
   /* End of parameter conversion */

   USHORT uiRow, uiCol, uiSize;
   int iLength = ( uiRight - uiLeft ) + 1;
   int iCount, iColOld, iColNew, iColSize;

   hb_gtGetPos( &uiRow, &uiCol );

   if( hb_gtRectSize( uiTop, uiLeft, uiBottom, uiRight, &uiSize ) == 0 )
   {
      char * fpBlank = ( char * ) hb_xgrab( iLength );
      char * fpBuff = ( char * ) hb_xgrab( iLength * 2 );
      if( fpBlank && fpBuff )
      {
         memset( fpBlank, ' ', iLength );

         iColOld = iColNew = uiLeft;
         if( iCols >= 0 )
         {
            iColOld += iCols;
            iColSize = uiRight - uiLeft;
            iColSize -= iCols;
         }
         else
         {
            iColNew -= iCols;
            iColSize = uiRight - uiLeft;
            iColSize += iCols;
         }

         for( iCount = ( iRows >= 0 ? uiTop : uiBottom );
              ( iRows >= 0 ? iCount <= uiBottom : iCount >= uiTop );
              ( iRows >= 0 ? iCount++ : iCount-- ) )
         {
            int iRowPos = iCount + iRows;

            /* Blank the scroll region in the current row */
            hb_gt_Puts( iCount, uiLeft, attr, fpBlank, iLength );

            if( ( iRows || iCols ) && iRowPos <= uiBottom && iRowPos >= uiTop )
            {
               /* Read the text to be scrolled into the current row */
               hb_gt_GetText( iRowPos, iColOld, iRowPos, iColOld + iColSize, fpBuff );

               /* Write the scrolled text to the current row */
               hb_gt_PutText ( iCount, iColNew, iCount, iColNew + iColSize, fpBuff );
            }
         }
      }
      if( fpBlank ) hb_xfree( fpBlank );
      if( fpBuff ) hb_xfree( fpBuff );
   }

   hb_gtSetPos( uiRow, uiCol );
}

void hb_gt_DispBegin(void)
{
/* ptucker */
#ifndef __DJGPP__
  if( hb_gtDispCount() == 1 )
  {
    char FAR *ptr;
    ULONG nSize;

    nSize = hb_gt_GetScreenWidth() * hb_gt_GetScreenHeight() * 2;

    ptr = scrnPtr;
    if( (scrnPtr = scrnStealth) == (char *)-1)
       scrnPtr = (char FAR *)hb_xgrab( nSize );
    scrnStealth = ptr;
    memcpy( (void *)scrnPtr, (void *)ptr, nSize );
  }
#endif
}

void hb_gt_DispEnd(void)
{
/* ptucker */
#ifndef __DJGPP__
  if( hb_gtDispCount() == 1 )
  {
    char FAR *ptr;
    ULONG nSize;

    nSize = hb_gt_GetScreenWidth() * hb_gt_GetScreenHeight() * 2;

    ptr = scrnPtr;
    scrnPtr = scrnStealth;
    scrnStealth = ptr;
    memcpy( (void *)scrnPtr, (void *)ptr, nSize );
  }
#endif
}

BOOL hb_gt_SetMode( USHORT uiRows, USHORT uiCols )
{
   uiRows=uiCols=0;
   return 0;
}

void hb_gt_Replicate(char c, DWORD nLength)
{
   c= ' ';
   nLength = 0;

}

BOOL hb_gt_GetBlink()
{
#if defined(__WATCOMC__) && defined(__386__)
    return *((char *)0x0465) & 0x10;
#elif defined(__DJGPP__)
    return _farpeekb( 0x0040, 0x0065 ) & 0x10;
#else
    return *((char FAR *)MK_FP(0x0040, 0x0065)) &0x10;
#endif
}

void hb_gt_SetBlink( BOOL bBlink )
{
#if defined(__TURBOC__)
    _AX = 0x1003;
    _BX = bBlink;
    geninterrupt(0x10);
    return;
#else
    union REGS regs;
    regs.h.ah = 0x10;
    regs.h.al = 0x03;
    regs.h.bh = 0;
    regs.h.bl = bBlink;
#if defined(__WATCOMC__) && defined(__386__)
    int386(0x10, &regs, &regs);
#else
    int86(0x10, &regs, &regs);
#endif
#endif
}
