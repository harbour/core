/*
 * $Id$
 */

/*
 *  GTOS2.C: Video subsystem for OS/2 compilers.
 *
 *  This module is partially based on VIDMGR by Andrew Clarke and modified
 *  for the Harbour project
 *
 *  User programs should never call this layer directly!
 */

#define INCL_KBD
#define INCL_VIO
#define INCL_DOSPROCESS

#include <string.h>
#include <os2.h>
#include "gtapi.h"

static char hb_gt_GetCellSize(void);
static void hb_gt_SetCursorSize(char start, char end, int visible);
static void hb_gt_GetCursorSize(char *start, char *end);

void hb_gt_Init(void)
{
  /* TODO: Is anything required to initialize the video subsystem? */
}

void hb_gt_Done(void)
{
  /* TODO: */
}

int hb_gt_IsColor(void)
{
/* Chen Kedem <niki@actcom.co.il> */
    VIOMODEINFO vi;

    vi.cb = sizeof(VIOMODEINFO);
    VioGetMode(&vi, 0);
    return (vi.fbType);        /* 0 = monochrom-compatible mode */
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
    return y;
}

char hb_gt_Col(void)
{
    USHORT x, y;

    VioGetCurPos(&y, &x, 0);
    return x;
}


void hb_gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, char attribute, SHORT sVert, SHORT sHoriz )
{
/* Chen Kedem <niki@actcom.co.il> */

    BYTE bCell[ 2 ];                            /* character/attribute pair */

    bCell [ 0 ] = ' ';
    bCell [ 1 ] = (BYTE)attribute;
    if ( (sVert | sHoriz) == 0 )                /* both zero, clear region */
        VioScrollUp ( usTop, usLeft, usBottom, usRight, 0xFFFF, bCell, 0 );
    else
    {
        if ( sVert > 0 )                        /* scroll up */
            VioScrollUp ( usTop, usLeft, usBottom, usRight, sVert, bCell, 0 );
        else if ( sVert < 0 )                   /* scroll down */
            VioScrollDn ( usTop, usLeft, usBottom, usRight, -sVert, bCell, 0 );

        if ( sHoriz > 0 )                       /* scroll left */
            VioScrollLf ( usTop, usLeft, usBottom, usRight, sHoriz, bCell, 0 );
        else if ( sHoriz < 0 )                  /* scroll right */
            VioScrollRt ( usTop, usLeft, usBottom, usRight, -sHoriz, bCell, 0 );
    }
}

/* QUESTION: not been used, do we need this function ? */
/* Answer: In the dos version, this gets called by hb_gt_GetCursorStyle()
   as that function is written below, we don't need this */

/*
static void hb_gt_GetCursorSize(char *start, char *end)
{
    VIOCURSORINFO vi;

    VioGetCurType(&vi, 0);
    *start = vi.yStart;
    *end = vi.cEnd;
}
*/

static void hb_gt_SetCursorSize(char start, char end, int visible)
{
/* Chen Kedem <niki@actcom.co.il> */
    VIOCURSORINFO vi;

    vi.yStart = start;
    vi.cEnd = end;
    vi.cx = 0;
    vi.attr = ( visible ? 0 : -1 );
    VioSetCurType(&vi, 0);
}

static char hb_gt_GetCellSize()
{
/* Chen Kedem <niki@actcom.co.il> */
    char rc ;

    VIOMODEINFO vi;
    vi.cb = sizeof(VIOMODEINFO);
    VioGetMode(&vi, 0);
    rc = (char)(vi.row ? (vi.vres / vi.row)-1 : 0 );
    return rc;
}

int hb_gt_GetCursorStyle(void)
{
/* Chen Kedem <niki@actcom.co.il> */
    int rc;
    char cellsize;
    VIOCURSORINFO vi;

    VioGetCurType(&vi, 0);

    if ( vi.attr )
    {
        rc=SC_NONE;
    }
    else
    {
        cellsize = hb_gt_GetCellSize();

        if ( vi.yStart == 0 && vi.cEnd == 0 )
        {
            rc=SC_NONE;
        }
        else if ( ( vi.yStart == cellsize-1 || vi.yStart == cellsize-2 ) && vi.cEnd == cellsize )
        {
            rc=SC_NORMAL;
        }
        else if ( vi.yStart == cellsize/2 && vi.cEnd == cellsize )
        {
            rc=SC_INSERT;
        }
        else if ( vi.yStart == 0 && vi.cEnd == cellsize )
        {
            rc=SC_SPECIAL1;
        }
        else if ( vi.yStart == 0 && vi.cEnd == cellsize/2 )
        {
            rc=SC_SPECIAL2;
        }
        else
        {
            rc=SC_NONE;
        }
    }

    return(rc);
}

void hb_gt_SetCursorStyle(int style)
{
/* Chen Kedem <niki@actcom.co.il> */
    char cellsize;
    VIOCURSORINFO vi;

    cellsize = hb_gt_GetCellSize();
    switch(style)
    {
    case SC_NONE:
        hb_gt_SetCursorSize( 0, 0, 0);
        break;

    case SC_NORMAL:
        hb_gt_SetCursorSize(cellsize-1, cellsize, 1);
        break;

    case SC_INSERT:
        hb_gt_SetCursorSize(cellsize/2, cellsize, 1);
        break;

    case SC_SPECIAL1:
        hb_gt_SetCursorSize(0, cellsize, 1);
        break;

    case SC_SPECIAL2:
        hb_gt_SetCursorSize(0, cellsize/2, 1);
        break;

    default:
        break;
    }
}

void hb_gt_Puts(char cRow, char cCol, char attr, char *str, int len)
{
    VioWrtCharStrAtt(str, (USHORT) len, (USHORT) cRow, (USHORT) cCol, (BYTE *) &attr, 0);
}

void hb_gt_GetText(char cTop, char cLeft, char cBottom, char cRight, char *dest)
{
    USHORT width;
    char y;

    width = (USHORT) ((cRight - cLeft + 1) * 2);
    for (y = cTop; y <= cBottom; y++)
    {
        VioReadCellStr((BYTE *) dest, &width, (USHORT) y, (USHORT) cLeft, 0);
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
        VioWrtCellStr((BYTE *) srce, width, (USHORT) y, (USHORT) cLeft, 0);
        srce += width;
    }
}

void hb_gt_SetAttribute( char cTop, char cLeft, char cBottom, char cRight, char attribute )
{
/* Chen Kedem <niki@actcom.co.il> */
   /*
      TODO: work with DispBegin DispEnd
      NOTE: type of attribute should be change from char to unsigned char to
            allow the >127 attributes (sames goes for hb_gt_DrawShadow)
   */

    USHORT width;
    char y;

   /*
      assume top level check that coordinate are all valid and fall
      within visible screen, else if width cannot be fit on current line
      it is going to warp to the next line
   */
    width = (USHORT) (cRight - cLeft + 1);
    for (y = cTop; y <= cBottom; y++)
        VioWrtNAttr( &attribute, width, y, (USHORT) cLeft, 0);
}

void hb_gt_DrawShadow( char cTop, char cLeft, char cBottom, char cRight, char attribute )
{
/* Chen Kedem <niki@actcom.co.il> */

    hb_gt_SetAttribute( cBottom+1, cLeft+1, cBottom+1, cRight+1, attribute );
    hb_gt_SetAttribute( cTop+1, cRight+1, cBottom+1, cRight+1, attribute );
}

void hb_gt_DispBegin(void)
{
   /* TODO: Is there a way to change screen buffers?
            ie: can we write somewhere without it going to the screen
            and then update the screen from this buffer at a later time?
            We will initially want to copy the current screen to this buffer.
   */
}

void hb_gt_DispEnd(void)
{
   /* TODO: here we flush the buffer, and restore normal screen writes */
}

BOOL hb_gt_SetMode( USHORT uiRows, USHORT uiCols )
{
    VIOMODEINFO vi;

    VioGetMode(&vi, 0);        /* fill structure with current settings */
    vi.row = uiRows;
    vi.col = uiCols;
    return (BOOL)VioSetMode(&vi, 0);   /* 0 = Ok, other = Fail */
}

void hb_gt_Replicate(char c, ULONG nLength)
{
   /* TODO: this will write character c nlength times to the screen.
            Note that it is not used yet
            If there is no native function that supports this, it is
            already handled in a generic way by higher level functions.
   */

   c= ' ';
   nLength = 0;

}

BOOL hb_gt_GetBlink()
{
/* Chen Kedem <niki@actcom.co.il> */
   VIOINTENSITY vi;

   vi.cb   = sizeof(VIOINTENSITY);      /* 6                          */
   vi.type = 2;                         /* get intensity/blink toggle */
   VioGetState(&vi, 0);
   return (vi.fs == 0);                 /* 0 = blink, 1 = intens      */
}

void hb_gt_SetBlink( BOOL bBlink )
{
/* Chen Kedem <niki@actcom.co.il> */
   VIOINTENSITY vi;

   vi.cb   = sizeof(VIOINTENSITY);      /* 6                          */
   vi.type = 2;                         /* set intensity/blink toggle */
   vi.fs   = (bBlink ? 0 : 1);          /* 0 = blink, 1 = intens      */
   VioSetState(&vi, 0);
}
