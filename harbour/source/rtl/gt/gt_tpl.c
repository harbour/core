/*
 * $Id$
 */

/*
 *  GT_TPL.C: Video subsystem template.
 *
 *  User programs should never call this layer directly!
 */

/* TODO: include any standard headers here */

#include "gtapi.h"

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
    /* TODO: How to detect this? */
   return 1;
}

char hb_gt_GetScreenWidth(void)
{
   /* TODO: How many columns on screen? */
    return (char)0;
}

char hb_gt_GetScreenHeight(void)
{
   /* TODO: How many rows on screen? */
    return (char)0;
}

void hb_gt_SetPos(char x, char y)
{
   /* TODO: How to reposition the cursor? */
}

char hb_gt_Col(void)
{
   /* TODO: What Column is the cursor on? */
   return 0;
}

char hb_gt_Row(void)
{
   /* TODO: What Row is the cursor on? */
   return 0;
}

static void hb_gt_GetCursorSize(char *start, char *end)
{
   /* TODO: if your system supports the concept of cursor scan lines, 
            fill this in - otherwise, you need some way to detect the
            size of the current screen cursor. */
    *start = 0;
    *end = 0;
}

int hb_gt_GetCursorStyle(void)
{
   /* TODO: What shape is the cursor? */
    int rc=0;
/*
    char start, end
    if ( !visible )
    {
        rc=SC_NONE;
    }
    else
    {
*/
        /* example from the dos driver */
/*
        hb_gt_GetCursorSize( &start, &end )
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
    }
*/
    return(rc);
}

void hb_gt_SetCursorStyle(int style)
{
   /* TODO: How to set the shape of the cursor? */
   /* see ..\..\..\tests\working\cursrtst.prg for an explanation */
    switch(style)
    {
    case SC_NONE:
        /* TODO: turn it off */
        break;

    case SC_NORMAL:
        break;

    case SC_INSERT:
        break;

    case SC_SPECIAL1:
        break;

    case SC_SPECIAL2:
        break;

    default:
        break;
    }
}

void hb_gt_Puts(char x, char y, char attr, char *str, int len)
{
}

void hb_gt_GetText(char x1, char y1, char x2, char y2, char *dest)
{
}

void hb_gt_PutText(char x1, char y1, char x2, char y2, char *srce)
{
}

void hb_gt_SetAttribute( char cTop, char cLeft, char cBottom, char cRight, char attribute )
{
   /* TODO: we want to take a screen that is say bright white on blue,
            and change the attributes only for a section of the screen
            to white on black.
   */
}

void hb_gt_DrawShadow( char cTop, char cLeft, char cBottom, char cRight, char attribute )
{
   /* TODO: similar to above - see gtwin.c for an idea */
}

void hb_gt_Scroll( char cTop, char cLeft, char cBottom, char cRight, char attribute, char vert, char horiz )
{
}

void hb_gt_DispBegin(void)
{
   /* TODO: Is there a way to change screen buffers?
            ie: can we write somewhere without it going to the screen
            and then update the screen from this buffer at a later time?
            We will initially want to copy the current screen to this buffer.
   */
}

void hb_gt_DispEnd()
{
   /* TODO: here we flush the buffer, and restore normal screen writes */
}

void hb_gt_SetMode( USHORT uiRows, USHORT uiCols )
{
   /* TODO: How to change the size of the screen? */
}

void hb_gt_Replicate( char c, DWORD nLength )
{
   /* TODO: this will write character c nlength times to the screen.
            Note that it is not used yet
            If there is no native function that supports this, it is
            already handled in a generic way by higher level functions.
   */

}

BOOL hb_gt_GetBlink()
{
   /* TODO: under dos, the background 'intensity' bit can be switched 
            from intensity to 'blinking'
            does this work under your platform?
   */
   return FALSE;
}

void hb_gt_SetBlink( BOOL bBlink )
{
   /* TODO: set the bit if it's supported */
}
