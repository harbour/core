/*
 * $Id$
 */

/*
 *  GT_TPL.C: Video subsystem template.
 *
 */

/* TODO: include any standard headers here */

#include "gtapi.h"

void hb_gt_Init(void)
{
}

void hb_gt_Done(void)
{
}

int hb_gt_IsColor(void)
{
   return 1;
}

char hb_gt_GetScreenWidth(void)
{
    return (char)0;
}

char hb_gt_GetScreenHeight(void)
{
    return (char)0;
}

void hb_gt_SetPos(char x, char y)
{
}

char hb_gt_Col(void)
{
}

char hb_gt_Row(void)
{
}

void hb_gt_SetCursorStyle(int style)
{
}

int hb_gt_GetCursorStyle(void)
{
    return(0);
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
}

void hb_gt_DrawShadow( char cTop, char cLeft, char cBottom, char cRight, char attribute )
{
}

void hb_gt_Scroll( char cTop, char cLeft, char cBottom, char cRight, char attribute, char vert, char horiz )
{
}

void hb_gt_DispBegin(void)
{
}

void hb_gt_DispEnd()
{
}

void hb_gt_SetMode( USHORT uiRows, USHORT uiCols )
{
}

void hb_gt_Replicate( char c, DWORD nLength )
{
}
