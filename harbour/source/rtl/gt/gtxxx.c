/*
 * $Id$
 */

/*
 *  GTXXX.C: Video subsystem template.
 *
 *  This module is based on VIDMGR by Andrew Clarke and modified for
 *  the Harbour project
 */

/* TODO: include any standard headers here */

#include "gtapi.h"

void hb_gt_Init(void)
{
}

void hb_gt_Done(void)
{
}

char hb_gt_GetScreenWidth(void)
{
    return (char)0;
}

char hb_gt_GetScreenHeight(void)
{
    return (char)0;
}

void hb_gt_GotoXY(char x, char y)
{
}

char hb_gt_WhereX(void)
{
}

char hb_gt_WhereY(void)
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

