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

void gtInit(void)
{
}

void gtDone(void)
{
}

char gtGetScreenWidth(void)
{
    return (char)0;
}

char gtGetScreenHeight(void)
{
    return (char)0;
}

void gtGotoXY(char x, char y)
{
}

char gtWhereX(void)
{
}

char gtWhereY(void)
{
}

void gtSetCursorStyle(int style)
{
}

int gtGetCursorStyle(void)
{
    return(0);
}

void gtPuts(char x, char y, char attr, char *str, int len)
{
}

void gtGetText(char x1, char y1, char x2, char y2, char *dest)
{
}

void gtPutText(char x1, char y1, char x2, char y2, char *srce)
{
}

