/*
 * $Id$
 */

/*
 *  GTWIN.C: Video subsystem for Windows 95/NT compilers.
 *
 *  This module is based on VIDMGR by Andrew Clarke and modified for
 *  the Harbour project
 */

#include <stdlib.h>
#include <string.h>

#define WIN32_LEAN_AND_MEAN

#include <windows.h>
#include <gtapi.h>

#ifdef __CYGWIN32__
typedef WORD far *LPWORD;
#endif

static HANDLE HInput = INVALID_HANDLE_VALUE;
static HANDLE HOutput = INVALID_HANDLE_VALUE;
static unsigned long key_hit = 0xFFFFFFFFUL;

void gtInit(void)
{
    HInput = GetStdHandle(STD_INPUT_HANDLE);
    HOutput = GetStdHandle(STD_OUTPUT_HANDLE);
}

HARBOUR GTINIT( void )
{
   gtInit();
}

void gtDone(void)
{
    CloseHandle(HInput);
    HInput = INVALID_HANDLE_VALUE;
    CloseHandle(HOutput);
    HOutput = INVALID_HANDLE_VALUE;
}

char gtGetScreenWidth(void)
{
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    GetConsoleScreenBufferInfo(HOutput, &csbi);
    return (char)csbi.dwSize.X;
}

char gtGetScreenHeight(void)
{
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    GetConsoleScreenBufferInfo(HOutput, &csbi);
    return (char)csbi.dwSize.Y;
}

void gtGotoXY(char x, char y)
{
    COORD dwCursorPosition;
    dwCursorPosition.X = (SHORT) (x);
    dwCursorPosition.Y = (SHORT) (y);
    SetConsoleCursorPosition(HOutput, dwCursorPosition);
}

void gtSetCursorStyle(int style)
{
    CONSOLE_CURSOR_INFO cci;

    GetConsoleCursorInfo(HOutput, &cci);
    switch (style)
    {
    case 0:
        cci.bVisible = 0;
        SetConsoleCursorInfo(HOutput, &cci);
        break;

    case 1:
        cci.bVisible = 1;
        cci.dwSize = 12;
        SetConsoleCursorInfo(HOutput, &cci);
        break;

    case 2:
        cci.bVisible = 1;
        cci.dwSize = 99;
        SetConsoleCursorInfo(HOutput, &cci);
        break;

    case 3:
        cci.bVisible = 1;
        cci.dwSize = 49;
        SetConsoleCursorInfo(HOutput, &cci);
        break;

    default:
        break;
    }
}

int gtGetCursorStyle(void)
{
    CONSOLE_CURSOR_INFO cci;
    int rc;

    GetConsoleCursorInfo(HOutput, &cci);

    if(cci.bVisible)
    {
        rc=0;
    }
    else
    {
        switch(cci.dwSize)
        {
            case 12:
            rc=1;
            break;

            case 49:
            rc=2;
            break;

            case 99:
            rc=3;
            break;

            /* TODO: cannot tell if the block is upper or lower for cursor */
            default:
            rc=4;
        }
    }

    return(rc);
}

void gtPuts(char x, char y, char attr, char *str, int len)
{
    DWORD i, dwlen;
    COORD coord;
    LPWORD pwattr;
    pwattr = (LPWORD) malloc(strlen(str) * sizeof(*pwattr));
    if (!pwattr)
    {
        return;
    }
    coord.X = (DWORD) (x);
    coord.Y = (DWORD) (y);
    for (i = 0; i < strlen(str); i++)
    {
        *(pwattr + i) = attr;
    }
    WriteConsoleOutputCharacterA(HOutput, str, (DWORD) len, coord, &dwlen);
    WriteConsoleOutputAttribute(HOutput, pwattr, (DWORD) len, coord, &dwlen);
    free(pwattr);
}

void gtGetText(char x1, char y1, char x2, char y2, char *dest)
{
    DWORD i, len, width;
    COORD coord;
    LPWORD pwattr;
    char y, *pstr;
    width = (x2 - x1 + 1);
    pwattr = (LPWORD) malloc(strlen(str) * sizeof(*pwattr));
    if (!pwattr)
    {
        return;
    }
    pstr = (char *)malloc(width);
    if (!pstr)
    {
        free(pwattr);
        return;
    }
    for (y = y1; y <= y2; y++)
    {
        coord.X = (DWORD) (x1);
        coord.Y = (DWORD) (y);
        ReadConsoleOutputCharacterA(HOutput, pstr, width, coord, &len);
        ReadConsoleOutputAttribute(HOutput, pwattr, width, coord, &len);
        for (i = 0; i < width; i++)
        {
            *dest = *(pstr + i);
            dest++;
            *dest = (char)*(pwattr + i);
            dest++;
        }
    }
    free(pwattr);
    free(pstr);
}

void gtPutText(char x1, char y1, char x2, char y2, char *srce)
{
    DWORD i, len, width;
    COORD coord;
    LPWORD pwattr;
    char y, *pstr;
    width = (x2 - x1 + 1);
    pwattr = (LPWORD) malloc(strlen(str) * sizeof(*pwattr));
    if (!pwattr)
    {
        return;
    }
    pstr = (char *)malloc(width);
    if (!pstr)
    {
        free(pwattr);
        return;
    }
    for (y = y1; y <= y2; y++)
    {
        for (i = 0; i < width; i++)
        {
            *(pstr + i) = *srce;
            srce++;
            *(pwattr + i) = *srce;
            srce++;
        }
        coord.X = (DWORD) (x1);
        coord.Y = (DWORD) (y);
        WriteConsoleOutputCharacterA(HOutput, pstr, width, coord, &len);
        WriteConsoleOutputAttribute(HOutput, pwattr, width, coord, &len);
    }
    free(pwattr);
    free(pstr);
}

char gtWhereX(void)
{
    COORD dwCursorPosition;
    GetConsoleCursorPosition(HOutput, &dwCursorPosition);
    return dwCursorPosition.X;
}

char gtWhereY(void)
{
    COORD dwCursorPosition;
    GetConsoleCursorPosition(HOutput, &dwCursorPosition);
    return dwCursorPosition.Y;
}
