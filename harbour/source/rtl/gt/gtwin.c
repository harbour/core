/*
 * $Id$
 */

/*
 *  GTWIN.C: Video subsystem for Windows 95/NT compilers.
 *
 *  This module is based (somewhat) on VIDMGR by
 *   Andrew Clarke and modified for the Harbour project
 */

#include <stdlib.h>
#include <string.h>

#define WIN32_LEAN_AND_MEAN

#if defined(__GNUC__)
#define HB_DONT_DEFINE_BASIC_TYPES
#endif /* __GNUC__ */

#include <windows.h>
#include "gtapi.h"

#if ! defined(__GNUC__)
#ifdef __CYGWIN32__
typedef WORD far *LPWORD;
#endif
#endif /* __GNUC__ */

static HANDLE HInput = INVALID_HANDLE_VALUE;
static HANDLE HOutput = INVALID_HANDLE_VALUE;

#define HB_LOG 0

#if (defined(HB_LOG) && (HB_LOG != 0))
static FILE* flog = 0;
int line = 0;
#define LOG(x) \
do { \
  flog = fopen("c:/tmp/gt.log", "a"); \
  fprintf(flog, "%5d> GT: %s\n", line++, x); \
  fflush(flog); \
  fclose(flog); \
} while (0)
#else
#define LOG(x)
#endif /* #if (defined(HB_LOG) && (HB_LOG != 0)) */

void gtInit(void)
{
  LOG("Initializing");
  HInput = GetStdHandle(STD_INPUT_HANDLE);
  HOutput = GetStdHandle(STD_OUTPUT_HANDLE);
}

/* TODO: this can very likely be removed */
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
  LOG("Ending");
}

char gtGetScreenWidth(void)
{
  CONSOLE_SCREEN_BUFFER_INFO csbi;

  LOG("GetScreenWidth");
  GetConsoleScreenBufferInfo(HOutput, &csbi);
  return (char)csbi.dwSize.X;
}

char gtGetScreenHeight(void)
{
  CONSOLE_SCREEN_BUFFER_INFO csbi;

  LOG("GetScreenHeight");
  GetConsoleScreenBufferInfo(HOutput, &csbi);
  return (char)csbi.dwSize.Y;
}

void gtGotoXY(char x, char y)
{
  COORD dwCursorPosition;

  LOG("GotoXY");
  dwCursorPosition.X = (SHORT) (x);
  dwCursorPosition.Y = (SHORT) (y);
  LOG(".. Calling SetConsoleCursorPosition()");
  SetConsoleCursorPosition(HOutput, dwCursorPosition);
  LOG("..  Called SetConsoleCursorPosition()");
}

void gtSetCursorStyle(int style)
{
  CONSOLE_CURSOR_INFO cci;

  LOG("SetCursorStyle");
  GetConsoleCursorInfo(HOutput, &cci);
  switch (style)
    {
    case SC_NONE:
      cci.bVisible = 0;
      SetConsoleCursorInfo(HOutput, &cci);
      break;

    case SC_NORMAL:
      cci.bVisible = 1;
      cci.dwSize = 12;
      SetConsoleCursorInfo(HOutput, &cci);
      break;

    case SC_INSERT:
      cci.bVisible = 1;
      cci.dwSize = 99;
      SetConsoleCursorInfo(HOutput, &cci);
      break;

    case SC_SPECIAL1:
      cci.bVisible = 1;
      cci.dwSize = 49;
      SetConsoleCursorInfo(HOutput, &cci);
      break;

    case SC_SPECIAL2:
      /* TODO: Why wasn't this implemented ? */
      break;

    default:
      break;
    }
}

int gtGetCursorStyle(void)
{
  CONSOLE_CURSOR_INFO cci;
  int rc;

  LOG("GetCursorStyle");
  GetConsoleCursorInfo(HOutput, &cci);

  if(cci.bVisible)
    {
      rc=SC_NORMAL;
    }
  else
    {
      switch(cci.dwSize)
        {
        case 12:
	  rc=SC_NORMAL;
	  break;

        case 99:
	  rc=SC_INSERT;
	  break;

        case 49:
	  rc=SC_SPECIAL1;
	  break;

	  /* TODO: cannot tell if the block is upper or lower for cursor */
        default:
	  rc=SC_SPECIAL2;
        }
    }

  return(rc);
}

void gtPuts(char x, char y, char attr, char *str, int len)
{
  DWORD i, dwlen;
  COORD coord;
  LPWORD pwattr;

  LOG("Puts");
  pwattr = (LPWORD) hb_xgrab(len * sizeof(*pwattr));
  if (!pwattr)
    {
      return;
    }
  coord.X = (DWORD) (x);
  coord.Y = (DWORD) (y);
  for (i = 0; i < len; i++)
    {
      *(pwattr + i) = attr;
    }
  WriteConsoleOutputCharacterA(HOutput, str, (DWORD) len, coord, &dwlen);
  WriteConsoleOutputAttribute(HOutput, pwattr, (DWORD) len, coord, &dwlen);
  hb_xfree(pwattr);
}

void gtGetText(char x1, char y1, char x2, char y2, char *dest)
{
  DWORD i, len, width;
  COORD coord;
  LPWORD pwattr;
  char y, *pstr;

  LOG("GetText");
  width = (x2 - x1 + 1);
  pwattr = (LPWORD) hb_xgrab(width * sizeof(*pwattr));
  if (!pwattr)
    {
      return;
    }
  pstr = (char *)hb_xgrab(width);
  if (!pstr)
    {
      hb_xfree(pwattr);
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
  hb_xfree(pwattr);
  hb_xfree(pstr);
}

void gtPutText(char x1, char y1, char x2, char y2, char *srce)
{
  DWORD i, len, width;
  COORD coord;
  LPWORD pwattr;
  char y, *pstr;

  LOG("PutText");
  width = (x2 - x1 + 1);
  pwattr = (LPWORD) hb_xgrab(width * sizeof(*pwattr));
  if (!pwattr)
    {
      return;
    }
  pstr = (char *)hb_xgrab(width);
  if (!pstr)
    {
      hb_xfree(pwattr);
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
  hb_xfree(pwattr);
  hb_xfree(pstr);
}

void gtSetAttribute( char x1, char y1, char x2, char y2, char attribute )
{
/* ptucker */

  DWORD len, i, width;
  COORD coord;
  LPWORD pwattr;
  width = (y2 - y1 + 1);

  pwattr = (LPWORD) hb_xgrab(width * sizeof(*pwattr));
  if (!pwattr)
    return;
  memset( pwattr, attribute, width *sizeof(*pwattr) );

  coord.X = (DWORD) (y1); /* note */
  coord.Y = (DWORD) (x2);
  WriteConsoleOutputAttribute(HOutput, pwattr, width, coord, &len);

  coord.X = (DWORD)( y2 );
  for( i=x1;i<=x2;i++)
  {
     coord.Y = (DWORD) (i);
     WriteConsoleOutputAttribute(HOutput, pwattr, 1, coord, &len);
  }

  hb_xfree( pwattr );
}

char gtWhereX(void)
{
  CONSOLE_SCREEN_BUFFER_INFO csbi;

  LOG("WhereX");
  GetConsoleScreenBufferInfo(HOutput, &csbi);
  return csbi.dwCursorPosition.X;
}

char gtWhereY(void)
{
  CONSOLE_SCREEN_BUFFER_INFO csbi;

  LOG("WhereY");
  GetConsoleScreenBufferInfo(HOutput, &csbi);
  return csbi.dwCursorPosition.Y;
}
