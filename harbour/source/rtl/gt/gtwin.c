/*
 * $Id$
 */

/*
 *  GTWIN.C: Video subsystem for Windows 95/NT compilers.
 *
 *  This module is based (somewhat) on VIDMGR by
 *   Andrew Clarke and modified for the Harbour project
 *
 *  User programs should never call this layer directly!
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

void hb_gt_Init(void)
{
  LOG("Initializing");
  HInput = GetStdHandle(STD_INPUT_HANDLE);
  HOutput = GetStdHandle(STD_OUTPUT_HANDLE);
  hb_gt_ScreenBuffer((ULONG)HOutput);
}

void hb_gt_Done(void)
{
  CloseHandle(HInput);
  HInput = INVALID_HANDLE_VALUE;
  CloseHandle(HOutput);
  HOutput = INVALID_HANDLE_VALUE;
  LOG("Ending");
}

int hb_gt_IsColor(void)
{
   /* TODO: need to call something to do this instead of returning TRUE */
   return TRUE;
}

char hb_gt_GetScreenWidth(void)
{
  CONSOLE_SCREEN_BUFFER_INFO csbi;

  LOG("GetScreenWidth");
  GetConsoleScreenBufferInfo(HOutput, &csbi);
  return (char)csbi.dwSize.X;
}

char hb_gt_GetScreenHeight(void)
{
  CONSOLE_SCREEN_BUFFER_INFO csbi;

  LOG("GetScreenHeight");
  GetConsoleScreenBufferInfo(HOutput, &csbi);
  return (char)csbi.dwSize.Y;
}

void hb_gt_SetPos(char cRow, char cCol)
{
  COORD dwCursorPosition;

  LOG("GotoXY");
  dwCursorPosition.X = (SHORT) (cCol);
  dwCursorPosition.Y = (SHORT) (cRow);
  LOG(".. Calling SetConsoleCursorPosition()");
  SetConsoleCursorPosition(HOutput, dwCursorPosition);
  LOG("..  Called SetConsoleCursorPosition()");
}

void hb_gt_SetCursorStyle(int style)
{
  CONSOLE_CURSOR_INFO cci;

  LOG("SetCursorStyle");
  GetConsoleCursorInfo(HOutput, &cci);
  cci.bVisible = 1; /* always visible unless explicitly request off */
  switch (style)
    {
    case SC_NONE:
      cci.bVisible = 0;
      break;

    case SC_NORMAL:
      cci.dwSize = 12;
      break;

    case SC_INSERT:
      cci.dwSize = 99;
      break;

    case SC_SPECIAL1:
      cci.dwSize = 49;  /* supposed to be upper half, but this will do */
      break;

    case SC_SPECIAL2:
      /* TODO: Why wasn't this implemented ? */
      /* because in their infinite wisdom, MS doesn't support this...*/
      break;

    default:
      break;
    }
    SetConsoleCursorInfo(HOutput, &cci);
}

int hb_gt_GetCursorStyle(void)
{
  CONSOLE_CURSOR_INFO cci;
  int rc;

  LOG("GetCursorStyle");
  GetConsoleCursorInfo(HOutput, &cci);

  if(!cci.bVisible)
    {
      rc=SC_NONE;
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
          /* Answer: Supposed to be upper third, but ms don't support it. */
        default:
	  rc=SC_SPECIAL2;
        }
    }

  return(rc);
}

void hb_gt_Puts(char cRow, char cCol, char attr, char *str, int len)
{
  DWORD dwlen;
  COORD coord;

  LOG("Puts");
  coord.X = (DWORD) (cCol);
  coord.Y = (DWORD) (cRow);
  WriteConsoleOutputCharacterA(HOutput, str, (DWORD) len, coord, &dwlen);
  FillConsoleOutputAttribute(HOutput, (WORD)attr, (DWORD)len, coord, &dwlen);
}

void hb_gt_GetText(char cTop, char cLeft, char cBottom, char cRight, char *dest)
{
  DWORD i, len, width;
  COORD coord;
  LPWORD pwattr;
  char y, *pstr;

  LOG("GetText");
  width = (cRight - cLeft + 1);
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
  for (y = cTop; y <= cBottom; y++)
    {
      coord.X = (DWORD) (cLeft);
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

void hb_gt_PutText(char cTop, char cLeft, char cBottom, char cRight, char *srce)
{
  DWORD i, len, width;
  COORD coord;
  LPWORD pwattr;
  char y, *pstr;

  LOG("PutText");
  width = (cRight - cLeft + 1);
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
  for (y = cTop; y <= cBottom; y++)
    {
      for (i = 0; i < width; i++)
        {
	  *(pstr + i) = *srce;
	  srce++;
	  *(pwattr + i) = (WORD)*srce;
	  srce++;
        }
      coord.X = (DWORD) (cLeft);
      coord.Y = (DWORD) (y);
      WriteConsoleOutputCharacterA(HOutput, pstr, width, coord, &len);
      WriteConsoleOutputAttribute(HOutput, pwattr, width, coord, &len);
    }
  hb_xfree(pwattr);
  hb_xfree(pstr);
}

void hb_gt_SetAttribute( char cTop, char cLeft, char cBottom, char cRight, char attribute )
{
/* ptucker */

  DWORD len, y, width;
  COORD coord;
  width = (cRight - cLeft + 1);

  coord.X = (DWORD) (cLeft);

  for( y=cTop;y<=cBottom;y++)
  {
     coord.Y = y;
     FillConsoleOutputAttribute(HOutput, (WORD)attribute, width, coord, &len);
  }

}

void hb_gt_DrawShadow( char cTop, char cLeft, char cBottom, char cRight, char attribute )
{
/* ptucker */

  DWORD len, y, width;
  COORD coord;
  width = (cRight - cLeft + 1);

  coord.X = (DWORD) (cLeft);
  coord.Y = (DWORD) (cBottom);

  FillConsoleOutputAttribute(HOutput, (WORD)attribute, width, coord, &len);

  coord.X = (DWORD) (cRight);
  for( y=cTop;y<=cBottom;y++)
  {
     coord.Y = y;
     FillConsoleOutputAttribute(HOutput, (WORD)attribute, 1, coord, &len);
  }

}

char hb_gt_Col(void)
{
  CONSOLE_SCREEN_BUFFER_INFO csbi;

  LOG("WhereX");
  GetConsoleScreenBufferInfo(HOutput, &csbi);
  return csbi.dwCursorPosition.X;
}

char hb_gt_Row(void)
{
  CONSOLE_SCREEN_BUFFER_INFO csbi;

  LOG("WhereY");
  GetConsoleScreenBufferInfo(HOutput, &csbi);
  return csbi.dwCursorPosition.Y;
}

void hb_gt_Scroll( char cTop, char cLeft, char cBottom, char cRight, char attribute, char vert, char horiz )
{
/* ptucker */

  SMALL_RECT Source, Clip;
  COORD      Target;
  CHAR_INFO  FillChar;

  Source.Top    = cTop;
  Source.Left   = cLeft;
  Source.Bottom = cBottom;
  Source.Right  = cRight;

  memcpy( &Clip, &Source, sizeof(Clip) );

  if( (horiz | vert) == 0 )
  {
     Target.Y = cBottom+1;  /* set outside the clipping region */
     Target.X = cRight+1;
  }
  else
  {
     Target.Y = cTop-vert;
     Target.X = cLeft-horiz;
  }
  FillChar.Char.AsciiChar = ' ';
  FillChar.Attributes = (WORD)attribute;

  ScrollConsoleScreenBuffer(HOutput, &Source, &Clip, Target, &FillChar);
}

void hb_gt_DispBegin(void)
{
/* ptucker */
  HANDLE hNewBuffer;
  COORD dwWriteCoord = {0, 0};
  char row, col;
  char * pBuffer;
  USHORT uiX;

  hb_gtRectSize( 0,0, hb_gt_GetScreenHeight()-1, hb_gt_GetScreenWidth()-1, &uiX );
  pBuffer = ( char * ) hb_xgrab( uiX );
  hb_gtSave( 0,0, hb_gt_GetScreenHeight()-1, hb_gt_GetScreenWidth()-1, pBuffer );

  hNewBuffer = CreateConsoleScreenBuffer(
               GENERIC_READ    | GENERIC_WRITE,
               FILE_SHARE_READ | FILE_SHARE_WRITE,
               NULL,
               CONSOLE_TEXTMODE_BUFFER,
               NULL);

  row = hb_gt_Row();
  col = hb_gt_Col();
  hb_gt_ScreenBuffer( (ULONG)HOutput );

  HOutput = hNewBuffer;
  hb_gtRest( 0,0,  hb_gt_GetScreenHeight()-1, hb_gt_GetScreenWidth()-1, pBuffer );
  hb_gt_SetPos( row, col );
  hb_xfree( pBuffer );
}

void hb_gt_DispEnd()
{
/* ptucker */
  char * pBuffer;
  char row, col;
  USHORT uiX;

  hb_gtRectSize( 0,0, hb_gt_GetScreenHeight()-1, hb_gt_GetScreenWidth()-1, &uiX );
  pBuffer = ( char * ) hb_xgrab( uiX );
  hb_gtSave( 0,0, hb_gt_GetScreenHeight()-1, hb_gt_GetScreenWidth()-1, pBuffer );

  row = hb_gt_Row();
  col = hb_gt_Col();
  CloseHandle( HOutput );
  HOutput = (HANDLE)hb_gt_ScreenBuffer( 0 );

  hb_gtRest( 0,0, hb_gt_GetScreenHeight()-1, hb_gt_GetScreenWidth()-1, pBuffer );
  hb_gt_SetPos( row, col );
  hb_xfree( pBuffer );
}

void hb_gt_SetMode( USHORT uiRows, USHORT uiCols )
{
/* ptucker */
  CONSOLE_SCREEN_BUFFER_INFO csbi;
  SMALL_RECT srRect;
  COORD coordScreen;

  GetConsoleScreenBufferInfo(HOutput, &csbi);
  coordScreen = GetLargestConsoleWindowSize(HOutput);

  /* new console window size and scroll position */
  srRect.Right  = (SHORT) (min(uiCols, coordScreen.X) - 1);
  srRect.Bottom = (SHORT) (min(uiRows, coordScreen.Y) - 1);
  srRect.Left   = srRect.Top = (SHORT) 0;

  /* new console buffer size */
  coordScreen.X = uiCols;
  coordScreen.Y = uiRows;

  /* if the current buffer is larger than what we want, resize the */
  /* console window first, then the buffer */
  if ((DWORD) csbi.dwSize.X * csbi.dwSize.Y > (DWORD) uiCols * uiRows )
  {
    SetConsoleWindowInfo(HOutput, TRUE, &srRect);
    SetConsoleScreenBufferSize(HOutput, coordScreen);
  }
  else if ((DWORD) csbi.dwSize.X * csbi.dwSize.Y < (DWORD) uiCols * uiRows )
  {
    SetConsoleScreenBufferSize(HOutput, coordScreen);
    SetConsoleWindowInfo(HOutput, TRUE, &srRect);
  }
}
