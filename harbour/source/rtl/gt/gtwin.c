/*
 * $Id$
 */

/*
 *  GTWIN.C: Video subsystem for Windows 95/NT compilers.
 *
 *  Portions of this module are based (somewhat) on VIDMGR by
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
#ifdef __CYGWIN__
typedef WORD far *LPWORD;
#endif
#endif /* __GNUC__ */

static HANDLE HInput = INVALID_HANDLE_VALUE;
static HANDLE HOutput = INVALID_HANDLE_VALUE;
static HANDLE HCursor;  /* When Dispbegin is in effect, all cursor related
                           functions must refer to the original handle
                           otherwise turds are left on the screen when
                           running in a window
                         */
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
  HOutput = HCursor = GetStdHandle(STD_OUTPUT_HANDLE);
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
  SetConsoleCursorPosition(HCursor, dwCursorPosition);
  LOG("..  Called SetConsoleCursorPosition()");
}

void hb_gt_SetCursorStyle(int style)
{
  CONSOLE_CURSOR_INFO cci;

  LOG("SetCursorStyle");
  GetConsoleCursorInfo(HCursor, &cci);
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
      /* because in their infinite wisdom, MS doesn't support cursors that
         don't start at the bottom of the cell */
      break;

    default:
      break;
    }
}

int hb_gt_GetCursorStyle(void)
{
  CONSOLE_CURSOR_INFO cci;
  int rc;

  LOG("GetCursorStyle");
  GetConsoleCursorInfo(HCursor, &cci);

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
  WriteConsoleOutputCharacterA(HOutput, str, (DWORD)len, coord, &dwlen);
  FillConsoleOutputAttribute(HOutput, (WORD)((unsigned char)attr&0xff), (DWORD)len, coord, &dwlen);
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
  memset(pwattr,0,width*sizeof(*pwattr));
  pstr = (char *)hb_xgrab(width);
  if (!pstr)
    {
      hb_xfree(pwattr);
      return;
    }
  memset(pstr,0,width);
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
          *dest = (char)*(pwattr + i)&0xff;
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
          *(pwattr + i) = (WORD)((unsigned char)*srce)&0xff;
          srce++;
        }
      coord.X = (DWORD) (cLeft);
      coord.Y = (DWORD) (y);
      WriteConsoleOutputAttribute(HOutput, pwattr, width, coord, &len);
      WriteConsoleOutputCharacterA(HOutput, pstr, width, coord, &len);
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
     FillConsoleOutputAttribute(HOutput, (WORD)((unsigned char)attribute)&0xff, width, coord, &len);
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

  FillConsoleOutputAttribute(HOutput, (WORD)((unsigned char)attribute)&0xff, width, coord, &len);

  coord.X = (DWORD) (cRight);
  for( y=cTop;y<=cBottom;y++)
  {
     coord.Y = y;
     FillConsoleOutputAttribute(HOutput, (WORD)((unsigned char)attribute)&0xff, 1, coord, &len);
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
  FillChar.Attributes = (WORD)((unsigned char)attribute)&0xff;

  ScrollConsoleScreenBuffer(HOutput, &Source, &Clip, Target, &FillChar);
}

void hb_gt_DispBegin(void)
{
/* ptucker */
  COORD coDest = {0, 0};
  COORD coBuf;                        /* the size of the buffer to read into */
  CHAR_INFO *pCharInfo;       /* buffer to store info from ReadConsoleOutput */
  SMALL_RECT srWin;                         /* source rectangle to read from */
  CONSOLE_CURSOR_INFO cci;
/*
  DWORD dsize;
  int vis;

  GetConsoleCursorInfo(HOutput, &cci);
  vis = cci.bVisible;
  dsize = cci.dwSize;
  cci.bVisible = 0;
  cci.dwSize = 0;
  SetConsoleCursorInfo(HCursor, &cci);
*/
  srWin.Top    = srWin.Left = 0;
  srWin.Right  = (coBuf.X = hb_gt_GetScreenWidth()) -1;
  srWin.Bottom = (coBuf.Y = hb_gt_GetScreenHeight()) -1;

  /* allocate a buffer for the screen rectangle */
  pCharInfo = (CHAR_INFO *)hb_xgrab(coBuf.X * coBuf.Y * sizeof(CHAR_INFO));

  hb_gt_ScreenBuffer( (ULONG)HOutput );         /* store current handle      */

  /* read the screen rectangle into the buffer */
  ReadConsoleOutput(HOutput,                       /* current screen handle  */
               pCharInfo,                          /* transfer area          */
               coBuf,                  /* col/row size of destination buffer */
               coDest,                 /* upper-left cell to write data to   */
               &srWin);              /* screen buffer rectangle to read from */

  HOutput = CreateConsoleScreenBuffer(
               GENERIC_READ    | GENERIC_WRITE,    /* Access flag            */
               FILE_SHARE_READ | FILE_SHARE_WRITE, /* Buffer share mode      */
               NULL,                               /* Security attribute ptr */
               CONSOLE_TEXTMODE_BUFFER,            /* Type of buffer         */
               NULL);                              /* reserved               */

  WriteConsoleOutput(HOutput,                               /* output handle */
               pCharInfo,                                   /* data to write */
               coBuf,                       /* col/row size of source buffer */
               coDest,          /* upper-left cell to write data from in src */
               &srWin);               /* screen buffer rect to write data to */
/*
  cci.bVisible = vis;
  cci.dwSize = dsize;
  SetConsoleCursorInfo(HCursor, &cci);
*/
  hb_xfree(pCharInfo);
}

void hb_gt_DispEnd(void)
{
/* ptucker */
  COORD coDest = {0, 0};
  COORD coBuf;                        /* the size of the buffer to read into */
  CHAR_INFO *pCharInfo;       /* buffer to store info from ReadConsoleOutput */
  SMALL_RECT srWin;                         /* source rectangle to read from */

  srWin.Top    = srWin.Left = 0;
  srWin.Right  = (coBuf.X = hb_gt_GetScreenWidth()) -1;
  srWin.Bottom = (coBuf.Y = hb_gt_GetScreenHeight()) -1;

  /* allocate a buffer for the screen rectangle */
  pCharInfo = (CHAR_INFO *)hb_xgrab(coBuf.X * coBuf.Y * sizeof(CHAR_INFO));

  /* read the screen rectangle into the buffer */
  ReadConsoleOutput(HOutput,                        /* current screen buffer */
             pCharInfo,                             /* transfer area         */
             coBuf,                    /* col/row size of destination buffer */
             coDest,             /* upper-left cell to write data to in dest */
             &srWin);                /* screen buffer rectangle to read from */

  CloseHandle( HOutput );

  HOutput = (HANDLE)hb_gt_ScreenBuffer( 0 );    /* get previous handle       */

  WriteConsoleOutput(HOutput,                   /* output buffer             */
                pCharInfo,                      /* buffer with data to write */
                coBuf,                      /* col/row size of source buffer */
                coDest,         /* upper-left cell to write data from in src */
                &srWin);              /* screen buffer rect to write data to */

  hb_xfree(pCharInfo);
}

void hb_gt_SetMode( USHORT uiRows, USHORT uiCols )
{
/* ptucker */
  CONSOLE_SCREEN_BUFFER_INFO csbi;
  SMALL_RECT srWin;
  COORD coBuf;

  GetConsoleScreenBufferInfo(HOutput, &csbi);
  coBuf = GetLargestConsoleWindowSize(HOutput);

  /* new console window size and scroll position */
  srWin.Top    = srWin.Left = 0;
  srWin.Bottom = (SHORT) (min(uiRows, coBuf.Y) - 1);
  srWin.Right  = (SHORT) (min(uiCols, coBuf.X) - 1);

  /* new console buffer size */
  coBuf.Y = uiRows;
  coBuf.X = uiCols;

  /* if the current buffer is larger than what we want, resize the */
  /* console window first, then the buffer */
  if ((DWORD) csbi.dwSize.X * csbi.dwSize.Y > (DWORD) uiCols * uiRows )
  {
    SetConsoleWindowInfo(HOutput, TRUE, &srWin);
    SetConsoleScreenBufferSize(HOutput, coBuf);
  }
  else if ((DWORD) csbi.dwSize.X * csbi.dwSize.Y < (DWORD) uiCols * uiRows )
  {
    SetConsoleScreenBufferSize(HOutput, coBuf);
    SetConsoleWindowInfo(HOutput, TRUE, &srWin);
  }
}

void hb_gt_Replicate(char c, DWORD nLength)
{

/* ptucker */
  COORD coBuf = {0,0};
  DWORD nWritten;

/* later... */
  FillConsoleOutputCharacter(
          HOutput,                              /* handle to screen buffer   */
          c,                                    /* character to write        */
          nLength,                              /* number of cells to write  */
          coBuf,                                /* coordinates of first cell */
          &nWritten                         /* receives actual number written */
          );

}

