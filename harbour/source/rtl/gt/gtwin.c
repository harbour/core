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

static BOOL hb_gt_SetScreenBuffer( HANDLE HNew, HANDLE HOld );
static HANDLE HOsave;
static HANDLE HSsave;
static HANDLE HDOutput  = INVALID_HANDLE_VALUE;
static HANDLE HDStealth = INVALID_HANDLE_VALUE;
static HANDLE HInput    = INVALID_HANDLE_VALUE;
static HANDLE HOutput   = INVALID_HANDLE_VALUE;
static HANDLE HStealth  = INVALID_HANDLE_VALUE; /* DispBegin buffer */
static HANDLE HOriginal;                      /* used to restore before quit */
static HANDLE HCursor;  /* When DispBegin is in effect, all cursor related
                           functions must refer to the active handle!
                           Otherwise turds are left on the screen when
                           running in a window. This handle will always
                           refer to the currently _active_ buffer which could
                           be different than the one being written to.
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
  HOriginal = HOutput = HCursor = GetStdHandle(STD_OUTPUT_HANDLE);
}

void hb_gt_Done(void)
{
  if( HOutput != HOriginal )
  {
    /* ptucker */
    /* because the current screen may not be the one that was active
       when the app started, we need to restore that screen and update
       it with the current image before quitting.
     */
    /* easy fix ;-) */
    hb_gtDispBegin();  /* must use these versions ! */
    hb_gtDispEnd();

  }
  CloseHandle(HInput);
  HInput = INVALID_HANDLE_VALUE;
  CloseHandle(HOutput);
  HOutput = INVALID_HANDLE_VALUE;
  if( HStealth != INVALID_HANDLE_VALUE )
  {
     CloseHandle(HStealth);
     HStealth = INVALID_HANDLE_VALUE;
  }
  LOG("Ending");
}

int hb_gt_IsColor(void)
{
   /* TODO: need to call something to do this instead of returning TRUE */
   return TRUE;
}

USHORT hb_gt_GetScreenWidth(void)
{
  CONSOLE_SCREEN_BUFFER_INFO csbi;

  LOG("GetScreenWidth");
  GetConsoleScreenBufferInfo(HOutput, &csbi);
/*   return csbi.dwMaximumWindowSize.X; */
/*  return max(csbi.srWindow.Right - csbi.srWindow.Left +1,40); */
  return max(csbi.dwSize.X,40);
}

USHORT hb_gt_GetScreenHeight(void)
{
  CONSOLE_SCREEN_BUFFER_INFO csbi;

  LOG("GetScreenHeight");
  GetConsoleScreenBufferInfo(HOutput, &csbi);
/*   return csbi.dwMaximumWindowSize.Y; */
/*  return max(csbi.srWindow.Bottom - csbi.srWindow.Top +1,25); */
  return max(csbi.dwSize.Y,25);
}

void hb_gt_SetPos(USHORT cRow, USHORT cCol)
{
  COORD dwCursorPosition;

  LOG("GotoXY");
  dwCursorPosition.X = (SHORT) (cCol);
  dwCursorPosition.Y = (SHORT) (cRow);
  LOG(".. Calling SetConsoleCursorPosition()");
  SetConsoleCursorPosition(HCursor, dwCursorPosition);
  LOG("..  Called SetConsoleCursorPosition()");
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
        case 50:
          rc=SC_INSERT;   /* half block in clipper */
          break;

        case 99:
          rc=SC_SPECIAL1; /* full block in clipper */
          break;

        case 66:
          rc=SC_SPECIAL2; /* upper half block in clipper */
          break;
          /* TODO: cannot tell if the block is upper or lower for cursor */
          /* Answer: Supposed to be upper third, but ms don't support it. */

        default:
          rc=SC_NORMAL;  /* anything else, we'll call it normal */
          break;
        }
    }

  return(rc);
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

    case SC_INSERT:
      cci.dwSize = 50;
      break;

    case SC_SPECIAL1:
      cci.dwSize = 99; 
      break;

    case SC_SPECIAL2:
      cci.dwSize = 66;
      /* In their infinite wisdom, MS doesn't support cursors that
         don't start at the bottom of the cell */
      break;
    case SC_NORMAL:
    default:           /* traps for invalid values */
      cci.dwSize = 25; /* this was 12, but when used in full screen dos window
                          cursor state is erratic  - doesn't turn off, etc. */
      break;

    }
   SetConsoleCursorInfo(HCursor, &cci);

}

void hb_gt_Puts(USHORT cRow, USHORT cCol, BYTE attr, char *str, int len)
{
  DWORD dwlen;
  COORD coord;

  LOG("Puts");
  coord.X = (DWORD) (cCol);
  coord.Y = (DWORD) (cRow);
  WriteConsoleOutputCharacterA(HOutput, str, (DWORD)len, coord, &dwlen);
  FillConsoleOutputAttribute(HOutput, (WORD)(attr&0xff), (DWORD)len, coord, &dwlen);
}

void hb_gt_GetText(USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, char *dest)
{
  DWORD len;
  COORD coord;
  LPWORD pwattr;
  char * pstr;
  USHORT width, i, y;

  LOG("GetText");
  width = (usRight - usLeft + 1);
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
  for (y = usTop; y <= usBottom; y++)
    {
      coord.X = (DWORD) (usLeft);
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

void hb_gt_PutText(USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, char *srce)
{
  DWORD len;
  COORD coord;
  LPWORD pwattr;
  char *pstr;
  USHORT width, i, y;

  LOG("PutText");
  width = (usRight - usLeft + 1);
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
  for (y = usTop; y <= usBottom; y++)
    {
      for (i = 0; i < width; i++)
        {
          *(pstr + i) = *srce;
          srce++;
          *(pwattr + i) = ((WORD)((unsigned char)*srce)&0xff);
          srce++;
        }
      coord.X = (DWORD) (usLeft);
      coord.Y = (DWORD) (y);
      WriteConsoleOutputAttribute(HOutput, pwattr, width, coord, &len);
      WriteConsoleOutputCharacterA(HOutput, pstr, width, coord, &len);
    }
  hb_xfree(pwattr);
  hb_xfree(pstr);
}

void hb_gt_SetAttribute( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr )
{
/* ptucker */

  DWORD len;
  COORD coord;
  USHORT width, y;

  width = (usRight - usLeft + 1);

  coord.X = (DWORD) (usLeft);

  for (y = usTop; y <= usBottom; y++)
  {
     coord.Y = y;
     FillConsoleOutputAttribute(HOutput, (WORD)(attr&0xff), width, coord, &len);
  }

}

void hb_gt_DrawShadow( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr )
{
/* ptucker */

  DWORD len;
  COORD coord;
  USHORT width;

  width = (usRight - usLeft + 1);

  coord.X = (DWORD) (usLeft);
  coord.Y = (DWORD) (usBottom);

  FillConsoleOutputAttribute(HOutput, (WORD)(attr&0xff), width, coord, &len);
  hb_gt_SetAttribute( usTop, usRight, usBottom, usRight, attr );

}

USHORT hb_gt_Col(void)
{
  CONSOLE_SCREEN_BUFFER_INFO csbi;

  LOG("WhereX");
  GetConsoleScreenBufferInfo(HCursor, &csbi);
  return csbi.dwCursorPosition.X;
}

USHORT hb_gt_Row(void)
{
  CONSOLE_SCREEN_BUFFER_INFO csbi;

  LOG("WhereY");
  GetConsoleScreenBufferInfo(HCursor, &csbi);
  return csbi.dwCursorPosition.Y;
}

void hb_gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr, SHORT sVert, SHORT sHoriz )
{
/* ptucker */

  SMALL_RECT Source, Clip;
  COORD      Target;
  CHAR_INFO  FillChar;

  Source.Top    = usTop;
  Source.Left   = usLeft;
  Source.Bottom = usBottom;
  Source.Right  = usRight;

  memcpy( &Clip, &Source, sizeof(Clip) );

  if( (sHoriz | sVert) == 0 ) /* both zero? */
  {
     Target.Y = usBottom+1;  /* set outside the clipping region */
     Target.X = usRight+1;
  }
  else
  {
     Target.Y = usTop-sVert;
     Target.X = usLeft-sHoriz;
  }
  FillChar.Char.AsciiChar = ' ';
  FillChar.Attributes = (WORD)(attr&0xff);

  ScrollConsoleScreenBuffer(HOutput, &Source, &Clip, Target, &FillChar);
}

void hb_gt_DispBegin(void)
{
/* ptucker */
  if( hb_gtDispCount() == 1 )
  {
    COORD coDest = {0, 0};
    COORD coBuf;                      /* the size of the buffer to read into */
    CHAR_INFO *pCharInfo;     /* buffer to store info from ReadConsoleOutput */
    SMALL_RECT srWin;                       /* source rectangle to read from */
    CONSOLE_SCREEN_BUFFER_INFO csbi;

    GetConsoleScreenBufferInfo(HCursor, &csbi);
    srWin.Top    = srWin.Left = 0;
    srWin.Bottom = (coBuf.Y = csbi.dwSize.Y) -1;
    srWin.Right  = (coBuf.X = csbi.dwSize.X) -1;

    /* allocate a buffer for the screen rectangle */
    pCharInfo = (CHAR_INFO *)hb_xgrab(coBuf.Y * coBuf.X * sizeof(CHAR_INFO));

    /* read the screen rectangle into the buffer */
    ReadConsoleOutput(HOutput,                     /* current screen handle  */
                 pCharInfo,                        /* transfer area          */
                 coBuf,                        /* size of destination buffer */
                 coDest,               /* upper-left cell to write data to   */
                 &srWin);            /* screen buffer rectangle to read from */

    if( HStealth == INVALID_HANDLE_VALUE )
    {
       HStealth = CreateConsoleScreenBuffer(
                  GENERIC_READ    | GENERIC_WRITE,    /* Access flag         */
                  FILE_SHARE_READ | FILE_SHARE_WRITE, /* Buffer share mode   */
                  NULL,                               /* Security attribute  */
                  CONSOLE_TEXTMODE_BUFFER,            /* Type of buffer      */
                  NULL);                              /* reserved            */

    }

    hb_gt_SetScreenBuffer( HStealth, HOutput );

    HOutput = HStealth;
    WriteConsoleOutput(HOutput,                             /* output handle */
                 pCharInfo,                                 /* data to write */
                 coBuf,                     /* col/row size of source buffer */
                 coDest,        /* upper-left cell to write data from in src */
                 &srWin);             /* screen buffer rect to write data to */

    hb_xfree(pCharInfo);
  }
}

void hb_gt_DispEnd(void)
{
/* ptucker */

  if( hb_gtDispCount() == 1 )
  {
    HANDLE htmp = HStealth;

    HStealth = HCursor;
    hb_gt_DispBegin();
    HStealth = htmp;
  }
}

static BOOL hb_gt_SetScreenBuffer( HANDLE HNew, HANDLE HOld )
{
/* ptucker */

/* set a new buffer to have the same characteristics as an existing buffer */
  CONSOLE_SCREEN_BUFFER_INFO csbi;
  SMALL_RECT srWin;

  GetConsoleScreenBufferInfo(HOld, &csbi);

  /* new console window size and scroll position */
  srWin.Top    = srWin.Left = 0;
  srWin.Bottom = csbi.dwSize.Y - 1;
  srWin.Right  = csbi.dwSize.X - 1;

  SetConsoleScreenBufferSize(HNew, csbi.dwSize);
  SetConsoleWindowInfo(HNew, TRUE,  &csbi.srWindow);
  SetConsoleWindowInfo(HNew, FALSE, &srWin);

  return 0;
}

BOOL hb_gt_SetMode( USHORT uiRows, USHORT uiCols )
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
  return 0;
}

void hb_gt_Replicate(char c, DWORD nLength)
{

/* ptucker */
  COORD coBuf = {0,0};
  DWORD nWritten;

/* TODO: later... */
  FillConsoleOutputCharacter(
          HOutput,                              /* handle to screen buffer   */
          c,                                    /* character to write        */
          nLength,                              /* number of cells to write  */
          coBuf,                                /* coordinates of first cell */
          &nWritten                        /* receives actual number written */
          );

}

BOOL hb_gt_GetBlink()
{
   return FALSE;
}

void hb_gt_SetBlink( BOOL bBlink )
{
   bBlink = FALSE;
}

void hb_gt_DebugScreen( BOOL activate )
{
   if( activate )
   {
      if( HDOutput == INVALID_HANDLE_VALUE )
      {
        HDOutput = CreateConsoleScreenBuffer(
                   GENERIC_READ    | GENERIC_WRITE,    /* Access flag         */
                   FILE_SHARE_READ | FILE_SHARE_WRITE, /* Buffer share mode   */
                   NULL,                               /* Security attribute  */
                   CONSOLE_TEXTMODE_BUFFER,            /* Type of buffer      */
                   NULL);                              /* reserved            */

        hb_gt_SetScreenBuffer( HDOutput, HOutput );
      }
      HOsave = HOutput;
      HOutput = HCursor = HDOutput;
      hb_gtDispBegin();
      hb_gtDispEnd();
   }
   else
   {
      HOutput = HOsave;
      HCursor = HOriginal;
   }
   SetConsoleActiveScreenBuffer( HOutput );
}

