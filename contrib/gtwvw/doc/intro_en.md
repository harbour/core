# GTWVW CONTENTS

## Introduction

Thanks to: All developers of Harbour. Budyanto Dj., the author of
GTWVW. xHarbour Newsgroup. Forum Clipper on line, a Brazilian forum to
discuss Clipper and Harbour with a lot of great people, ready to help
and share knowledge. This is a working in progress. If you find an error
or an incorrect concept, please send an email to angeiras@gmail.com or
angeiras@yahoo.com, with the correct information. By the way, this is not
really an English documentation of GTWVW. This is a Portuguese documentation
translated to English. So, please, correct me if you find something awful.
There is a group in Yahoo (<https://br.groups.yahoo.com/neo/groups/gtwvw/info>),
in Portuguese, but you can send your question or suggestion in English,
Spanish, Italian, etc. (maybe I'll not answer in your mother tongue, but
I'll give a try :).

Copyright © 2008 July/August Manoel N. Angeiras N., All rights reserved.


## GTWVW

GTWVW is a General Terminal (GT) driver for Harbour, with some runtime
library allowing programmer to have text and GUI elements mixed, in
a multi-windowed application. GTWVW is exclusively designed for Windows
platform.

Using GTWVW programmer can use all standard GT functions, normally
indirectly called through standard Harbour I/O functions/commands
as:

   - ?, ?? (QOut(), QQOut())
   - @ ... SAY ... (DevPos(), DevOut())
   - Scroll()
   - SetPos()
   - _GET_()
   - ReadModal()
   - Inkey()
   - AChoice()
   - Alert()
   - etc.

All those commands/functions behave the same way as in other GTs. Unlike
other GTs, however, GTWVW supports multi-windows.

To use GTWVW, build GTWVW library using hbmk2 (in `/contrib/gtwvw`),
then link it to your application as your GT using `gtwvw.hbc` (currently
GTWVW cannot coexist with GTWVT in a MULTIGT environment).

Some special characteristics of GTWVW:

- Allow programmer to do some window operations (open, close, ...) and
  direct OUTPUT to any window.
- As with GTWVT, programmer can mix text and pseudo GUI elements.
- Additionally, GTWVW also has some native Windows controls as statusbar,
  toolbars,
  scrollbars and pushbuttons.


## Windows

### Some basic conventions:
- Windows are numbered from 0 to n, with 0 being the main window, and 'n'
  the topmost window.
- Main window is automatically opened during program init.
- All windows are automatically closed during program exit.
- Parent of to be opened window n is current window (typically window n-1).
- Most of GTWVW functions uses the first parameter as actual window, but
  the GTWVW doesn't uses it. Instead, GTWVW find the topmost window and
  uses it. Besides that, of course, we need to consider the first parameter
  in a function call. For example, the function to delete a combobox is
  defined like:
     wvw_cbDestroy( nWinNum, nCBId )
  Where nWinNum is the window id and nCBId is the id of combobox. To GTWVW
  the first parameter is ignored, so we can call this function like
     wvw_cbDestroy( nWinNum, nCBId )
        or
     wvw_cbDestroy( , nCBId )

### COORDINATES

There are two screen coordinate models (and the user can switch at any
moment):

#### Standard Mode
- In this mode, all coordinates are relative to topmost window.
- Topmost window are always set, from the beginning of program or in
  every operation of opening and closing.
- All input/output from window are oriented to the topmost window. In
  this way, the functions MaxRow() and MaxCol() will return the boundary
  of topmost window.

#### Maincoord Mode
- In this mode, the coordinates are relative to main window (like Clipper).
- All output work in current window, which is internally switched depending
  of requested row/column position. After each operation, current window is
  reset back to main window. Notice that this way MaxRow() and MaxCol() will
  always return main window's, as they do in Clipper. To support this feature,
  each window keeps records of row and column offset to the main window,
  specified during window opening.
- MainCoord Mode is not supported by exported C functions (user must specify
  which window to write, using coordinates within that window.)
- MainCoord Mode it was projected to be the quickest way of porting an
  existing Clipper application to Harbour + GTWVW.


## Minimize, Maximize and Repainting

Some GUI elements of GTWVW are not repainted when windows are minimized and
maximized or covered by another GUI elements. Our application must "remember"
which GUI elements need to be repainted (and GTWVW has some specific
functions to help us).

There is a function, WVW_PAINT(), defined in our application, which is called
to GTWVW when we need to repaint our GUI elements or anything else.

Some functions of GTWVW that we need to take care when repaiting:
   wvw_DrawBoxGet()
   wvw_DrawBoxRaised()
   wvw_DrawBoxRecessed()
   wvw_DrawBoxGroup()
   wvw_DrawImage()
   wvw_DrawLabel()

The repainting is not an automatic feature of GTWVW. The interval to repaint
can be defined by application using the function wvw_SetPaintRefresh().

If we set the interval to zero, GTWVW will call the function WVW_PAINT() in
every repaint requisition by the Windows (except if a previous call has not
returned).

If we set the interval for values greater than zero (valid values greater
than 50), so the WVW_PAINT() function will be called after this interval,
in millisecond, only if there was a pending WM_PAINT() request. The default
interval value is set to 100.


## Cursor

There are two styles of caret: horizontal (classic, console style) and
vertical (commonly used in GUI applications).

Programmer can select which style to choose, using wvw_SetVertCaret()
function. Currently the chosen style will be applied to all windows
(in current implementation of GTWVW caret can only be displayed on
topmost window, the only one allowed to accept input). Default caret
style is horizontal.


## Line Spacing

Programmer may choose to have spacing between lines. This may be desirable,
among other reasons, because GUI elements may overwrite the line above or
the line below the GUI objects. Each window may have its own line spacing,
settable via wvw_SetLineSpacing() function.

Next open window will use default line spacing of nDefLineSpacing settable
via wvw_SetDefLineSpacing() function.

nDefLineSpacing is initially 0.


## Fonts

In many GTWVW font functions, there are a lot of parameters to control
type, height, width and others styles like underline, bold, etc. For
example, the wvw_CreateFont() function has the following list of parameters:
  cFontFace, nHeight, nWidth, nWeight, lItalic, lUnderline, lStrikeout,
  nCharset, nQuality, nEscapement and the meaning is:

- `cFontFace` A null-terminated string that specifies the typeface name of
  the font. The length of this string must not exceed 32 characters.
  If `cFontFace` is an empty string, GDI uses the first font that matches
  the other specified attributes.
- `nHeight` Specifies the height, in logical units, of the font's character
  cell or character. The character height value is the character cell height
  value minus the internal-leading value.
- `nWidth` Specifies the average width, in logical units, of characters in
  the font. If zero, the aspect ratio of the device is matched against the
  digitization aspect ratio of the available fonts to find the closest match,
  determined by the absolute value of the difference.
- `nWeight` Specify the weight of font, varying from 0 to 1000. For example,
  a weight of 400 determine a normal font, a weight of 700, bold. There are
  15 models of font weight (you can see all the models in file `wingdi.ch`
  FW_DONTCARE, FW_THIN, etc.).
- `lItalic` Specifies an italic font if set to TRUE.
- `lUnderline` Specifies an underlined font if set to TRUE.
- `lStrikeOut` Specifies a strikeout font if set to TRUE.
- `nCharSet` Identify the character set. For example, ANSI_CHARSET,
  DEFAULT_CHARSET, OEM_CHARSET, etc. (all models can be viewed in file
  `wingdi.ch`).
- `nQuality` Specifies the output quality. The output quality defines how
  carefully the graphics device interface (GDI) must attempt to match the
  logical-font attributes to those of an actual physical font. It can be
  one of the following values: DEFAULT_QUALITY Appearance of the font does
  not matter.

  - DRAFT_QUALITY
    Appearance of the font is less important than when PROOF_QUALITY is used.
    For GDI raster fonts, scaling is enabled, which means that more font sizes
    are available, but the quality may be lower. Bold, italic, underline,
    and strikeout fonts are synthesized if necessary.
  - PROOF_QUALITY
    Character quality of the font is more important than exact matching of
    the logical-font attributes. For GDI raster fonts, scaling is disabled
    and the font closest in size is chosen. Although the chosen font size
    may not be mapped exactly when PROOF_QUALITY is used, the quality of
    the font is high and there is no distortion of appearance. Bold, italic,
    underline, and strikeout fonts are synthesized if necessary.

- `nEscapement` Specifies the angle, in tenths of degrees, between the
  escapement vector and the x-axis of the device. The escapement vector
  is parallel to the base line of a row of text. Default is zero.


## Callback Functions

There are some functions in GTWVW that need to be defined in our program
(in fact, these functions are called from GTWVW directly). Some of these
functions:

- WVW_PAINT( nWinNum )

  Is called every time Windows receive a message WV_PAINT (to repaint
  screen). The interval of WVW_PAINT() calling can be configured by
  function wvw_SetPaintRefresh().

- WVW_TIMER( nWinNum, hWnd, message, wParam, lParam )

  Is called in every time interval, defined by function wvw_SetTimer().

- WVW_SIZE( nWindow, hWnd, message, wParam, lParam )

  Is called every time a window is minimized, maximized or restored. We
  need to define the function wvw_Size_Ready(), to verify if the processing
  of function WVW_Size() is needed.


## Examples

In my personal web site () you can download an example of application,
screen shots of converted applications and some sample of code, exploring
a lot of interesting features (menus, google maps, skype, crystal reports,
MSN, etc.). And of course, you can download this little documentation.
Enjoy it !
