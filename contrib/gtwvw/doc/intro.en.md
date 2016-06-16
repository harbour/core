# Introduction

Thanks to: All developers of Harbour. Budyanto Dj., the author of
GTWVW. xHarbour Newsgroup. Forum Cl\*pper on line, a Brazilian forum to
discuss Cl\*pper and Harbour with a lot of great people, ready to help
and share knowledge. This is a working in progress. If you find an error
or an incorrect concept, please send an email to <angeiras@gmail.com> or
<angeiras@yahoo.com>, with the correct information. By the way, this is not
really an English documentation of GTWVW. This is a Portuguese documentation
translated to English. So, please, correct me if you find something awful.
There is a group in Yahoo (<https://br.groups.yahoo.com/neo/groups/gtwvw/info>),
in Portuguese, but you can send your question or suggestion in English,
Spanish, Italian, etc. (maybe I'll not answer in your mother tongue, but
I'll give a try :).

Copyright © 2008 July/August Manoel N. Angeiras N., All rights reserved.


# GTWVW

GTWVW is a General Terminal (GT) driver for Harbour, with some runtime
library allowing programmer to have text and GUI elements mixed, in
a multi-windowed application. GTWVW is exclusively designed for Windows
platform. Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>

Using GTWVW programmer can use all standard GT functions, normally
indirectly called through standard Harbour I/O functions/commands
as:

   - ?, ?? (QOut(), QQOut())
   - @ ... SAY ... (DevPos(), DevOut())
   - Scroll()
   - SetPos()
   - \_GET\_()
   - ReadModal()
   - Inkey()
   - AChoice()
   - Alert()
   - etc.

To use GTWVW, build GTWVW library using hbmk2 (in `/contrib/gtwvw`),
then link it to your application as your GT using `gtwvw.hbc` (currently
GTWVW cannot coexist with GTWVT in a MULTIGT environment).

Some special characteristics of GTWVW:

- Allow programmer to do some window operations (open, close, ...) and
  direct OUTPUT to any window.
- As with GTWVT, programmer can mix text and pseudo GUI elements.
- Additionally, GTWVW also has some native Windows controls as statusbar,
  toolbars, scrollbars and pushbuttons.


# ACKNOWLEDGEMENTS

GTWVW does not exist without the followings:
- Peter Rees, the original author of GTWVT
- Pritpal Bedi, with his WVTGUI extensions
- All other GTWVT contributors
- All (x)Harbour developers, who made it all possible!!!

Thanks to all of you!

Also thanks to those who gave me feedbacks during the initial stage of GTWVW.
And to those who has been persistenly asking me to continue developing it :-).


# VERSION INFO

This document is written when GTWVW version info is as follows.

Latest Update: 2004-10-25

Latest Sync with:

xHarbour of: (Beta1-2)
`=Id: ChangeLog,v 1.3493 2004/10/06 02:06:09 ronpinkas Exp =`

gtwvt of:
`=Id: hbgtwvt.h,v 1.31 2004/09/28 03:25:07 vouchcac Exp =`
`=Id: gtwvt.c,v 1.133 2004/09/28 03:25:17 vouchcac Exp =`
`=Id: wvtutils.c,v 1.18 2004/09/28 03:25:18 vouchcac Exp =`
`=Id: wvtcore.c,v 1.10 2004/08/30 14:10:20 vouchcac Exp =`


# Windows

## Some basic conventions:
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
     `wvw_cbDestroy( nWinNum, nCBId )`
  Where nWinNum is the window id and nCBId is the id of combobox. To GTWVW
  the first parameter is ignored, so we can call this function like
     `wvw_cbDestroy( nWinNum, nCBId )`
     or
     `wvw_cbDestroy( , nCBId )`

## INPUT

- Mouse and keyboard input accepted only when topmost window is on focus.
  (That is, currently GTWVW windows are modals.)
- Inputs are therefore stored in topmost window's buffer.
- Inputs on non topmost window can now be handled via `WVW_INPUTFOCUS()` function
  (menu, toolbar, scrollbar, pushbutton, and other keyboard/mouse events).

## COORDINATES

There are two screen coordinate models (and the user can switch at any
moment during run time by using `wvw_SetMainCoord()`):

### Standard Mode (default)
- In this mode, all coordinates are relative to current (topmost) window.
- Topmost window are always set, from the beginning of program or in
  every operation of opening (`wvw_nOpenWindow()`) and closing (`wvw_lCloseWindow()`).
- All input/output from window are oriented to the topmost window. In
  this way, the functions `MaxRow()` and `MaxCol()` will return the boundary
  of topmost window.
- Programmer can change CurrentWindow by using `wvw_nSetCurWindow()`

  Example:
     ```
     nCurWin := wvw_nSetCurWindow( 1 )
     ? "In Window 1"
     wvw_nSetCurWindow( nCurWin )
     ? "Back in Window", hb_ntos( nCurWin )
     ```
- All exported, output oriented C functions work on window designated by
  nWinNum parameter.
- All .prg level GTWVW functions work on window designated by nWinNum
  parameter.

### Maincoord Mode

- In this mode, the coordinates are relative to main window (like Cl\*pper).
- All output work in current window, which is internally switched depending
  of requested row/column position. After each operation, current window is
  reset back to main window. Notice that this way `MaxRow()` and `MaxCol()` will
  always return main window's, as they do in Cl\*pper. To support this feature,
  each window keeps records of row and column offset to the main window,
  specified during window opening.
- MainCoord Mode is not supported by exported C functions (user must specify
  which window to write, using coordinates within that window.)
- MainCoord Mode it was projected to be the quickest way of porting an
  existing Cl\*pper application to Harbour + GTWVW.
- CurrentWindow is automatically reset to MAIN window during program start
  and after `wvw_nOpenWindow()` and after `wvw_lCloseWindow()`.
- All output oriented `GT_FUNC()` work on CurrentWindow, which is internally
  switched depending of requested row,col position. After each operation,
  CurrentWindow is reset back to MAIN window. Notice that this way `MaxRow()`
  and `MaxCol()` will always return MAIN window's, as they do in Cl\*pper.
  To support this feature, each window keeps records of Row and Col Offset
  to the Main Window, specified during Window opening.

  Example:

  Assume this window states:

  ```
  +------...
  |Main Window (Window 0)
  |MaxRow()=24 MaxCol()=79
  |   +---------------...
  |   |Window1 RowOfs=3 ColOfs=4
  |   |MaxRow()=9 MaxCol()=29
  |   |          +---------------------------...
  |   |          |Window2 RowOfs=6 ColOfs=15
  |   |          |MaxRow()=3 MaxCol()=49
  |   |          |
  ```

  ```
  @ 6,15 SAY "text1" // will be written to Window2 starting at 0, 0
  @ 3,15 SAY "text2" // will be written to Window1 starting at 0, 11
  @ 3, 2 SAY "text3" // will be written to Main Window starting at 3, 2
  @ 4, 2 SAY ""      // Main Window
  ?? "t"             // Main Window
  ?? "e"             // Window 1
  ?? "x"             // Window 1
  ?? "t"             // Window 1
  ?? "4"             // Window 1
  ```

  Note that the whole "text3" will be written to Main Window, but
  "ext4" will be written to Window 1.<br />
  Note also that all window corners must be within MAIN window's scope.
- User can change CurrentWindow by using `wvw_nSetCurWindow()`, BUT
  it is NOT recommended because it is ridiculous and probably dangerous!
  (If you need to write to a specific window when you are in MainCoord
  Mode, it is recommended that you turn off MainCoord Mode, select
  CurrentWindow, do the operation, and turn MainCoord Mode back on when
  it's done.)
- MainCoord Mode is NOT supported by exported C functions.
  (User must specify which nWinNum to write, using coordinates within that
  window.)
- All .prg level GTWVW functions support MainCoord Mode limited only in
  translating passed screen-wide row,col coordinates into ones of the
  selected nWinNum.

Notes:
MainCoord Mode is meant to be the quickest way of porting an existing
Cl\*pper application to Harbour + GTWVW.


# Minimize, Maximize and Repainting

Some GUI elements of GTWVW are not repainted when windows are minimized and
maximized or covered by another GUI elements. Our application must "remember"
which GUI elements need to be repainted (and GTWVW has some specific
functions to help us).

There is a function, `WVW_PAINT()`, defined in our application, which is called
to GTWVW when we need to repaint our GUI elements or anything else.

Some functions of GTWVW that we need to take care when repaiting:
```
   wvw_DrawBoxGet()
   wvw_DrawBoxRaised()
   wvw_DrawBoxRecessed()
   wvw_DrawBoxGroup()
   wvw_DrawImage()
   wvw_DrawLabel()
```
The repainting is not an automatic feature of GTWVW. The interval to repaint
can be defined by application using the function `wvw_SetPaintRefresh()`.

If we set the interval to zero, GTWVW will call the function `WVW_PAINT()` in
every repaint requisition by the Windows (except if a previous call has not
returned).

If we set the interval for values greater than zero (valid values greater
than 50), so the `WVW_PAINT()` function will be called after this interval,
in millisecond, only if there was a pending `WM_PAINT()` request.

In any case, `WVW_PAINT()` can consult `wvw_GetPaintRect()` to check the pixel
regions needing actual repainting.

The default interval value is set to 100.


# Cursor

There are two styles of caret: horizontal (classic, console style) and
vertical (commonly used in GUI applications).

Programmer can select which style to choose, using `wvw_SetVertCaret()`
function. Currently the chosen style will be applied to all windows
(in current implementation of GTWVW caret can only be displayed on
topmost window, the only one allowed to accept input).

Default caret style is horizontal.


# Line Spacing

Programmer may choose to have spacing between lines. This may be desirable,
among other reasons, because GUI elements may overwrite the line above or
the line below the GUI objects. Each window may have its own line spacing,
settable via `wvw_SetLineSpacing()` function.

Next open window will use default line spacing of nDefLineSpacing settable
via `wvw_SetDefLineSpacing()` function.

nDefLineSpacing is initially 0.


# Fonts

In many GTWVW font functions, there are a lot of parameters to control
type, height, width and others styles like underline, bold, etc. For
example, the `wvw_CreateFont()` function has the following list of parameters:
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
  `FW_DONTCARE`, `FW_THIN`, etc.).
- `lItalic` Specifies an italic font if set to TRUE.
- `lUnderline` Specifies an underlined font if set to TRUE.
- `lStrikeOut` Specifies a strikeout font if set to TRUE.
- `nCharSet` Identify the character set. For example, `ANSI_CHARSET`,
  `DEFAULT_CHARSET`, `OEM_CHARSET`, etc. (all models can be viewed in file
  `wingdi.ch`).
- `nQuality` Specifies the output quality. The output quality defines how
  carefully the graphics device interface (GDI) must attempt to match the
  logical-font attributes to those of an actual physical font. It can be
  one of the following values: `DEFAULT_QUALITY` Appearance of the font does
  not matter.

  - `DRAFT_QUALITY`
    Appearance of the font is less important than when `PROOF_QUALITY` is used.
    For GDI raster fonts, scaling is enabled, which means that more font sizes
    are available, but the quality may be lower. Bold, italic, underline,
    and strikeout fonts are synthesized if necessary.
  - `PROOF_QUALITY`
    Character quality of the font is more important than exact matching of
    the logical-font attributes. For GDI raster fonts, scaling is disabled
    and the font closest in size is chosen. Although the chosen font size
    may not be mapped exactly when `PROOF_QUALITY` is used, the quality of
    the font is high and there is no distortion of appearance. Bold, italic,
    underline, and strikeout fonts are synthesized if necessary.

- `nEscapement` Specifies the angle, in tenths of degrees, between the
  escapement vector and the x-axis of the device. The escapement vector
  is parallel to the base line of a row of text. Default is zero.


# Callback Functions

There are some functions in GTWVW that need to be defined in our program
(in fact, these functions are called from GTWVW directly). Some of these
functions:

- `WVW_PAINT( nWinNum )`

  Is called every time Windows receive a message `WV_PAINT` (to repaint
  screen). The interval of `WVW_PAINT()` calling can be configured by
  function `wvw_SetPaintRefresh()`.

- `WVW_TIMER( nWinNum, hWnd, message, wParam, lParam )`

  Is called in every time interval, defined by function wvw_SetTimer().

- `WVW_SIZE( nWindow, hWnd, message, wParam, lParam )`

  Is called every time a window is minimized, maximized or restored. We
  need to define the function `wvw_Size_Ready()`, to verify if the processing
  of function `WVW_SIZE()` is needed.


# Status Bar

GTWVW uses native Windows Status Bar Control. This Status Bar will be placed
below the `MaxRow()` line. In GTWVT, on the other hand, Status Bar is drawn
at `MaxRow()` line.

Status Bar is automatically resized whenever its parent window is resized, and
automatically destroyed when its parent window is closed.

Status Bar functions are named `wvw_sb*()`.


# Timer

Programmer may set a timer process using `wvw_SetTimer()`, and end it using
`wvw_KillTimer()`. This is meant for a quick, simple timer process like displaying
clock on the status bar.

GTWVT has similar feature, but currently it is not activated.


# Tool Bar

GTWVW uses native Windows Tool Bar Control. This Tool Bar will be placed
above the 0 line. In GTWVT, on the other hand, Tool Bar is drawn anywhere
between 0 - `MaxRow()` line, drawn using GUI primitive functions.

Programmer may use system bitmaps in Tool Bar, or use his own bitmap.

Tool Bar is automatically resized following its parent window, and destroyed
when its parent window is closed.

Tool Bar functions are named `wvw_tb*()`.


# Scroll Bar

GTWVW uses native Windows Scroll Bar Control. This Scroll Bar will be placed
inside text area, floating above the texts. In GTWVT, on the other hand,
Scroll Bar is drawn using GUI primitive functions.

A window can have several scroll bars at the same time, vertically or
horizontally.

Scroll Bar is positioned using character coordinates, automatically resized
when its parent window changes dimension. Scroll Bar is also automatically
destroyed when its parent window is closed.

Scroll Bar functions are named `wvw_xb*()`.


# Push Button

GTWVW uses native Windows Push Button Control. This Push Button will be placed
inside text area, floating above the texts. In GTWVT, on the other hand,
Push Button is drawn using GUI primitive functions.

A window can have several push button at the same time. Programmer creates it
by defining where it will be placed, and what's the codeblock to execute when
the pushbutton is clicked. That's it.

Push button is positioned using character coordinates, automatically resized
when its parent window changes dimension. Push button is also automatically
destroyed when its parent window is closed.

Push button functions are named `wvw_pb*()`.


# Progress Bar

GTWVW uses native Windows Progress Bar Control. This Progress Bar will be placed
inside text area, floating above the texts. In GTWVT, on the other hand,
Progress Bar is drawn using GUI primitive functions.

A window can have several progress bar at the same time, vertically or horizontally.

Progress bar is positioned using character coordinates, automatically resized
when its parent window changes dimension. Progress bar is also automatically
destroyed when its parent window is closed.

Progress bar functions are named `wvw_pg*()`.


# HARBOUR CALLABLE FUNCTIONS

This section contains list (and description) of GTWVW .prg callable functions.
These descriptions and function list are taken from GTWVW source code,
with only a few additional notes. I've done my best to list all GTWVW
functions here. However, you will see that some of them are not
accompanied with description at all. Additionally, even the descriptions
given herein may not be accurate.

In other words, do not take this document as a reference. It is merely
an aid to help you get start with GTWVW. If you need a complete reference,
read the source code :-).

- GTWVW specific function list (with brief descriptions)
  - Window operation
  - GTWVW parameter settings
    - Paint Refresh Interval
    - Vertical/Horizontal Caret
    - Line Spacing
  - Status Bar
  - Timer
  - Tool Bar
  - Scroll Bar
  - Push Buttons
  - Progress Bar
- GTWVT compatibility function list (imported from GTWVT)
  - Window independent functions
  - Window dependent functions
  - General Windows API functions
- Brief descriptions of some GTWVW functions imported from GTWVT

See function skeletons inside the source code and in doc directory.


# GTWVW and GTWVT

## Compiling GTWVT program in GTWVW

GTWVW uses the same framework as its parent, GTWVT, and is maintained to be
as similar as possible in structure. Although it is not a correct perpective
a GTWVT application can be thought of as a GTWVW application with one window
(ie. the Main window). With this perspective, one may expect to compile
and link his GTWVT program using GTWVW. And yes, to some extent that is possible.
One of the simplest way is to include a function translator (`wvt2wvw.ch`) that
will translate GTWVT functions into GTWVW ones, assuming Current Window is
used in all `wvw_*()` functions. (Details of translation is described in `wvt2wvw.ch`).

Notes:
If one uses wvtclass functions, he must recompile/link `wvtclass.prg` and `wvtpaint.prg`,
after adding `#include "wvt2wvw.ch"`, using GTWVW.

Notes:
There is no guarantee that GTWVW will always be compatible with GTWVT.

## Compiling GTWVW program in GTWVT

I can't imagine that one will ever need to run his program in GTWVW and GTWVT
back and forth. Converting GTWVT program to GTWVW is fairly simple, but not the
other way around. Well, except if the program never calls GTWVW specific functions.
But then in that case why bother linking it with GTWVW anyway?

## Differences

There are some differences between GTWVW and GTWVT, besides the fact that GTWVW allows
multi-window operation, as described below.

## Miscellaneous

There are also some lower level differences between GTWVW and GTWVT. For example,
when executing `SetMode()` GTWVW will validate that the window size must not exceed
system's client (desktop) area. That's why some overly sized window, as in
"Dialog 1" of WVT*Class demo, will work in GTWVT but not in GTWVW.


# FUTURE OF GTWVW

Current GTWVW is not put into Harbour core as another standard GT, because it is
not ideal according to Harbour standards of GT. Ideally GTWVW should be split into
two parts:
- GTWVW the GT, following Harbour standards of GT API.
- WVW auxiliary library, which provides non standard interaction with the GT.

The framework of supporting multi GT has been designed by Giancarlo Niccolai et al, ie.
using GTINFO messages. (See doc/gtinfo.txt). The multi-window aspects of GTWVW
may be easily fit into this scheme, by introducing new message types, for example:
- `GTI_ISMULTIWINDOW` to query if the GT supports multi-windowing
- `GTI_NUMWINDOWS` to query number of windows opened
- `GTI_CURWINDOW` to query/set current window (the target of GT output commands)
- `GTI_FOCUSEDWINDOW` to query/set focused window
- etc.

However, IMHO this kind of decision requires serious, careful collective thoughts,
involving cross-platform gurus of Harbour, not by bursting ideas like I did in
above paragraph :-).

Additionally, some aspects of current GTWVW may not be accepted as a *good practice*
by other developers. For example, I am using Windows native controls (toolbar,
statusbar, scrollbar) which are intentionally avoided by GTWVT. At best, it is
because I can't give a convincing arguments. At worst, it is really a bad design
of mine... :-)

Well, frankly speaking my design decisions are driven by my actual need to port my
(big) application to Harbour GUI, in Windows environment. I am now parallelly developing
GTWVW and porting this application at the same time.

Unfortunately..., both these jobs are not my main jobs...

That explains why GTWVW evolves so slowly, and why I prefer pragmatic approach as in
applying more 'good looking' and 'simpler to handle' GUI elements like the native
Windows controls. In the future perhaps I even deliberately add more controls like
dropdown listbox, etc. as I see necessary in my application to port
(and if I have time to do that :-)).

Today I have a chance of uploading GTWVW into Harbour contrib area.
Hopefully this will make it easier for anybody interested in it. Some other
aspects of improvements (like C compiler portability) are also expected.
Lorenzo Fiorini and others are helping me with it at the moment.


# Examples

In my personal website () you can download an example of application,
screen shots of converted applications and some sample of code, exploring
a lot of interesting features. And of course, you can download this little
documentation.
Enjoy it!


# EPILOGUE

- This document may contain errors, because nobody is perfect :-)
- All specifications herein may change due to further development of GTWVW.
- Any comments, suggestions, and feedbacks are much appreciated
  (please send them to <gtwvw@csacomputer.com>).

Bandung, 2004-10-25
Budyanto Dj.
