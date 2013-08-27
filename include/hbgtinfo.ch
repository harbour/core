/*
 * Harbour Project source code:
 * Header file for the GTINFO API
 *
 * Copyright 2004 Mauricio Abre <maurifull@datafull.com>
 * www - http://www.xharbour.org
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* NOTE: This file is also used by C code. */

#ifndef HB_GTINFO_CH_
#define HB_GTINFO_CH_

#define HB_GTI_ISGRAPHIC        0   /* 1 if GT has graphic support / pixel oriented */
#define HB_GTI_SCREENWIDTH      1   /* Get/set width of application window in pixels */
#define HB_GTI_SCREENHEIGHT     2   /* Get/set height of application window in pixels */
#define HB_GTI_SCREENDEPTH      3   /* Amount of bits used for colors in the application */
#define HB_GTI_FONTSIZE         4   /* Get/set height of application font in pixels */
#define HB_GTI_FONTWIDTH        5   /* Get/set width of application font characters */
#define HB_GTI_DESKTOPWIDTH     6   /* Get width of desktop in pixels */
#define HB_GTI_DESKTOPHEIGHT    7   /* Get height of desktop in pixels */
#define HB_GTI_DESKTOPDEPTH     8   /* Amount of bits used for colors in system */
#define HB_GTI_COMPATBUFFER     9   /* Use DOS CGA/EGA/VGA character/attribute buffer in SAVE/REST SCREEN */
#define HB_GTI_KBDSHIFTS        10  /* Keyboard shift/ctrl/alt, caps/num/scroll & winkeys state */
#define HB_GTI_KBDSPECIAL       11  /* This will get/set the status of the top row
                                       shift state handling. Enable to correct a
                                       documented keyboard handling bug under Win9x.
                                       Enable if the caps-lock key affects the top
                                       row keys.  (Alternate language keys are not
                                       handled properly by this temporary fix.
                                       Default is disabled. */
#define HB_GTI_KBDALT           12  /* This will get/set the status of the alt-numpad
                                       key handling.
                                       Default is Enabled. */
#define HB_GTI_ISSCREENPOS      13  /* Is full screen cursor positioning supported by GT driver? */
#define HB_GTI_FULLSCREEN       HB_GTI_ISSCREENPOS /* Compatibility. Do not use it. */
#define HB_GTI_KBDSUPPORT       14  /* Is it keyboard input supported? */
#define HB_GTI_CLIPBOARDDATA    15  /* Get/Set clipboard */
#define HB_GTI_CLIPBOARDPASTE   16  /* Paste clipboard data into keyboard buffer */
#define HB_GTI_CURSORBLINKRATE  19  /* Get/Set cursor blinking rate in milliseconds */
#define HB_GTI_DESKTOPROWS      20  /* Get Size of desktop in character rows */
#define HB_GTI_DESKTOPCOLS      21  /* Get Size of desktop in character cols */
#define HB_GTI_FONTWEIGHT       22  /* Get/set the weight of the font used in application */
#define HB_GTI_FONTQUALITY      23  /* Get/set quality of font rendering in the appl. */
#define HB_GTI_FONTNAME         24  /* Set-only font name */
#define HB_GTI_CODEPAGE         25  /* codepage */
#define HB_GTI_WINTITLE         26  /* title */
#define HB_GTI_ICONFILE         27  /* icon file */
#define HB_GTI_ICONRES          28  /* icon resource */
#define HB_GTI_MOUSESTATUS      29  /* mouse enabled = 1 mouse disabled = 0 */

#define HB_GTI_INPUTFD          30  /* Get Standard input stream of application/GT */
#define HB_GTI_OUTPUTFD         31  /* Get Standard output stream of application/GT */
#define HB_GTI_ERRORFD          32  /* Get Standard error stream of application/GT */

#define HB_GTI_ESCDELAY         33  /* Get/Set escape key delay */

/* these 2 are used for MaxCol(?) and MaxRow(?) */
#define HB_GTI_VIEWMAXHEIGHT    34  /* Maximum viewable height:for current mode */
#define HB_GTI_VIEWMAXWIDTH     35  /* Maximum viewable width:either win or full scrn */

#define HB_GTI_VIEWPORTHEIGHT   36  /* Current viewport height:for current mode */
#define HB_GTI_VIEWPORTWIDTH    37  /* Current viewport width:either win or full scrn */

#define HB_GTI_STDOUTCON        38  /* redirect STDOUT to console */
#define HB_GTI_STDERRCON        39  /* redirect STDERR to console */

#define HB_GTI_ISCTWIN          40  /* is CTWIN supported? */
#define HB_GTI_ISMULTIWIN       41  /* is multi window supported? */
#define HB_GTI_GETWIN           42  /* get current window handle or screen settings */
#define HB_GTI_SETWIN           43  /* restore window or screen settings */
#define HB_GTI_NEWWIN           44  /* create new window */

#define HB_GTI_ADDKEYMAP        45  /* add key escape sequences */
#define HB_GTI_DELKEYMAP        46  /* del key escape sequences */

#define HB_GTI_ISUNICODE        47  /* is Unicode input/output enabled? */

#define HB_GTI_SELECTCOPY       48  /* toggles screen content selection and copy to clipboard (supported by: GTWVT) */
#define HB_GTI_RESIZABLE        49  /* toggles ability to resize window (supported by: GTWVT) */
#define HB_GTI_CLOSABLE         50  /* toggles ability to close window (supported by: GTWVT) */

/* Additional constants to enhance GT */
#define HB_GTI_NOTIFIERBLOCK    51  /* Deprecated. Use HB_K_* inkey.ch events instead. */
#define HB_GTI_SCREENSIZE       52  /* Get/Set height/width of application window in pixels */
#define HB_GTI_PALETTE          53  /* Get/Set console colors 0 - 15 given an array of 16 elements containing RGB colors */

#define HB_GTI_RESIZEMODE       54  /* Get/Set console resize mode : HB_GTI_RESIZEMODE_FONT | HB_GTI_RESIZEMODE_ROWS */
#define HB_GTI_SETPOS_XY        55  /* Get/Set current top-left position coordinates of the window by pixels */
#define HB_GTI_SETPOS_ROWCOL    56  /* Set current top-left position coordinates of the window by row/cols */

#define HB_GTI_BOXCP            57  /* Codepage used for box drawing */

#define HB_GTI_CARGO            58  /* Storage of any user defined value */
#define HB_GTI_FONTSEL          59  /* X11 style font selecting */

#define HB_GTI_INKEYFILTER      60  /* Get/Set inkey keycodes filter */
#define HB_GTI_INKEYREAD        61  /* Get/Set inkey read block */

#define HB_GTI_ALTENTER         62  /* Toggles Alt+Enter as full screen switch (supported by: GTWVT) */
#define HB_GTI_ISFULLSCREEN     63  /* Is the GT windows using the full physical display? (supported by: GTWIN, GTWVT) */
#define HB_GTI_ONLINE           64  /* Is terminal connected? */
#define HB_GTI_VERSION          65  /* Get terminal version string */

#define HB_GTI_MAXIMIZED        66  /* Get/Set Window's Maximized status (supported by: GTWVT) */
#define HB_GTI_FONTATTRIBUTE    67  /* Get/Set font attribute */
#define HB_GTI_UNITRANS         68  /* Set translation table for UNICODE characters */
#define HB_GTI_WINHANDLE        69  /* Get console window low level handle */
#define HB_GTI_MOUSEPOS_XY      70  /* Get mouse position in pixels */
#define HB_GTI_DISPIMAGE        71  /* Display image with given name */
#define HB_GTI_REDRAWMAX        72  /* Maximum number of unchanged neighboring chars in redrawn line */
#define HB_GTI_RESIZESTEP       73  /* Enable/Disable window resizing steps */

/* Font weights */
#define HB_GTI_FONTW_THIN       1
#define HB_GTI_FONTW_NORMAL     2
#define HB_GTI_FONTW_BOLD       3

/* Font qualities */
#define HB_GTI_FONTQ_DRAFT      1
#define HB_GTI_FONTQ_NORMAL     2
#define HB_GTI_FONTQ_HIGH       3

/* Font attributes */
#define HB_GTI_FONTA_FIXMETRIC  0x0001
#define HB_GTI_FONTA_CLRBKG     0x0002
#define HB_GTI_FONTA_DRAWBOX    0x0004
#define HB_GTI_FONTA_CTRLCHARS  0x0008
#define HB_GTI_FONTA_NOSTRETCH  0x0010

/* Keyboard shifts states */
#define HB_GTI_KBD_SHIFT        0x000001
#define HB_GTI_KBD_CTRL         0x000002
#define HB_GTI_KBD_ALT          0x000004
#define HB_GTI_KBD_LWIN         0x000008
#define HB_GTI_KBD_RWIN         0x000010
#define HB_GTI_KBD_MENU         0x000020
#define HB_GTI_KBD_INSERT       0x000080
#define HB_GTI_KBD_SCROLOCK     0x000100
#define HB_GTI_KBD_NUMLOCK      0x000200
#define HB_GTI_KBD_CAPSLOCK     0x000400
#define HB_GTI_KBD_INALTSEQ     0x000800
#define HB_GTI_KBD_ACCENT1      0x001000
#define HB_GTI_KBD_ACCENT2      0x002000
#define HB_GTI_KBD_ACCENT3      0x004000
#define HB_GTI_KBD_ACCENT4      0x008000
#define HB_GTI_KBD_LSHIFT       0x010000
#define HB_GTI_KBD_RSHIFT       0x020000
#define HB_GTI_KBD_LCTRL        0x040000
#define HB_GTI_KBD_RCTRL        0x080000
#define HB_GTI_KBD_LALT         0x100000
#define HB_GTI_KBD_RALT         0x200000

#ifdef HB_LEGACY_LEVEL4
/* Harbour GT callback events - WORK IN PROGRESS */
#define HB_GTE_ACTIVATE         1
#define HB_GTE_SETFOCUS         2
#define HB_GTE_KILLFOCUS        3
#define HB_GTE_CLOSE            4
#define HB_GTE_RESIZED          5
#endif

/* Harbour GT Reszing mode constants */
#define HB_GTI_RESIZEMODE_FONT  0   /* Default */
#define HB_GTI_RESIZEMODE_ROWS  1

#endif /* HB_GTINFO_CH_ */
