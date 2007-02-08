/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the GTINFO API
 *
 * Copyright 2004 Mauricio Abre <maurifull@datafull.com>
 * www - http://www.xharbour.org
 * www - http://www.harbour-project.org
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
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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

/* 
   Minimal informations:
   
   READ doc/gtinfo.txt
*/
    
#ifndef HB_GTINFO_CH_
#define HB_GTINFO_CH_

#define GTI_ISGRAPHIC          0  /* 1 if GT has graphic support / pixel oriented */
#define GTI_SCREENWIDTH        1  /* Get/set width of application window in pixels */
#define GTI_SCREENHEIGHT       2  /* Get/set height of appl. window in pixels */
#define GTI_SCREENDEPTH        3  /* Amount of bits used for colors in the application */
#define GTI_FONTSIZE           4  /* Get/set height of application font in pixels */
#define GTI_FONTWIDTH          5  /* Get/set width of application font characters */
#define GTI_DESKTOPWIDTH       6  /* Get width of desktop in pixels */
#define GTI_DESKTOPHEIGHT      7  /* Get height of desktop in pixels */
#define GTI_DESKTOPDEPTH       8  /* Amount of bits used for colors in system */
#define GTI_COMPATBUFFER       9  /* Use DOS CGA/EGA/VGA character/attribute buffer in SAVE/REST SCREEN */
#define GTI_KBDSHIFTS         10  /* Keyboard shift/ctrl/alt, caps/num/scroll & winkeys state */
#define GTI_KBDSPECIAL        11  /* This will get/set the status of the top row
                                     shift state handling. Enable to correct a
                                     documented keyboard handling bug under Win9x.
                                     Enable if the caps-lock key affects the top
                                     row keys.  (Alternate language keys are not
                                     handled properly by this temporary fix.
                                     Default is disabled.
                                   */
#define GTI_KBDALT            12  /* This will get/set the status of the alt-numpad
                                     key handling.
                                     Default is Enabled.
                                   */
#define GTI_FULLSCREEN        13  /* Is it full screen GT driver? */
#define GTI_KBDSUPPORT        14  /* Is it keyboard input supported? */
#define GTI_CLIPBOARDDATA     15  /* Get/Set clipboard */
#define GTI_CLIPBOARDPAST     16  /* Paste clipboard data into keyboard buffer */
#define GTI_CURSORBLINKRATE   19  /* Get/Set cursor blinking rate in milliseconds */
#define GTI_DESKTOPROWS       20  /* Get Size of desktop in character rows */
#define GTI_DESKTOPCOLS       21  /* Get Size of desktop in character cols */
#define GTI_FONTWEIGHT        22  /* Get/set the weight of the font used in application */
#define GTI_FONTQUALITY       23  /* Get/set quality of font rendering in the appl. */
#define GTI_FONTNAME          24  /* Set-only font name */
#define GTI_CODEPAGE          25  /* codepage */
#define GTI_WINTITLE          26  /* title */
#define GTI_ICONFILE          27  /* icon file */
#define GTI_ICONRES           28  /* icon resource */
#define GTI_MOUSESTATUS       29  /* mouse enabled = 1 mouse disabled = 0 */

#define GTI_INPUTFD           30  /* Get Standard input stream of application/GT */
#define GTI_OUTPUTFD          31  /* Get Standard output stream of application/GT */
#define GTI_ERRORFD           32  /* Get Standard error stream of application/GT */

#define GTI_ESCDELAY          33  /* Get/Set escape key delay */

/* these 2 are used for MaxCol(?) and MaxRow(?) */
#define GTI_VIEWMAXHEIGHT     34  /* Maximum viewable height:for current mode */
#define GTI_VIEWMAXWIDTH      35  /* Maximum viewable width:either win or full scrn */

#define GTI_VIEWPORTHEIGHT    36  /* Current viewport height:for current mode */
#define GTI_VIEWPORTWIDTH     37  /* Current viewport width:either win or full scrn */


#define GTI_STDOUTCON         38  /* redirect STDOUT to console */
#define GTI_STDERRCON         39  /* redirect STDERR to console */

#define GTI_ISCTWIN           40  /* is CTWIN supported? */

/* these are used _by_ MaxRow/Col */
#define GTI_WINDOW         0  /* Maximum window size ('window' in CT terms) */
#define GTI_SCREEN         1  /* Maximum screen size ('Screen' in CT terms) */
#define GTI_CLIENT         2  /* Maximum possible client size of a window */
#define GTI_MAX            3  /* Maximum possible window size (in Windows) */

/* Font weights */
#define GTI_FONTW_THIN     1
#define GTI_FONTW_NORMAL   2
#define GTI_FONTW_BOLD     3

/* Font sizes */
#define GTI_FONTQ_DRAFT    1
#define GTI_FONTQ_NORMAL   2
#define GTI_FONTQ_HIGH     3

/* Keyboard shifts states */
#define GTI_KBD_SHIFT         1
#define GTI_KBD_CTRL          2
#define GTI_KBD_ALT           4
#define GTI_KBD_LWIN          8
#define GTI_KBD_RWIN         16
#define GTI_KBD_MENU         32
#define GTI_KBD_SCROLOCK    256
#define GTI_KBD_NUMLOCK     512
#define GTI_KBD_CAPSLOCK   1024
#define GTI_KBD_INALTSEQ   2048
#define GTI_KBD_ACCENT1    4096
#define GTI_KBD_ACCENT2    8192
#define GTI_KBD_ACCENT3   16384
#define GTI_KBD_ACCENT4   32768

#endif /* HB_GTINFO_CH_ */
