/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Set API
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/*
 * ChangeLog:
 *
 * V 1.25   Paul Tucker                 Converted HB_SET_DEBUG back to Logical.
 *                                      Clipper 5.3 docs are incorrect on this.
 * V 1.19   Victor Szel                 #include <x> changed to #include "x".
 * V 1.18   David G. Holm               Added INKEY_EXTENDED to allow some
 *                                      platforms to select between normal
 *                                      and extended key codes.
 * V 1.17   Bruno Cantero               Added prototypes for hb_rddInitialize
 *                                      and hb_rddRelease.
 * V 1.16   David G. Holm               Added prototypes for hb_consoleRelease
 *                                      and hb_consoleInitialize, because set
 *                                      must be initialized before console and
 *                                      released after console.
 * V 1.15   David G. Holm               Changed hb_set.HB_SET_COLOR to be a
 *                                      fixed 64 byte buffer. Added prototype
 *                                      for hb_setColor()
 * V 1.14   David G. Holm               Removed the obsolete hb_set_fixed
 *                                      and HB_SETFIXED(), which I should
 *                                      have done when I took HB_SETFIXED()
 *                                      out of source/rtl/set.c in V 1.27.
 * V 1.13   David G. Holm               Added my email address.
 * V 1.12   David G. Holm               Added copyright and license header,
 *                                      along with a complete version history.
 * V 1.11   David G. Holm               Added hb_set_extrahan.
 * V 1.10   Victor Szel                 Renamed InitializeSets() to
 *                                      hb_setInitialize() and renamed
 * V 1.9    Victor Szel                 ReleaseSets() to hb_setRelease().
 * V 1.8    David G. Holm               Changed HB_SET_DEBUG to numeric.
 * V 1.7    Gonzalo A. Diethelm         Made the #include guard ANSI compliant.
 * V 1.6    Gonzalo A. Diethelm         Ensured that all Harbour functions
 *                                      are declared as HB_FUNCTION( void );
 * V 1.5    David G. Holm               Added hb_set_althan & hb_set_printhan.
 * V 1.4    David G. Holm               Added hb_set_fixed & HB_SET_FIXED().
 * V 1.3    David G. Holm               Changed __SETCENTURY to HB_SETCENTURY.
 *                                      Changed all _SET_name to HB_SET_name
 *                                      Changed the name of the function that
 *                                      initializes sets to InitializeSets().
 * V 1.2    Gonzalo A. Diethelm         Added comment with CVS Id keyword.
 * V 1.1    Antonio Linares             Committed to CVS.
 * V 1.0    David G. Holm               Initial version.
 *
 */

#ifndef HB_SET_H_
#define HB_SET_H_

#include "hbdefs.h"
#include "filesys.h"

typedef enum
{
   SC_NONE              = 0,    /* None */
   SC_NORMAL            = 1,    /* Underline */
   SC_INSERT            = 2,    /* Lower half block */
   SC_SPECIAL1          = 3,    /* Full block */
   SC_SPECIAL2          = 4     /* Upper half block */
} HB_cursor_enum;

typedef enum
{
   INKEY_MOVE           = 1,    /* Mouse Events */
   INKEY_LDOWN          = 2,    /* Mouse Left Click Down */
   INKEY_LUP            = 4,    /* Mouse Left Click Up */
   INKEY_RDOWN          = 8,    /* Mouse Right Click Down */
   INKEY_RUP            = 16,   /* Mouse Right Click Up */
   INKEY_KEYBOARD       = 128,  /* Keyboard Events */
   INKEY_ALL            = 159,  /* All Mouse and Keyboard Events */
   INKEY_EXTENDED       = 256   /* Extended Keyboard Events */
} HB_inkey_enum;

typedef enum
{
   HB_SET_INVALID_      = 0,

   HB_SET_EXACT         = 1,
   HB_SET_FIXED         = 2,
   HB_SET_DECIMALS      = 3,
   HB_SET_DATEFORMAT    = 4,
   HB_SET_EPOCH         = 5,
   HB_SET_PATH          = 6,
   HB_SET_DEFAULT       = 7,

   HB_SET_EXCLUSIVE     = 8,
   HB_SET_SOFTSEEK      = 9,
   HB_SET_UNIQUE        = 10,
   HB_SET_DELETED       = 11,

   HB_SET_CANCEL        = 12,
   HB_SET_DEBUG         = 13,
   HB_SET_TYPEAHEAD     = 14,

   HB_SET_COLOR         = 15,
   HB_SET_CURSOR        = 16,
   HB_SET_CONSOLE       = 17,
   HB_SET_ALTERNATE     = 18,
   HB_SET_ALTFILE       = 19,
   HB_SET_DEVICE        = 20,
   HB_SET_EXTRA         = 21,
   HB_SET_EXTRAFILE     = 22,
   HB_SET_PRINTER       = 23,
   HB_SET_PRINTFILE     = 24,
   HB_SET_MARGIN        = 25,

   HB_SET_BELL          = 26,
   HB_SET_CONFIRM       = 27,
   HB_SET_ESCAPE        = 28,
   HB_SET_INSERT        = 29,
   HB_SET_EXIT          = 30,
   HB_SET_INTENSITY     = 31,
   HB_SET_SCOREBOARD    = 32,
   HB_SET_DELIMITERS    = 33,
   HB_SET_DELIMCHARS    = 34,

   HB_SET_WRAP          = 35,
   HB_SET_MESSAGE       = 36,
   HB_SET_MCENTER       = 37,
   HB_SET_SCROLLBREAK   = 38,

   HB_SET_EVENTMASK     = 39
} HB_set_enum;

typedef struct
{
   BOOL   HB_SET_EXACT;
   BOOL   HB_SET_FIXED;
   int    HB_SET_DECIMALS;
   char * HB_SET_DATEFORMAT;
   int    HB_SET_EPOCH;
   char * HB_SET_PATH;
   char * HB_SET_DEFAULT;
   BOOL   HB_SET_EXCLUSIVE;
   BOOL   HB_SET_SOFTSEEK;
   BOOL   HB_SET_UNIQUE;
   BOOL   HB_SET_DELETED;
   BOOL   HB_SET_CANCEL;
   BOOL   HB_SET_DEBUG;
   int    HB_SET_TYPEAHEAD;
   char   HB_SET_COLOR[ 64 ];
   HB_cursor_enum HB_SET_CURSOR;
   BOOL   HB_SET_CONSOLE;
   BOOL   HB_SET_ALTERNATE;
   char * HB_SET_ALTFILE;
   char * HB_SET_DEVICE;
   BOOL   HB_SET_EXTRA;
   char * HB_SET_EXTRAFILE;
   BOOL   HB_SET_PRINTER;
   char * HB_SET_PRINTFILE;
   int    HB_SET_MARGIN;
   BOOL   HB_SET_BELL;
   BOOL   HB_SET_CONFIRM;
   BOOL   HB_SET_ESCAPE;
   BOOL   HB_SET_INSERT;
   BOOL   HB_SET_EXIT;
   BOOL   HB_SET_INTENSITY;
   BOOL   HB_SET_SCOREBOARD;
   char * HB_SET_DELIMCHARS;
   BOOL   HB_SET_DELIMITERS;
   BOOL   HB_SET_WRAP;
   int    HB_SET_MESSAGE;
   BOOL   HB_SET_MCENTER;
   BOOL   HB_SET_SCROLLBREAK;   /* QUESTION: What does this do ? */
   HB_inkey_enum HB_SET_EVENTMASK;
}  HB_set_struct;

extern HB_set_struct hb_set;
extern BOOL    hb_set_century;
extern FHANDLE hb_set_althan;
extern FHANDLE hb_set_extrahan;
extern FHANDLE hb_set_printhan;

extern void hb_setInitialize( void );
extern void hb_setRelease( void );

#endif /* HB_SET_H_ */
