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

#ifndef HB_SET_H_
#define HB_SET_H_

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapigt.h"

#if defined(HB_EXTERN_C)
extern "C" {
#endif

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

   HB_SET_EVENTMASK     = 39,

   HB_SET_VIDEOMODE     = 40,

   HB_SET_MBLOCKSIZE    = 41,
   HB_SET_MFILEEXT      = 42,

   HB_SET_STRICTREAD    = 43,
   HB_SET_OPTIMIZE      = 44, 
   HB_SET_AUTOPEN       = 45,
   HB_SET_AUTORDER      = 46,
   HB_SET_AUTOSHARE     = 47,

   /* Harbour SET extensions start at 100 */
   HB_SET_LANGUAGE      = 100
} HB_set_enum;

typedef struct
{
   BOOL    HB_SET_EXACT;
   BOOL    HB_SET_FIXED;
   BOOL    hb_set_century;
   int     HB_SET_DECIMALS;
   char *  HB_SET_DATEFORMAT;
   int     HB_SET_EPOCH;
   char *  HB_SET_PATH;
   char *  HB_SET_DEFAULT;
   BOOL    HB_SET_EXCLUSIVE;
   BOOL    HB_SET_SOFTSEEK;
   BOOL    HB_SET_UNIQUE;
   BOOL    HB_SET_DELETED;
   BOOL    HB_SET_CANCEL;
   BOOL    HB_SET_DEBUG;
   int     HB_SET_TYPEAHEAD;
   char    HB_SET_COLOR[ CLR_STRLEN ];
   BOOL    HB_SET_CONSOLE;
   BOOL    HB_SET_ALTERNATE;
   char *  HB_SET_ALTFILE;
   FHANDLE hb_set_althan;
   char *  HB_SET_DEVICE;
   BOOL    HB_SET_EXTRA;
   char *  HB_SET_EXTRAFILE;
   FHANDLE hb_set_extrahan;
   BOOL    HB_SET_PRINTER;
   char *  HB_SET_PRINTFILE;
   FHANDLE hb_set_printhan;
   int     HB_SET_MARGIN;
   BOOL    HB_SET_BELL;
   BOOL    HB_SET_CONFIRM;
   BOOL    HB_SET_ESCAPE;
   BOOL    HB_SET_INSERT;
   BOOL    HB_SET_EXIT;
   BOOL    HB_SET_INTENSITY;
   BOOL    HB_SET_SCOREBOARD;
   char *  HB_SET_DELIMCHARS;
   BOOL    HB_SET_DELIMITERS;
   BOOL    HB_SET_WRAP;
   int     HB_SET_MESSAGE;
   BOOL    HB_SET_MCENTER;
   BOOL    HB_SET_SCROLLBREAK;
   HB_inkey_enum HB_SET_EVENTMASK;
   int     HB_SET_MBLOCKSIZE;
   char *  HB_SET_MFILEEXT;
   BOOL    HB_SET_STRICTREAD;
   BOOL    HB_SET_OPTIMIZE;
   BOOL    HB_SET_AUTOPEN;
   BOOL    HB_SET_AUTORDER;
   BOOL    HB_SET_AUTOSHARE;
   int     HB_SET_VIDEOMODE;
} HB_SET_STRUCT;

extern HB_SET_STRUCT hb_set;

extern void hb_setInitialize( void );
extern void hb_setRelease( void );

#if defined(HB_EXTERN_C)
}
#endif

#endif /* HB_SET_H_ */
