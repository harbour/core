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
   HB_SET_LANGUAGE      = 100,
   HB_SET_IDLEREPEAT    = 101

} HB_set_enum;

typedef struct
{
   /* Lower case members are indirectly related to a SET */
   FHANDLE hb_set_althan;
   BOOL    hb_set_century;
   FHANDLE hb_set_extrahan;
   FHANDLE hb_set_printhan;

   /* Upper case members are directly related to a SET */
   BOOL    HB_SET_ALTERNATE;
   char *  HB_SET_ALTFILE;
   BOOL    HB_SET_AUTOPEN;
   BOOL    HB_SET_AUTORDER;
   int     HB_SET_AUTOSHARE;
   BOOL    HB_SET_BELL;
   BOOL    HB_SET_CANCEL;
   char    HB_SET_COLOR[ CLR_STRLEN ];
   BOOL    HB_SET_CONFIRM;
   BOOL    HB_SET_CONSOLE;
   char *  HB_SET_DATEFORMAT;
   BOOL    HB_SET_DEBUG;
   int     HB_SET_DECIMALS;
   char *  HB_SET_DEFAULT;
   BOOL    HB_SET_DELETED;
   char *  HB_SET_DELIMCHARS;
   BOOL    HB_SET_DELIMITERS;
   char *  HB_SET_DEVICE;
   int     HB_SET_EPOCH;
   BOOL    HB_SET_ESCAPE;
   HB_inkey_enum  HB_SET_EVENTMASK;
   BOOL    HB_SET_EXACT;
   BOOL    HB_SET_EXCLUSIVE;
   BOOL    HB_SET_EXIT;
   BOOL    HB_SET_EXTRA;
   char *  HB_SET_EXTRAFILE;
   BOOL    HB_SET_FIXED;
   BOOL    HB_SET_IDLEREPEAT;
   BOOL    HB_SET_INSERT;
   BOOL    HB_SET_INTENSITY;
   char *  HB_SET_PATH;
   int     HB_SET_MARGIN;
   int     HB_SET_MBLOCKSIZE;
   BOOL    HB_SET_MCENTER;
   int     HB_SET_MESSAGE;
   char *  HB_SET_MFILEEXT;
   BOOL    HB_SET_OPTIMIZE;
   BOOL    HB_SET_PRINTER;
   char *  HB_SET_PRINTFILE;
   BOOL    HB_SET_SCOREBOARD;
   BOOL    HB_SET_SCROLLBREAK;
   BOOL    HB_SET_SOFTSEEK;
   BOOL    HB_SET_STRICTREAD;
   int     HB_SET_TYPEAHEAD;
   BOOL    HB_SET_UNIQUE;
   int     HB_SET_VIDEOMODE;
   BOOL    HB_SET_WRAP;
} HB_SET_STRUCT;

extern HB_SET_STRUCT hb_set;

extern void hb_setInitialize( void );
extern void hb_setRelease( void );
extern PATHNAMES * hb_setGetFirstSetPath( void );

typedef enum
{
   HB_SET_LISTENER_BEFORE,
   HB_SET_LISTENER_AFTER
} HB_set_listener_enum;
typedef void HB_SET_LISTENER_CALLBACK( HB_set_enum, HB_set_listener_enum );

extern int hb_setListenerAdd( HB_SET_LISTENER_CALLBACK * );
extern void hb_setListenerNotify( HB_set_enum, HB_set_listener_enum );
extern int hb_setListenerRemove( int );

#if defined(HB_EXTERN_C)
}
#endif

#endif /* HB_SET_H_ */
