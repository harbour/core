/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Keyboard API
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

#ifndef HB_INKEY_H_
#define HB_INKEY_H_

#include "hbdefs.h"

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

/* Harbour keyboard support functions */
extern int  hb_inkey ( double seconds, HB_inkey_enum event_mask, BOOL wait, BOOL forever ); /* Wait for keyboard input */
extern int  hb_inkeyGet( void );            /* Extract the next key from the Harbour keyboard buffer */
extern void hb_inkeyPut( int ch );          /* Inserts an inkey code into the keyboard buffer */
extern int  hb_inkeyLast( void );           /* Return the value of the last key that was extracted */
extern int  hb_inkeyNext( void );           /* Return the next key without extracting it */
extern void hb_inkeyPoll( void );           /* Poll the console keyboard to stuff the Harbour buffer */
extern void hb_inkeyReset( BOOL allocate ); /* Reset the Harbour keyboard buffer */

extern void hb_releaseCPU( void );          /* Attempt to release a CPU time slice */

#endif /* HB_INKEY_H_ */
