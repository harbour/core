/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour mouse subsystem for OS/2 compilers
 *
 * Copyright 1999 Chen Kedem <niki@actcom.co.il>
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
 * V 1.3    Chen Kedem                  hb_mouse_Hide() now work in any text
 *                                      screen mode by calling VioGetMode.
 *                                      hb_mouse_IsButtonPressed() now return
 *                                      TRUE if any button was pressed.
 * V 1.0    Chen Kedem                  Initial version.
 *
 */

#define INCL_MOU
#define INCL_VIO             /* needed only for VioGetMode/VIOMODEINFO    */
#define INCL_NOPMAPI         /* exclude Presentation Manager Include File */

#include <os2.h>
#include "mouseapi.h"

static unsigned s_uMouHandle;                   /* mouse logical handle */

void hb_mouse_Init()
{
    USHORT fsEvents = MOUSE_MOTION_WITH_BN1_DOWN | MOUSE_BN1_DOWN |
                      MOUSE_MOTION_WITH_BN2_DOWN | MOUSE_BN2_DOWN |
                      MOUSE_MOTION_WITH_BN3_DOWN | MOUSE_BN3_DOWN ;

    if ( MouOpen ( 0L, &s_uMouHandle ) )        /* try to open mouse */
        s_uMouHandle = 0;                       /* no mouse found */
    else
        MouSetEventMask ( &fsEvents, s_uMouHandle );     /* mask some events */
}

void hb_mouse_Exit(void)
{
    if ( s_uMouHandle )
    {
        MouClose ( s_uMouHandle );              /* relese mouse handle */
        s_uMouHandle = 0;
    }
}

BOOL hb_mouse_IsPresent(void)
{
    return ( s_uMouHandle != 0 );
}

void hb_mouse_Show(void)
{
    if ( s_uMouHandle )
        MouDrawPtr ( s_uMouHandle );
}

void hb_mouse_Hide(void)
{
    /*
       NOTE: mouse cursor always visible if not in full screen
    */
    NOPTRRECT rect;
    VIOMODEINFO vi;                             /* needed to get max Row/Col */
    if ( s_uMouHandle )
    {
        /*
           QUESTION: should I call the GT MaxRow/Col function ?
           pro: encapsulating of the GetScreen function
           con: calling function from another module, GT must be linked in
           con: VioGetMode is been called twice
        */
        vi.cb = sizeof(VIOMODEINFO);
        VioGetMode(&vi, 0);
        rect.row  = 0;                          /* x-coordinate upper left */
        rect.col  = 0;                          /* y-coordinate upper left */
        rect.cRow = vi.row - 1;                 /* x-coordinate lower right */
        rect.cCol = vi.col - 1;                 /* y-coordinate lower right */
        MouRemovePtr ( &rect, s_uMouHandle );
    }
}

/*
   QUESTION: when getting mouse coordinate you normally need both
   row and column, we should think about using just one function
   hb_mouse_GetPos( &row, &col ) or something like that
*/

int hb_mouse_Col(void)
{
    PTRLOC pos;
    if ( s_uMouHandle )
    {
        MouGetPtrPos ( &pos, s_uMouHandle );
    }
    return ( (int)pos.col );
}

int hb_mouse_Row(void)
{
    PTRLOC pos;
    if ( s_uMouHandle )
    {
        MouGetPtrPos ( &pos, s_uMouHandle );
    }
    return ( (int)pos.row );
}

void hb_mouse_SetPos( int row, int col )
{
    PTRLOC pos;
    if ( s_uMouHandle )
    {
        pos.row = (USHORT)row;
        pos.col = (USHORT)col;
        MouSetPtrPos ( &pos, s_uMouHandle );
    }
}

BOOL hb_mouse_IsButtonPressed( int iButton )
{
    /*
       TOFIX: every time I read event from the queue I lose the result
       so I can not check if iButton was pressed, so for now I ignore
       iButton and return TRUE if the last event saved had DOWN in it.
       also to keep the noise level down I mask out MOUSE_MOTION events.
    */
    USHORT uMask = 0x0000;
    USHORT WaitOption = 0;    /* 1 = wait until mouse event exist, 0 = don't */
    MOUEVENTINFO MouEvent;
    if ( s_uMouHandle )
    {
        MouReadEventQue ( &MouEvent, &WaitOption, s_uMouHandle );
        uMask = MouEvent.fs;

#ifdef PLEASE_PLEASE_MAKE_IT_WORK
        switch(iButton)
        {
        case 1:
            uMask &= ( MOUSE_MOTION_WITH_BN1_DOWN | MOUSE_BN1_DOWN );
            break;
        case 2:
            uMask &= ( MOUSE_MOTION_WITH_BN2_DOWN | MOUSE_BN2_DOWN );
            break;
        case 3:
            uMask &= ( MOUSE_MOTION_WITH_BN3_DOWN | MOUSE_BN3_DOWN );
            break;
        }
#else
            uMask &= (
                       MOUSE_MOTION_WITH_BN1_DOWN | MOUSE_BN1_DOWN |
                       MOUSE_MOTION_WITH_BN2_DOWN | MOUSE_BN2_DOWN |
                       MOUSE_MOTION_WITH_BN3_DOWN | MOUSE_BN3_DOWN
                     ) ;
#endif
    }
    return ( uMask != 0 );
}

int hb_mouse_CountButton(void)
{
    USHORT usButtons = 0;
    if ( s_uMouHandle )
    {
        MouGetNumButtons ( &usButtons, s_uMouHandle );
    }
    return ( (int)usButtons );
}

void hb_mouse_SetBounds( int iTop, int iLeft, int iBottom, int iRight )
{
    /*
       TODO: (I don't think that the OS/2 got a function to do it)
    */
}

void hb_mouse_GetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight )
{
    /*
       TODO: (I don't think that the OS/2 got a function to do it)
    */
}

