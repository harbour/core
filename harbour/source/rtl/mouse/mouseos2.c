/*
 * $Id:
 */

/*
   Harbour mouse subsystem for OS/2 compilers.

   Copyright 1999  by Chen Kedem <niki@actcom.co.il>
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).

   V 1.0    Chen Kedem                  Initial version.
*/

/*---------------------------------------------------------------------------*/
/* TODO: those exist in os2.h which I don't have                             */
/* #define INCL_???? as needed                                               */
/* #include <os2.h>                                                          */
typedef unsigned BOOL;
typedef unsigned short USHORT;
typedef unsigned long ULONG;
#define API unsigned extern far pascal
#define TRUE 1
#define FALSE 0
API MouOpen ( void far *, unsigned far * );
API MouClose ( unsigned );
API MouDrawPtr ( unsigned );
API MouRemovePtr ();
API MouGetPtrPos ();
API MouSetPtrPos ();
API MouGetNumButtons ();
API MouReadEventQue ();
typedef struct _NOPTRRECT {
        USHORT row;
        USHORT col;
        USHORT cRow;
        USHORT cCol;
} NOPTRRECT;
typedef struct _PTRLOC {
        USHORT row;
        USHORT col;
} PTRLOC;
typedef struct _MOUEVENTINFO {
        USHORT fs;     /* Bit(s)    Significance (if set)       Mask ?
                          0         mouse move, no buttons down MOUSE_MOTION
                          1         mouse move, button 1 down   MOUSE_MOTION_WITH_BN1_DOWN
                          2         button 1 down               MOUSE_BN1_DOWN
                          3         mouse move, button 2 down   MOUSE_MOTION_WITH_BN2_DOWN
                          4         button 2 down               MOUSE_BN2_DOWN
                          5         mouse move, button 3 down   MOUSE_MOTION_WITH_BN3_DOWN
                          6         button 3 down               MOUSE_BN3_DOWN
                          7-15      reserved (0)                             */
        ULONG  time;
        USHORT row;
        USHORT col;
} MOUEVENTINFO;
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* TODO: those should probably be in mouse.h                                 */
extern void   hb_mouse_Init( void );
extern void   hb_mouse_Exit( void );
extern BOOL   hb_mouse_IsPresent( void );
extern void   hb_mouse_Show( void );
extern void   hb_mouse_Hide( void );
extern int    hb_mouse_Col( void );
extern int    hb_mouse_Row( void );
extern void   hb_mouse_SetPos( int, int );
/* extern BOOL   hb_mouse_GetCursor( void ); */  /* top level should do it */
/* extern void   hb_mouse_SetCursor( BOOL ); */  /* top level should do it */
extern BOOL   hb_mouse_IsButtonPressed( int );
extern int    hb_mouse_CountButton( void );
extern void   hb_mouse_SetBounds( int iTop, int iLeft, int iBottom, int iRight );
extern void   hb_mouse_GetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight );
/*---------------------------------------------------------------------------*/

static unsigned suMouHandle;            /* mouse logical handle */
/* static BOOL     sbMouVisible = FALSE; */  /* is mouse cursor visible ? */

void hb_mouse_Init()
{
    if ( MouOpen ( 0L, &suMouHandle ) ) /* try to open mouse */
        suMouHandle = 0;                /* no mouse found */
}

void hb_mouse_Exit(void)
{
    if ( suMouHandle )
    {
        MouClose ( suMouHandle );       /* relese mouse handle */
        suMouHandle = 0;
    }
}

BOOL hb_mouse_IsPresent(void)
{
    return ( suMouHandle != 0 );
}

void hb_mouse_Show(void)
{
    if ( suMouHandle )
    {
        MouDrawPtr ( suMouHandle );
/*      sbMouVisible = TRUE; */
    }
}

void hb_mouse_Hide(void)
{
    /*
       NOTE: mouse cursor always visible if not in full screen
    */
    NOPTRRECT rect;
    if ( suMouHandle )
    {
        /*
           QUESTION: should I call the GT function ?
           pro: encapsulating of the GetScreen function
           con: calling function from another module, GT must be linked in
           con: VioGetMode is been called twice
           may be a just a call to MaxRow/Col would be enough
        */
        rect.row  = 0;                        /* x-coordinate upper left */
        rect.col  = 0;                        /* y-coordinate upper left */
        rect.cRow = 20/*hb_gt_GetScreenHeight()*/;  /* x-coordinate lower right */
        rect.cCol = 60/*hb_gt_GetScreenWidth()*/;   /* y-coordinate lower right */
        MouRemovePtr ( &rect, suMouHandle );
/*      sbMouVisible = FALSE; */
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
    if ( suMouHandle )
    {
        MouGetPtrPos ( &pos, suMouHandle );
    }
    return ( (int)pos.col );
}

int hb_mouse_Row(void)
{
    PTRLOC pos;
    if ( suMouHandle )
    {
        MouGetPtrPos ( &pos, suMouHandle );
    }
    return ( (int)pos.row );
}

void hb_mouse_SetPos( int row, int col )
{
    PTRLOC pos;
    if ( suMouHandle )
    {
        pos.row = (USHORT)row;
        pos.col = (USHORT)col;
        MouSetPtrPos ( &pos, suMouHandle );
    }
}

/*
   NOTE: I think top level should handle that
*/
/*
BOOL hb_mouse_GetCursor(void)
{
    return ( sbMouVisible );
}
*/

/*
   NOTE: I think top level should handle that
*/
/*
void hb_mouse_SetCursor( BOOL bVisible )
{
    if ( bVisible )
        hb_mouse_Show();
    else
        hb_mouse_Hide();
    return ( sbMouVisible );
}
*/

BOOL hb_mouse_IsButtonPressed( int iButton )
{
    /*
       TODO: just a sample, a work should be done here !
    */
    BOOL bPressed = FALSE;
    USHORT WaitOption = 0;    /* 1 = wait until mouse event exist, 0 = don't */
    MOUEVENTINFO MouEvent;
    if ( suMouHandle )
    {
        MouReadEventQue ( &MouEvent, &WaitOption, suMouHandle );
        /*
           now MouEvent.fs contain the mask for the mouse event,
           what to do next?
       */
    }
    return ( bPressed );
}

int hb_mouse_CountButton(void)
{
    USHORT usButtons = 0;
    if ( suMouHandle )
    {
        MouGetNumButtons ( &usButtons, suMouHandle );
    }
    return ( (int)usButtons );
}

void hb_mouse_SetBounds( int iTop, int iLeft, int iBottom, int iRight )
{
    /*
       NOTE: I don't know if the OS/2 got a function to do that,
       the old book I'm using does not have such.
    */
}

void hb_mouse_GetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight )
{
    /*
       NOTE: I don't know if the OS/2 got a function to do that,
       the old book I'm using does not have such.
    */
}
