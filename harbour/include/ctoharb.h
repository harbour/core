/*
 * $Id$

   Copyright(C) 1999 by Antonio Linares.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to:

   The Free Software Foundation, Inc.,
   675 Mass Ave, Cambridge, MA 02139, USA.

   You can contact me at: alinares@fivetech.com
 */

#ifndef HB_CTOHARB_H_
#define HB_CTOHARB_H_

/* Calling Harbour from C code */

/* executing Harbour code from C */
void Message( PSYMBOL );
void PushSymbol( PSYMBOL pSym ); /* pushes a function pointer onto the stack */
void Push( PHB_ITEM pItem );        /* pushes any item to the stack */
void PushNil( void );            /* in this case it places nil at self */
/* parameters should come here using Push...() */
void PushInteger( int iNumber );
void PushLong( long lNumber );
void PushDouble( double dNumber, WORD wDec );
void PushString( char * szText, ULONG length );  /* pushes a string on to the stack */
void PushLogical( int iTrueFalse ); /* pushes a logical value onto the stack */
void PushSymbol( PSYMBOL );
void Do( WORD wParams );         /* invokes the virtual machine */
void Function( WORD wParams );         /* invokes the virtual machine */
void StackShow( void );

#endif /* HB_CTOHARB_H_ */
