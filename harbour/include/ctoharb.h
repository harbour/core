/*
 * $Id$
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
void PushString( char * szText, WORD wLength );  /* pushes a string on to the stack */
void PushLogical( int iTrueFalse ); /* pushes a logical value onto the stack */
void PushSymbol( PSYMBOL );
void Do( WORD wParams );         /* invokes the virtual machine */
void Function( WORD wParams );         /* invokes the virtual machine */
void StackShow( void );

#endif /* HB_CTOHARB_H_ */
