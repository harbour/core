// Calling Harbour from C code

/* executing Harbour code from C */
void PushSymbol( PSYMBOL pSym ); /* pushes a function pointer onto the stack */
void Push( PITEM pItem );        /* pushes any item to the stack */
void PushNil( void );            /* in this case it places nil at self */
/* parameters should come here using Push...() */
void PushInteger( int iNumber );
void PushLong( long lNumber );
void PushDouble( double dNumber );
void PushString( char * szText, WORD wLength );  /* pushes a string on to the stack */
void Do( WORD wParams );         /* invokes the virtual machine */
void Function( WORD wParams );         /* invokes the virtual machine */
void StackShow( void );
