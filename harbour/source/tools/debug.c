#include <extend.h>
#include <ctoharb.h>
#include <ctype.h>

extern STACK stack;
extern ITEM  aStatics;
PITEM _itemReturn( PITEM );
PITEM ArrayClone( PITEM );

/*
   __aStatic() -> Return the statics array
*/
HARBOUR __ASTATIC()
{
   _itemReturn( &aStatics );
}

/*
   __Static( nStatic ) -> Returns a specific static

   nStatic : Number of static
*/
HARBOUR __STATIC()
{
   PITEM pStatic;
   WORD  wStatic;

   wStatic = _parni(1);
   pStatic = ( ( PBASEARRAY ) aStatics.value.pBaseArray )->pItems + stack.iStatics +
             wStatic - 1;
   _itemReturn( pStatic );
}


