/*
 * $Id$
 */

/* The Harbour implementation of codeblocks */

#include <extend.h>
#include <string.h>

extern STACK stack;

#define FALSE   0
#define TRUE    1

/* Uncomment this to trace codeblocks activity
#define CODEBLOCKDEBUG
*/

/* Creates the codeblock structure
 *
 * The buffer should contain:
 * +0 bytes -> number of referenced local variables
 * +2 bytes -> table of referenced local variables
 * +2 + 2 *(number of referenced variables) -> codeblock pcode
 */
PCODEBLOCK CodeblockNew( BYTE * pBuffer, WORD wSize, PSYMBOL pSymbols,
            int iStaticsBase, WORD wStackBase )
{
  PCODEBLOCK pCBlock;
  WORD wVars;

  pCBlock =( PCODEBLOCK ) _xgrab( sizeof(CODEBLOCK) );

  /* Check the number of referenced local variables
   */
  wVars = * ( (WORD *) pBuffer );
  wSize -= ( wVars + 1 ) * 2;
  pBuffer +=2;
  pCBlock->wLocals =wVars;
  if( wVars )
  {
    WORD w = 0;

    /* Create the table with references to local variables
     * If this codeblock will be exported from a function then
     * all references will be replaced with current values of
     * these variables
     */
    pCBlock->pItems =(PITEM) _xgrab( sizeof(ITEM) * wVars );

    while( wVars-- )
    {
      pCBlock->pItems[ w ].wType =IT_INTEGER; /* not really integer  */
      pCBlock->pItems[ w ].value.wItem = * ( (WORD*) pBuffer );
      ++w;
      pBuffer +=2;
    }
  }
  else
    pCBlock->pItems =NULL;

  /* the codeblock initally contains references to local variables
   */
  pCBlock->wDetached =FALSE;
  /*
   * pcode is stored in static segment now.
   * The only allowed operation on a codeblock is evaluating it then
   * there is no need to duplicate its pcode -just store the pointer to it
   */
  pCBlock->pCode = pBuffer;

  pCBlock->pSymbols  =pSymbols;
  pCBlock->wDetached =FALSE;
  pCBlock->lCounter  =1;
  pCBlock->iStatBase =iStaticsBase;
  /*
   * wStackBase is stack base of function where the codeblock was defined
   * We need it because stack.pBase points to a stack base of EVAL function
   * at the time of codeblock evaluation.
   */
  pCBlock->wRefBase  =wStackBase;

#ifdef CODEBLOCKDEBUG
  printf( "codeblock created (%li) %lx\n", pCBlock->lCounter, pCBlock );
#endif
  return pCBlock;
}

/* Delete a codeblock
 */
void  CodeblockDelete( PCODEBLOCK pCBlock )
{
#ifdef CODEBLOCKDEBUG
  printf( "delete a codeblock (%li) %lx\n", pCBlock->lCounter, pCBlock );
#endif
  if( --pCBlock->lCounter == 0 )
  {
    WORD w = 0;

    /* free space allocated for local variables
    */
    if( pCBlock->pItems )
    {
      while( w < pCBlock->wLocals )
        ItemRelease( &pCBlock->pItems[ w++ ] );
      _xfree( pCBlock->pItems );
    }

    /* free space allocated for a CODEBLOCK structure
    */
    _xfree( pCBlock );
    #ifdef CODEBLOCKDEBUG
      printf( "codeblock deleted (%li) %lx\n", pCBlock->lCounter, pCBlock );
    #endif
  }
}

/* Function to unlink variables referenced in a codeblock from a function
 * where this codeblock was created
 */
void CodeblockDetach( PCODEBLOCK pCBlock )
{
  if( pCBlock->wLocals && !pCBlock->wDetached )
  {
    /* this codeblock refers to local variables */
    WORD w = 0;
    PITEM pItem;

    while( w < pCBlock->wLocals )
    {
      /* replace the position of local variable on the stack with
       * it's current value
       * stack.pBase still points to a stack frame of function
       * where this codeblock was defined
       */
      pItem =pCBlock->pItems + w;
      pItem =stack.pBase +pItem->value.wItem + 1;
      if( IS_BYREF( pItem ) )
          pItem =stack.pItems +pItem->value.wItem;
      ItemCopy( pCBlock->pItems + w, pItem );
      ++w;
    }
    pCBlock->wDetached =TRUE;
  }
    #ifdef CODEBLOCKDEBUG
      printf( "codeblock detached(%li) %lx\n", pCBlock->lCounter, pCBlock );
    #endif
}

/* Evaluate passed codeblock
 * wStackBase is stack base of function where the codeblock was defined
 * We need it because stack.pBase points to a stack base of EVAL function
 */
void CodeblockEvaluate( PCODEBLOCK pCBlock )
{
  int iStatics = stack.iStatics;

  stack.iStatics = pCBlock->iStatBase;
  VirtualMachine( pCBlock->pCode, pCBlock->pSymbols );
  stack.iStatics = iStatics;
}

/* Get local variable referenced in a codeblock
 */
PITEM  CodeblockGetVar( PITEM pItem, SHORT iItemPos )
{
  PCODEBLOCK pCBlock = (PCODEBLOCK)pItem->value.pCodeblock;
  PITEM pLocalVar;

  pLocalVar =&pCBlock->pItems[ -iItemPos -1 ];
  /* if a codeblock have detached local variables then it stores their value */
  if( !pCBlock->wDetached )
  {
    /* when variables are not detached then a codeblock stores the variable's
     * position on the stack
     */
    pLocalVar =stack.pItems +pCBlock->wRefBase +pLocalVar->value.wItem + 1;
  }

  return pLocalVar;
}

/* Copy the codeblock
 * TODO: check if such simple pointer coping will allow to evaluate
 * codeblocks recursively
 */
void  CodeblockCopy( PITEM pDest, PITEM pSource )
{
  pDest->value.pCodeblock =pSource->value.pCodeblock;
  ((PCODEBLOCK) pDest->value.pCodeblock)->lCounter++;
  #ifdef CODEBLOCKDEBUG
    printf( "copy a codeblock (%li) %lx\n", ((PCODEBLOCK) pDest->value.pCodeblock)->lCounter, pDest->value.pCodeblock);
  #endif
}
