/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler main file
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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

/* this #define HAVE TO be placed before all #include directives
 */
#define  HB_MACRO_SUPPORT

#include "macro.h"

/* TODO:
 * include these variables in SET subsystem ?
 */
static BOOL hb_comp_bShortCuts = TRUE;  /* .and. & .or. expressions shortcuts */
static BOOL hb_comp_bUseName10 = FALSE;  /* names limited to 10 characters */

/* ************************************************************************* */

/* Compile passes string into a pcode buffer
 */
static int hb_macroCompile( HB_MACRO_PTR pMacro, char * szString, int iFlag )
{
   /* initialize the input buffer - it will be scanned by lex */
   pMacro->string = szString;
   pMacro->length = strlen( szString );
   pMacro->pos    = 0;

   /* initialize the output (pcode) buffer - it will be filled by yacc */
   pMacro->pCodeInfo = (HB_PCODE_INFO_PTR ) hb_xgrab( sizeof( HB_PCODE_INFO ) );
   pMacro->pCodeInfo->lPCodeSize = HB_PCODE_SIZE;
   pMacro->pCodeInfo->lPCodePos  = 0;
   pMacro->pCodeInfo->pLocals    = NULL;
   pMacro->pCodeInfo->pPrev      = NULL;
   pMacro->pCodeInfo->pCode      = ( BYTE * ) hb_xgrab( HB_PCODE_SIZE );

   /* We have to specify if we want a push or pop operation because
    * we are using different pcodes for these operations
    */
   pMacro->Flags = iFlag;

   return hb_compParse( pMacro );
}

static void hb_macroDelete( HB_MACRO_PTR pMacro )
{
   hb_xfree( (void *) pMacro->pCodeInfo->pCode );
   hb_xfree( (void *) pMacro->pCodeInfo );
}

static BOOL hb_macroCheckParam( HB_ITEM_PTR pItem )
{
   BOOL bValid = TRUE;

   if( ! IS_STRING(pItem) )
   {
      HB_ITEM_PTR pResult = hb_errRT_BASE_Subst( EG_ARG, 1080, NULL, "&" );

      bValid = FALSE;
      if( pResult )
      {
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
         bValid = TRUE;
      }
   }
   return bValid;
}

static void hb_macroRun( HB_MACRO_PTR pMacro )
{
   hb_vmExecute( pMacro->pCodeInfo->pCode, pMacro->pSymbols );
}

static void hb_macroSyntaxError( HB_MACRO_PTR pMacro )
{
   HB_ITEM_PTR pResult = hb_errRT_BASE_Subst( EG_SYNTAX, 1449, NULL, "&" );

   if( pResult )
   {
      hb_vmPush( pResult );
      hb_itemRelease( pResult );
   }
   HB_SYMBOL_UNUSED( pMacro );   /* TODO: better error messagess */
}


/* NOTE:
 *   This will be called when macro variable or macro expression is
 * placed on the right side of the assignment or when it is used as
 * a parameter.
 */
void hb_macroGetValue( HB_ITEM_PTR pItem, PHB_SYMB pTable )
{
   if( hb_macroCheckParam( pItem ) )
   {
      HB_MACRO struMacro;
      int iStatus;
      char * szString = pItem->item.asString.value;

      struMacro.bShortCuts = hb_comp_bShortCuts;
      struMacro.bName10    = hb_comp_bUseName10;
      struMacro.pSymbols   = pTable;
      iStatus = hb_macroCompile( &struMacro, szString, HB_P_MACROPUSH );

      hb_stackPop();    /* remove compiled string */
      if( iStatus == HB_MACRO_OK && struMacro.status == HB_MACRO_OK )
         hb_macroRun( &struMacro );
      else
         hb_macroSyntaxError( &struMacro );
      hb_macroDelete( &struMacro );
   }
}

/* NOTE:
 *   This will be called when macro variable or macro expression is
 * placed on the left side of the assignment
 */
void hb_macroSetValue( HB_ITEM_PTR pItem, PHB_SYMB pTable )
{
   if( hb_macroCheckParam( pItem ) )
   {
      HB_MACRO struMacro;
      int iStatus;
      char * szString = pItem->item.asString.value;

      struMacro.bShortCuts = hb_comp_bShortCuts;
      struMacro.bName10    = hb_comp_bUseName10;
      struMacro.pSymbols   = pTable;
      iStatus = hb_macroCompile( &struMacro, szString, HB_P_MACROPOP );

      hb_stackPop();    /* remove compiled string */
      if( iStatus == HB_MACRO_OK && struMacro.status == HB_MACRO_OK )
         hb_macroRun( &struMacro );
      else
         hb_macroSyntaxError( &struMacro );
      hb_macroDelete( &struMacro );
   }
}

/* ************************************************************************* */

/* returns the order + 1 of a variable if defined or zero */
int hb_compLocalVarGetPos( char * szVarName, HB_MACRO_DECL )
{
   int iVar = 1;
   HB_CBVAR_PTR pVars = HB_PCODE_DATA->pLocals;

   while( pVars )
   {
      if( pVars->szName && ! strcmp( pVars->szName, szVarName ) )
         return iVar;
      else
      {
         if( pVars->pNext )
         {
            pVars = pVars->pNext;
            iVar++;
         }
         else
            return 0;
      }
   }
   return 0;
}


ULONG hb_compGenJump( LONG lOffset, HB_MACRO_DECL )
{
   /* TODO: We need a longer offset (longer then two bytes)
    */
   if( lOffset < ( LONG ) SHRT_MIN || lOffset > ( LONG ) SHRT_MAX )
      hb_macroError( HB_MACRO_TOO_COMPLEX, HB_MACRO_PARAM );

   hb_compGenPCode3( HB_P_JUMP, HB_LOBYTE( lOffset ), HB_HIBYTE( lOffset ), HB_MACRO_PARAM );

   return HB_PCODE_DATA->lPCodePos - 2;
}

ULONG hb_compGenJumpFalse( LONG lOffset, HB_MACRO_DECL )
{
   /* TODO: We need a longer offset (longer then two bytes)
    */
   if( lOffset < ( LONG ) SHRT_MIN || lOffset > ( LONG ) SHRT_MAX )
      hb_macroError( HB_MACRO_TOO_COMPLEX, HB_MACRO_PARAM );

   hb_compGenPCode3( HB_P_JUMPFALSE, HB_LOBYTE( lOffset ), HB_HIBYTE( lOffset ), HB_MACRO_PARAM );

   return HB_PCODE_DATA->lPCodePos - 2;
}

void hb_compGenJumpThere( ULONG ulFrom, ULONG ulTo, HB_MACRO_DECL )
{
   BYTE * pCode = HB_PCODE_DATA->pCode;
   LONG lOffset = ulTo - ulFrom + 1;

   /* TODO: We need a longer offset (longer then two bytes)
    */
   if( lOffset < ( LONG ) SHRT_MIN || lOffset > ( LONG ) SHRT_MAX )
      hb_macroError( HB_MACRO_TOO_COMPLEX, HB_MACRO_PARAM );

   pCode[ ( ULONG ) ulFrom ]     = HB_LOBYTE( lOffset );
   pCode[ ( ULONG ) ulFrom + 1 ] = HB_HIBYTE( lOffset );
}

void hb_compGenJumpHere( ULONG ulOffset, HB_MACRO_DECL )
{
   hb_compGenJumpThere( ulOffset, HB_PCODE_DATA->lPCodePos, HB_MACRO_PARAM );
}

ULONG hb_compGenJumpTrue( LONG lOffset, HB_MACRO_DECL )
{
   /* TODO: We need a longer offset (longer then two bytes)
    */
   if( lOffset < ( LONG ) SHRT_MIN || lOffset > ( LONG ) SHRT_MAX )
      hb_macroError( HB_MACRO_TOO_COMPLEX, HB_MACRO_PARAM );

   hb_compGenPCode3( HB_P_JUMPTRUE, HB_LOBYTE( lOffset ), HB_HIBYTE( lOffset ), HB_MACRO_PARAM );

   return HB_PCODE_DATA->lPCodePos - 2;
}

/*
 * Function generates pcode for passed memvar name
 */
void hb_compMemvarGenPCode( BYTE bPCode, char * szVarName, HB_MACRO_DECL )
{
   HB_DYNS_PTR pSym;

   /* Find the address of passed symbol - create the symbol if doesn't exist
    */
   pSym = hb_dynsymGet( szVarName );
   hb_compGenPCode1( bPCode, HB_MACRO_PARAM );
   hb_compGenPCodeN( ( BYTE * )( &pSym ), sizeof( pSym ), HB_MACRO_PARAM );
}

/* generates the pcode to push a symbol on the virtual machine stack */
void hb_compGenPushSymbol( char * szSymbolName, int iIsFunction, HB_MACRO_DECL )
{
   HB_DYNS_PTR pSym;

   if( iIsFunction )
   {
      char * pName = hb_compReservedName( szSymbolName );
      /* If it is reserved function name then we should truncate
       * the requested name.
       * We have to use passed szSymbolName so we can latter deallocate it
       * (pName points to static data)
       */
      if( pName )
         szSymbolName[ strlen( pName ) ] ='\0';
   }

   pSym = hb_dynsymGet( szSymbolName );
   hb_compGenPCode1( HB_P_MPUSHSYM, HB_MACRO_PARAM );
   hb_compGenPCodeN( ( BYTE * ) &pSym, sizeof( pSym ), HB_MACRO_PARAM );
}

/* generates the pcode to push a long number on the virtual machine stack */
void hb_compGenPushLong( long lNumber, HB_MACRO_DECL )
{
   if( lNumber )
   {
      hb_compGenPCode1( HB_P_PUSHLONG, HB_MACRO_PARAM );
      hb_compGenPCode1( ( ( char * ) &lNumber )[ 0 ], HB_MACRO_PARAM );
      hb_compGenPCode1( ( ( char * ) &lNumber )[ 1 ], HB_MACRO_PARAM );
      hb_compGenPCode1( ( ( char * ) &lNumber )[ 2 ], HB_MACRO_PARAM );
      hb_compGenPCode1( ( ( char * ) &lNumber )[ 3 ], HB_MACRO_PARAM );
   }
   else
      hb_compGenPCode1( HB_P_ZERO, HB_MACRO_PARAM );
}


/* sends a message to an object */
void hb_compGenMessage( char * szMsgName, HB_MACRO_DECL )
{
   /* Find the address of passed symbol - create the symbol if doesn't exist
    */
   HB_DYNS_PTR pSym = hb_dynsymGet( szMsgName );

   hb_compGenPCode1( HB_P_MMESSAGE, HB_MACRO_PARAM );
   hb_compGenPCodeN( ( BYTE * ) &pSym, sizeof( pSym ), HB_MACRO_PARAM );
}

/* generates an underscore-symbol name for a data assignment */
void hb_compGenMessageData( char * szMsg, HB_MACRO_DECL )
{
   char * szResult = ( char * ) hb_xgrab( strlen( szMsg ) + 2 );

   strcpy( szResult, "_" );
   strcat( szResult, szMsg );

   hb_compGenMessage( szResult, HB_MACRO_PARAM );
}

/* generates the pcode to pop a value from the virtual machine stack onto a variable */
void hb_compGenPopVar( char * szVarName, HB_MACRO_DECL )
{
   int iVar;

   iVar = hb_compLocalVarGetPos( szVarName, HB_MACRO_PARAM );
   if( iVar )
   {
      /* this is a codeblock parameter */
      hb_compGenPCode3( HB_P_POPLOCAL, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), HB_MACRO_PARAM );
   }
   else
   {
      /* NOTE: In clipper all undeclared variables are assumed MEMVAR if
       * they are popped however there is nno such assumption if avariable
       * is pushed on the eval stack
       */
      hb_compMemvarGenPCode( HB_P_MPOPMEMVAR, szVarName, HB_MACRO_PARAM );
   }
}

/* generates the pcode to pop a value from the virtual machine stack onto
 * an aliased variable
 */
void hb_compGenPopAliasedVar( char * szVarName,
                              BOOL bPushAliasValue,
                              char * szAlias,
                              long lWorkarea, HB_MACRO_DECL )
{
   if( bPushAliasValue )
   {
      if( szAlias )
      {
         if( szAlias[ 0 ] == 'M' && szAlias[ 1 ] == '\0' )
         {  /* M->variable */
            hb_compMemvarGenPCode( HB_P_MPOPMEMVAR, szVarName, HB_MACRO_PARAM );
         }
         else
         {
            int iCmp = strncmp( szAlias, "MEMVAR", 4 );
            if( iCmp == 0 )
                  iCmp = strncmp( szAlias, "MEMVAR", strlen( szAlias ) );
            if( iCmp == 0 )
            {  /* MEMVAR-> or MEMVA-> or MEMV-> */
               hb_compMemvarGenPCode( HB_P_MPOPMEMVAR, szVarName, HB_MACRO_PARAM );
            }
            else
            {  /* field variable */
               iCmp = strncmp( szAlias, "FIELD", 4 );
               if( iCmp == 0 )
                  iCmp = strncmp( szAlias, "FIELD", strlen( szAlias ) );
               if( iCmp == 0 )
               {  /* FIELD-> */
                  hb_compMemvarGenPCode( HB_P_MPOPFIELD, szVarName, HB_MACRO_PARAM );
               }
               else
               {  /* database alias */
                  hb_compGenPushSymbol( hb_strdup( szAlias ), 0, HB_MACRO_PARAM );
                  hb_compMemvarGenPCode( HB_P_MPOPALIASEDFIELD, szVarName, HB_MACRO_PARAM );
               }
            }
         }
      }
      else
      {
         hb_compGenPushLong( lWorkarea, HB_MACRO_PARAM );
         hb_compMemvarGenPCode( HB_P_MPOPALIASEDFIELD, szVarName, HB_MACRO_PARAM );
      }
   }
   else
      /* Alias is already placed on stack */
      hb_compMemvarGenPCode( HB_P_MPOPALIASEDFIELD, szVarName, HB_MACRO_PARAM );
}

/* generates the pcode to push a nonaliased variable value to the virtual
 * machine stack
 */
void hb_compGenPushVar( char * szVarName, HB_MACRO_DECL )
{
   int iVar;

   iVar = hb_compLocalVarGetPos( szVarName, HB_MACRO_PARAM );
   if( iVar )
   {
      /* this is a codeblock parameter */
      hb_compGenPCode3( HB_P_PUSHLOCAL, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), HB_MACRO_PARAM );
   }
   else
   {
      /* NOTE: In clipper all undeclared variables are assumed MEMVAR if
       * they are popped however there is nno such assumption if avariable
       * is pushed on the eval stack
       */
      hb_compMemvarGenPCode( HB_P_MPUSHVARIABLE, szVarName, HB_MACRO_PARAM );
   }
}

/* generates the pcode to push a variable by reference to the virtual machine stack */
void hb_compGenPushVarRef( char * szVarName, HB_MACRO_DECL )
{
   USHORT iVar;

   iVar = hb_compLocalVarGetPos( szVarName, HB_MACRO_PARAM );
   if( iVar )
      hb_compGenPCode3( HB_P_PUSHLOCALREF, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), HB_MACRO_PARAM );
   else
   {
      hb_compMemvarGenPCode( HB_P_MPUSHMEMVARREF, szVarName, HB_MACRO_PARAM );
   }
}

 /* generates the pcode to push an aliased variable value to the virtual
  * machine stack
  */
void hb_compGenPushAliasedVar( char * szVarName,
                               BOOL bPushAliasValue,
                               char * szAlias,
                               long lWorkarea, HB_MACRO_DECL )
{
   if( bPushAliasValue )
   {
      if( szAlias )
      {
         /* myalias->var
         * FIELD->var
         * MEMVAR->var
         */
         if( szAlias[ 0 ] == 'M' && szAlias[ 1 ] == '\0' )
         {  /* M->variable */
            hb_compMemvarGenPCode( HB_P_MPUSHMEMVAR, szVarName, HB_MACRO_PARAM );
         }
         else
         {
            int iCmp = strncmp( szAlias, "MEMVAR", 4 );
            if( iCmp == 0 )
                  iCmp = strncmp( szAlias, "MEMVAR", strlen( szAlias ) );
            if( iCmp == 0 )
            {  /* MEMVAR-> or MEMVA-> or MEMV-> */
               hb_compMemvarGenPCode( HB_P_MPUSHMEMVAR, szVarName, HB_MACRO_PARAM );
            }
            else
            {  /* field variable */
               iCmp = strncmp( szAlias, "FIELD", 4 );
               if( iCmp == 0 )
                  iCmp = strncmp( szAlias, "FIELD", strlen( szAlias ) );
               if( iCmp == 0 )
               {  /* FIELD-> */
                  hb_compMemvarGenPCode( HB_P_MPUSHFIELD, szVarName, HB_MACRO_PARAM );
               }
               else
               {  /* database alias */
                  hb_compGenPushSymbol( hb_strdup( szAlias ), 0, HB_MACRO_PARAM );
                  hb_compMemvarGenPCode( HB_P_MPUSHALIASEDFIELD, szVarName, HB_MACRO_PARAM );
               }
            }
         }
      }
      else
      {
         hb_compGenPushLong( lWorkarea, HB_MACRO_PARAM );
         hb_compMemvarGenPCode( HB_P_MPUSHALIASEDFIELD, szVarName, HB_MACRO_PARAM );
      }
   }
   else
      /* Alias is already placed on stack */
      hb_compMemvarGenPCode( HB_P_MPUSHALIASEDFIELD, szVarName, HB_MACRO_PARAM );
}

/* pushes a logical value on the virtual machine stack , */
void hb_compGenPushLogical( int iTrueFalse, HB_MACRO_DECL )
{
   if( iTrueFalse )
      hb_compGenPCode1( HB_P_TRUE, HB_MACRO_PARAM );
   else
      hb_compGenPCode1( HB_P_FALSE, HB_MACRO_PARAM );
}

/* generates the pcode to push a double number on the virtual machine stack */
void hb_compGenPushDouble( double dNumber, BYTE bDec, HB_MACRO_DECL )
{
   hb_compGenPCode1( HB_P_PUSHDOUBLE, HB_MACRO_PARAM );
   hb_compGenPCodeN( ( BYTE * ) &dNumber, sizeof( double ), HB_MACRO_PARAM );
   hb_compGenPCode1( bDec, HB_MACRO_PARAM );
}

void hb_compGenPushFunCall( char * szFunName, HB_MACRO_DECL )
{
   char * szFunction;

   szFunction = hb_compReservedName( szFunName );
   if( szFunction )
   {
      /* Abbreviated function name was used - change it for whole name
       */
      hb_compGenPushSymbol( szFunction, 1, HB_MACRO_PARAM );
   }
   else
      hb_compGenPushSymbol( szFunName, 1, HB_MACRO_PARAM );
}

/* generates the pcode to push a string on the virtual machine stack */
void hb_compGenPushString( char * szText, ULONG ulStrLen, HB_MACRO_DECL )
{
   hb_compGenPCode3( HB_P_PUSHSTR, HB_LOBYTE( ulStrLen ), HB_HIBYTE( ulStrLen ), HB_MACRO_PARAM );
   hb_compGenPCodeN( ( BYTE * ) szText, ulStrLen, HB_MACRO_PARAM );
}


void hb_compGenPCode1( BYTE byte, HB_MACRO_DECL )
{
   HB_PCODE_INFO_PTR pFunc = HB_PCODE_DATA;

   if( ( pFunc->lPCodeSize - pFunc->lPCodePos ) < 1 )
      pFunc->pCode = ( BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->lPCodeSize += HB_PCODE_SIZE );

   pFunc->pCode[ pFunc->lPCodePos++ ] = byte;
}

void hb_compGenPCode3( BYTE byte1, BYTE byte2, BYTE byte3, HB_MACRO_DECL )
{
   HB_PCODE_INFO_PTR pFunc = HB_PCODE_DATA;

   if( ( pFunc->lPCodeSize - pFunc->lPCodePos ) < 3 )
      pFunc->pCode = ( BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->lPCodeSize += HB_PCODE_SIZE );

   pFunc->pCode[ pFunc->lPCodePos++ ] = byte1;
   pFunc->pCode[ pFunc->lPCodePos++ ] = byte2;
   pFunc->pCode[ pFunc->lPCodePos++ ] = byte3;
}

void hb_compGenPCodeN( BYTE * pBuffer, ULONG ulSize, HB_MACRO_DECL )
{
   HB_PCODE_INFO_PTR pFunc = HB_PCODE_DATA;

   if( pFunc->lPCodePos + ulSize > pFunc->lPCodeSize )
   {
      /* not enough free space in pcode buffer - increase it */
      pFunc->lPCodeSize += ( ( ( ulSize / HB_PCODE_SIZE ) + 1 ) * HB_PCODE_SIZE );
      pFunc->pCode = ( BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->lPCodeSize );
   }

   memcpy( pFunc->pCode + pFunc->lPCodePos, pBuffer, ulSize );
   pFunc->lPCodePos += ulSize;
}

/* ************************************************************************* */

void hb_macroError( int iError, HB_MACRO_DECL )
{
   HB_MACRO_DATA->status = iError;
}

/*
 * Start a new pcode buffer for a codeblock
*/
void hb_compCodeBlockStart( HB_MACRO_DECL )
{
   HB_PCODE_INFO_PTR pCB = ( HB_PCODE_INFO_PTR ) hb_xgrab( sizeof( HB_PCODE_INFO ) );

   /* replace current pcode buffer with the new one
    */
   pCB->pPrev = HB_PCODE_DATA;
   HB_PCODE_DATA = pCB;

   pCB->pCode = ( BYTE * ) hb_xgrab( HB_PCODE_SIZE );
   pCB->lPCodeSize = HB_PCODE_SIZE;
   pCB->lPCodePos  = 0;
   pCB->pLocals    = NULL;
}

void hb_compCodeBlockEnd( HB_MACRO_DECL )
{
   HB_PCODE_INFO_PTR pCodeblock;   /* pointer to the current codeblock */
   USHORT wSize;
   USHORT wParms = 0;   /* number of codeblock parameters */
   HB_CBVAR_PTR pVar, pFree;

   /* a currently processed codeblock */
   pCodeblock = HB_PCODE_DATA;

   /* return to pcode buffer of a codeblock in which the current
    * codeblock was defined
    */
   HB_PCODE_DATA = pCodeblock->pPrev;

   /* generate a proper codeblock frame with a codeblock size and with
    * a number of expected parameters
    */
   /*QUESTION: would be 64kB enough for a codeblock size?
    * we are assuming now a USHORT for a size of codeblock
    */

   /* Count the number of codeblock parameters */
   pVar = pCodeblock->pLocals;
   while( pVar )
   {
      pVar = pVar->pNext;
      ++wParms;
   }

   /*NOTE:  8 = HB_P_MPUSHBLOCK + USHORT( size ) + USHORT( wParams ) + USHORT( 0 ) + _ENDBLOCK
    * runtime compiled codeblock cannot reference local variables defined in a
    * function
    */
   wSize = ( USHORT ) pCodeblock->lPCodePos + 8;

   /*NOTE: HB_P_MPUSHBLOCK differs from HB_P_PUSHBLOCK - the pcode
    * is stored in dynamic memory pool instead of static memory
    */
   hb_compGenPCode3( HB_P_MPUSHBLOCK, HB_LOBYTE( wSize ), HB_HIBYTE( wSize ), HB_MACRO_PARAM );
   hb_compGenPCode1( HB_LOBYTE( wParms ), HB_MACRO_PARAM );
   hb_compGenPCode1( HB_HIBYTE( wParms ), HB_MACRO_PARAM );
   hb_compGenPCode1( 0, HB_MACRO_PARAM );
   hb_compGenPCode1( 0, HB_MACRO_PARAM );

   /* copy a codeblock pcode buffer */
   hb_compGenPCodeN( pCodeblock->pCode, pCodeblock->lPCodePos, HB_MACRO_PARAM );
   hb_compGenPCode1( HB_P_ENDBLOCK, HB_MACRO_PARAM ); /* finish the codeblock */

   /* free memory allocated for a codeblock */
   hb_xfree( ( void * ) pCodeblock->pCode );
   pVar = pCodeblock->pLocals;
   while( pVar )
   {
      /* free used variables */
      pFree = pVar;
      hb_xfree( ( void * ) pFree->szName );
      pVar = pVar->pNext;
      hb_xfree( ( void * ) pFree );
   }
   hb_xfree( ( void * ) pCodeblock );
}

/* ************************************************************************ */

/* Include the common part of expression optimalizer
 * NOTE: It cannot be compiled into a single library because the code
 * required for macro compiler differs a little - we are passing additional
 * parameter that holds macro compiler internal data
 */
#include "../compiler/expropt.c"

