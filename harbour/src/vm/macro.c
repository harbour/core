/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Macro compiler main file
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
 * www - http://harbour-project.org
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

/* this #define HAS TO be placed before all #include directives
 */
#ifndef HB_MACRO_SUPPORT
#  define HB_MACRO_SUPPORT
#endif

#include "hbvmopt.h"
#include "hbmacro.h"
#include "hbcomp.h"
#include "hbstack.h"

/* various flags for macro compiler */
#ifndef HB_SM_DEFAULT
#  ifdef HB_CLP_STRICT
#     define HB_SM_DEFAULT    ( HB_SM_SHORTCUTS )
#  else
#     define HB_SM_DEFAULT    ( HB_SM_SHORTCUTS | HB_SM_HARBOUR )
#  endif
#endif

#if defined( HB_MT_VM )

static void hb_macroFlagsInit( void * pFlags )
{
   * ( ( int * ) pFlags ) = HB_SM_DEFAULT;
}

static HB_TSD_NEW( s_macroFlags, sizeof( int ), hb_macroFlagsInit, NULL );

static int hb_macroFlags( void )
{
   return * ( ( int * ) hb_stackGetTSD( &s_macroFlags ) );
}

static void hb_macroFlagsSet( int flag )
{
   * ( ( int * ) hb_stackGetTSD( &s_macroFlags ) ) = flag;
}

#else

   static int s_macroFlags = HB_SM_DEFAULT;
#  define hb_macroFlags()     s_macroFlags
#  define hb_macroFlagsSet(f) do { s_macroFlags = (f); } while(0)

#endif

#define HB_SM_ISUSERCP()      ( HB_CDP_ISCHARUNI(hb_vmCDP()) ? HB_COMPFLAG_USERCP : 0 )

/* ************************************************************************* */

/* Compile passed string into a pcode buffer
 *
 * 'pMacro' - pointer to HB_MACRO structure that will hold all information
 *    nedded for macro compilation and evaluation
 * 'szString'  - a string to compile
 * 'iFlag' - specifies if compiled code should generate pcodes either for push
 *    operation (for example: var :=&macro) or for pop operation (&macro :=var)
 */
static int hb_macroParse( HB_MACRO_PTR pMacro )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_macroParse(%p)", pMacro));

   /* initialize the output (pcode) buffer - it will be filled by yacc */
   pMacro->pCodeInfo = &pMacro->pCodeInfoBuffer;
   pMacro->pCodeInfo->nPCodeSize = HB_PCODE_SIZE;
   pMacro->pCodeInfo->nPCodePos  = 0;
   pMacro->pCodeInfo->fVParams   = HB_FALSE;
   pMacro->pCodeInfo->pLocals    = NULL;
   pMacro->pCodeInfo->pPrev      = NULL;
   pMacro->pCodeInfo->pCode      = ( HB_BYTE * ) hb_xgrab( HB_PCODE_SIZE );

   /* reset the type of compiled expression - this should be filled after
    * successfully compilation
    */
   pMacro->pError = NULL;
   pMacro->uiListElements = 0;
   pMacro->exprType = HB_ET_NONE;

   return hb_macroYYParse( pMacro );
}

/* releases all memory allocated for macro evaluation
 * NOTE:
 *    Only members of HB_MACRO structure are deallocated
 *    the 'pMacro' pointer is not released - it can be a pointer
 *    to a memory allocated on the stack.
 */
void hb_macroDelete( HB_MACRO_PTR pMacro )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_macroDelete(%p)", pMacro));

   hb_xfree( pMacro->pCodeInfo->pCode );
   if( pMacro->pError )
      hb_errRelease( pMacro->pError );
   if( pMacro->Flags & HB_MACRO_DEALLOCATE )
      hb_xfree( pMacro );
}

/* checks if a correct ITEM was passed from the virtual machine eval stack
 */
static HB_BOOL hb_macroCheckParam( PHB_ITEM pItem )
{
   HB_BOOL bValid = HB_TRUE;

   HB_TRACE(HB_TR_DEBUG, ("hb_macroCheckParam(%p)", pItem));

   if( ! HB_IS_STRING( pItem ) )
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1065, NULL, "&", 1, pItem );

      bValid = HB_FALSE;
      if( pResult )
      {
         HB_STACK_TLS_PRELOAD
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
   }
   return bValid;
}

/* It handles an error generated during checking of expression type
 */
static HB_ERROR_HANDLE( hb_macroErrorType )
{
   HB_MACRO_PTR pMacro = ( HB_MACRO_PTR ) ErrorInfo->Cargo;

   /* copy error object for later diagnostic usage */
   if( !pMacro->pError )
      pMacro->pError = hb_itemNew( ErrorInfo->Error );

   pMacro->status &= ~HB_MACRO_CONT;

   /* ignore rest of compiled code */
   hb_vmRequestEndProc();

   return NULL;   /* ignore this error */
}


/* Executes pcode compiled by macro compiler
 *
 * pMacro is a pointer to HB_MACRO structure created by macro compiler
 *
 */
void hb_macroRun( HB_MACRO_PTR pMacro )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_macroRun(%p)", pMacro));

   hb_vmExecute( pMacro->pCodeInfo->pCode, NULL );
}

static void hb_macroSyntaxError( HB_MACRO_PTR pMacro )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_macroSyntaxError(%p)", pMacro));

   if( pMacro && pMacro->pError )
   {
      HB_TRACE(HB_TR_DEBUG, ("hb_macroSyntaxError.(%s)", pMacro->string));

      hb_stackPop();    /* remove compiled string */

      hb_errLaunch( pMacro->pError );
      hb_errRelease( pMacro->pError );
      pMacro->pError = NULL;
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_SYNTAX, 1449, NULL, "&", 1, hb_stackItemFromTop( -1 ) );

      if( pResult )
      {
         hb_stackPop();    /* remove compiled string */
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
   }
}

/* This replaces all '&var' or '&var.' occurences within a given string
 * with the value of variable 'var' if this variable exists and contains
 * a string value. The value of variable is also searched for
 * occurences of macro operator and if it is found then it is expanded
 * until there is no more macro operators.
 * NOTE:
 *    this does not evaluate a macro expression - there is a simple text
 *    substitution only
 * NOTE:
 *    hb_macroTextSubst returns either a pointer that points to the passed
 *    string if there was no macro operator in it or a pointer to a new
 *    allocated memory with expanded string if there was a macro operator
 *    in passed string.
 * NOTE:
 *    Clipper restarts scanning of the text from the beginning of
 *    inserted text after macro expansion, for example:
 *    PRIVATE a:='&', b:='c'
 *    PRIVATE &a.b   // this will create 'c' variable
 *
 *    PRIVATE a:=0, b:='b', ab:='c'
 *    PRIVATE &a&b   //this will cause syntax error '&'
 *
 */
static char * hb_macroTextSubst( const char * szString, HB_SIZE * pnStringLen )
{
   char * szResult;
   HB_SIZE nResStrLen;
   HB_SIZE nResBufLen;
   HB_SIZE nCharsLeft;
   char * pHead;
   char * pTail;

   HB_TRACE(HB_TR_DEBUG, ("hb_macroTextSubst(%s, %" HB_PFS "u)", szString, *pnStringLen));

   pHead = ( char * ) memchr( szString, '&', *pnStringLen );
   if( pHead == NULL )
      return ( char * ) szString;  /* no more processing is required */

   /* initial length of the string and the result buffer (it can contain null bytes) */
   nResBufLen = nResStrLen = *pnStringLen;
   /* initial buffer for return value */
   szResult = ( char * ) hb_xgrab( nResBufLen + 1 );

   /* copy the input string with trailing zero byte
    */
   memcpy( szResult, szString, nResStrLen + 1 );
   /* switch the pointer so it will point into the result buffer
    */
   pHead = szResult + ( pHead - szString );

   do
   {
      /* store the position where '&' was found so we can restart scanning
       * from this point after macro expansion
       */
      pTail = pHead;
      /* check if the next character can start a valid identifier
      * (only _a-zA-Z are allowed)
      */
      ++pHead;    /* skip '&' character */
      if( *pHead == '_' ||
          ( *pHead >= 'A' && *pHead <= 'Z' ) ||
          ( *pHead >= 'a' && *pHead <= 'z' ) )
      {
         /* extract a variable name */
         /* NOTE: the extracted name can be longer then supported maximal
          * length of identifiers (HB_SYMBOL_NAME_LEN) - only the max allowed
          * are used for name lookup however the whole string is replaced
          */
         HB_SIZE nNameLen = 1;
         char * pName = pHead;

         while( *++pHead && ( *pHead == '_' ||
                              ( *pHead >= 'A' && *pHead <= 'Z' ) ||
                              ( *pHead >= 'a' && *pHead <= 'z' ) ||
                              ( *pHead >= '0' && *pHead <= '9' ) ) )
         {
            ++nNameLen;
         }
         /* pHead points now at the character that terminated a variable name */

         /* NOTE: '_' is invalid variable name
          */
         if( nNameLen > 1 || *pName != '_' )
         {
            /* this is not the "&_" string */
            char * szValPtr;
            HB_SIZE nValLen;

            /* Get a pointer to the string value stored in this variable
             * or NULL if variable doesn't exist or doesn't contain a string
             * value.
             * NOTE: This doesn't create a copy of the value then it
             * shouldn't be released here.
             */
            nValLen = nNameLen;   /* the length of name */
            szValPtr = hb_memvarGetStrValuePtr( pName, &nValLen );
            if( szValPtr )
            {
               if( *pHead == '.' )
               {
                  /* we have stopped at the macro terminator '.' - skip it */
                  ++pHead;
                  ++nNameLen;
               }
               ++nNameLen;   /* count also the '&' character */

               /* number of characters left on the right side of a variable name */
               nCharsLeft = nResStrLen - ( pHead - szResult );

               /* NOTE:
                * if a replacement string is shorter then the variable
                * name then we don't have to reallocate the result buffer:
                * 'nResStrLen' stores the current length of a string in the buffer
                * 'nResBufLen' stores the length of the buffer
                */
               if( nValLen > nNameLen )
               {
                  nResStrLen += ( nValLen - nNameLen );
                  if( nResStrLen > nResBufLen )
                  {
                     HB_SIZE nHead = pHead - szResult;
                     HB_SIZE nTail = pTail - szResult;
                     nResBufLen = nResStrLen;
                     szResult = ( char * ) hb_xrealloc( szResult, nResBufLen + 1 );
                     pHead = szResult + nHead;
                     pTail = szResult + nTail;
                  }
               }
               else
                  nResStrLen -= ( nNameLen - nValLen );

               /* move bytes located on the right side of a variable name */
               memmove( pTail + nValLen, pHead, nCharsLeft + 1 );
               /* copy substituted value */
               memcpy( pTail, szValPtr, nValLen );
               /* restart scanning from the beginning of replaced string */
               /* NOTE: This causes that the following code:
                *    a := '&a'
                *    var := '&a.b'
                * is the same as:
                *    var := '&ab'
                */
               pHead = pTail;
            }
         }
      }
      nCharsLeft = nResStrLen - ( pHead - szResult );
   }
   while( nCharsLeft && ( pHead = ( char * ) memchr( pHead, '&', nCharsLeft ) ) != NULL );

   if( nResStrLen < nResBufLen )
   {
      /* result string is shorter then allocated buffer -
       * cut it to a required length
       */
      szResult = ( char * ) hb_xrealloc( szResult, nResStrLen + 1 );
   }
   szResult[ nResStrLen ] = 0;  /* place terminating null character */
   /* return a length of result string */
   *pnStringLen = nResStrLen;

   return szResult;   /* a new memory buffer was allocated */
}


/* NOTE:
 *   This will be called when macro variable or macro expression is
 * placed on the right side of the assignment or when it is used as
 * a parameter.
 * PUSH operation
 * iContext contains additional info when HB_SM_XBASE is enabled
 *  = 0 - in Clipper strict compatibility mode
 *  = HB_P_MACROPUSHLIST
 *  = HB_P_MACROPUSHPARE
 *
 * iContext contains HB_P_MACROPUSHPARE if a macro is used inside a codeblock
 * EVAL( {|| &macro} )
 *
 */

void hb_macroGetValue( PHB_ITEM pItem, int iContext, int flags )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_macroGetValue(%p)", pItem));

   if( hb_macroCheckParam( pItem ) )
   {
      HB_MACRO struMacro;
      int iStatus;
      char * pszFree;

      struMacro.mode       = HB_MODE_MACRO;
      struMacro.supported  = ( ( flags & HB_SM_RT_MACRO ) ? hb_macroFlags() : flags ) |
                             HB_SM_ISUSERCP();
      struMacro.Flags      = HB_MACRO_GEN_PUSH;
      struMacro.uiNameLen  = HB_SYMBOL_NAME_LEN;
      struMacro.status     = HB_MACRO_CONT;
      struMacro.length     = pItem->item.asString.length;
      /*
       * Clipper appears to expand nested macros staticly vs. by
       * Macro Parser, f.e.:
       *       PROCEDURE Main()
       *          LOCAL cText
       *          cText := "( v := 'A' ) + &v"
       *          M->v := "'B'"
       *          ? "Macro:", cText
       *          ? "Result:", &cText
       *          ? "Type:", type(cText)
       *       RETURN
       */
      pszFree = hb_macroTextSubst( pItem->item.asString.value, &struMacro.length );
      struMacro.string = pszFree;
      if( pszFree == pItem->item.asString.value )
         pszFree = NULL;

      if( iContext != 0 )
      {
         /*
          * If compiled in Xbase++ compatibility mode:
          * macro := "1,2"
          * funCall( &macro )  ==>  funCall( 1, 2 )
          * { &macro }  ==>  { 1, 2 }
          * var[ &macro ]  ==>  var[ 1, 2 ]
          * var := (somevalue, &macro)  ==> var := 2
          *
          * Always:
          * macro := "1,2"
          * EVAL( {|| &macro} )
          *
          */
         struMacro.Flags |= HB_MACRO_GEN_LIST;
         if( iContext == HB_P_MACROPUSHPARE )
         {
            struMacro.Flags |= HB_MACRO_GEN_PARE;
         }
      }

      iStatus = hb_macroParse( &struMacro );

      if( iStatus == HB_MACRO_OK && ( struMacro.status & HB_MACRO_CONT ) )
      {
         hb_stackPop();    /* remove compiled string */
         hb_macroRun( &struMacro );

         if( iContext == HB_P_MACROPUSHLIST )
            hb_vmPushLong( struMacro.uiListElements + 1 );
      }
      else
         hb_macroSyntaxError( &struMacro );

      if( pszFree )
         hb_xfree( pszFree );

      hb_macroDelete( &struMacro );
   }
   else if( iContext == HB_P_MACROPUSHLIST && hb_vmRequestQuery() == 0 )
   {
      hb_vmPushInteger( 1 );
   }
}

/* NOTE:
 *   This will be called when macro variable or macro expression is
 * placed on the left side of the assignment
 * POP operation
 */
void hb_macroSetValue( PHB_ITEM pItem, int flags )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_macroSetValue(%p)", pItem));

   if( hb_macroCheckParam( pItem ) )
   {
      HB_MACRO struMacro;
      int iStatus;

      struMacro.mode       = HB_MODE_MACRO;
      struMacro.supported  = ( ( flags & HB_SM_RT_MACRO ) ? hb_macroFlags() : flags ) |
                             HB_SM_ISUSERCP();
      struMacro.Flags      = HB_MACRO_GEN_POP;
      struMacro.uiNameLen  = HB_SYMBOL_NAME_LEN;
      struMacro.status     = HB_MACRO_CONT;
      struMacro.string     = pItem->item.asString.value;
      struMacro.length     = pItem->item.asString.length;

      iStatus = hb_macroParse( &struMacro );

      if( iStatus == HB_MACRO_OK && ( struMacro.status & HB_MACRO_CONT ) )
      {
         hb_stackPop();    /* remove compiled string */
         hb_macroRun( &struMacro );
      }
      else
         hb_macroSyntaxError( &struMacro );

      hb_macroDelete( &struMacro );
   }
   else if( hb_vmRequestQuery() == 0 )
   {
      hb_stackPop();
      hb_stackPop();
   }
}

/* NOTE:
 *   This will be called when macro variable or macro expression is
 *   passed by reference or used in optimized left side of the <op>=
 *   expression or as argument of ++ or -- operation
 */
void hb_macroPushReference( PHB_ITEM pItem )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_macroPushReference(%p)", pItem));

   if( hb_macroCheckParam( pItem ) )
   {
      HB_MACRO struMacro;
      int iStatus;

      struMacro.mode       = HB_MODE_MACRO;
      struMacro.supported  = HB_SM_SHORTCUTS | HB_SM_HARBOUR | HB_SM_ARRSTR;
      struMacro.Flags      = HB_MACRO_GEN_PUSH | HB_MACRO_GEN_REFER;
      struMacro.uiNameLen  = HB_SYMBOL_NAME_LEN;
      struMacro.status     = HB_MACRO_CONT;
      struMacro.string     = pItem->item.asString.value;
      struMacro.length     = pItem->item.asString.length;

      iStatus = hb_macroParse( &struMacro );

      if( iStatus == HB_MACRO_OK && ( struMacro.status & HB_MACRO_CONT ) )
      {
         hb_stackPop();    /* remove compiled string */
         hb_macroRun( &struMacro );
      }
      else
         hb_macroSyntaxError( &struMacro );

      hb_macroDelete( &struMacro );
   }
}

/*
 * Compile and run:
 *    &alias->var or
 *    alias->&var
 * NOTE:
 *    Clipper implements these two cases as: &( alias +'->' + variable )
 *    This causes some non expected behaviours, for example:
 *    A :="M + M"
 *    ? &A->&A
 *    is the same as:
 *    &( "M + M->M + M" )
 *    instead of
 *    &( "M + M" ) -> &( "M + M" )
 */
static void hb_macroUseAliased( PHB_ITEM pAlias, PHB_ITEM pVar, int iFlag, int iSupported )
{
   HB_STACK_TLS_PRELOAD

   if( HB_IS_STRING( pAlias ) && HB_IS_STRING( pVar ) )
   {
      /* grab memory for "alias->var"
      */
      HB_SIZE nLen = pAlias->item.asString.length + pVar->item.asString.length + 2;
      char * szString = ( char * ) hb_xgrab( nLen + 1 );
      HB_MACRO struMacro;
      int iStatus;

      memcpy( szString, pAlias->item.asString.value, pAlias->item.asString.length );
      szString[ pAlias->item.asString.length ]     = '-';
      szString[ pAlias->item.asString.length + 1 ] = '>';
      memcpy( szString + pAlias->item.asString.length + 2, pVar->item.asString.value, pVar->item.asString.length );
      szString[ nLen ] = '\0';

      struMacro.mode       = HB_MODE_MACRO;
      struMacro.supported  = ( ( iSupported & HB_SM_RT_MACRO ) ? hb_macroFlags() : iSupported ) |
                             HB_SM_ISUSERCP();
      struMacro.Flags      = iFlag;
      struMacro.uiNameLen  = HB_SYMBOL_NAME_LEN;
      struMacro.status     = HB_MACRO_CONT;
      struMacro.string     = szString;
      struMacro.length     = nLen;

      iStatus = hb_macroParse( &struMacro );

      hb_stackPop();    /* remove compiled variable name */
      hb_stackPop();    /* remove compiled alias */

      if( iStatus == HB_MACRO_OK && ( struMacro.status & HB_MACRO_CONT ) )
      {
         hb_macroRun( &struMacro );
      }
      else
      {
         hb_vmPushString( szString, nLen );
         hb_macroSyntaxError( &struMacro );
      }

      hb_xfree( szString );
      hb_macroDelete( &struMacro );
   }
   else if( hb_macroCheckParam( pVar ) )
   {
      /* only right side of alias operator is a string - macro-compile
       * this part only
       */
      HB_MACRO struMacro;
      int iStatus;

      struMacro.mode       = HB_MODE_MACRO;
      struMacro.supported  = ( ( iSupported & HB_SM_RT_MACRO ) ? hb_macroFlags() : iSupported ) |
                             HB_SM_ISUSERCP();
      struMacro.Flags      = iFlag | HB_MACRO_GEN_ALIASED;
      struMacro.uiNameLen  = HB_SYMBOL_NAME_LEN;
      struMacro.status     = HB_MACRO_CONT;
      struMacro.string     = pVar->item.asString.value;
      struMacro.length     = pVar->item.asString.length;

      iStatus = hb_macroParse( &struMacro );

      if( iStatus == HB_MACRO_OK && ( struMacro.status & HB_MACRO_CONT ) )
      {
         hb_stackPop();    /* remove compiled string */
         hb_macroRun( &struMacro );
      }
      else
         hb_macroSyntaxError( &struMacro );

      hb_macroDelete( &struMacro );
   }
}

/* Compiles and run an aliased macro expression - generated pcode
 * pops a value from the stack
 *    &alias->var := any
 *    alias->&var := any
 */
void hb_macroPopAliasedValue( PHB_ITEM pAlias, PHB_ITEM pVar, int flags )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_macroPopAliasedValue(%p, %p)", pAlias, pVar));

   hb_macroUseAliased( pAlias, pVar, HB_MACRO_GEN_POP, flags );
}

/* Compiles and run an aliased macro expression - generated pcode
 * pushes a value onto the stack
 *    any := &alias->var
 *    any := alias->&var
 */
void hb_macroPushAliasedValue( PHB_ITEM pAlias, PHB_ITEM pVar, int flags )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_macroPushAliasedValue(%p, %p)", pAlias, pVar));

   hb_macroUseAliased( pAlias, pVar, HB_MACRO_GEN_PUSH, flags );
}

/* Check for '&' operator and replace it with a macro variable value
 * Returns: the passed string if there is no '&' operator ( pbNewString := FALSE )
 * new string if a valid macro text substitution was found (and sets
 * pbNewString to TRUE)
*/
char * hb_macroExpandString( const char *szString, HB_SIZE nLength, HB_BOOL *pfNewString )
{
   char *szResultString;

   HB_TRACE(HB_TR_DEBUG, ("hb_macroExpandString(%s,%" HB_PFS "u,%p)", szString, nLength, pfNewString));

   if( szString )
      szResultString = hb_macroTextSubst( szString, &nLength );
   else
      szResultString = ( char * ) szString;
   *pfNewString = ( szString != szResultString );
   return szResultString;
}

char * hb_macroTextSymbol( const char *szString, HB_SIZE nLength, HB_BOOL *pfNewString )
{
   char * szResult = NULL;

   HB_TRACE(HB_TR_DEBUG, ("hb_macroTextSymbol(%s,%" HB_PFS "u,%p)", szString, nLength, pfNewString));

   if( szString )
   {
      HB_SIZE nLen = 0;

      szResult = hb_macroTextSubst( szString, &nLength );

      while( nLength && ( szResult[ 0 ] == ' ' || szResult[ 0 ] == '\t' ) )
      {
         ++szResult;
         ++szString;
         --nLength;
      }

      while( nLength && ( szResult[ nLength - 1 ] == ' ' ||
                          szResult[ nLength - 1 ] == '\t' ) )
         --nLength;

      /* NOTE: This uses _a-zA-Z0-9 pattern to check for a valid name
       * "_" is not valid macro string
       */
      while( nLen < nLength )
      {
         char c = szResult[ nLen ];
         if( c >= 'a' && c <= 'z' )
         {
            if( szResult == szString )
            {
               szResult = ( char * ) hb_xgrab( nLength + 1 );
               memcpy( szResult, szString, nLength );
               szResult[ nLength ] = '\0';
            }
            szResult[ nLen ] = c - ( 'a' - 'A' );
         }
         else if( ! ( c == '_' || ( c >= 'A' && c <= 'Z' ) ||
                      ( nLen && ( c >= '0' && c <= '9' ) ) ) )
         {
            break;
         }
         ++nLen;
      }
      if( nLen == nLength && nLen > ( HB_SIZE ) ( szResult[ 0 ] == '_' ? 1 : 0 ) )
      {
         if( nLen > HB_SYMBOL_NAME_LEN )
            nLen = HB_SYMBOL_NAME_LEN;
         if( szResult[ nLen ] )
         {
            if( szResult == szString )
            {
               szResult = ( char * ) hb_xgrab( nLen + 1 );
               memcpy( szResult, szString, nLen );
            }
            szResult[ nLen ] = '\0';
         }
      }
      else
      {
         if( szResult != szString )
            hb_xfree( szResult );
         szResult = NULL;
      }
   }
   *pfNewString = szResult && szString != szResult;
   return szResult;
}

/* compile a string and return a pcode to push a value of expression
 * NOTE: it can be called to implement an index key evaluation
 * use hb_macroRun() to evaluate a compiled pcode
 */
HB_MACRO_PTR hb_macroCompile( const char * szString )
{
   HB_MACRO_PTR pMacro;
   int iStatus;

   HB_TRACE(HB_TR_DEBUG, ("hb_macroCompile(%s)", szString));

   pMacro = ( HB_MACRO_PTR ) hb_xgrab( sizeof( HB_MACRO ) );
   pMacro->mode      = HB_MODE_MACRO;
   pMacro->supported = hb_macroFlags() | HB_SM_ISUSERCP();
   pMacro->Flags     = HB_MACRO_DEALLOCATE | HB_MACRO_GEN_PUSH |
                       HB_MACRO_GEN_LIST | HB_MACRO_GEN_PARE;
   pMacro->uiNameLen = HB_SYMBOL_NAME_LEN;
   pMacro->status    = HB_MACRO_CONT;
   pMacro->string    = szString;
   pMacro->length    = strlen( szString );

   iStatus = hb_macroParse( pMacro );
   if( ! ( iStatus == HB_MACRO_OK && ( pMacro->status & HB_MACRO_CONT ) ) )
   {
      hb_macroDelete( pMacro );
      pMacro = NULL;
   }

   return pMacro;
}

static void hb_macroBlock( const char * szString, PHB_ITEM pItem )
{
   HB_MACRO_PTR pMacro = hb_macroCompile( szString );

   if( pMacro )
   {
      pMacro->pCodeInfo->pCode[ pMacro->pCodeInfo->nPCodePos - 1 ] = HB_P_ENDBLOCK;

      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );

      pItem->item.asBlock.value = hb_codeblockMacroNew( pMacro->pCodeInfo->pCode,
                                                        pMacro->pCodeInfo->nPCodePos );
      pItem->type = HB_IT_BLOCK;
      pItem->item.asBlock.paramcnt = 0;
      pItem->item.asBlock.lineno = 0;
      pItem->item.asBlock.hclass = 0;
      pItem->item.asBlock.method = 0;

      hb_macroDelete( pMacro );
   }
}

HB_FUNC( HB_MACROBLOCK )
{
   const char * szMacro = hb_parc( 1 );

   if( szMacro )
   {
      HB_STACK_TLS_PRELOAD
      hb_macroBlock( szMacro, hb_stackReturnItem() );
   }
}

/* This function handles a macro function calls, e.g. var :=&macro()
 * and creating memvar variables using PUBLIC/PRIVATE command
 * PUBLIC &macro
 *
 * 'pItem' points to a ITEM that contains a string value which after
 *    text substitution will return a function name
 */
void hb_macroPushSymbol( PHB_ITEM pItem )
{
   HB_STACK_TLS_PRELOAD
   HB_TRACE(HB_TR_DEBUG, ("hb_macroPushSymbol(%p)", pItem));

   if( hb_macroCheckParam( pItem ) )
   {
      char * szString;
      HB_BOOL fNewBuffer;

      szString = hb_macroTextSymbol( pItem->item.asString.value,
                                     pItem->item.asString.length,
                                     &fNewBuffer );
      if( szString )
      {
         HB_DYNS_PTR pDynSym = hb_dynsymGetCase( szString );

         if( fNewBuffer )
            hb_xfree( szString );   /* free space allocated in hb_macroTextSymbol */

         hb_stackPop();    /* remove compiled string */
         /* NOTE: checking for valid function name (valid pointer) is done
          * in hb_vmDo()
          */
         hb_vmPushSymbol( pDynSym->pSymbol );  /* push compiled symbol instead of a string */
         return;
      }
      else
         hb_macroSyntaxError( NULL );
   }

   if( !HB_IS_SYMBOL( hb_stackItemFromTop( -1 ) ) && hb_vmRequestQuery() == 0 )
   {
      hb_stackPop();    /* remove compiled string */
      hb_vmPushDynSym( hb_dynsymGetCase( "" ) );  /* push compiled symbol instead of a string */
   }
}

/* Macro text substitution
 *
 * 'pItem' points to a ITEM that contains a string value which after
 *    text substitution will be returned
 */
void hb_macroTextValue( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_macroTextValue(%p)", pItem));

   if( hb_macroCheckParam( pItem ) )
   {
      char * szString;
      HB_SIZE nLength = pItem->item.asString.length;

      szString = hb_macroTextSubst( pItem->item.asString.value, &nLength );

      if( szString != pItem->item.asString.value )
      {
         /* replace the old value on the eval stack with the new one
         */
         hb_itemPutCLPtr( pItem, szString, nLength );
      }
      /*
       * else
       *    leave original value on the eval stack - there was no '&' operator
       *    inside a string
       */
   }
}

const char * hb_macroGetType( PHB_ITEM pItem )
{
   HB_STACK_TLS_PRELOAD
   const char * szType;

   HB_TRACE(HB_TR_DEBUG, ("hb_macroGetType(%p)", pItem));

   if( hb_macroCheckParam( pItem ) )
   {
      HB_MACRO struMacro;
      int iStatus;

      struMacro.mode       = HB_MODE_MACRO;
      struMacro.supported  = hb_macroFlags() | HB_SM_ISUSERCP();
      struMacro.Flags      = HB_MACRO_GEN_PUSH | HB_MACRO_GEN_TYPE;
      struMacro.uiNameLen  = HB_SYMBOL_NAME_LEN;
      struMacro.status     = HB_MACRO_CONT;
      struMacro.string     = pItem->item.asString.value;
      struMacro.length     = pItem->item.asString.length;
      iStatus = hb_macroParse( &struMacro );

      if( iStatus == HB_MACRO_OK )
      {
         /* passed string was successfully compiled
          */
         if( struMacro.exprType == HB_ET_CODEBLOCK )
         {
            /* Clipper ignores any undeclared symbols or UDFs if the
             * compiled expression is a valid codeblock
             */
            szType = "B";
         }
         else if( struMacro.status & HB_MACRO_UNKN_SYM )
         {
            /* request for a symbol that is not in a symbol table or
             * for a variable that is not visible
             */
            szType = "U";
         }
         else if( struMacro.status & HB_MACRO_UDF )
         {
            szType = "UI";  /* UDF function was used - cannot determine a type */
         }
         else if( struMacro.status & HB_MACRO_CONT )
         {
            /* OK - the pcode was generated and it can be evaluated
            */
            HB_ERROR_INFO struErr;
            HB_ERROR_INFO_PTR pOld;

            /* Set our temporary error handler. We do not need any error
             * messages here - we need to know only if evaluation was
             * successfull. If evaluation was successfull then the data type
             * of expression can be determined.
             */
            struErr.Func  = hb_macroErrorType;
            struErr.Cargo = ( void * ) &struMacro;
            pOld = hb_errorHandler( &struErr );
            hb_macroRun( &struMacro );
            hb_errorHandler( pOld );

            if( struMacro.status & HB_MACRO_CONT )
            {
               /* Evaluation was successfull
                * Now the value of expression is placed on the eval stack -
                * check its type and pop it from the stack
                */
               szType = hb_itemTypeStr( hb_stackItemFromTop( -1 ) );
               hb_stackPop();
            }
            else
            {
               /* something unpleasant happened during macro evaluation */
               if( struMacro.pError )
               {
                  HB_ERRCODE ulGenCode = hb_errGetGenCode( struMacro.pError );

                  if( ulGenCode == EG_NOVAR || ulGenCode == EG_NOALIAS )
                  {
                     /* Undeclared variable returns 'U' in Clipper */
                     szType = "U";
                  }
                  else
                     szType = "UE";
               }
               else
                  szType = "UE";
            }
         }
         else
            szType = "UE";
      }
      else
         szType = "UE";  /* syntax error during compilation */

      hb_macroDelete( &struMacro );
   }
   else
      szType = "U";

   return szType;
}

/*
 * Set macro capabilities if flag > 0 or get current macro capabilities
 * if flag == 0
 */
int hb_macroSetMacro( HB_BOOL fSet, int flag )
{
   int currentFlags = hb_macroFlags();

   if( flag > 0 )
   {
      if( fSet )
         hb_macroFlagsSet( currentFlags | flag );
      else
         hb_macroFlagsSet( currentFlags & ~flag );
   }

   return currentFlags;
}

HB_FUNC( HB_SETMACRO )
{
   HB_STACK_TLS_PRELOAD
   int iPrmCnt = hb_pcount();

   if( iPrmCnt > 0 )
   {
       int flags = hb_parni( 1 );
       PHB_ITEM pValue;

       switch( flags )
       {
          case HB_SM_HARBOUR:
             /* enable/disable extended Harbour compatibility */
          case HB_SM_XBASE:
             /* enable/disable extended Xbase++ compatibility */
          case HB_SM_ARRSTR:
             /* enable/disable processing of strings as an array of bytes */
          case HB_SM_SHORTCUTS:
             /* enable/disable support for shortcut logical operators */
             hb_retl( hb_macroFlags() & flags );
             pValue = hb_param( 2, HB_IT_LOGICAL );
             if( pValue )
                hb_macroSetMacro( hb_itemGetL( pValue ), flags );
             break;

          default:
              ;/* do nothing */
      }
   }
   else
      hb_ret();    /* return NIL */
}

/* ************************************************************************* */

/* returns the order + 1 of a variable if defined or zero */
int hb_macroLocalVarGetPos( const char * szVarName, HB_COMP_DECL )
{
   int iVar = 1;
   HB_CBVAR_PTR pVars = HB_PCODE_DATA->pLocals;

   while( pVars )
   {
      if( pVars->szName && ! strcmp( pVars->szName, szVarName ) )
         return iVar;
      pVars = pVars->pNext;
      iVar++;
   }
   return 0;
}

HB_BOOL hb_macroIsValidMacroText( const char * szText, HB_SIZE nLen )
{
   if( nLen )
   {
      while( --nLen )
      {
         if( *szText++ == '&' )
         {
            char ch = *szText;
            if( ( ch >= 'A' && ch <= 'Z' ) ||
                ( ch >= 'a' && ch <= 'z' ) || ch == '_' )
               return HB_TRUE;
         }
      }
   }

   return HB_FALSE;
}

HB_SIZE hb_macroGenJump( HB_ISIZ nOffset, HB_COMP_DECL )
{
   if( nOffset == 0 )
      hb_macroGenPCode4( HB_P_JUMPFAR, 0, 0, 0, HB_COMP_PARAM );
   else if( HB_LIM_INT8( nOffset ) )
      hb_macroGenPCode2( HB_P_JUMPNEAR, HB_LOBYTE( nOffset ), HB_COMP_PARAM );
   else if( HB_LIM_INT16( nOffset ) )
      hb_macroGenPCode3( HB_P_JUMP, HB_LOBYTE( nOffset ), HB_HIBYTE( nOffset ), HB_COMP_PARAM );
   else if( HB_LIM_INT24( nOffset ) )
      hb_macroGenPCode4( HB_P_JUMPFAR, HB_LOBYTE( nOffset ), HB_HIBYTE( nOffset ), HB_ULBYTE( nOffset ), HB_COMP_PARAM );
   else
      hb_macroError( HB_MACRO_TOO_COMPLEX, HB_COMP_PARAM );

   return HB_PCODE_DATA->nPCodePos - 3;
}

HB_SIZE hb_macroGenJumpFalse( HB_ISIZ nOffset, HB_COMP_DECL )
{
   if( nOffset == 0 )
      hb_macroGenPCode4( HB_P_JUMPFALSEFAR, 0, 0, 0, HB_COMP_PARAM );
   else if( HB_LIM_INT8( nOffset ) )
      hb_macroGenPCode2( HB_P_JUMPFALSENEAR, HB_LOBYTE( nOffset ), HB_COMP_PARAM );
   else if( HB_LIM_INT16( nOffset ) )
      hb_macroGenPCode3( HB_P_JUMPFALSE, HB_LOBYTE( nOffset ), HB_HIBYTE( nOffset ), HB_COMP_PARAM );
   else if( HB_LIM_INT24( nOffset ) )
      hb_macroGenPCode4( HB_P_JUMPFALSEFAR, HB_LOBYTE( nOffset ), HB_HIBYTE( nOffset ), HB_ULBYTE( nOffset ), HB_COMP_PARAM );
   else
      hb_macroError( HB_MACRO_TOO_COMPLEX, HB_COMP_PARAM );

   return HB_PCODE_DATA->nPCodePos - 3;
}

HB_SIZE hb_macroGenJumpTrue( HB_ISIZ nOffset, HB_COMP_DECL )
{
   if( nOffset == 0 )
      hb_macroGenPCode4( HB_P_JUMPTRUEFAR, 0, 0, 0, HB_COMP_PARAM );
   else if( HB_LIM_INT8( nOffset ) )
      hb_macroGenPCode2( HB_P_JUMPTRUENEAR, HB_LOBYTE( nOffset ), HB_COMP_PARAM );
   else if( HB_LIM_INT16( nOffset ) )
      hb_macroGenPCode3( HB_P_JUMPTRUE, HB_LOBYTE( nOffset ), HB_HIBYTE( nOffset ), HB_COMP_PARAM );
   else if( HB_LIM_INT24( nOffset ) )
      hb_macroGenPCode4( HB_P_JUMPTRUEFAR, HB_LOBYTE( nOffset ), HB_HIBYTE( nOffset ), HB_ULBYTE( nOffset ), HB_COMP_PARAM );
   else
      hb_macroError( HB_MACRO_TOO_COMPLEX, HB_COMP_PARAM );

   return HB_PCODE_DATA->nPCodePos - 3;
}

void hb_macroGenJumpThere( HB_SIZE nFrom, HB_SIZE nTo, HB_COMP_DECL )
{
   HB_BYTE * pCode = HB_PCODE_DATA->pCode;
   HB_ISIZ nOffset = nTo - nFrom + 1;

   if( HB_LIM_INT24( nOffset ) )
      HB_PUT_LE_UINT24( &pCode[ nFrom ], nOffset );
   else
      hb_macroError( HB_MACRO_TOO_COMPLEX, HB_COMP_PARAM );
}

void hb_macroGenJumpHere( HB_SIZE nOffset, HB_COMP_DECL )
{
   hb_macroGenJumpThere( nOffset, HB_PCODE_DATA->nPCodePos, HB_COMP_PARAM );
}

/*
 * Function generates pcode for passed memvar name
 */
static void hb_macroMemvarGenPCode( HB_BYTE bPCode, const char * szVarName, HB_COMP_DECL )
{
   HB_BYTE byBuf[ sizeof( HB_DYNS_PTR ) + 1 ];
   HB_DYNS_PTR pSym;

   if( HB_MACRO_DATA->Flags & HB_MACRO_GEN_TYPE )
   {
      /* we are determining the type of expression (called from TYPE() function)
       * then we shouldn't create the requested variable if it doesn't exist
       */
      pSym = hb_dynsymFind( szVarName );
      if( !pSym )
      {
         HB_MACRO_DATA->status |= HB_MACRO_UNKN_VAR;
         pSym = hb_dynsymGetCase( szVarName );
      }
   }
   else
      /* Find the address of passed symbol - create the symbol if doesn't exist
       * (Clipper compatibility). */
      pSym = hb_dynsymGetCase( szVarName );

   byBuf[ 0 ] = bPCode;
   HB_PUT_PTR( &byBuf[ 1 ], pSym );
   hb_macroGenPCodeN( byBuf, sizeof( byBuf ), HB_COMP_PARAM );
}

/* generates the pcode to push a symbol on the virtual machine stack */
void hb_macroGenPushSymbol( const char * szSymbolName, HB_BOOL bFunction, HB_COMP_DECL )
{
   HB_BYTE byBuf[ sizeof( HB_DYNS_PTR ) + 1 ];
   HB_DYNS_PTR pSym;

   if( HB_MACRO_DATA->Flags & HB_MACRO_GEN_TYPE )
   {
      /* we are determining the type of expression (called from TYPE() function)
       */
      pSym = hb_dynsymFind( szSymbolName );
      if( ! pSym )
      {
         HB_MACRO_DATA->status |= HB_MACRO_UNKN_SYM;
         HB_MACRO_DATA->status &= ~HB_MACRO_CONT;  /* don't run this pcode */
         /*
          * NOTE: the compiled pcode will be not executed then we can ignore
          * NULL value for pSym
          */
      }
      else if( bFunction )
      {
         if( pSym->pSymbol->value.pFunPtr == NULL )
         {
            /* static functions are not allowed in macro */
            HB_MACRO_DATA->status |= HB_MACRO_UNKN_SYM;
            HB_MACRO_DATA->status &= ~HB_MACRO_CONT;  /* don't run this pcode */
         }
      }
   }
   else
      pSym = hb_dynsymGetCase( szSymbolName );

   byBuf[ 0 ] = HB_P_MPUSHSYM;
   HB_PUT_PTR( &byBuf[ 1 ], pSym );
   hb_macroGenPCodeN( byBuf, sizeof( byBuf ), HB_COMP_PARAM );
}

/* generates the pcode to push a long number on the virtual machine stack */
void hb_macroGenPushLong( HB_MAXINT nNumber, HB_COMP_DECL )
{
   if( nNumber == 0 )
   {
      hb_macroGenPCode1( HB_P_ZERO, HB_COMP_PARAM );
   }
   else if( nNumber == 1 )
   {
      hb_macroGenPCode1( HB_P_ONE, HB_COMP_PARAM );
   }
   else if( HB_LIM_INT8( nNumber ) )
   {
      hb_macroGenPCode2( HB_P_PUSHBYTE, ( HB_BYTE ) nNumber, HB_COMP_PARAM );
   }
   else if( HB_LIM_INT16( nNumber ) )
   {
      hb_macroGenPCode3( HB_P_PUSHINT, HB_LOBYTE( nNumber ), HB_HIBYTE( nNumber ), HB_COMP_PARAM );
   }
   else if( HB_LIM_INT32( nNumber ) )
   {
      HB_BYTE pBuffer[ 5 ];
      pBuffer[ 0 ] = HB_P_PUSHLONG;
      HB_PUT_LE_UINT32( pBuffer + 1, nNumber );
      hb_macroGenPCodeN( pBuffer, sizeof( pBuffer ), HB_COMP_PARAM );
   }
   else
   {
      HB_BYTE pBuffer[ 9 ];
      pBuffer[ 0 ] = HB_P_PUSHLONGLONG;
      HB_PUT_LE_UINT64( pBuffer + 1, nNumber );
      hb_macroGenPCodeN( pBuffer, sizeof( pBuffer ), HB_COMP_PARAM );
   }
}

/* generates the pcode to push a date on the virtual machine stack */
void hb_macroGenPushDate( long lDate, HB_COMP_DECL )
{
   HB_BYTE pBuffer[ 5 ];

   pBuffer[ 0 ] = HB_P_PUSHDATE;
   HB_PUT_LE_UINT32( pBuffer + 1, lDate );
   hb_macroGenPCodeN( pBuffer, sizeof( pBuffer ), HB_COMP_PARAM );
}

/* generates the pcode to push a timestamp on the virtual machine stack */
void hb_macroGenPushTimeStamp( long lDate, long lTime, HB_COMP_DECL )
{
   HB_BYTE pBuffer[ 9 ];

   pBuffer[ 0 ] = HB_P_PUSHTIMESTAMP;
   HB_PUT_LE_UINT32( pBuffer + 1, lDate );
   HB_PUT_LE_UINT32( pBuffer + 5, lTime );
   hb_macroGenPCodeN( pBuffer, sizeof( pBuffer ), HB_COMP_PARAM );
}

/* sends a message to an object */
void hb_macroGenMessage( const char * szMsgName, HB_BOOL bIsObject, HB_COMP_DECL )
{
   if( szMsgName )
   {
      HB_BYTE byBuf[ sizeof( HB_DYNS_PTR ) + 1 ];

      /* Find the address of passed symbol - create the symbol if doesn't exist
       */
      HB_DYNS_PTR pSym = hb_dynsymGetCase( szMsgName );

      byBuf[ 0 ] = HB_P_MMESSAGE;
      HB_PUT_PTR( &byBuf[ 1 ], pSym );
      hb_macroGenPCodeN( byBuf, sizeof( byBuf ), HB_COMP_PARAM );
   }
   if( !bIsObject )     /* used in full compiler only */
      hb_macroGenPCode3( HB_P_WITHOBJECTMESSAGE, 0xFF, 0xFF, HB_COMP_PARAM );
}

/* generates an underscore-symbol name for a data assignment */
void hb_macroGenMessageData( const char * szMsg, HB_BOOL bIsObject, HB_COMP_DECL )
{
   char szResult[ HB_SYMBOL_NAME_LEN + 1 ];
   int iLen;

   HB_TRACE(HB_TR_DEBUG, ("hb_macroGenMessageData(%s)", szMsg));

   iLen = ( int ) strlen( szMsg );
   if( iLen > HB_SYMBOL_NAME_LEN - 1 )
      iLen = HB_SYMBOL_NAME_LEN - 1;
   szResult[ 0 ] = '_';
   memcpy( szResult + 1, szMsg, iLen );
   szResult[ iLen + 1 ] = '\0';
   hb_macroGenMessage( szResult, bIsObject, HB_COMP_PARAM );
}

/* generates the pcode to pop a value from the virtual machine stack onto a variable */
void hb_macroGenPopVar( const char * szVarName, HB_COMP_DECL )
{
   int iVar;

   iVar = hb_macroLocalVarGetPos( szVarName, HB_COMP_PARAM );
   if( iVar )
   {
      /* this is a codeblock parameter */
      hb_macroGenPCode3( HB_P_POPLOCAL, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), HB_COMP_PARAM );
   }
   else
   {
      /* TODO: memvars created inside TYPE() function should have PUBLIC scope */
      hb_macroMemvarGenPCode( HB_P_MPOPMEMVAR, szVarName, HB_COMP_PARAM );
   }
}

/* generates the pcode to pop a value from the virtual machine stack onto a variable */
void hb_macroGenPopMemvar( const char * szVarName, HB_COMP_DECL )
{
   hb_macroMemvarGenPCode( HB_P_MPOPMEMVAR, szVarName, HB_COMP_PARAM );
}

/* generates the pcode to pop a value from the virtual machine stack onto
 * an aliased variable
 */
void hb_macroGenPopAliasedVar( const char * szVarName,
                               HB_BOOL bPushAliasValue,
                               const char * szAlias,
                               HB_MAXINT nWorkarea, HB_COMP_DECL )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_macroGenPopAliasedVar(%s->%s)",szAlias,szVarName));

   if( bPushAliasValue )
   {
      if( szAlias )
      {
         int iLen = ( int ) strlen( szAlias );

         if( szAlias[ 0 ] == 'M' && ( iLen == 1 ||
             ( iLen >= 4 && iLen <= 6 && strncmp( szAlias, "MEMVAR", iLen ) == 0 ) ) )
         {  /* M-> or MEMV-> or MEMVA-> or MEMVAR-> variable */
            /* TODO: memvars created inside TYPE() function should have PUBLIC scope */
            hb_macroMemvarGenPCode( HB_P_MPOPMEMVAR, szVarName, HB_COMP_PARAM );
         }
         else if( iLen >= 4 && iLen <= 6 &&
                  ( strncmp( szAlias, "FIELD", iLen ) == 0 ||
                    strncmp( szAlias, "_FIELD", iLen ) == 0 ) )
         {  /* FIELD-> */
            hb_macroMemvarGenPCode( HB_P_MPOPFIELD, szVarName, HB_COMP_PARAM );
         }
         else
         {  /* database alias */
            hb_macroGenPushSymbol( szAlias, HB_FALSE, HB_COMP_PARAM );
            hb_macroMemvarGenPCode( HB_P_MPOPALIASEDFIELD, szVarName, HB_COMP_PARAM );
         }
      }
      else
      {
         hb_macroGenPushLong( nWorkarea, HB_COMP_PARAM );
         hb_macroMemvarGenPCode( HB_P_MPOPALIASEDFIELD, szVarName, HB_COMP_PARAM );
      }
   }
   else
   {
      /* Alias is already placed on stack
       * NOTE: An alias will be determined at runtime then we cannot decide
       * here if passed name is either a field or a memvar
       */
      /* TODO: memvars created inside TYPE() function should have PUBLIC scope */
      hb_macroMemvarGenPCode( HB_P_MPOPALIASEDVAR, szVarName, HB_COMP_PARAM );
   }
}

/* generates the pcode to push a nonaliased variable value to the virtual
 * machine stack
 */
void hb_macroGenPushVar( const char * szVarName, HB_COMP_DECL )
{
   int iVar;

   iVar = hb_macroLocalVarGetPos( szVarName, HB_COMP_PARAM );
   if( iVar )
   {
      /* this is a codeblock parameter */
      hb_macroGenPCode3( HB_P_PUSHLOCAL, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), HB_COMP_PARAM );
   }
   else
   {
      hb_macroMemvarGenPCode( HB_P_MPUSHVARIABLE, szVarName, HB_COMP_PARAM );
   }
}

/* generates the pcode to push a variable by reference to the virtual machine stack */
void hb_macroGenPushVarRef( const char * szVarName, HB_COMP_DECL )
{
   int iVar;

   iVar = hb_macroLocalVarGetPos( szVarName, HB_COMP_PARAM );
   if( iVar )
      hb_macroGenPCode3( HB_P_PUSHLOCALREF, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), HB_COMP_PARAM );
   else
   {
      hb_macroMemvarGenPCode( HB_P_MPUSHMEMVARREF, szVarName, HB_COMP_PARAM );
   }
}

/* generates the pcode to push a variable by reference to the virtual machine stack */
void hb_macroGenPushMemvarRef( const char * szVarName, HB_COMP_DECL )
{
   hb_macroMemvarGenPCode( HB_P_MPUSHMEMVARREF, szVarName, HB_COMP_PARAM );
}

/* generates the pcode to push an aliased variable value to the virtual
 * machine stack
 */
void hb_macroGenPushAliasedVar( const char * szVarName,
                                HB_BOOL bPushAliasValue,
                                const char * szAlias,
                                HB_MAXINT nWorkarea, HB_COMP_DECL )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_macroGenPushAliasedVar(%s->%s)",szAlias,szVarName));

   if( bPushAliasValue )
   {
      if( szAlias )
      {
         /* myalias->var
         * FIELD->var
         * MEMVAR->var
         */
         int iLen = ( int ) strlen( szAlias );

         if( szAlias[ 0 ] == 'M' && ( iLen == 1 ||
             ( iLen >= 4 && iLen <= 6 && strncmp( szAlias, "MEMVAR", iLen ) == 0 ) ) )
         {  /* M-> or MEMV-> or MEMVA-> or MEMVAR-> variable */
            hb_macroMemvarGenPCode( HB_P_MPUSHMEMVAR, szVarName, HB_COMP_PARAM );
         }
         else if( iLen >= 4 && iLen <= 6 &&
                  ( strncmp( szAlias, "FIELD", iLen ) == 0 ||
                    strncmp( szAlias, "_FIELD", iLen ) == 0 ) )
         {  /* FIELD-> */
            hb_macroMemvarGenPCode( HB_P_MPUSHFIELD, szVarName, HB_COMP_PARAM );
         }
         else
         {  /* database alias */
            hb_macroGenPushSymbol( szAlias, HB_FALSE, HB_COMP_PARAM );
            hb_macroMemvarGenPCode( HB_P_MPUSHALIASEDFIELD, szVarName, HB_COMP_PARAM );
         }
      }
      else
      {
         hb_macroGenPushLong( nWorkarea, HB_COMP_PARAM );
         hb_macroMemvarGenPCode( HB_P_MPUSHALIASEDFIELD, szVarName, HB_COMP_PARAM );
      }
   }
   else
   {
      /* Alias is already placed on stack
       * NOTE: An alias will be determined at runtime then we cannot decide
       * here if passed name is either a field or a memvar
       */
      hb_macroMemvarGenPCode( HB_P_MPUSHALIASEDVAR, szVarName, HB_COMP_PARAM );
   }
}

/* pushes a logical value on the virtual machine stack , */
void hb_macroGenPushLogical( int iTrueFalse, HB_COMP_DECL )
{
   if( iTrueFalse )
      hb_macroGenPCode1( HB_P_TRUE, HB_COMP_PARAM );
   else
      hb_macroGenPCode1( HB_P_FALSE, HB_COMP_PARAM );
}

/* generates the pcode to push a double number on the virtual machine stack */
void hb_macroGenPushDouble( double dNumber, HB_BYTE bWidth, HB_BYTE bDec, HB_COMP_DECL )
{
   HB_BYTE pBuffer[ sizeof( double ) + sizeof( HB_BYTE ) + sizeof( HB_BYTE ) + 1 ];

   pBuffer[ 0 ] = HB_P_PUSHDOUBLE;
   HB_PUT_LE_DOUBLE( &( pBuffer[ 1 ] ), dNumber );
   pBuffer[ 1 + sizeof( double ) ] = bWidth;
   pBuffer[ 1 + sizeof( double ) + sizeof( HB_BYTE ) ] = bDec;

   hb_macroGenPCodeN( pBuffer, 1 + sizeof( double ) + sizeof( HB_BYTE ) + sizeof( HB_BYTE ), HB_COMP_PARAM );
}

void hb_macroGenPushFunSym( const char * szFunName, int iFlags, HB_COMP_DECL )
{
   if( ( iFlags & HB_FN_RESERVED ) == 0 )
      HB_MACRO_DATA->status |= HB_MACRO_UDF; /* this is used in hb_macroGetType */
   hb_macroGenPushSymbol( szFunName, HB_TRUE, HB_COMP_PARAM );
}

void hb_macroGenPushFunCall( const char * szFunName, int iFlags, HB_COMP_DECL )
{
   hb_macroGenPushFunSym( szFunName, iFlags, HB_COMP_PARAM );
   hb_macroGenPCode1( HB_P_PUSHNIL, HB_COMP_PARAM );
}

void hb_macroGenPushFunRef( const char * szFunName, HB_COMP_DECL )
{
   hb_macroGenPushSymbol( szFunName, HB_TRUE, HB_COMP_PARAM );
}

/* generates the pcode to push a string on the virtual machine stack */
void hb_macroGenPushString( const char * szText, HB_SIZE nStrLen, HB_COMP_DECL )
{
   if( nStrLen <= UINT24_MAX )
   {
      if( nStrLen <= USHRT_MAX )
         hb_macroGenPCode3( HB_P_MPUSHSTR, HB_LOBYTE( nStrLen ), HB_HIBYTE( nStrLen ), HB_COMP_PARAM );
      else
         hb_macroGenPCode4( HB_P_MPUSHSTRLARGE, HB_LOBYTE( nStrLen ), HB_HIBYTE( nStrLen ), HB_ULBYTE( nStrLen ), HB_COMP_PARAM );
      hb_macroGenPCodeN( ( const HB_BYTE * ) szText, nStrLen, HB_COMP_PARAM );
   }
   else
      hb_macroError( HB_MACRO_TOO_COMPLEX, HB_COMP_PARAM );
}

void hb_macroGenPCode1( HB_BYTE byte, HB_COMP_DECL )
{
   HB_PCODE_INFO_PTR pFunc = HB_PCODE_DATA;

   if( ( pFunc->nPCodeSize - pFunc->nPCodePos ) < 1 )
      pFunc->pCode = ( HB_BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->nPCodeSize += HB_PCODE_SIZE );

   pFunc->pCode[ pFunc->nPCodePos++ ] = byte;
}

void hb_macroGenPCode2( HB_BYTE byte1, HB_BYTE byte2, HB_COMP_DECL )
{
   HB_PCODE_INFO_PTR pFunc = HB_PCODE_DATA;

   if( ( pFunc->nPCodeSize - pFunc->nPCodePos ) < 2 )
      pFunc->pCode = ( HB_BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->nPCodeSize += HB_PCODE_SIZE );

   pFunc->pCode[ pFunc->nPCodePos++ ] = byte1;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte2;
}

void hb_macroGenPCode3( HB_BYTE byte1, HB_BYTE byte2, HB_BYTE byte3, HB_COMP_DECL )
{
   HB_PCODE_INFO_PTR pFunc = HB_PCODE_DATA;

   if( ( pFunc->nPCodeSize - pFunc->nPCodePos ) < 3 )
      pFunc->pCode = ( HB_BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->nPCodeSize += HB_PCODE_SIZE );

   pFunc->pCode[ pFunc->nPCodePos++ ] = byte1;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte2;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte3;
}

void hb_macroGenPCode4( HB_BYTE byte1, HB_BYTE byte2, HB_BYTE byte3, HB_BYTE byte4, HB_COMP_DECL )
{
   HB_PCODE_INFO_PTR pFunc = HB_PCODE_DATA;

   if( ( pFunc->nPCodeSize - pFunc->nPCodePos ) < 4 )
      pFunc->pCode = ( HB_BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->nPCodeSize += HB_PCODE_SIZE );

   pFunc->pCode[ pFunc->nPCodePos++ ] = byte1;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte2;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte3;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte4;
}

void hb_macroGenPCodeN( const HB_BYTE * pBuffer, HB_SIZE nSize, HB_COMP_DECL )
{
   HB_PCODE_INFO_PTR pFunc = HB_PCODE_DATA;

   if( pFunc->nPCodePos + nSize > pFunc->nPCodeSize )
   {
      /* not enough free space in pcode buffer - increase it */
      pFunc->nPCodeSize += ( ( ( nSize / HB_PCODE_SIZE ) + 1 ) * HB_PCODE_SIZE );
      pFunc->pCode = ( HB_BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->nPCodeSize );
   }

   memcpy( pFunc->pCode + pFunc->nPCodePos, pBuffer, nSize );
   pFunc->nPCodePos += nSize;
}

/* ************************************************************************* */

void hb_macroError( int iError, HB_COMP_DECL )
{
   HB_MACRO_DATA->status |= iError;
   HB_MACRO_DATA->status &= ~HB_MACRO_CONT;  /* clear CONT bit */
}

/*
 * Start a new pcode buffer for a codeblock
*/
void hb_macroCodeBlockStart( HB_COMP_DECL )
{
   HB_PCODE_INFO_PTR pCB;

   HB_TRACE(HB_TR_DEBUG, ("hb_macroCodeBlockStart(%p)", HB_COMP_PARAM));

   pCB = ( HB_PCODE_INFO_PTR ) hb_xgrab( sizeof( HB_PCODE_INFO ) );

   pCB->pCode = ( HB_BYTE * ) hb_xgrab( HB_PCODE_SIZE );
   pCB->nPCodeSize = HB_PCODE_SIZE;
   pCB->nPCodePos  = 0;
   pCB->fVParams   = HB_FALSE;
   pCB->pLocals    = NULL;

   /* replace current pcode buffer with the new one
    */
   pCB->pPrev = HB_PCODE_DATA;
   HB_PCODE_DATA = pCB;
}

void hb_macroCodeBlockEnd( HB_COMP_DECL )
{
   HB_PCODE_INFO_PTR pCodeblock;   /* pointer to the current codeblock */
   HB_SIZE nSize;
   HB_USHORT usParms = 0;   /* number of codeblock parameters */
   HB_CBVAR_PTR pVar;

   HB_TRACE(HB_TR_DEBUG, ("hb_macroCodeBlockEnd(%p)", HB_COMP_PARAM));

   /* a currently processed codeblock */
   pCodeblock = HB_PCODE_DATA;

   /* return to pcode buffer of a codeblock in which the current
    * codeblock was defined
    */
   HB_PCODE_DATA = pCodeblock->pPrev;

   /* generate a proper codeblock frame with a codeblock size and with
    * a number of expected parameters
    */

   /* Count the number of codeblock parameters */
   pVar = pCodeblock->pLocals;
   while( pVar )
   {
      pVar = pVar->pNext;
      ++usParms;
   }

   /* NOTE: 6 = HB_P_MPUSHBLOCK + HB_USHORT( size ) + HB_USHORT( wParams ) + _ENDBLOCK
    * runtime compiled codeblock cannot reference local variables defined in a
    * function
    */
   nSize = pCodeblock->nPCodePos + 6;

   /* NOTE: HB_P_MPUSHBLOCK differs from HB_P_PUSHBLOCK - the pcode
    * is stored in dynamic memory pool instead of static memory
    */
   if( nSize <= USHRT_MAX )
      hb_macroGenPCode3( HB_P_MPUSHBLOCK, HB_LOBYTE( nSize ), HB_HIBYTE( nSize ), HB_COMP_PARAM );
   else
   {
      ++nSize;
      hb_macroGenPCode4( HB_P_MPUSHBLOCKLARGE, HB_LOBYTE( nSize ), HB_HIBYTE( nSize ), HB_ULBYTE( nSize ), HB_COMP_PARAM );
   }
   hb_macroGenPCode2( HB_LOBYTE( usParms ), HB_HIBYTE( usParms ), HB_COMP_PARAM );

   /* copy a codeblock pcode buffer */
   hb_macroGenPCodeN( pCodeblock->pCode, pCodeblock->nPCodePos, HB_COMP_PARAM );
   hb_macroGenPCode1( HB_P_ENDBLOCK, HB_COMP_PARAM ); /* finish the codeblock */

   /* free memory allocated for a codeblock */
   hb_xfree( pCodeblock->pCode );
   hb_xfree( pCodeblock );
}
