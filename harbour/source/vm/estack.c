/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The eval stack management functions
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
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

#if defined(HB_INCLUDE_WINEXCHANDLER)
   #define HB_OS_WIN_32_USED
#endif

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbapicls.h"
#include "hbdefs.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapierr.h"

/* ------------------------------- */

#if !defined( STACK_INITHB_ITEMS )
   #define STACK_INITHB_ITEMS      200
#endif
#if !defined( STACK_EXPANDHB_ITEMS )
   #define STACK_EXPANDHB_ITEMS    20
#endif

HB_STACK hb_stack;

static HB_SYMB s_initSymbol = { "hb_stackInit", { HB_FS_STATIC }, { NULL }, NULL };

/* ------------------------------- */

#undef hb_stackId
void * hb_stackId( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_stackId()"));

   return ( void * ) &hb_stack;
}

#undef hb_stackPop
void hb_stackPop( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_stackPop()"));

   if( --hb_stack.pPos <= hb_stack.pBase )
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );

   if( HB_IS_COMPLEX( * hb_stack.pPos ) )
      hb_itemClear( * hb_stack.pPos );
}

#undef hb_stackPopReturn
void hb_stackPopReturn( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_stackPopReturn()"));

   if( HB_IS_COMPLEX( &hb_stack.Return ) )
      hb_itemClear( &hb_stack.Return );

   if( --hb_stack.pPos <= hb_stack.pBase )
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );

   hb_itemRawMove( &hb_stack.Return, * hb_stack.pPos );
}

#undef hb_stackDec
void hb_stackDec( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_stackDec()"));

   if( --hb_stack.pPos <= hb_stack.pBase )
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );
}

#undef hb_stackDecrease
void hb_stackDecrease( ULONG ulItems )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_stackDecrease()"));

   if( ( hb_stack.pPos -= ulItems ) <= hb_stack.pBase )
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );
}

void hb_stackFree( void )
{
   LONG i;

   HB_TRACE(HB_TR_DEBUG, ("hb_stackFree()"));

   i = hb_stack.wItems - 1;
   while( i >= 0 )
      hb_xfree( hb_stack.pItems[ i-- ] );
   hb_xfree( hb_stack.pItems );
   hb_stack.pItems = hb_stack.pPos = hb_stack.pBase = NULL;
}

#undef hb_stackPush
void hb_stackPush( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_stackPush()"));

   /* enough room for another item ? */
   if( ++hb_stack.pPos == hb_stack.pEnd )
      hb_stackIncrease();
}

#undef hb_stackAllocItem
HB_ITEM_PTR hb_stackAllocItem( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_stackAllocItem()"));

   if( ++hb_stack.pPos == hb_stack.pEnd )
      hb_stackIncrease();

   return * ( hb_stack.pPos - 1 );
}

#undef hb_stackPushReturn
void hb_stackPushReturn( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_stackPushReturn()"));

   hb_itemRawMove( * hb_stack.pPos, &hb_stack.Return );

   /* enough room for another item ? */
   if( ++hb_stack.pPos == hb_stack.pEnd )
      hb_stackIncrease();
}

void hb_stackIncrease( void )
{
   LONG BaseIndex;   /* index of stack base */
   LONG CurrIndex;   /* index of current top item */
   LONG EndIndex;    /* index of current top item */

   HB_TRACE(HB_TR_DEBUG, ("hb_stackIncrease()"));

   BaseIndex = hb_stack.pBase - hb_stack.pItems;
   CurrIndex = hb_stack.pPos - hb_stack.pItems;
   EndIndex  = hb_stack.pEnd - hb_stack.pItems;

   /* no, make more headroom: */
   hb_stack.pItems = ( PHB_ITEM * ) hb_xrealloc( ( void * ) hb_stack.pItems,
            sizeof( PHB_ITEM ) * ( hb_stack.wItems + STACK_EXPANDHB_ITEMS ) );

   /* fix possibly modified by realloc pointers: */
   hb_stack.pPos   = hb_stack.pItems + CurrIndex;
   hb_stack.pBase  = hb_stack.pItems + BaseIndex;
   hb_stack.wItems += STACK_EXPANDHB_ITEMS;
   hb_stack.pEnd   = hb_stack.pItems + hb_stack.wItems;

   do
   {
      hb_stack.pItems[ EndIndex ] = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) );
      hb_stack.pItems[ EndIndex ]->type = HB_IT_NIL;
   }
   while( ++EndIndex < hb_stack.wItems );
}

void hb_stackInit( void )
{
   LONG i;

   HB_TRACE(HB_TR_DEBUG, ("hb_stackInit()"));

   memset( &hb_stack, 0, sizeof( HB_STACK ) );

   hb_stack.pItems = ( PHB_ITEM * ) hb_xgrab( sizeof( PHB_ITEM ) * STACK_INITHB_ITEMS );
   hb_stack.pBase  = hb_stack.pItems;
   hb_stack.pPos   = hb_stack.pItems;     /* points to the first stack item */
   hb_stack.wItems = STACK_INITHB_ITEMS;
   hb_stack.pEnd   = hb_stack.pItems + hb_stack.wItems;

   for( i = 0; i < hb_stack.wItems; ++i )
   {
      hb_stack.pItems[ i ] = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) );
      hb_stack.pItems[ i ]->type = HB_IT_NIL;
   }

   hb_stack.pPos++;
   hb_itemPutSymbol( * hb_stack.pItems, &s_initSymbol );
   ( * hb_stack.pItems )->item.asSymbol.stackstate = &hb_stack.state;
}

void hb_stackRemove( LONG lUntilPos )
{
   HB_ITEM_PTR * pEnd = hb_stack.pItems + lUntilPos;

   while( hb_stack.pPos > pEnd )
   {
      --hb_stack.pPos;
      if( HB_IS_COMPLEX( * hb_stack.pPos ) )
         hb_itemClear( * hb_stack.pPos );
   }
}

HB_ITEM_PTR hb_stackNewFrame( PHB_STACK_STATE pStack, USHORT uiParams )
{
   HB_ITEM_PTR * pBase, pItem;

   pBase = hb_stack.pPos - uiParams - 2;
   pItem = * pBase;  /* procedure symbol */

   if( ! HB_IS_SYMBOL( pItem ) )
   {
      hb_stackDispLocal();
      hb_errInternal( HB_EI_VMNOTSYMBOL, NULL, "hb_vmDo()", NULL );
   }

   pStack->lBaseItem = hb_stack.pBase - hb_stack.pItems;
   pStack->lStatics = hb_stack.lStatics;
   pStack->ulPrivateBase = hb_memvarGetPrivatesBase();
   pStack->uiClass = pStack->uiMethod = pStack->uiLineNo = 0;

   pItem->item.asSymbol.stackstate = pStack;
   pItem->item.asSymbol.paramcnt = uiParams;
   /* set default value of 'paramdeclcnt' - it will be updated
    * in hb_vmVFrame only
    */
   pItem->item.asSymbol.paramdeclcnt = uiParams;
   hb_stack.pBase = pBase;

   return pItem;
}

void hb_stackOldFrame( PHB_STACK_STATE pStack )
{
   if( hb_stack.pPos <= hb_stack.pBase )
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );

   do
   {
      --hb_stack.pPos;
      if( HB_IS_COMPLEX( * hb_stack.pPos ) )
         hb_itemClear( * hb_stack.pPos );
   }
   while( hb_stack.pPos > hb_stack.pBase );

   hb_stack.pBase = hb_stack.pItems + pStack->lBaseItem;
   hb_stack.lStatics = pStack->lStatics;
   hb_memvarSetPrivatesBase( pStack->ulPrivateBase );
}

#undef hb_stackItem
HB_ITEM_PTR hb_stackItem( LONG iItemPos )
{
   if( iItemPos < 0 )
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );

   return * ( hb_stack.pItems + iItemPos );
}

#undef hb_stackItemFromTop
HB_ITEM_PTR hb_stackItemFromTop( int iFromTop )
{
   if( iFromTop >= 0 )
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );

   return * ( hb_stack.pPos + iFromTop );
}

#undef hb_stackItemFromBase
HB_ITEM_PTR hb_stackItemFromBase( int iFromBase )
{
   if( iFromBase < 0 )
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );

   return * ( hb_stack.pBase + iFromBase + 1 );
}

#undef hb_stackLocalVariable
HB_ITEM_PTR hb_stackLocalVariable( int *piFromBase )
{
   HB_ITEM_PTR pBase = *hb_stack.pBase;

/*   
   if( *piFromBase <= 0 )
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );
*/
   if( pBase->item.asSymbol.paramcnt > pBase->item.asSymbol.paramdeclcnt )
   {
      /* function with variable number of parameters:
       * FUNCTION foo( a,b,c,...)
       * LOCAL x,y,z
       * number of passed parameters is bigger then number of declared
       * parameters - skip additional parameters only for local variables
       */
      if( *piFromBase > pBase->item.asSymbol.paramdeclcnt )
         *piFromBase += pBase->item.asSymbol.paramcnt - pBase->item.asSymbol.paramdeclcnt;
   }
   return * ( hb_stack.pBase + *piFromBase + 1 );
}

#undef hb_stackBaseItem
HB_ITEM_PTR hb_stackBaseItem( void )
{
   return * hb_stack.pBase;
}

/* Returns SELF object, an evaluated codeblock or NIL for normal func/proc
*/
#undef hb_stackSelfItem
HB_ITEM_PTR hb_stackSelfItem( void )
{
   return * ( hb_stack.pBase + 1 );
}

#undef hb_stackReturnItem
HB_ITEM_PTR hb_stackReturnItem( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_stackReturnItem()"));

   return &hb_stack.Return;
}

#undef hb_stackTopOffset
LONG hb_stackTopOffset( void )
{
   return hb_stack.pPos - hb_stack.pItems;
}

#undef hb_stackBaseOffset
LONG hb_stackBaseOffset( void )
{
   return hb_stack.pBase - hb_stack.pItems + 1;
}

#undef hb_stackTotalItems
LONG hb_stackTotalItems( void )
{
   return hb_stack.wItems;
}

#undef hb_stackDateBuffer
char * hb_stackDateBuffer( void )
{
   return hb_stack.szDate;
}

#undef hb_stackGetStaticsBase
LONG hb_stackGetStaticsBase( void )
{
   return hb_stack.lStatics;
}

#undef hb_stackSetStaticsBase
void hb_stackSetStaticsBase( LONG lBase )
{
   hb_stack.lStatics = lBase;
}

#undef hb_stackGetRecoverBase
LONG hb_stackGetRecoverBase( void )
{
   return hb_stack.lRecoverBase;
}

#undef hb_stackSetRecoverBase
void hb_stackSetRecoverBase( LONG lBase )
{
   hb_stack.lRecoverBase = lBase;
}

#undef hb_stackGetActionRequest
USHORT hb_stackGetActionRequest( void )
{
   return hb_stack.uiActionRequest;
}

#undef hb_stackSetActionRequest
void hb_stackSetActionRequest( USHORT uiAction )
{
   hb_stack.uiActionRequest = uiAction;
}

#undef hb_stackWithObjectItem
PHB_ITEM hb_stackWithObjectItem( void )
{
   return hb_stack.lWithObject ?
                        * ( hb_stack.pItems + hb_stack.lWithObject ) : NULL;
}

#undef hb_stackWithObjectOffset
LONG hb_stackWithObjectOffset( void )
{
   return hb_stack.lWithObject;
}

#undef hb_stackWithObjectSetOffset
void hb_stackWithObjectSetOffset( LONG lOffset )
{
   hb_stack.lWithObject = lOffset;
}

#undef hb_stackItemBasePtr
PHB_ITEM ** hb_stackItemBasePtr( void )
{
   return &hb_stack.pItems;
}

void hb_stackClearMevarsBase( void )
{
   PHB_ITEM pBase;

   HB_TRACE(HB_TR_DEBUG, ("hb_stackClearMevarsBase()"));

   pBase = * hb_stack.pBase;

   while( pBase->item.asSymbol.stackstate->ulPrivateBase != 0 )
   {
      pBase->item.asSymbol.stackstate->ulPrivateBase = 0;
      pBase = * ( hb_stack.pItems + pBase->item.asSymbol.stackstate->lBaseItem );
   }
}

int hb_stackCallDepth( void )
{
   LONG lOffset = hb_stack.pBase - hb_stack.pItems;
   int iLevel = 0;

   while( lOffset > 0 )
   {
      lOffset = ( * ( hb_stack.pItems + lOffset ) )->item.asSymbol.stackstate->lBaseItem;
      ++iLevel;
   }

   return iLevel;
}

LONG hb_stackBaseProcOffset( int iLevel )
{
   LONG lOffset = hb_stack.pBase - hb_stack.pItems;

   while( iLevel-- > 0 && lOffset > 0 )
      lOffset = ( * ( hb_stack.pItems + lOffset ) )->item.asSymbol.stackstate->lBaseItem;

   if( iLevel < 0 && ( lOffset > 0 || HB_IS_SYMBOL( * hb_stack.pItems ) ) )
      return lOffset;
   else
      return -1;
}

void hb_stackBaseProcInfo( char * szProcName, USHORT * puiProcLine )
{
   /*
    * This function is called by FM module and has to be ready for execution
    * before hb_stack initialization, [druzus]
    * szProcName should be at least HB_SYMBOL_NAME_LEN + 1 bytes buffer
    */
   if( hb_stack.pPos > hb_stack.pBase )
   {
      hb_strncpy( szProcName, ( * hb_stack.pBase )->item.asSymbol.value->szName,
                  HB_SYMBOL_NAME_LEN );
      * puiProcLine = ( * hb_stack.pBase )->item.asSymbol.stackstate->uiLineNo;
   }
   else
   {
      szProcName[ 0 ] = '\0';
      * puiProcLine = 0;
   }
}

/* NOTE: DEBUG function */
void hb_stackDispLocal( void )
{
   PHB_ITEM * pBase;

   HB_TRACE(HB_TR_DEBUG, ("hb_stackDispLocal()"));

   printf( hb_conNewLine() );
   printf( HB_I_("Virtual Machine Stack Dump at %s(%i):"),
           ( *hb_stack.pBase )->item.asSymbol.value->szName,
           ( *hb_stack.pBase )->item.asSymbol.stackstate->uiLineNo );
   printf( hb_conNewLine() );
   printf( "--------------------------" );

   for( pBase = hb_stack.pBase; pBase <= hb_stack.pPos; pBase++ )
   {
      printf( hb_conNewLine() );

      switch( hb_itemType( *pBase ) )
      {
         case HB_IT_NIL:
            printf( HB_I_("NIL ") );
            break;

         case HB_IT_ARRAY:
            if( hb_arrayIsObject( *pBase ) )
               printf( HB_I_("OBJECT = %s "), hb_objGetClsName( *pBase ) );
            else
               printf( HB_I_("ARRAY ") );
            break;

         case HB_IT_BLOCK:
            printf( HB_I_("BLOCK ") );
            break;

         case HB_IT_DATE:
            {
               char szDate[ 9 ];
               printf( HB_I_("DATE = \"%s\" "), hb_itemGetDS( *pBase, szDate ) );
            }
            break;

         case HB_IT_DOUBLE:
            printf( HB_I_("DOUBLE = %f "), hb_itemGetND( *pBase ) );
            break;

         case HB_IT_LOGICAL:
            printf( HB_I_("LOGICAL = %s "), hb_itemGetL( *pBase ) ? ".T." : ".F." );
            break;

         case HB_IT_LONG:
            printf( HB_I_("LONG = %" PFHL "i ") , hb_itemGetNInt( *pBase ) );
            break;

         case HB_IT_INTEGER:
            printf( HB_I_("INTEGER = %i "), hb_itemGetNI( *pBase ) );
            break;

         case HB_IT_STRING:
            printf( HB_I_("STRING = \"%s\" "), hb_itemGetCPtr( *pBase ) );
            break;

         case HB_IT_SYMBOL:
            printf( HB_I_("SYMBOL = %s "), ( *pBase )->item.asSymbol.value->szName );
            break;

         case HB_IT_POINTER:
            printf( HB_I_("POINTER = %p "), ( *pBase )->item.asPointer.value );
            break;

         default:
            printf( HB_I_("UNKNOWN = TYPE %i "), hb_itemType( *pBase ) );
            break;
      }
   }
}

void hb_stackDispCall( void )
{
   PHB_ITEM * pBase = hb_stack.pBase;

   HB_TRACE(HB_TR_DEBUG, ("hb_stackDispCall()"));

   while( pBase > hb_stack.pItems )
   {
      char buffer[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 32 ];

      if( HB_IS_OBJECT( *( pBase + 1 ) ) )
         snprintf( buffer, sizeof( buffer ), HB_I_("Called from %s:%s(%i)"), hb_objGetClsName( *(pBase + 1) ),
                   ( *pBase )->item.asSymbol.value->szName,
                   ( *pBase )->item.asSymbol.stackstate->uiLineNo );
      else
         snprintf( buffer, sizeof( buffer ), HB_I_("Called from %s(%i)"),
                   ( *pBase )->item.asSymbol.value->szName,
                   ( *pBase )->item.asSymbol.stackstate->uiLineNo );

      hb_conOutErr( buffer, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );

      pBase = hb_stack.pItems + ( *pBase )->item.asSymbol.stackstate->lBaseItem;
   }
}

/* ------------------------------------------------------------------------ */
/* The garbage collector interface */
/* ------------------------------------------------------------------------ */

/* Mark all locals as used so they will not be released by the
 * garbage collector
 */
void hb_vmIsLocalRef( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmIsLocalRef()"));

   if( hb_stack.pPos > hb_stack.pItems )
   {
      /* the eval stack is not cleared yet */
      HB_ITEM_PTR * pItem = hb_stack.pPos - 1;

      do
      {
         if( HB_IS_GCITEM( *pItem ) )
            hb_gcItemRef( *pItem );
      }
      while( --pItem > hb_stack.pItems );
   }
}

#ifdef HB_INCLUDE_WINEXCHANDLER

#if defined(HB_OS_WIN_32)

LONG WINAPI hb_UnhandledExceptionFilter( struct _EXCEPTION_POINTERS * ExceptionInfo )
{
   PHB_ITEM *pBase = hb_stack.pBase;

   char msg[ ( HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 32 ) * 32 ];

   HB_SYMBOL_UNUSED( ExceptionInfo );

   msg[ 0 ] = '\0';

   while( pBase > hb_stack.pItems )
   {
      char buffer[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 32 ];

      if( HB_IS_OBJECT( *( pBase + 1 ) ) )
         snprintf( buffer, sizeof( buffer ), HB_I_("Called from %s:%s(%i)\n"), hb_objGetClsName( *(pBase + 1) ),
                   ( *pBase )->item.asSymbol.value->szName,
                   ( *pBase )->item.asSymbol.stackstate->uiLineNo );
      else
         snprintf( buffer, sizeof( buffer ), HB_I_("Called from %s(%i)\n"),
                   ( *pBase )->item.asSymbol.value->szName,
                   ( *pBase )->item.asSymbol.stackstate->uiLineNo );

      hb_strncat( msg, buffer, sizeof( msg ) - 1 );

      pBase = hb_stack.pItems + ( *pBase )->item.asSymbol.stackstate->lBaseItem;
   }

   MessageBox( NULL, msg, HB_I_("Harbour Exception"), MB_ICONSTOP );

   return EXCEPTION_CONTINUE_SEARCH; /* EXCEPTION_EXECUTE_HANDLER; */
}

#endif

#endif

#if defined(HB_OS_OS2)

ULONG _System OS2TermHandler( PEXCEPTIONREPORTRECORD       p1,
                              PEXCEPTIONREGISTRATIONRECORD p2,
                              PCONTEXTRECORD               p3,
                              PVOID                        pv )
{
   HB_SYMBOL_UNUSED(p1);
   HB_SYMBOL_UNUSED(p2);
   HB_SYMBOL_UNUSED(p3);
   HB_SYMBOL_UNUSED(pv);

   /* Don't print stack trace if inside unwind, normal process termination or process killed or
      during debugging */
   if( p1->ExceptionNum != XCPT_UNWIND && p1->ExceptionNum < XCPT_BREAKPOINT )
   {
      PHB_ITEM *pBase = hb_stack.pBase;

      fprintf(stderr, HB_I_("\nException %lx at address %p \n"), p1->ExceptionNum, p1->ExceptionAddress);

      while( pBase > hb_stack.pItems )
      {
         if( HB_IS_OBJECT( *( pBase + 1 ) ) )
            fprintf( stderr, HB_I_("Called from %s:%s(%i)\n"), hb_objGetClsName( *(pBase + 1) ),
                     ( *pBase )->item.asSymbol.value->szName,
                     ( *pBase )->item.asSymbol.stackstate->uiLineNo );
         else
            fprintf( stderr, HB_I_("Called from %s(%i)\n"),
                     ( *pBase )->item.asSymbol.value->szName,
                     ( *pBase )->item.asSymbol.stackstate->uiLineNo );

         pBase = hb_stack.pItems + ( *pBase )->item.asSymbol.stackstate->lBaseItem;
      }
   }

   return XCPT_CONTINUE_SEARCH;          /* Exception not resolved... */
}
#endif
