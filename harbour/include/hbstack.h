/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The eval stack 
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

/* TOFIX: There are several things in this file which are not part of the
          standard Harbour API, in other words these things are not
          guaranteed to remain unchanged. To avoid confusion these should be
          moved to somewhere else (like HBRTL.H). [vszakats] */

#ifndef HB_STACK_H_
#define HB_STACK_H_

#include "hbvmpub.h"

#if defined(HB_EXTERN_C)
extern "C" {
#endif

/* stack managed by the virtual machine */
typedef struct
{
   PHB_ITEM * pItems;       /* pointer to the stack items */
   PHB_ITEM * pPos;         /* pointer to the latest used item */
   LONG     wItems;       /* total items that may be holded on the stack */
   HB_ITEM  Return;       /* latest returned value */
   PHB_ITEM * pBase;        /* stack frame position for the current function call */
   PHB_ITEM * pEvalBase;    /* stack frame position for the evaluated codeblock */
   int      iStatics;     /* statics base for the current function call */
   char     szDate[ 9 ];  /* last returned date from _pards() yyyymmdd format */
} HB_STACK;

extern HB_STACK hb_stack;

typedef struct
{
   LONG lBaseItem;
   LONG iStatics;
} HB_STACK_STATE;    /* used to save/restore stack state in hb_vmDo)_ */

#if defined(HB_STACK_MACROS)

#define hb_stackItemFromTop( n )    ( * ( hb_stack->pPos + (n) ) )
#define hb_stackItemFromBase( n )   ( * ( hb_stack->pBase + (n) + 1 ) )
#define hb_stackTopOffset( )        ( hb_stack.pPos - hb_stack.pItems )
#define hb_stackBaseOffset( )       ( hb_stack.pBase - hb_stack.pItems + )
#define hb_stackTopItem( )          ( * hb_stack.pPos )
#define hb_stackBaseItem( )         ( * hb_stack.pBase )
#define hb_stackSelfItem( )         ( * ( hb_stack.pBase + 1 ) )
#define hb_stackItem( iItemPos )    ( * ( hb_stack.pItems + iItemPos ) )

#else

extern HB_ITEM_PTR hb_stackItemFromTop( int nFromTop );
extern HB_ITEM_PTR hb_stackItemFromBase( int nFromBase );
extern LONG hb_stackTopOffset( void );
extern LONG hb_stackBaseOffset( void );
extern HB_ITEM_PTR hb_stackTopItem( void );
extern HB_ITEM_PTR hb_stackBaseItem( void );
extern HB_ITEM_PTR hb_stackSelfItem( void );
extern HB_ITEM_PTR hb_stackItem( LONG iItemPos );

#endif

extern void hb_stackRemove( LONG lUntilPos );
extern HB_ITEM_PTR hb_stackNewFrame( HB_STACK_STATE * pStack, USHORT uiParams );
extern void hb_stackOldFrame( HB_STACK_STATE * pStack );

/* stack management functions */
extern void    hb_stackDispLocal( void );  /* show the types of the items on the stack for debugging purposes */
extern void    hb_stackDispCall( void );
extern void    hb_stackDec( void );        /* pops an item from the stack without clearing it's contents */
extern void    hb_stackFree( void );       /* releases all memory used by the stack */
extern void    hb_stackPush( void );       /* pushes an item on to the stack */
extern void    hb_stackPop( void );        /* pops an item from the stack */
extern void    hb_stackInit( void );       /* initializes the stack */

#if defined(HB_EXTERN_C)
}
#endif

#endif /* HB_STACK_H_ */
