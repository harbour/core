/*
 * $Id$
*/

/*
 * Harbour Project source code:
 * Flex/Bison generated source code splitter for 16-bit compilers
 *
 * Copyright 1999 Ron Pinkas
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

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "extend.h"
#include "compiler.h"
#include "hberrors.h"

extern int _iWarnings;
extern char *_szCWarnings[];

typedef struct
{
   char cType;
}
STACK_VAL_TYPE, * PSTACK_VAL_TYPE;


static PSTACK_VAL_TYPE pStackValType = NULL; /* compile time stack values linked list */
static long lStackTop = 0;
static long lStackLen;

#define debug_msg2( x, y )
#define debug_msg( x )


void ValTypePush( char cType )
{
   if( _iWarnings )
   {
      if( pStackValType == NULL )
      {
         lStackTop =1;     /* start from the one */
         lStackLen =64;
         pStackValType =(PSTACK_VAL_TYPE)hb_xgrab( sizeof( STACK_VAL_TYPE ) * lStackLen );
      }
      else if( lStackTop == lStackLen )
      {
         lStackLen +=64;
         pStackValType = ( PSTACK_VAL_TYPE ) hb_xrealloc( pStackValType, sizeof( STACK_VAL_TYPE ) * lStackLen );
      }

      pStackValType[ lStackTop++ ].cType = cType;

      debug_msg2( "\nValTypePush( %c )", cType );
   }
}

void ValTypePop( int iCount )
{
   if( _iWarnings )
   {
      debug_msg2( "\nValTypePop( %i )", iCount );
      if( lStackTop )
      {
         while( lStackTop && iCount-- )
            --lStackTop;
      }
      else
         debug_msg( "\nValTypePop() Compile time stack underflow\n");
   }
}

void ValTypePlus( void )
{
   if( _iWarnings )
   {
      debug_msg( "\nValTypePlus()" );

      if( lStackTop > 2 )  /* at least two expressions are required */
      {
         PSTACK_VAL_TYPE pOperand1 = NULL, pOperand2;
         char sType1[ 2 ], sType2[ 2 ], cType = ' ';

         --lStackTop;
         pOperand2 = pStackValType + lStackTop;
         sType2[ 0 ] = pOperand2->cType;
         sType2[ 1 ] = '\0';

         /* skip back to the 1st. operand */
         pOperand1 = pStackValType + lStackTop - 1;
         sType1[ 0 ] = pOperand1->cType;
         sType1[ 1 ] = '\0';

         /* TODO: Adding numerical to date
         *
         */
         if( pOperand1->cType != ' ' && pOperand2->cType != ' ' && pOperand1->cType != pOperand2->cType )
            GenWarning( _szCWarnings, 'W', WARN_OPERANDS_INCOMPATBLE, sType1, sType2 );
         else if( pOperand2->cType != ' ' && pOperand1->cType == ' ' )
            GenWarning( _szCWarnings, 'W', WARN_OPERAND_SUSPECT, sType2, NULL );
         else if( pOperand1->cType != ' ' && pOperand2->cType == ' ' )
            GenWarning( _szCWarnings, 'W', WARN_OPERAND_SUSPECT, sType1, NULL );
         else
            cType = pOperand1->cType;

         /* compile time 1st. operand has to be released *but* result will be pushed and type as calculated */
         pOperand1->cType = cType;
      }
      else
         debug_msg( " Compile time stack underflow" );
   }
}

void ValTypeRelational( void )
{
   if( _iWarnings )
   {
      debug_msg( "\nValTypeRelational()" );

      if( lStackTop > 2 )  /* at least two expressions are required */
      {
         PSTACK_VAL_TYPE pOperand1 = NULL, pOperand2;
         char sType1[ 2 ], sType2[ 2 ];

         /* 2nd. Operand (stack top)*/
         --lStackTop;
         pOperand2 = pStackValType + lStackTop;
         sType2[ 0 ] = pOperand2->cType;
         sType2[ 1 ] = '\0';

         /* skip back to the 1st. operand */
         pOperand1 = pStackValType + lStackTop - 1;
         sType1[ 0 ] = pOperand1->cType;
         sType1[ 1 ] = '\0';

         if( pOperand1->cType != ' ' && pOperand2->cType != ' ' && pOperand1->cType != pOperand2->cType )
            GenWarning( _szCWarnings, 'W', WARN_OPERANDS_INCOMPATBLE, sType1, sType2 );
         else if( pOperand2->cType != ' ' && pOperand1->cType == ' ' )
            GenWarning( _szCWarnings, 'W', WARN_OPERAND_SUSPECT, sType2, NULL );
         else if( pOperand1->cType != ' ' && pOperand2->cType == ' ' )
            GenWarning( _szCWarnings, 'W', WARN_OPERAND_SUSPECT, sType1, NULL );

         /* compile time 1st. operand has to be released *but* result will be pushed and of type logical */
         pOperand1->cType = 'L';
      }
      else
         debug_msg( " Compile time stack underflow" );
   }
}

void ValTypeCheck( char cExpected, int iExpWarning, int iSuspWarning )
{
   if( _iWarnings )
   {
      if( lStackTop )
      {
         if(  pStackValType[ lStackTop - 1 ].cType == ' ' )
            GenWarning( _szCWarnings, 'W', iSuspWarning, NULL, NULL );
         else if( pStackValType[ lStackTop -1  ].cType != cExpected )
         {
            char sType[ 2 ];

            sType[ 0 ] = pStackValType[ lStackTop -1 ].cType;
            sType[ 1 ] = '\0';

            GenWarning( _szCWarnings, 'W', iExpWarning, sType, NULL );
         }
      }
      else
         debug_msg2( "\nValTypeCheck( %c ) Compile time stack underflow", cExpected );
   }
}

void ValTypeCheck2( char cExpected, int iExpWarning, int iSuspWarning )
{
   if( _iWarnings )
   {
      debug_msg2( "\nValTypeCheck2( %c )", cExpected );

      if( lStackTop > 2 )  /* at least two expressions are required */
      {
         PSTACK_VAL_TYPE pOperand1 = NULL, pOperand2;
         char sType1[ 2 ], sType2[ 2 ];

         /* 2nd. Operand (stack top)*/
         pOperand2 = pStackValType + lStackTop - 1;
         sType2[ 0 ] = pOperand2->cType;
         sType2[ 1 ] = '\0';

         /* skip back to the 1st. operand */
         pOperand1 = pStackValType + lStackTop - 2;
         sType1[ 0 ] = pOperand1->cType;
         sType1[ 1 ] = '\0';

         if( pOperand1->cType != cExpected && pOperand1->cType != ' ' )
            GenWarning( _szCWarnings, 'W', iExpWarning, sType1, NULL );
         else if( pOperand1->cType == ' ' )
            GenWarning( _szCWarnings, 'W', iSuspWarning, NULL, NULL );

         if( pOperand2->cType != cExpected && pOperand2->cType != ' ' )
            GenWarning( _szCWarnings, 'W', iExpWarning, sType2, NULL );
         else if( pOperand2->cType == ' ' )
            GenWarning( _szCWarnings, 'W', iSuspWarning, NULL, NULL );
      }
      else
         debug_msg( " Compile time stack underflow" );
   }
}


char ValTypeGet( )
{
   return  (lStackTop > 1) ? pStackValType[ lStackTop - 1 ].cType : 0;
}

void ValTypePut( char cType )
{
   if( _iWarnings )
   {
      if( lStackTop > 1 )
         /* reusing the place holder of the result value */
         pStackValType[ lStackTop - 1 ].cType = cType;
      else
         debug_msg( "\nValTypePut() Compile time stack underflow\n" );
   }
}

void ValTypeAssign( char *szVarName )
{
   if( _iWarnings )
   {
      if( lStackTop > 2 )  /* at least two expressions are required */
      {
         PSTACK_VAL_TYPE pLeft= NULL, pRight;
         char sType1[ 2 ], sType2[ 2 ];

         /* 2nd. Operand (stack top)*/
         --lStackTop;
         pRight = pStackValType + lStackTop;
         sType2[ 0 ] = pRight->cType;
         sType2[ 1 ] = '\0';

         /* skip back to the 1st. operand */
         --lStackTop;
         pLeft = pStackValType + lStackTop;
         sType1[ 0 ] = pLeft->cType;
         sType1[ 1 ] = '\0';

         if( pRight->cType != ' ' && pLeft->cType == ' ' )
            GenWarning( _szCWarnings, 'W', WARN_ASSIGN_SUSPECT, szVarName, sType2 );
         else if( pRight->cType != ' ' && pRight->cType != pLeft->cType )
            GenWarning( _szCWarnings, 'W', WARN_ASSIGN_TYPE, szVarName, sType1 );
      }
      else
         debug_msg( "\nValTypeAssign() Compile time stack underflow\n" );
   }
}

void ValTypeReset( void )
{
   /* Clear the compile time stack values (should be empty at this point) */
   if( lStackTop > 1 )
      debug_msg2( "\n* *Compile time stack overflow: %i\n", lStackTop );
   lStackTop =1;  /* first position to store a value */
}
