/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour implementation of Class(y) Class Symbol
 *
 * Copyright 2003 Antonio Linares <alinares@fivetechsoft.com>
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

/* uncomment this section for a working sample

// Class(y) Class Symbol documentation is located at:
// http://www.clipx.net/ng/classy/ngdebc.php

function Main()

   local oSym := Symbol():New( "QOUT" )

   ? "Now test the :Exec() method"

   oSym:Exec( "This string is being printed by QOUT" )
   oSym:Exec( "which is being invoked by the :Exec()" )
   oSym:Exec( "method in the Symbol class." )

   ?
   ? "symbol name: ", oSym:name

   ? "Comparing QOut symbol with xOut symbol"
   ? oSym:IsEqual( Symbol():New( "xOut" ) )

   ? "done!"
   ?

return nil */

#include "hbclass.ch"

CLASS Symbol

   DATA   nSym  HIDDEN       // internal pointer to the Symbols table symbol

   METHOD New( cSymName )    // Constructor. cSymName may already exists or not

   METHOD IsEqual( oSymbol ) // Compares two symbol objects

   METHOD exec() // Executes the function referred to by the Symbol object,
                 // with an optional parameters list

   METHOD _name( cName ) VIRTUAL // name simulates a read-only DATA so it
                                 // can't be assigned
   METHOD name() INLINE GetSymbolName( ::nSym ) // retrieves the symbol name

ENDCLASS

METHOD New( cSymName ) CLASS Symbol

   ::nSym = GetSymbolPointer( cSymName )

return Self

METHOD IsEqual( oSymbol ) CLASS Symbol

   if ValType( oSymbol ) == "O"
      return ::nSym == oSymbol:nSym
   endif

return .f.

#pragma BEGINDUMP

#include <hbapi.h>
#include <hbvm.h>

HB_FUNC( GETSYMBOLPOINTER )
{
   hb_retnl( ( long ) hb_dynsymGet( hb_parc( 1 ) ) );
}

HB_FUNC( GETSYMBOLNAME )
{
   PHB_DYNS pDynSym = ( PHB_DYNS ) hb_parnl( 1 );

   hb_retc( ( pDynSym != NULL ? pDynSym->pSymbol->szName : "" ) );
}

HB_FUNC( SYMBOL_EXEC )
{
   PHB_ITEM pSelf = hb_param( 0, HB_IT_OBJECT ); /* we retrieve Self */
   PHB_DYNS pSym;

   static PHB_DYNS pDynSym = NULL; /* We use a static value to look for the
                                      symbol nSym just once */
   if( pDynSym == NULL )
      pDynSym = hb_dynsymFindName( "nSym" );

   /* Lets retrieve DATA nSym from Self,
      first we place the nSym symbol at the HVM stack */
   hb_vmPushSymbol( pDynSym->pSymbol );

   /* we place the object at the HVM stack */
   hb_vmPush( pSelf );

   /* we invoke the message with no parameters */
   hb_vmFunction( 0 );

   pSym = ( PHB_DYNS ) hb_parnl( -1 );  /* we take the returned DATA value from
                                           the HVM hb_stack.Return */
   if( pSym != NULL && pSym->pSymbol->pFunPtr )
   {
      int i;

      hb_vmPushSymbol( pSym->pSymbol );
      hb_vmPushNil(); /* No Self. We are going to execute a function or a procedure */

      for( i = 1; i <= hb_pcount(); i++ ) /* number of supplied params */
         /* pushes a generic item (supplied parameter) to the HVM */
         hb_vmPush( hb_param( i, HB_IT_ANY ) );

      hb_vmFunction( hb_pcount() ); /* executes the symbol */
   }
   else
      hb_ret();  /* clean the HVM hb_stack.Return */
}

#pragma ENDDUMP
