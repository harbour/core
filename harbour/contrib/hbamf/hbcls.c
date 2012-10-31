/*
 * $Id$
 */

/*******
 *
 *  by Aleksander Czajczynski <hb/at/fki.pl> 2011
 **
 *  Some class support functions for AMF3 (de)serialization
 *
 ********/

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbstack.h"
#include "hbvm.h"
#include "hboo.ch"

HB_BOOL hbamf_is_cls_externalizable( HB_USHORT uiClass )
{
   PHB_DYNS pSymbol = hb_dynsymGet( "__CLSMSGTYPE" );
   HB_BOOL  result  = HB_FALSE;

   /* as far as i know, there is no exported Harbour C level api for this */

   if( uiClass && pSymbol )
   {
      PHB_ITEM pRetCopy = hb_itemNew( NULL );

      hb_itemMove( pRetCopy, hb_stackReturnItem() );

      hb_vmPushDynSym( pSymbol );
      hb_vmPushNil();
      hb_vmPushInteger( uiClass );
      hb_vmPushString( "EXTERNALIZABLE", 14 );
      hb_vmDo( 2 );

      if( hb_itemGetNI( hb_stackReturnItem() ) == HB_OO_MSG_CLASSDATA )
         result = HB_TRUE;

      hb_itemMove( hb_stackReturnItem(), pRetCopy );
      hb_itemRelease( pRetCopy );
   }

   return result;
}
