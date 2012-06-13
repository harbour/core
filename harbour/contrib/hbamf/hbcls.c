/*******
 *
 *  hbref.c by Aleksander Czajczynski <hb/at/fki.pl> 2011
 **
 *  hbref.c - Some class support functions for AMF3 (de)serialization
 *
 ********/

#include "hbapi.h"
#include "hbapicls.h"
#include "hbapiitm.h"
#include "hbapistr.h"
#include "hbstack.h"
#include "hbvm.h"
#include "hboo.ch"

HB_BOOL is_cls_externalizable( HB_USHORT uiClass )
{
   PHB_DYNS pSymbol  = hb_dynsymGet( "__CLSMSGTYPE" );
   HB_BOOL  result   = HB_FALSE;

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

PHB_ITEM cls_externalizable_instance( PHB_ITEM pClassFuncStr )
{
   PHB_DYNS pSymbol = hb_dynsymGet( hb_itemGetCPtr( pClassFuncStr ) );

   if( pSymbol )
   {
      PHB_ITEM pRetCopy = hb_itemNew( NULL );
      PHB_ITEM pNewItem = hb_itemNew( NULL );
      hb_itemMove( pRetCopy, hb_stackReturnItem() );

      hb_vmPushDynSym( pSymbol );
      hb_vmPushNil();
      hb_vmDo( 0 );

      hb_objSendMsg( hb_stackReturnItem(), "NEW", 0 );

      hb_itemMove( pNewItem, hb_stackReturnItem() );
      hb_itemMove( hb_stackReturnItem(), pRetCopy );

      hb_itemRelease( pRetCopy );

      if( pNewItem )
      {
         if( ! HB_IS_OBJECT( pNewItem ) )
         {
            hb_itemRelease( pNewItem );
            pNewItem = NULL;
         }
      }

      return pNewItem;
   }

   return NULL;
}
