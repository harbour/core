/*
 * $Id$
 */
/*
   sx_SetScope()/sx_GetScope()/sx_ClearScope()/sx_QueryRecCount()
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   USE "test/test"
   FERASE( "TEST.NSX" )
   INDEX ON LAST TO TEST
   ? 'sx_QueryRecCount() =>', sx_QueryRecCount()
   ? 'BROWSE without scoping ... Press any key ...'
   PAUSE
   CLS
   BROWSE
   CLS
   ? 'sx_SetScope( "Jones", "Smith" ) =>', sx_SetScope( "Jones", "Smith" )
   ? 'sx_GetScope()                   =>', sx_GetScope()
   ? 'sx_GetScope(1)                  =>', sx_GetScope( 1 )
   ? 'sx_QueryRecCount()              =>', sx_QueryRecCount()
   ? 'BROWSE with scoping ... Press any key ...'
   PAUSE
   CLS
   BROWSE
   CLS
   ? 'sx_ClearScope()    =>', sx_ClearScope()
   ? 'sx_GetScope()      =>', sx_GetScope()
   ? 'sx_GetScope(1)     =>', sx_GetScope( 1 )
   ? 'sx_QueryRecCount() =>', sx_QueryRecCount()
   ? 'Scope has been cleared ... Press any key ...'
   PAUSE
   CLS
   BROWSE
