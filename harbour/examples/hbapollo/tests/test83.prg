/*
 * $Id$
 */
/*
   sx_Query(), sx_QueryTest(), sx_QueryRecCount()
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   USE "TEST/TEST"
   ? 'Creating index ...'
   FERASE( "TEST.NSX" )
   INDEX ON STATE TO TEST
   ? 'Making Query ...'
   ? [sx_QueryTest( "STATE LIKE '%L%'" ) =], sx_QueryTest( "STATE LIKE '%L'" )
   
   ? [sx_QueryTest( "STATE='LA'" ) =], sx_QueryTest( "STATE='LA'" )
   ? [sx_Query( "STATE='LA'" ) =], sx_Query( "STATE='LA'" )
   ? [sx_QueryRecCount() =], sx_QueryRecCount()
   ? 'Now Browse .. Press any key ...'
   PAUSE
   BROWSE
   CLS
   ?
   ? 'Now clear the Query and BROWSE ... Press any key ...'
   PAUSE
   sx_QueryClear()
   BROWSE
