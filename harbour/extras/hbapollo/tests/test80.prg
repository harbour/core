/*
 * $Id$
 */
/*
   sx_SetRelation()/sx_ClearRelation()
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL struct1 := { { "PART_NO","C",5,0 }, { "BOUGHT","N",10,2 } }
   LOCAL struct2 := { { "PART_NO","C",5,0 }, { "SOLD","N",10,2 } }
   LOCAL i, nParent, nChild

   ? 'Creating Files ...'
   sx_DBCreate( "PARENT", struct1 )
   ? 'PARENT Created ...'
   USE "PARENT" ALIAS PARENT VAR nParent EXCLUSIVE
   FOR i := 1 TO 100
      sx_Append()
      sx_Replace( "PART_NO", PADL( i,5,"0" ) )
      sx_Replace( "BOUGHT" , i * 2 )
   NEXT
   ? 'Indexing ...'
   INDEX ON PART_NO TO PARENT
   ? 'Open Index ...'
   sx_IndexOpen( "PARENT.NSX" )

   sx_DBCreate( "CHILD" , struct2 )
   ? 'CHILD Created .....'
   USE "CHILD" ALIAS CHILD VAR nChild EXCLUSIVE
   FOR i := 1 TO 100
      sx_Append()
      sx_Replace( "PART_NO", PADL( i,5,"0" ) )
      sx_Replace( "SOLD" , i * 2.5 )
   NEXT
   INDEX ON PART_NO TO CHILD
   sx_IndexOpen( "CHILD.NSX" )

   sx_Select( "PARENT" )
   sx_SetRelation( "CHILD", "PART_NO" )
   sx_Gotop( "PARENT" )

   ? 'Parent is related to Child with PART_NO as key ... Press any key ...'
   PAUSE
   WHILE !sx_Eof( "PARENT" )
      ? sx_GetValue( "PART_NO", "PARENT" ), sx_GetValue( "BOUGHT" , "PARENT" ), sx_GetValue( "SOLD", "CHILD" )
      sx_Skip( 1, "PARENT" )
   ENDDO

   ?
   ? 'Now relation is to be cleared ... Press any key ... '
   sx_ClearRelation ( "PARENT" )
   sx_GoTop( "PARENT" )
   PAUSE
   WHILE !sx_Eof( "PARENT" )
      ? sx_GetValue( "PART_NO", "PARENT" ), sx_GetValue( "BOUGHT" , "PARENT" ), sx_GetValue( "SOLD", "CHILD" )
      sx_Skip( 1, "PARENT" )
   ENDDO
