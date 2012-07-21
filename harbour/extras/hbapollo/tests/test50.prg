/*
 * $Id$
 */
/*
   Index thingies ...
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL nArea, i
   LOCAL cFile   := "test\test.dbf"

   SX_RDDSETDEFAULT( "SDEFOX" )
   SET CENTURY ON
   SET DATE ANSI
   SX_COMMITLEVEL( 2 )

   IF file( "test\mytest.cdx" )
      ferase( "test\mytest.cdx" )
   ENDIF

   USE cFile ALIAS MYFILE EXCLUSIVE

   sx_IndexTag ( "mytest", "first", "first", 0, .F. , , )
   sx_IndexTag ( "mytest", "last", "last", 0, .F. , , )
   sx_IndexTag ( "mytest", "complete", "first+last", 0, .F. , , )

   ? 'sx_IndexOrd()        =', __trim( sx_IndexOrd() )
   ? 'sx_IndexType()       =', __trim( sx_IndexType() )
   ? 'sx_TagName()         =', sx_TagName()
   ? 'sx_TagName(1)        =', sx_TagName( 1 )
   ? 'sx_TagName(2)        =', sx_TagName( 2 )
   ? 'sx_TagArea("first")  =', __trim( sx_TagArea( "first" ) )
   ? 'sx_TagArea("last")   =', __trim( sx_TagArea( "last" ) )
   ? 'sx_TagArea("dummy")  =', __trim( sx_TagArea( "dummy" ) )
   ? 'sx_IndexKey()        =', sx_IndexKey()
   ? 'sx_IndexKeyField ()  =', sx_IndexKeyField ()
   ? 'sx_IndexName     ()  =', sx_IndexName     ()
   ? 'sx_IndexName    (1)  =', sx_IndexName     ( 1 )
   ? 'sx_OrderPosSet  (1)  =', sx_OrderPosSet   ( 1 )
   ? 'sx_OrderPosGet   ()  =', __trim( sx_OrderPosGet() )
   ? 'sx_OrderRecNo    ()  =', __trim( sx_OrderRecNo () )
   ? 'sx_IndexCondition()  =', sx_IndexCondition   ()
   ?
   ? 'sx_SetOrder     (2)  =', __trim( sx_SetOrder( 2 ) )
   ? 'sx_IndexOrd()        =', __trim( sx_IndexOrd() )
   ? 'sx_OrdNumber()       =', __trim( sx_OrdNumber() )
   ?
   ? 'sx_SetOrder     (1)  =', __trim( sx_SetOrder( 1 ) )
   ? 'sx_IndexOrd()        =', __trim( sx_IndexOrd() )
   ? 'sx_OrdNumber()       =', __trim( sx_OrdNumber() )
   ?
   ? 'sx_SetOrder("last")  =', __trim( sx_SetOrder( "last" ) )
   ? 'sx_IndexOrd()        =', __trim( sx_IndexOrd() )
   ? 'sx_OrdNumber()       =', __trim( sx_OrdNumber() )
   ?
   ? 'sx_SetOrder("first") =', __trim( sx_SetOrder( "first" ) )
   ? 'sx_IndexOrd()        =', __trim( sx_IndexOrd() )
   ? 'sx_OrdNumber()       =', __trim( sx_OrdNumber() )
   ?
   ? 'sx_TagCount      ()  =', __trim( sx_TagCount() )
   ?
   ? 'sx_OrdSetFocus  (2)  =', __trim( sx_OrdSetFocus( 2 ) )
   ? 'sx_IndexOrd()        =', __trim( sx_IndexOrd() )
   ? 'sx_OrdNumber()       =', __trim( sx_OrdNumber() )
   ? 'sx_IndexKey()        =', sx_IndexKey()
   ? 'sx_OrdKey()          =', sx_OrdKey()
   ? 'sx_IndexKeyField ()  =', sx_IndexKeyField ()
   ?
   ? 'sx_OrdSetFocus  (1)  =', __trim( sx_OrdSetFocus( 1 ) )
   ? 'sx_IndexOrd()        =', __trim( sx_IndexOrd() )
   ? 'sx_OrdNumber()       =', __trim( sx_OrdNumber() )
   ? 'sx_IndexKey()        =', sx_IndexKey()
   ? 'sx_OrdKey()          =', sx_OrdKey()
   ? 'sx_IndexKeyField ()  =', sx_IndexKeyField ()
   ?
   ? 'sx_OrdSetFocus("last")     =', __trim( sx_OrdSetFocus( "last" ) )
   ? 'sx_IndexOrd()              =', __trim( sx_IndexOrd() )
   ? 'sx_OrdNumber()             =', __trim( sx_OrdNumber() )
   ? 'sx_IndexKey()              =', sx_IndexKey()
   ? 'sx_OrdKey()                =', sx_OrdKey()
   ? 'sx_IndexKeyField ()        =', sx_IndexKeyField ()
   ?
   ? 'sx_OrdSetFocus("first")    =', __trim( sx_OrdSetFocus( "first" ) )
   ? 'sx_IndexOrd()              =', __trim( sx_IndexOrd() )
   ? 'sx_OrdNumber()             =', __trim( sx_OrdNumber() )
   ? 'sx_IndexKey()              =', sx_IndexKey()
   ? 'sx_OrdKey()                =', sx_OrdKey()
   ? 'sx_IndexKeyField ()        =', sx_IndexKeyField ()
   ?
   ? 'sx_OrdSetFocus("complete") =', __trim( sx_OrdSetFocus( "complete" ) )
   ? 'sx_IndexOrd()              =', __trim( sx_IndexOrd() )
   ? 'sx_OrdNumber()             =', __trim( sx_OrdNumber() )
   ? 'sx_IndexKey()              =', sx_IndexKey()
   ? 'sx_OrdKey()                =', sx_OrdKey()
   ? 'sx_IndexKeyField ()        =', sx_IndexKeyField ()

   ?
   ? 'sx_SetAutoOpen()    =', sx_SetAutoOpen()
   ? 'sx_SetAutoOpen(.F.) =', sx_SetAutoOpen( .F. )
   ? 'sx_SetAutoOpen()    =', sx_SetAutoOpen()
   ?

   ? 'sx_SetAutoOpen(.T.) =', sx_SetAutoOpen( .T. )
   ? 'sx_SetAutoOpen()    =', sx_SetAutoOpen()

   ?
   ? 'sx_TagDelete("complete")   =', sx_TagDelete( "complete" )
   ? 'sx_TagCount()              =', __trim( sx_TagCount() )

   ? 'sx_TagDelete("first")      =', sx_TagDelete( "first" )
   ? 'sx_TagCount()              =', __trim( sx_TagCount() )

   ? 'sx_TagDelete("last")       =', sx_TagDelete( "last" )
   ? 'sx_TagCount()              =', __trim( sx_TagCount() )

FUNCTION __trim( n )

   RETURN ltrim( str( n ) )
