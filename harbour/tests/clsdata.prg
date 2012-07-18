/*
 * $Id$
 */

PROCEDURE Main()

   LOCAL o := HBObject():New()

   QOut( "o:Data1 => ", o:Data1 )
   QOut( "o:ClassData1 => ", o:ClassData1 )
   QOut( "o:Data2 => ", o:Data2 )
   QOut( "o:ClassData2 => ", o:ClassData2 )
   o:Test()

   RETURN

FUNCTION TBaseObject()

   STATIC oClass

   IF oClass == NIL
      oClass := HBClass():New( "TBaseObject" )
      oClass:AddData( "Data1" )
      oClass:AddClassData( "ClassData1" )
      oClass:AddMethod( "NewBase", @NewBase() )
      oClass:AddMethod( "Test", @Test() )
      oClass:AddMethod( "Method1", @Method1Base() )
      oClass:AddMethod( "Method2", @Method2Base() )
      oClass:Create()
   ENDIF

   RETURN oClass:Instance()

STATIC FUNCTION NewBase()

   LOCAL self := QSelf()

   ::Data1 := 1
   ::ClassData1 := "A"

   RETURN self

STATIC FUNCTION Test()

   LOCAL self := QSelf()

   QOut( "Inside ::Test() " )
   QOut( "calling ::Method1() " )
   ::Method1()

   RETURN self

STATIC FUNCTION Method1Base()

   LOCAL self := QSelf()

   QOut( "I am Method1 from TBaseObject" )
   ::Method2()

   RETURN self

STATIC FUNCTION Method2Base()

   LOCAL self := QSelf()

   QOut( "I am Method2 from TBaseObject" )

   RETURN self

FUNCTION HBObject()

   STATIC oClass

   IF oClass == NIL
      oClass := HBClass():New( "HBObject", "TBaseObject" )
      oClass:AddData( "Data2" )
      oClass:AddClassData( "ClassData2" )
      oClass:AddMethod( "New", @New() )
      oClass:AddMethod( "Method1", @Method1() )
      oClass:AddMethod( "Method2", @Method2() )
      oClass:Create()
   ENDIF

   RETURN oClass:Instance()

STATIC FUNCTION New()

   LOCAL self := QSelf()

   ::TBaseObject:NewBase()
   ::Data1 := 1
   ::ClassData1 := "A"
   ::Data2 := 2
   // ClassData2 override ClassData1
   ::ClassData2 := "B"

   RETURN self

STATIC FUNCTION Method1()

   LOCAL self := QSelf()

   QOut( "I am Method1 from HBObject" )
   ::TBaseObject:Method1()

   RETURN self

STATIC FUNCTION Method2()

   LOCAL self := QSelf()

   QOut( "I am Method2 from HBObject" )

   RETURN self
