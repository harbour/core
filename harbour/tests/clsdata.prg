/*
 * $Id$
 */

PROCEDURE Main()

   LOCAL o := HBObject():New()

   ? "o:Data1 => ", o:Data1
   ? "o:ClassData1 => ", o:ClassData1
   ? "o:Data2 => ", o:Data2
   ? "o:ClassData2 => ", o:ClassData2
   o:Test()

   RETURN

FUNCTION TBaseObject()

   STATIC s_oClass

   IF s_oClass == NIL
      s_oClass := HBClass():New( "TBaseObject" )
      s_oClass:AddData( "Data1" )
      s_oClass:AddClassData( "ClassData1" )
      s_oClass:AddMethod( "NewBase", @NewBase() )
      s_oClass:AddMethod( "Test", @Test() )
      s_oClass:AddMethod( "Method1", @Method1Base() )
      s_oClass:AddMethod( "Method2", @Method2Base() )
      s_oClass:Create()
   ENDIF

   RETURN s_oClass:Instance()

STATIC FUNCTION NewBase()

   LOCAL self := QSelf()

   ::Data1 := 1
   ::ClassData1 := "A"

   RETURN self

STATIC FUNCTION Test()

   LOCAL self := QSelf()

   ? "Inside ::Test()"
   ? "calling ::Method1()"
   ::Method1()

   RETURN self

STATIC FUNCTION Method1Base()

   LOCAL self := QSelf()

   ? "I am Method1 from TBaseObject"
   ::Method2()

   RETURN self

STATIC FUNCTION Method2Base()

   LOCAL self := QSelf()

   ? "I am Method2 from TBaseObject"

   RETURN self

FUNCTION HBObject()

   STATIC s_oClass

   IF s_oClass == NIL
      s_oClass := HBClass():New( "HBObject", "TBaseObject" )
      s_oClass:AddData( "Data2" )
      s_oClass:AddClassData( "ClassData2" )
      s_oClass:AddMethod( "New", @New() )
      s_oClass:AddMethod( "Method1", @Method1() )
      s_oClass:AddMethod( "Method2", @Method2() )
      s_oClass:Create()
   ENDIF

   RETURN s_oClass:Instance()

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

   ? "I am Method1 from HBObject"
   ::TBaseObject:Method1()

   RETURN self

STATIC FUNCTION Method2()

   LOCAL self := QSelf()

   ? "I am Method2 from HBObject"

   RETURN self
