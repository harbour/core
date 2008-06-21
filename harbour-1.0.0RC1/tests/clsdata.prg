//
// $Id$
//

Function Main()

   local o := HBObject():New()
   QOut( "o:Data1 => ", o:Data1 )
   QOut( "o:ClassData1 => ", o:ClassData1 )
   QOut( "o:Data2 => ", o:Data2 )
   QOut( "o:ClassData2 => ", o:ClassData2 )
   o:Test()

return NIL

Function TBaseObject()

   static oClass

   if oClass == NIL
      oClass := HBClass():New( "TBaseObject" )
      oClass:AddData( "Data1" )
      oClass:AddClassData( "ClassData1" )
      oClass:AddMethod( "NewBase", @NewBase() )
      oClass:AddMethod( "Test", @Test() )
      oClass:AddMethod( "Method1", @Method1Base() )
      oClass:AddMethod( "Method2", @Method2Base() )
      oClass:Create()
   endif
return oClass:Instance()

static function NewBase()

   local self := QSelf()

   ::Data1 := 1
   ::ClassData1 := "A"
return self

static function Test()

   local self := QSelf()

   QOut( "Inside ::Test() " )
   QOut( "calling ::Method1() " )
   ::Method1()
return self

static function Method1Base()

   local self := QSelf()

   QOut( "I am Method1 from TBaseObject" )
   ::Method2()
return self

static function Method2Base()

   local self := QSelf()

   QOut( "I am Method2 from TBaseObject" )
return self

Function HBObject()

   static oClass

   if oClass == NIL
      oClass := HBClass():New( "HBObject", "TBaseObject" )
      oClass:AddData( "Data2" )
      oClass:AddClassData( "ClassData2" )
      oClass:AddMethod( "New", @New() )
      oClass:AddMethod( "Method1", @Method1() )
      oClass:AddMethod( "Method2", @Method2() )
      oClass:Create()
   endif
return oClass:Instance()

static function New()

   local self := QSelf()

   ::TBaseObject:NewBase()
   ::Data1 := 1
   ::ClassData1 := "A"
   ::Data2 := 2
// ClassData2 override ClassData1
   ::ClassData2 := "B"
return self

static function Method1()

   local self := QSelf()

   QOut( "I am Method1 from HBObject" )
   ::TBaseObject:Method1()
return self

static function Method2()

   local self := QSelf()

   QOut( "I am Method2 from HBObject" )
return self
