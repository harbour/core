#define MET_METHOD    0
#define MET_DATA      1
#define MET_CLASSDATA 2
#define MET_INLINE    3
#define MET_VIRTUAL   4

//
// DynObj
//
// Implementation of operator overload in Harbour
//
// Date : 1999/05/15
//
// Written by Eddie Runia <eddie@runia.com>
// www - http://www.harbour-project.org
//
// Placed in the public domain
//

function Main()

   local oString := TString():New( "Hello" )

   QOut( oString:cValue )
   if oString == "Hello"
      QOut( "Ok" )
      if oString != "Hello"
         QOut( "Not ok" )
      endif
   endif
return nil


function TString()

   static oClass

   if oClass == nil
      oClass = TClass():New( "TSTRING" )  // starts a new class definition

      oClass:AddData( "cValue" )          // define this class objects datas

      oClass:AddMethod( "New", @New() )
      oClass:AddInline( "==",  {| self, cTest | ::cValue == cTest } )
      oClass:AddInline( "!=",  {| self, cTest | ::cValue != cTest } )

      oClass:Create()                     // builds this class
   endif

return oClass:Instance()                  // builds an object of this class


static function New( cText )

   local Self := QSelf()

   ::cValue := cText

return Self



