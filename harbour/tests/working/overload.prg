//
// $Id$
//

#include "hboo.ch"

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

   QOut( "Testing TString with Operator Overloading" )
   QOut( oString:cValue )
   QOut()

   QOut( "Equal........:", oString = "Hello" )
   QOut( "Exactly Equal:", oString == "Hello" )
   QOut( "Not Equal != :", oString != "Hello" )
   QOut( "Not Equal <> :", oString <> "Hello" )
   QOut( "Not Equal #  :", oString # "Hello" )
   QOut( "Substring $  :", oString $ "Hello" )
   QOut( "Less than    :", oString < "Hello" )
   QOut( "Less than or Equal:", oString <= "Hello" )
   QOut( "Greater than :", oString < "Hello" )
   QOut( "Greater than or Equal:", oString <= "Hello" )
   QOut( "Concatenation + :", oString + "Hello" )
   QOut( "Concatenation - :", oString - "Hello" )
   oString += " World"
   QOut( "Compound += :", oString )
   oString -= " World"
   QOut( "Compound -= :", oString )

return nil

function TString()

   static oClass

   if oClass == nil
      oClass = TClass():New( "TSTRING" )  // starts a new class definition

      oClass:AddData( "cValue" )          // define this class objects datas

      oClass:AddMethod( "New", @New() )

      oClass:AddInline( "=",  {| self, cTest | ::cValue = cTest } )
      oClass:AddInline( "==",  {| self, cTest | ::cValue == cTest } )
      oClass:AddInline( "!=",  {| self, cTest | ::cValue != cTest } )
      oClass:AddInline( "<>",  {| self, cTest | ::cValue <> cTest } )
      oClass:AddInline( "#",  {| self, cTest | ::cValue # cTest } )
      oClass:AddInline( "+=",  {| self, cTest | ::cValue += cTest } )
      oClass:AddInline( "-=",  {| self, cTest | ::cValue -= cTest } )
      oClass:AddInline( "+",  {| self, cTest | ::cValue := ::cValue + cTest } )
      oClass:AddInline( "-",  {| self, cTest | ::cValue := ::cValue - cTest } )
      oClass:AddInline( "$",  {| self, cTest | ::cValue $ cTest } )
      oClass:AddInline( "<",  {| self, cTest | ::cValue < cTest } )
      oClass:AddInline( "<=",  {| self, cTest | ::cValue <= cTest } )
      oClass:AddInline( ">",  {| self, cTest | ::cValue > cTest } )
      oClass:AddInline( ">=",  {| self, cTest | ::cValue >= cTest } )

      oClass:AddInline( "HasMsg", {| self, cMsg | __ObjHasMsg( QSelf(), cMsg ) } )

      oClass:Create()                     // builds this class
   endif

return oClass:Instance()                  // builds an object of this class

static function New( cText )

   local Self := QSelf()

   ::cValue := cText

return Self
