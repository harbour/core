/*
 * $Id$
 */

// Using Harbour Class HBClass

PROCEDURE Main()

   LOCAL oForm := TForm():New()
   LOCAL oSecond

   QOut( "What's the default oForm and calculate area" )
   Debug( oForm )
   QOut( oForm:CalcArea()  )
   QOut( "Set nTop to 5 and recalculate" )
   oForm:nTop := 5
   Debug( oForm )
   QOut( oForm:CalcArea()  )

   QOut( "Create a new instance and calculate area" )
   oSecond := TForm():New()
   Debug( oSecond )
   QOut( oSecond:CalcArea()  )

   RETURN

FUNCTION TForm()

   STATIC oClass

   IF oClass == NIL
      oClass := HBClass():New( "TFORM" )    // starts a new class definition

      oClass:AddData( "cName" )           // define this class objects datas
      oClass:AddData( "nTop"   , 10 )
      oClass:AddData( "nLeft"  , 10 )
      oClass:AddData( "nBottom", 20 )
      oClass:AddData( "nRight" , 40 )

      oClass:AddMethod( "New",  @New() )  // define this class objects methods
      oClass:AddMethod( "Show", @Show() )
      oClass:AddInline( "CalcArea", ;
         {| self | ( ::nRight - ::nLeft ) * ( ::nBottom - ::nTop ) } )

      oClass:Create()                     // builds this class
   ENDIF

   RETURN oClass:Instance()                  // builds an object of this class

STATIC FUNCTION New()

   LOCAL Self := QSelf()

   RETURN Self

STATIC FUNCTION Show()

   LOCAL Self := QSelf()

   QOut( "lets show a form from here :-)" )

   RETURN NIL
