// Using Harbour Class HBClass

PROCEDURE Main()

   LOCAL oForm := TForm():New()

   ? oForm:ClassName()

   oForm:Show()

   RETURN

STATIC FUNCTION TForm()

   STATIC s_oClass

   IF s_oClass == NIL
      s_oClass := HBClass():New( "TForm" )    // starts a new class definition

      s_oClass:AddData( "cName" )           // define this class objects datas
      s_oClass:AddData( "nTop" )
      s_oClass:AddData( "nLeft" )
      s_oClass:AddData( "nBottom" )
      s_oClass:AddData( "nRight" )

      s_oClass:AddMethod( "New",  @New() )  // define this class objects methods
      s_oClass:AddMethod( "Show", @Show() )

      s_oClass:Create()                     // builds this class
   ENDIF

   RETURN s_oClass:Instance()                  // builds an object of this class

STATIC FUNCTION New()

   LOCAL Self := QSelf()

   ::nTop    := 10
   ::nLeft   := 10
   ::nBottom := 20
   ::nRight  := 40

   RETURN Self

STATIC FUNCTION Show()

   LOCAL Self := QSelf()

   ? "lets show a form from here :-)"

   RETURN NIL
