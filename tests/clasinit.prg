// Using Harbour Class HBClass

PROCEDURE Main()

   LOCAL oForm := TForm():New()
   LOCAL oSecond

   ? "What's the default oForm and calculate area"
   ? hb_ValToExp( oForm )
   ? oForm:CalcArea()
   ? "Set nTop to 5 and recalculate"
   oForm:nTop := 5
   ? hb_ValToExp( oForm )
   ? oForm:CalcArea()

   ? "Create a new instance and calculate area"
   oSecond := TForm():New()
   ? hb_ValToExp( oSecond )
   ? oSecond:CalcArea()

   RETURN

FUNCTION TForm()

   STATIC s_oClass

   IF s_oClass == NIL
      s_oClass := HBClass():New( "TFORM" )    // starts a new class definition

      s_oClass:AddData( "cName" )           // define this class objects datas
      s_oClass:AddData( "nTop"   , 10 )
      s_oClass:AddData( "nLeft"  , 10 )
      s_oClass:AddData( "nBottom", 20 )
      s_oClass:AddData( "nRight" , 40 )

      s_oClass:AddMethod( "New",  @New() )  // define this class objects methods
      s_oClass:AddMethod( "Show", @Show() )
      s_oClass:AddInline( "CalcArea", ;
         {| self | ( ::nRight - ::nLeft ) * ( ::nBottom - ::nTop ) } )

      s_oClass:Create()                     // builds this class
   ENDIF

   RETURN s_oClass:Instance()                  // builds an object of this class

STATIC FUNCTION New()

   LOCAL Self := QSelf()

   RETURN Self

STATIC FUNCTION Show()

   LOCAL Self := QSelf()

   ? "lets show a form from here :-)"

   RETURN NIL
