//
// $Id$
//

// Using Harbour Class TClass

function Main()

   local oForm := TForm():New()
   local oSecond

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

return nil

function TForm()

   static oClass

   if oClass == nil
      oClass = TClass():New( "TFORM" )    // starts a new class definition

      oClass:AddData( "cName" )           // define this class objects datas
      oClass:AddData( "nTop"   , 10 )
      oClass:AddData( "nLeft"  , 10 )
      oClass:AddData( "nBottom", 20 )
      oClass:AddData( "nRight" , 40 )

      oClass:AddMethod( "New",  @New() )  // define this class objects methods
      oClass:AddMethod( "Show", @Show() )
      oClass:AddInline( "CalcArea", ;
             {|self| ( ::nRight  - ::nLeft ) * ( ::nBottom - ::nTop ) } )

      oClass:Create()                     // builds this class
   endif

return oClass:Instance()                  // builds an object of this class

static function New()

   local Self := QSelf()

return Self

static function Show()

   local Self := QSelf()

   QOut( "lets show a form from here :-)" )

return nil
