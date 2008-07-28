//
// $Id$
//

//
// Test of inline function
//
function Main()

   local oForm := TForm():New()

   QOut( oForm:ClassName() )
   oForm:cText := "Let's show a form here :-)"

   oForm:Show()

return nil

function TForm()

   static oClass

   if oClass == nil
      oClass := HBClass():New( "TFORM" )    // starts a new class definition

      oClass:AddData( "cText" )           // define this class objects datas
      oClass:AddData( "nTop" )
      oClass:AddData( "nLeft" )
      oClass:AddData( "nBottom" )
      oClass:AddData( "nRight" )

      oClass:AddMethod( "New",  @New() )  // define this class objects methods
      oClass:AddInline( "Show", {|self| QOut( self:cText ) } )

      oClass:Create()                     // builds this class
   endif

return oClass:Instance()                  // builds an object of this class

static function New()

   local Self := QSelf()

   ::nTop    := 10
   ::nLeft   := 10
   ::nBottom := 20
   ::nRight  := 40

return Self
