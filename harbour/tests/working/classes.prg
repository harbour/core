// Using Harbour Class TClass

function Main()

   local oForm := TForm():New()

   QOut( oForm:ClassName() )

   oForm:Show()

return nil

function TForm()

   static oClass

   if oClass == nil
      oClass = TClass():New( "TFORM" )    // starts a new class definition

      oClass:AddData( "cName" )           // define this class objects datas
      oClass:AddData( "nTop" )
      oClass:AddData( "nLeft" )
      oClass:AddData( "nBottom" )
      oClass:AddData( "nRight" )

      oClass:AddMethod( "New",  @New() )  // define this class objects methods
      oClass:AddMethod( "Show", @Show() )

      oClass:Create()                     // builds this class
   endif

return oClass:Instance()                  // builds an object of this class

static function New()

   local Self := QSelf()

   ::nTop    = 10
   ::nLeft   = 10
   ::nBottom = 20
   ::nRight  = 40

return Self

static function Show()

   local Self := QSelf()

   QOut( "lets show a form from here :-)" )

return nil
