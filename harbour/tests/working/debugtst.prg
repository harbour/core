//
// Debug function tests
//
function Main()

   local oForm := TForm():New()

   QOut( oForm:ClassName() )

   oForm:Show()

   QOut( "DEBUG" )

   QOut( "Statics = ",     ToChar( __aStatic(), ", ", .T. ) )
   QOut( "Type static[1]", ValType( __Static(1) ) )

return nil


//
// Always return a correct value
//
function Default( xArg, xDef )
return if( ValType(xArg) != ValType(xDef), xDef, xArg )


//
// ToChar -> Convert xTxt to character
//
// xTxt       : Item to write
// cSeparator : Separator for arrays. Def:' '
// lDebug     : Write debug output {"first",.F.}. Def:.F.
//
function ToChar( xTxt, cSeparator, lDebug )

   local cValTxt
   local cOut
   local n

   lDebug     := Default( lDebug, .F. )
   cValTxt    := ValType( xTxt )
   cSeparator := Default( cSeparator, " ")

   do case
      case cValTxt=="C" .or. cValTxt=="M"       // Character
         cOut := if( lDebug, '"'+xTxt+'"', xTxt )

      case cValTxt=="N"                         // Numeric
         cOut := Alltrim(Str(xTxt))

      case cValTxt=="U"                         // Nothing to write
         cOut := if( lDebug, "NIL", "" )

      case cValTxt=="D"                         // Date
         cOut := TransForm(xTxt, "")

      case cValTxt=="L"                         // Logical
         if lDebug
            cOut := if( xTxt, ".T.", ".F." )
         else
            cOut := if( xTxt, "True", "False" )
         endif

      case cValTxt=="A" .or. if( lDebug, cValTxt=="O", .F.)
                                                // Array or debug object
         cOut       := ""
         cSeparator := Default( cSeparator, " ")
         if lDebug
            cOut += if( cValTxt=="A", "{", "Object(" )
         endif
         for n := 1 to Len( xTxt )
            cOut += ToChar( xTxt[n], cSeparator, lDebug ) + cSeparator
         next n
         cOut := Substr( cOut, 1, Len( cOut ) - Len( cSeparator ) )
         if lDebug
            cOut += if( cValTxt=="O", ")", "}" )
         endif

      case cValTxt=="B"                         // Code block (??)
         if lDebug
            cOut := "Block"
         else
            cOut := Eval( xTxt )
         endif

      case cValTxt=="O"                         // Object (??)
         cOut := ToChar( xTxt:Run(), cSeparator, lDebug )

   endcase
return cOut


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


