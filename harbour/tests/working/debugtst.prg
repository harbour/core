/* $Doc$
 * $Description$  Debug function tests.
 *                Based on classes.prg
 * $Requirement$  source\tools\debug.c
 *                source\rtl\itemapi.c (1999/05/04)
 * $Date$         1999/05/06
 * $End$ */
function Main()

   local oForm := TForm():New()

   QOut( oForm:ClassName() )
   oForm:Show()

   QOut( "-DEBUG-" )

   QOut( "Statics        = ", ToChar ( __aStatic(), ", ", .T. ) )
   QOut( "Type static[1] = ", ValType( __Static(1) ) )
   QOut( "Stack length   = ", __StackLen() )
   QOut( "Stack          = ", ToChar ( __Stack(), ", ", .T. ) )

return nil


/* $Doc$
 * $FuncName$     <xRet> Default( <xArg>, <xDefault> )
 * $Description$  If argument is not set, return default
 * $End$ */
function Default( xArg, xDef )
return if( ValType(xArg) != ValType(xDef), xDef, xArg )


/* $Doc$
 * $FuncName$     <cOut> ToChar( <xTxt>, [cSeparator], [lDebug] )
 * $Description$  Convert to character
 * $Arguments$    <xTxt>       : Item to write
 *                [cSeparator] : Separator for arrays
 *                [lDebug]     : .T. -> Write debug output {"first",.F.}
 * $End$ */
function ToChar( xTxt, cSeparator, lDebug )

   local cValTxt
   local cOut
   local n
   local nLen

   cSeparator := Default( cSeparator, " " )
   lDebug     := Default( lDebug,     .F. )
   cValTxt    := ValType( xTxt )

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
         nLen := Len( xTxt )
         for n := 1 to nLen                     // For each item : Recurse !
            cOut += ToChar( xTxt[n], cSeparator, lDebug )
            if n != nLen
               cOut += cSeparator
            endif
         next n
         if lDebug
            cOut += if( cValTxt=="O", ")", "}" )
         endif

      case cValTxt=="B"                         // Codeblock
         if lDebug
            cOut := "Block"
         else
            cOut := Eval( xTxt )
         endif

      case cValTxt=="O"                         // Object
         cOut := ToChar( xTxt:Run(), cSeparator, lDebug )

   endcase
return cOut


/* $Doc$
 * $FuncName$     <oForm> TForm()
 * $Description$  Returns TForm object
 * $End$ */
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


/* $Doc$
 * $FuncName$     <oForm> TForm:New()
 * $Description$  Constructor
 * $End$ */
static function New()

   local Self := QSelf()

   ::nTop    = 10
   ::nLeft   = 10
   ::nBottom = 20
   ::nRight  = 40

return Self


/* $Doc$
 * $FuncName$     TForm:Show()
 * $Description$  Show a form
 * $End$ */
static function Show()

   local Self := QSelf()

   QOut( "lets show a form from here :-)" )

return nil


