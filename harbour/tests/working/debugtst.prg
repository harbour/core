/* $Doc$
 * $Description$  Debug function tests.
 *                Based on classes.prg
 * $Requirement$  source\tools\debug.c
 *                source\rtl\itemapi.c (1999/05/04)
 * $Date$         1999/05/06
 * $End$ */
function Main()

   local oForm   := TForm():New()
   local nNumber := 15

   QOut( oForm:ClassName() )
   oForm:Show()
   QOut()

   QOut( "-DEBUG Functions-")
   QOut( ToChar(oForm:ClassSel, ", ", .T.) )

   QOut( "-Statics-" )
   QOut( ToChar ( __aStatic(), ", ", .T. ) )

   QOut( "-Global Stack-" )
   QOut( ToChar ( __aGlobalStack(), ", ", .T. ) )

   QOut( "-Local Stack-" )
   QOut( ToChar ( __aStack(), ", ", .T. ) )

   QOut( "-Parameters-" )
   QOut( ToChar ( __aParam(), ", ", .T. ) )

   Pause()

   FuncSecond( 241, "Hello" )

return nil


function Pause()
return __Accept("")


function FuncSecond( nParam, cParam, uParam )

   local cWhat   := "Something"
   local nNumber := 2
   local uEmpty

   QOut()
   QOut( "-Second procedure-")
   QOut()

   QOut( "-Statics-" )
   QOut( ToChar ( __aStatic(), ", ", .T. ) )
   QOut()

   QOut( "-Global Stack- Len=", __GlobalStackLen() )
   QOut( ToChar ( __aGlobalStack(), ", ", .T. ) )
   QOut()

   QOut( "-Local Stack- Len=", __StackLen() )
   QOut( ToChar ( __aStack(), ", ", .T. ) )
   QOut()

   QOut( "-Parameters-" )
   QOut( ToChar ( __aParam(), ", ", .T. ) )

   Pause()

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
 *                [lDebug]     : .T. -> Write debug output
 *
 * In DEBUG mode :
 *
 * It will show the xItem according to the following format :
 *
 * <num>                        Numerical
 * dd/mm/yyyy                   Date
 * "<chr>"                      Character
 * {<el1>, <el2>, ...}          Array
 * NIL                          NIL
 * .T. / .F.                    Boolean
 * <ClassName>(<ClassH>):{ <val1>, <val2> ... | <DataSymbol1>, ... }
 *                              Object
 *
 *
 * TODO : <ClassName>(<ClassH>):{<DataSymbol1>:<val1>, ... }
 * Requirement : oSend( <object>, <Symbol>, <args,..> )
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

      case cValTxt=="A"                         // Array
         if lDebug
            cOut += "{"
         else
            cOut := ""
         endif
         nLen := Len( xTxt )
         for n := 1 to nLen                     // For each item : Recurse !
            cOut += ToChar( xTxt[n], cSeparator, lDebug )
            if n != nLen
               cOut += cSeparator
            endif
         next n
         if lDebug
            cOut += "}"
         endif

      case cValTxt=="B"                         // Codeblock
         if lDebug
            cOut := "Block"
         else
            cOut := Eval( xTxt )
         endif

      case cValTxt=="O"                         // Object
         if lDebug
            cOut := xTxt:ClassName() + "(#"+ToChar( xTxt:ClassH() )+"):{"
            nLen := Len( xTxt )
            for n := 1 to nLen                     // For each item : Recurse !
               cOut += ToChar( xTxt[n], cSeparator, lDebug )
               if n != nLen
                  cOut += cSeparator
               endif
            next n
            cOut += ";" + ToChar( aoData( xTxt ), ", " ) + "}"
         else
            cOut := ToChar( xTxt:Run(), cSeparator, lDebug )
         endif

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


//
// aData aOData( oObject )
//
// Return an array containing the names of all the data items of oObject.
//
function aOData( oObject )

   local aInfo  := aSort( oObject:ClassSel() )
   local aData  := {}
   local n

   for n := 1 to Len(aInfo)
      if SubStr( aInfo[ n ], 1, 1 ) != "_"
         if aScan( aInfo, "_" + aInfo[ n ] ) != 0
            aAdd( aData, aInfo[ n ] )
         endif
      endif
   next n

return aData


//
// <aSorted> aSort( <aUnsorted>, [nStart], [nCount], [bBlock] )
//
// Sort an array
//
function aSort( aIn, nStart, nCount, bBlock )

   nStart := Default( nStart, 1 )
   QuickSort( aIn,                                      ;
              nStart,                                   ;
              Default( nCount, Len(aIn) - nStart + 1 ), ;
              Default( bBlock, {| x, y | x < y } ) )
return aIn


//
// QuickSort( <aSort>, <nLeft>, <nRight>, <bOrder> )
//
// Perform a QuickSort of <aSort>.
//
// Warning : Recursion ahead !
//
// For instructions :
// http://monty.cnri.reston.va.us/grail/demo/quicksort/quicksort.htm
//
function QuickSort( aSort, nLeft, nRight, bOrder )

   local nUp     := nLeft
   local nDown   := nRight
   local xMiddle := aSort[ ( nLeft + nRight ) / 2 ]
   local xTemp
   local lOk     := .T.

   do while lOk
      do while Eval( bOrder, aSort[ nUp ], xMiddle   )
         nUp++
      enddo

      do while Eval( bOrder, xMiddle, aSort[ nDown ] )
         nDown--
      enddo

      if nUp <= nDown
         if nUp != nDown
            xTemp          := aSort[ nUp ]
            aSort[ nUp   ] := aSort[ nDown ]
            aSort[ nDown ] := xTemp
         endif
         nUp++
         nDown--
      endif

      lOk := nUp <= nDown
   enddo

   if nLeft < nDown
      QuickSort( aSort, nLeft, nDown , bOrder )
   endif

   if nUp < nRight
      QuickSort( aSort, nUp  , nRight, bOrder )
   endif

return nil


