/* $Doc$
 * $Description$  Debug function tests.
 *                Based on classes.prg
 * $Requirement$  source\tools\stringp.prg
 *                source\rtl\objfunc.prg
 *                source\rtl\asort.prg
 * $Date$
 * $End$ */

function Main()

   local oForm   := TForm():New()
   local nNumber := 15

   QOut( oForm:ClassName() )
   oForm:Show()
   QOut()

   QOut( "-OBJECT additions-" )
   QOut( "What is in oForm ? " )
   Debug( oForm:Transfer() )

   QOut( "Does transfer exists ? ", IsMessage( oForm, "Transfer" ) )
   QOut( "Is   transfer DATA   ? ", IsData   ( oForm, "Transfer" ) )
   QOut( "Is   transfer METHOD ? ", IsMethod ( oForm, "Transfer" ) )
   QOut( "Does nLeft    exists ? ", IsMessage( oForm, "nLeft"    ) )
   QOut( "Is   nLeft    DATA   ? ", IsData   ( oForm, "nLeft"    ) )
   QOut( "Is   nLeft    METHOD ? ", IsMethod ( oForm, "nLeft"    ) )
   QOut( "Does unknown  exists ? ", IsMessage( oForm, "Unknown"  ) )
   QOut( "Is   unknown  DATA   ? ", IsData   ( oForm, "Unknown"  ) )
   QOut( "Is   unknown  METHOD ? ", IsMethod ( oForm, "Unknown"  ) )

   QOut( "Set nLeft to 50 and nRight to 100" )
   oForm:Transfer( {"nLeft", 50}, {"nRight", 100} )
   Debug( oForm:Transfer() )

   Pause()


   QOut( "-DEBUG Functions-")
   QOut( "-Statics-" )
   Debug( __aStatic() )

   QOut( "-Global Stack-" )
   Debug ( __aGlobalStack() )

   QOut( "-Local Stack-" )
   Debug ( __aStack() )

   QOut( "-Parameters-" )
   Debug ( __aParam() )

   Pause()

   FuncSecond( 241, "Hello" )

return nil


function Pause()
return __Accept("")


function FuncSecond( nParam, cParam, uParam )

   local cWhat   := "Something"
   local nNumber := 2
   local xParam
   local xStack

   QOut()
   QOut( "-Second procedure-")
   QOut()

   QOut( "-Statics-" )
   Debug ( __aStatic() )
   QOut()

   QOut( "-Global Stack- Len=", __GlobalStackLen() )
   Debug ( __aGlobalStack() )
   QOut()

   QOut( "-Local Stack- Len=", __StackLen() )
   xStack := Debug ( __aStack() )
   QOut()

   QOut( "-Parameters-" )
   xParam := Debug( __aParam() )
   if xParam[ xStack[ 7 ] ] == "Hello"
      QOut( ":-)" )
   endif

   Pause()

return nil



