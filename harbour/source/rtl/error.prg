// Class error. We are keeping Clipper compatibility here, instead of using
// TError():New() style and also avoiding hungarian notation.

//----------------------------------------------------------------------------//

function ErrorNew()

   static oClass

   if oClass == nil
      oClass = TClass():New( "ERROR" )

      oClass:AddData( "args" )
      oClass:AddData( "CanDefault" )
      oClass:AddData( "CanRetry" )
      oClass:AddData( "CanSubstitute" )
      oClass:AddData( "Cargo" )
      oClass:AddData( "description" )
      oClass:AddData( "filename" )
      oClass:AddData( "GenCode" )
      oClass:AddData( "Operation" )
      oClass:AddData( "OsCode" )
      oClass:AddData( "Severity" )
      oClass:AddData( "SubCode" )
      oClass:AddData( "SubSystem" )
      oClass:AddData( "Tries" )

      oClass:Create()
   endif

return oClass:Instance()

//----------------------------------------------------------------------------//
