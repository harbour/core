/*
 * $Id$
 */

// Class error. We are keeping Clipper compatibility here, instead of using
// TError():New() style and also avoiding hungarian notation.

//----------------------------------------------------------------------------//

function ErrorNew()

   static oClass

   if oClass == nil
      oClass = TClass():New( "ERROR" )

      oClass:AddData( "args"         , 0 )
      oClass:AddData( "CanDefault"   , .F. )
      oClass:AddData( "CanRetry"     , .F. )
      oClass:AddData( "CanSubstitute", .F. )
      oClass:AddData( "Cargo" )
      oClass:AddData( "description"  , '' )
      oClass:AddData( "filename"     , '' )
      oClass:AddData( "GenCode"      , 0 )
      oClass:AddData( "Operation"    , '' )
      oClass:AddData( "OsCode"       , 0 )
      oClass:AddData( "Severity"     , 0 )
      oClass:AddData( "SubCode"      , 0 )
      oClass:AddData( "SubSystem"    , '' )
      oClass:AddData( "Tries"        , 0 )

      oClass:Create()
   endif

return oClass:Instance()

//----------------------------------------------------------------------------//
