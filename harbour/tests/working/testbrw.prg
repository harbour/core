// Harbour Class TBrowse and TBColumn sample

function Main()

   local oBrowse := TBrowse():New()

   oBrowse:AddColumn( TBColumnNew( "Test", { || "This is a test" } ) )

   Alert( oBrowse:ClassName() )
   Alert( oBrowse:GetColumn( 1 ):ClassName() )

return nil
