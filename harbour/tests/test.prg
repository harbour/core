//
// $Id$
//

//

procedure main()

   local s := " " + chr(0) + "  mab  " + chr(0) + " "

   StrDump( s )
   QOut( s )

   qout( '"' + ltrim(s) + '"' )
   qout( '"' + rtrim(s) + '"' )
   qout( '"' + alltrim(s) + '"' )

return

