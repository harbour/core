//
// $Id$
//

function Main()

   local aStruct := { { "CHARACTER", "C", 25, 0 }, ;
                      { "NUMERIC",   "N",  8, 0 }, ;
                      { "DOUBLE",    "N",  8, 2 }, ;
                      { "DATE",      "D",  8, 0 }, ;
                      { "LOGICAL",   "L",  1, 0 } }

   REQUEST DBFCDX

   rddSetDefault( "DBFCDX" )

   FErase( "testcdx.cdx" )
//   dbCreate( "testcdx", aStruct, "DBFCDX" )
   dbUseArea( ,, "testcdx", "MYALIAS" )
   ordCreate( "testcdx", "1Tag ", "FIELD->Character", { || FIELD->Character }, .F. )
   ordCreate( "testcdx", "2Tag ", "FIELD->Character", { || FIELD->Character }, .F. )
//   ? MYALIAS->CHARACTER

return nil

