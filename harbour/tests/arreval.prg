//
// $Id$
//

function Main()

   local a := { 100, 200, 300 }

   aEval(a, {|nValue, nIndex| QOut(nValue, nIndex) })

return nil

