
PROCEDURE Main()

   LOCAL a := { 100, 200, 300 }

   AEval( a, {| nValue, nIndex | QOut( nValue, nIndex ) } )

   RETURN
