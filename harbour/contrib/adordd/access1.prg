#include "adordd.ch"

REQUEST ADORDD

function Main()

   USE Test.mdb VIA "ADORDD" TABLE "Tabla1"

   Browse()

   USE

return nil