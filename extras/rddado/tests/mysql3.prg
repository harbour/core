#require "rddado"

// Testing LOCATE and CONTINUE

#include "rddado.ch"

REQUEST ADORDD

PROCEDURE Main()

   USE test00 VIA "ADORDD" TABLE "ACCOUNTS" MYSQL ;
      FROM "www.freesql.org" USER "myuser" PASSWORD "mypass"

   LOCATE FOR "City LIKE 'Chi*'"

   DO WHILE ! Eof()
      ? test00->First, test00->City
      CONTINUE
   ENDDO

   dbCloseArea()

   RETURN
