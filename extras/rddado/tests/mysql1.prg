#require "rddado"

#include "rddado.ch"

REQUEST ADO

PROCEDURE Main()

   USE test00 VIA "ADO" TABLE "ACCOUNTS" MYSQL ;
      FROM "www.freesql.org" USER "myuser" PASSWORD "mypass"

   Browse()

   dbCloseArea()

   RETURN
