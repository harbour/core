/*
 * $Id$
 */

#include "adordd.ch"

REQUEST ADORDD

function Main()

   USE test00 VIA "ADORDD" TABLE "ACCOUNTS" MYSQL ;
      FROM "www.freesql.org" USER "myuser" PASSWORD "mypass"

   Browse()

   USE

return nil
