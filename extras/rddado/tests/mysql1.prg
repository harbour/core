/*
 * $Id$
 */

#require "rddado"

#include "adordd.ch"

REQUEST ADORDD

PROCEDURE Main()

   USE test00 VIA "ADORDD" TABLE "ACCOUNTS" MYSQL ;
      FROM "www.freesql.org" USER "myuser" PASSWORD "mypass"

   Browse()

   USE

   RETURN
