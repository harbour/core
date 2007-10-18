/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration code for FOR EACH used for OLE objects
 *    this code needs HBOLE library
 *
 * Copyright 2007 Enrico Maria Giordano e.m.giordano at emagsoftware.it
 * www - http://www.harbour-project.org
 *
 */

FUNCTION MAIN()
   LOCAL oExcel := CREATEOBJECT( "Excel.Application" )
   LOCAL oWorkBook := oExcel:WorkBooks:Add()
   LOCAL oWorkSheet

   FOR EACH oWorkSheet IN oWorkBook:WorkSheets
      ? oWorkSheet:Name
   NEXT
   oExcel:Quit()
RETURN NIL
