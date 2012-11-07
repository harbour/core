/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main()

   LOCAL datamenu := { ;
      { "Customers", , {|| cust()    } }, ;
      { "Invoices",  , {|| inv()     } }, ;
      { "Vendors",   , {|| vendors() } }, ;
      { "Exit", "Return to Main Menu" } }

   LOCAL repmenu :=  { ;
      { "Customer List", , {|| custrep()   } }, ;
      { "Past Due",      , {|| pastdue()   } }, ;
      { "Weekly Sales",  , {|| weeksales() } }, ;
      { "Monthly P&L",   , {|| monthpl()   } }, ;
      { "Vendor List",   , {|| vendorrep() } }, ;
      { "Exit", "Return to Main Menu" } }

   LOCAL maintmenu := { ;
      { "Reindex",  "Rebuild index files", {|| re_ntx() } }, ;
      { "Backup",   "Backup data files"  , {|| backup() } }, ;
      { "Compress", "Compress data files", {|| compress() } }, ;
      { "Exit",     "Return to Main Menu" } }

   LOCAL MAINMENU := { ;
      { "DATA ENTRY",  "ENTER DATA",          {|| FT_MENU2( datamenu )  } }, ;
      { "Reports",     "Hard copy",           {|| FT_MENU2( repmenu )   } }, ;
      { "Maintenance", "Reindex files, etc.", {|| FT_MENU2( maintmenu ) } }, ;
      { "Quit",        "See ya later" } }

   CLS

   ft_Menu2( mainmenu )

   RETURN

/* stub functions to avoid missing symbols */

STATIC PROCEDURE cust() ; RETURN
STATIC PROCEDURE inv() ; RETURN
STATIC PROCEDURE vendors() ; RETURN
STATIC PROCEDURE custrep() ; RETURN
STATIC PROCEDURE pastdue() ; RETURN
STATIC PROCEDURE weeksales() ; RETURN
STATIC PROCEDURE monthpl() ; RETURN
STATIC PROCEDURE vendorrep() ; RETURN
STATIC PROCEDURE re_ntx() ; RETURN
STATIC PROCEDURE backup() ; RETURN
STATIC PROCEDURE compress() ; RETURN
