/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration/test code for OLE server using hash array with
 *    strict item order (associative hash array) to define OLE objects
 *    with fixed message numbers (DISPIDs)
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 */

#define CLS_Name  "MyOleTimeServer"
#define CLS_ID    "{5552013F-2990-4D6C-9C96-55A4BDDCE376}"

PROCEDURE DllMain()

   LOCAL hAction

   hAction := { => }
   hb_HKeepOrder( hAction, .T. )
   hb_HSetCaseMatch( hAction, .F. )
   hAction[ "DATE" ]     := @Date()          // DISPID=1
   hAction[ "TIME" ]     := @Time()          // DISPID=2
   hAction[ "DATETIME" ] := @hb_DateTime()   // DISPID=3
   hAction[ "VALUE" ]    := NIL              // DISPID=4
   hAction[ "GETDATA" ]  := @get_data()      // DISPID=5

   /* Initialize OLE server ID and name.
    * win_oleServerInit() should be executed from DllMain()
    */
   win_oleServerInit( CLS_ID, CLS_Name, hAction, .T. )

   RETURN


STATIC FUNCTION get_data( ... )

   LOCAL hAction := QSelf()

   IF hAction[ "VALUE" ] == NIL
      RETURN "(:VALUE IS NOT SET)"
   ENDIF

   RETURN ":VALUE='" + hAction[ "VALUE" ] + "'"


ANNOUNCE GT_SYS
REQUEST HB_GT_GUI_DEFAULT
