/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration/test code for OLE server which works like
 *    xHarbour.com OLE servers described at
 *    http://xharbour.com/index.asp?page=add_on_oleserver&show_sub=7&show_i=1
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#define CLS_Name  "MyOleServer"
#define CLS_ID    "{466AC7B2-35D7-4509-B909-C3C2F8FDBD3C}"

PROCEDURE DllMain()

   PUBLIC Property1

   M->Property1 := "MyProperty"

   /* Initialize OLE server ID and name.
    * WIN_OleServerInit() should be executed from DllMain()
    */
   WIN_OleServerInit( CLS_ID, CLS_Name )

   RETURN


FUNCTION MyMethod( ... )

   RETURN "Hello from MyOleServer [" + hb_ValToExp( { ... } ) + "]"


ANNOUNCE GT_SYS
REQUEST HB_GT_GUI_DEFAULT
