/*
 * Harbour Project source code:
 *    demonstration/test code for NETIO-RPC OLE server
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */


#define CLS_Name  "MyOleRPCServer"
#define CLS_ID    "{23245C3F-4487-404B-985F-E33886698D23}"

#include "hbclass.ch"

/* DllMain() is OLE server entry point
 * It's executed just after loading OLE inproc server
 * as server from other application and also by regsrv32.exe
 * during registration and unregistration procedure.
 * It should initialize OLE server ID and name.
 */
PROCEDURE DllMain()

   /* Initialize OLE server ID and name.
    * win_oleServerInit() should be executed from DllMain()
    *
    * win_oleServerInit( <cClassID>, <cServerName>, ;
    *                    [ <hAction> | <oAction> | <bAction> | <sAction> ], ;
    *                    [ <lHashClone> | <lAcceptAll> ] ) -> <lServerActive>
    *
    * <cClassID> is registered OLE server class GUID
    *
    * <cServerName> is OLE server class name
    *
    * <hAction> is optional parameter with hash array containing messages
    * and instance variables used by OLE server. The keys in hash array
    * are strings with message names and values are actions. Codeblock
    * and symbol items means that given message is a method call and
    * any other value means that it's variable.
    * By default the same hash array is shared between all objects
    * created by registered server. It's important when hash array
    * contains values which are neither codeblock nor symbol items
    * so they are not used as method but rather as instance variables
    * because such instance variables are shared between OLE objects.
    * Setting 4-th parameter <lHashClone> to .T. causes that each
    * objects receives it's own copy of <hAction> item so instance
    * variables inside hash array are also local to OLE object.
    * Alternatively programmer can use <bAction> or <sAction> to create
    * seprate copy of hash array for each object, i.e.:
    *    bAction := {|| hb_HClone( hValue ) }
    * When hash array contains symbol item (@funcName()) then when it's
    * executed by OLE object message it's possible to access the hash
    * array bound with given OLE object using QSelf() function. It maybe
    * useful if hash array contains instance variables and programmer
    * wants to access them.
    * Please remember that using hash array which was initialized to keep
    * original assign order by hb_HKeepOrder( <hAction>, .T. ) before
    * adding its items you can define strict message numbers (DISPIDs), i.e.:
    *    hAction := {=>}
    *    hb_HKeepOrder( hAction, .T. )
    *    hAction[ "OPEN" ]  := @myole_open()     // DISPID=1
    *    hAction[ "CLOSE" ] := @myole_close()    // DISPID=2
    *    hAction[ "SAVE" ]  := @myole_save()     // DISPID=3
    *    hAction[ "LOAD" ]  := @myole_load()     // DISPID=4
    *    hAction[ "PRINT" ] := @myole_print()    // DISPID=5
    * (see example in olesrv2.prg)
    *
    * <oAction> is optional parameter with Harbour object which is used
    * as base for all newly created OLE objects. All messages (method and
    * instance variables) supported explicitly by <oAction> object (except
    * ONERROR message redirecting) are inherited by OLE objects. Each
    * newly created OLE object uses the same <oAction> object so its
    * instance variables are shared between all of them. If programmer
    * wants to create separate Harbour object for each OLE object then
    * he should use <bAction> or <sAction>, i.e.:
    *       bAction := {|| myClass():new() }
    *
    * <bAction> is optional parameter with codeblock executed when new
    * OLE object is created. It should return hash array or Harbour object
    * which will be used as base for newly created OLE object.
    *
    * <sAction> is optional parameter with function symbol. This function
    * is executed when new OLE object is created and should return hash
    * array or Harbour object which is used as base for newly created
    * OLE object.
    *
    * If the 3-rd parameter is <oAction>, <bAction> or <sAction> then
    * it's possible to also set 4-th parameter <lAcceptAll> to .T. and
    * in such case <xAction> parameter is used in different way. Newly
    * created OLE object accepts any massage names invoking for each
    * of them Eval() message which is sent to <xAction> with OLE message
    * name inserted as the 1-st item to OLE object parameters.
    * It allows to create OLE server which will accept unknown messages
    * redirecting them to some other code, i.e.:
    *    if netio_Connect( cServer,,, cPasswd )
    *       win_oleServerInit( cClassID, cServerName, @netio_FuncExec(), .T. )
    *    endif
    * initialize OLE server which redirects all messages to default netio
    * connection established by netio_Connect().
    *
    * If 3-rd parameter is not given then all HVM functions becomes
    * OLE methods and HVM memvars (public and private variables) are
    * OLE object instance variables so they are shared with all OLE
    * objects created by this interface. It works just like xHarbour.com
    * OLE server described at
    * http://xharbour.com/index.asp?page=add_on_oleserver&show_sub=7&show_i=1
    */

   win_oleServerInit( CLS_ID, CLS_Name, {|| OleNetioSrv():new() }, .T. )

   RETURN


CREATE CLASS OleNetioSrv

   HIDDEN:

   VAR    pConn

   EXPORTED:

   METHOD Eval( cMethodName, ... )

ENDCLASS

METHOD Eval( cMethodName, ... ) CLASS OleNetioSrv

   LOCAL xRetVal, oErr

   BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
      SWITCH cMethodName
      CASE "CONNECT"
         xRetVal := ! Empty( ::pConn := netio_GetConnection( ... ) )
         EXIT
      CASE "DISCONNECT"
         ::pConn := NIL
         xRetVal := .T.
         EXIT
      CASE "PROCEXISTS"
         xRetVal := netio_ProcExists( ::pConn, ... )
         EXIT
      CASE "PROCEXEC"
         xRetVal := netio_ProcExec( ::pConn, ... )
         EXIT
      CASE "PROCEXECW"
         xRetVal := netio_ProcExecW( ::pConn, ... )
         EXIT
      CASE "FUNCEXEC"
         xRetVal := netio_FuncExec( ::pConn, ... )
         EXIT
      OTHERWISE
         /* redirect all other messages to RPC server as function calls */
         xRetVal := netio_FuncExec( ::pConn, cMethodName, ... )
      ENDSWITCH
   RECOVER USING oErr
      xRetVal := oErr
   END SEQUENCE

   RETURN xRetVal


ANNOUNCE GT_SYS
REQUEST HB_GT_GUI_DEFAULT
