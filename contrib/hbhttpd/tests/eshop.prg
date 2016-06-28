/* Requirement: Create certificate with ./mkcert.sh (rename to .bat as needed) */

#require "hbssl"
#require "hbhttpd"

REQUEST __HBEXTERN__HBSSL__

REQUEST DBFCDX

MEMVAR server, get, post, cookie, session

#define _FN_PKEY  "privatekey.pem"
#define _FN_CERT  "certificate.pem"

PROCEDURE Main()

   LOCAL oServer

   LOCAL oLogAccess
   LOCAL oLogError

   LOCAL nPort

   IF hb_argCheck( "help" )
      ? "Usage: app [options]"
      ? "Options:"
      ? "  //help               Print help"
      ? "  //stop               Stop running server"
      RETURN
   ENDIF

   IF hb_argCheck( "stop" )
      hb_MemoWrit( ".uhttpd.stop", "" )
      RETURN
   ELSE
      hb_vfErase( ".uhttpd.stop" )
   ENDIF

   IF ! hb_vfExists( _FN_PKEY ) .OR. ;
      ! hb_vfExists( _FN_CERT )

      ? "Certificate and/or private key missing."
      ? "Create them by running ./mkcert.sh"
      ? "(rename to .bat if your platform doesn't support POSIX shell)"
      RETURN
   ENDIF

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   rddSetDefault( "DBFCDX" )

   DO CASE
   CASE ! hb_dbExists( "users.dbf" )
      hb_dbDrop( "users.dbf", "users.cdx" )
      dbCreate( "users.dbf", { { "USER", "C", 16, 0 }, { "PASSWORD", "C", 16, 0 }, { "NAME", "C", 50, 0 } }, , .T., "user" )
      dbAppend()
      FIELD->USER := "demo"
      FIELD->PASSWORD := "demo"
      FIELD->NAME := "Demo"
      ordCreate( "users.cdx", "user", "USER" )
      dbCloseArea()
   CASE ! hb_dbExists( "users.dbf", "users.cdx" )
      dbUseArea( .T., , "users.dbf", , .F., .F. )
      ordCreate( "users.cdx", "user", "USER" )
      dbCloseArea()
   ENDCASE

   DO CASE
   CASE ! hb_dbExists( "carts.dbf" )
      hb_dbDrop( "carts.dbf", "carts.cdx" )
      dbCreate( "carts.dbf", { { "USER", "C", 16, 0 }, { "CODE", "C", 16, 0 }, { "AMOUNT", "N", 6, 0 }, { "TOTAL", "N", 9, 2 } }, , .T., "cart" )
      ordCreate( "carts.cdx", "user", "USER+CODE" )
      dbCloseArea()
   CASE ! hb_dbExists( "carts.dbf", "carts.cdx" )
      dbUseArea( .T., , "carts.dbf", , .F., .F. )
      ordCreate( "carts.cdx", "user", "USER+CODE" )
      dbCloseArea()
   ENDCASE

   DO CASE
   CASE ! hb_dbExists( "items.dbf" )
      hb_dbDrop( "items.dbf", "items.cdx" )
      dbCreate( "items.dbf", { { "CODE", "C", 16, 0 }, { "TITLE", "C", 80, 0 }, { "PRICE", "N", 9, 2 } }, , .T., "items" )
      ordCreate( "items.cdx", "code", "CODE" )
      dbCloseArea()
   CASE ! hb_dbExists( "items.dbf", "item.cdx" )
      dbUseArea( .T., , "items.dbf", , .F., .F. )
      ordCreate( "items.cdx", "code", "CODE" )
      dbCloseArea()
   ENDCASE

   oLogAccess := UHttpdLog():New( "eshop_access.log" )

   IF ! oLogAccess:Add( "" )
      oLogAccess:Close()
      ? "Access log file open error", hb_ntos( FError() )
      RETURN
   ENDIF

   oLogError := UHttpdLog():New( "eshop_error.log" )

   IF ! oLogError:Add( "" )
      oLogError:Close()
      oLogAccess:Close()
      ? "Error log file open error", hb_ntos( FError() )
      RETURN
   ENDIF

   ? "Listening on port:", nPort := 8002

   oServer := UHttpdNew()

   IF ! oServer:Run( { ;
         "FirewallFilter"      => "", ;
         "LogAccess"           => {| m | oLogAccess:Add( m + hb_eol() ) }, ;
         "LogError"            => {| m | oLogError:Add( m + hb_eol() ) }, ;
         "Trace"               => {| ... | QOut( ... ) }, ;
         "Port"                => nPort, ;
         "Idle"                => {| o | iif( hb_vfExists( ".uhttpd.stop" ), ( hb_vfErase( ".uhttpd.stop" ), o:Stop() ), NIL ) }, ;
         "PrivateKeyFilename"  => _FN_PKEY, ;
         "CertificateFilename" => _FN_CERT, ;
         "SSL"                 => .T., ;
         "Mount"          => { ;
         "/hello"            => {|| UWrite( "Hello!" ) }, ;
         "/info"             => {|| UProcInfo() }, ;
         "/files/*"          => {| x | QOut( hb_DirBase() + "/files/" + X ), UProcFiles( hb_DirBase() + "/files/" + X, .F. ) }, ;
         "/app/login"        => @proc_login(), ;
         "/app/logout"       => @proc_logout(), ;
         "/app/account"      => @proc_account(), ;
         "/app/account/edit" => @proc_account_edit(), ;
         "/app/register"     => @proc_register(), ;
         "/app/main"         => @proc_main(), ;
         "/app/shopping"     => @proc_shopping(), ;
         "/app/cart"         => @proc_cart(), ;
         "/"                 => {|| URedirect( "/app/login" ) } } } )
      oLogError:Close()
      oLogAccess:Close()
      ? "Server error:", oServer:cError
      ErrorLevel( 1 )
      RETURN
   ENDIF

   oLogError:Close()
   oLogAccess:Close()

   RETURN

STATIC FUNCTION proc_login()

   LOCAL cUser

   IF server[ "REQUEST_METHOD" ] == "POST"
      dbUseArea( .T., , "users", "users", .T., .T. )
      ordSetFocus( "user" )
      cUser := PadR( hb_HGetDef( post, "user", "" ), 16 )
      USessionStart()
      IF ! Empty( cUser ) .AND. dbSeek( cUser, .F. ) .AND. ! Deleted() .AND. ;
            PadR( hb_HGetDef( post, "password", "" ), 16 ) == FIELD->PASSWORD
         session[ "user" ] := cUser
         URedirect( "main" )
      ELSE
         URedirect( "login?err" )
         USessionDestroy()
      ENDIF
      dbCloseArea()
   ELSE
      IF "err" $ get
         RETURN { "errtext" => "Invalid user name or password!" }
      ENDIF
      RETURN { => }
   ENDIF

   RETURN NIL

STATIC FUNCTION proc_logout()

   USessionStart()
   USessionDestroy()

   RETURN { => }

STATIC FUNCTION proc_main()

   USessionStart()
   IF !( "user" $ session )
      URedirect( "/app/login" )
      RETURN NIL
   ENDIF

   RETURN { => }

STATIC FUNCTION proc_shopping()

   LOCAL oW, nT, cCode

   USessionStart()
   IF !( "user" $ session )
      URedirect( "/app/login" )
      RETURN NIL
   ENDIF

   dbUseArea( .T., , "carts", "carts", .T., .F. )
   ordSetFocus( "user" )
   dbUseArea( .T., , "items", "items", .T., .T. )
   ordSetFocus( "code" )

   IF "add" $ get
      cCode := PadR( get[ "add" ], 16 )
      IF items->( dbSeek( cCode ) ) .AND. carts->( FLock() )
         IF ! carts->( dbSeek( session[ "user" ] + cCode ) )
            carts->( dbAppend() )
            carts->USER := session[ "user" ]
            carts->CODE := cCode
         ENDIF
         carts->AMOUNT += 1
         carts->TOTAL += items->PRICE
         carts->( dbUnlock() )
      ENDIF
      URedirect( "shopping" )
      RETURN NIL
   ENDIF

   dbSelectArea( "carts" )
   ordScope( 0, session[ "user" ] )
   ordScope( 1, session[ "user" ] )
   nT := 0
   carts->( dbEval( {|| nT += FIELD->TOTAL } ) )
   dbSelectArea( "items" )
   oW := UWBrowseNew( "br_item" )
   oW:AddColumn( 101, "Item No.",    "CODE" )
   oW:AddColumn( 102, "Title",       "TITLE" )
   oW:AddColumn( 103, "Price",       "PRICE" )
   oW:AddColumn( 104, "",            {|| ULink( "Add to cart", "?add=" + RTrim( FIELD->CODE ) ) }, .T. )
   oW:nPageSize := 10
   IF "_pos" $ get
      oW:nPos := Val( get[ "_pos" ] )
   ENDIF

   RETURN { "browse" => oW:Output(), "cartsum" => nT }

STATIC FUNCTION proc_cart()

   LOCAL oW, nT, cCode

   USessionStart()
   IF !( "user" $ session )
      URedirect( "/app/login" )
      RETURN NIL
   ENDIF

   dbUseArea( .T., , "items", "items", .T., .T. )
   ordSetFocus( "code" )
   dbUseArea( .T., , "carts", "carts", .T., .F. )
   ordSetFocus( "user" )

   IF "del" $ get
      cCode := PadR( get[ "del" ], 16 )
      IF items->( dbSeek( cCode ) ) .AND. carts->( FLock() )
         IF carts->( dbSeek( session[ "user" ] + cCode ) )
            carts->( dbDelete() )
            carts->USER := ""
            carts->CODE := cCode
         ENDIF
         carts->( dbUnlock() )
      ENDIF
      URedirect( "cart" )
      RETURN NIL
   ENDIF

   ordScope( 0, session[ "user" ] )
   ordScope( 1, session[ "user" ] )
   nT := 0
   carts->( dbEval( {|| nT += FIELD->TOTAL } ) )

   oW := UWBrowseNew( "br_cart" )
   oW:AddColumn( 101, "Item No.",    "CODE" )
   oW:AddColumn( 102, "Title",       {|| items->( dbSeek( carts->CODE, .F. ), FIELD->TITLE ) } )
   oW:AddColumn( 103, "Amount",      "AMOUNT" )
   oW:AddColumn( 104, "Total",       "TOTAL" )
   oW:AddColumn( 104, "",            {|| ULink( "Delete", "?del=" + RTrim( FIELD->CODE ) ) }, .T. )
   oW:nPageSize := 10
   IF "_pos" $ get
      oW:nPos := Val( get[ "_pos" ] )
   ENDIF

   RETURN { "browse" => oW:Output(), "cartsum" => nT }

STATIC FUNCTION proc_account()

   USessionStart()
   IF !( "user" $ session )
      URedirect( "/app/login" )
      RETURN NIL
   ENDIF
   dbUseArea( .T., , "users", "users", .T., .F. )
   ordSetFocus( "user" )
   dbSeek( session[ "user" ], .F. )

   RETURN { "user" => users->USER, "name" => users->NAME }

STATIC FUNCTION proc_account_edit()

   LOCAL cName, cPassword1, cPassword2, aRet

   USessionStart()
   IF !( "user" $ session )
      URedirect( "/app/login" )
      RETURN NIL
   ENDIF
   dbUseArea( .T., , "users", "users", .T., .F. )
   ordSetFocus( "user" )
   dbSeek( session[ "user" ], .F. )

   cName := users->NAME
   IF "formdata_account/edit" $ session
      cName := session[ "formdata_account/edit", "name" ]
   ENDIF
   IF server[ "REQUEST_METHOD" ] == "POST"
      cName := hb_HGetDef( post, "name", "" )
      cPassword1 := hb_HGetDef( post, "password1", "" )
      cPassword2 := hb_HGetDef( post, "password2", "" )
      IF Empty( cName )
         session[ "formdata_account/edit" ] := { "name" => cName }
         URedirect( "?err=1" )
      ELSEIF ( ! Empty( cPassword1 ) .OR. ! Empty( cPassword2 ) ) .AND. !( cPassword1 == cPassword2 )
         session[ "formdata_account/edit" ] := { "name" => cName }
         URedirect( "?err=2" )
      ELSE
         FLock()
         FIELD->NAME := cName
         IF ! Empty( cPassword1 )
            FIELD->PASSWORD := cPassword1
         ENDIF
         dbUnlock()
         IF "formdata_account/edit" $ session
            hb_HDel( session, "formdata_account/edit" )
         ENDIF
         URedirect( "/app/account" )
      ENDIF
      RETURN NIL
   ENDIF

   aRet := { "user" => users->USER, "name" => cName }
   IF "err" $ get
      SWITCH get[ "err" ]
      CASE "1"
         aRet[ "errtext" ] := "Name value should not be empty!"
         EXIT
      CASE "2"
         aRet[ "errtext" ] := "Passwords do not match!"
         EXIT
      ENDSWITCH
   ENDIF

   RETURN aRet

STATIC FUNCTION proc_register()

   LOCAL cUser, cName, cPassword1, cPassword2, aRet

   USessionStart()
   cUser := ""
   cName := ""
   IF "formdata_register" $ session
      cUser := session[ "formdata_register", "user" ]
      cName := session[ "formdata_register", "name" ]
   ENDIF
   IF server[ "REQUEST_METHOD" ] == "POST"
      dbUseArea( .T., , "users", "users", .T., .F. )
      ordSetFocus( "user" )
      cUser := hb_HGetDef( post, "user", "" )
      cName := hb_HGetDef( post, "name", "" )
      cPassword1 := hb_HGetDef( post, "password1", "" )
      cPassword2 := hb_HGetDef( post, "password2", "" )

      IF Empty( cUser ) .OR. Empty( cName ) .OR. Empty( cPassword1 ) .OR. Empty( cPassword2 )
         session[ "formdata_register" ] := { "user" => cUser, "name" => cName }
         URedirect( "?err=1" )
      ELSEIF !( cPassword1 == cPassword2 )
         session[ "formdata_register" ] := { "user" => cUser, "name" => cName }
         URedirect( "?err=2" )
      ELSEIF dbSeek( cUser, .F. )
         session[ "formdata_register" ] := { "user" => cUser, "name" => cName }
         URedirect( "?err=3" )
      ELSE
         FLock()
         dbAppend()
         FIELD->USER := cUser
         FIELD->NAME := cName
         FIELD->PASSWORD := cPassword1
         dbUnlock()
         USessionDestroy()
         USessionStart()
         session[ "user" ] := cUser
         URedirect( "/app/main" )
      ENDIF
      RETURN NIL
   ENDIF
   aRet := { "user" => cUser, "name" => cName }
   IF "err" $ get
      SWITCH get[ "err" ]
      CASE "1"
         aRet[ "errtext" ] := "All fields are required!"
         EXIT
      CASE "2"
         aRet[ "errtext" ] := "Passwords does not match!"
         EXIT
      CASE "3"
         aRet[ "errtext" ] := "This user already exists!"
         EXIT
      ENDSWITCH
   ENDIF

   RETURN aRet
