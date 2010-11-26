/*
 * $Id$
 */

REQUEST DBFCDX

MEMVAR server, get, post, cookie, session

FUNCTION Main()

   LOCAL oServer

   LOCAL oLogAccess
   LOCAL oLogError

   LOCAL aMap

   IF HB_ARGCHECK( "help" )
      ? "Usage: app [options]"
      ? "Options:"
      ? "  //help               Print help"
      ? "  //stop               Stop running server"
      RETURN 0
   ENDIF

   IF HB_ARGCHECK( "stop" )
      HB_MEMOWRIT( ".uhttpd.stop", "" )
      RETURN 0
   ELSE
      FErase( ".uhttpd.stop" )
   ENDIF

   rddSetDefault( "DBFCDX" )
   SET( _SET_DATEFORMAT, "yyyy-mm-dd" )

   IF ! HB_FILEEXISTS( "users.dbf" )
      FErase( "users.cdx" )
      dbCreate( "users", { { "USER", "C", 16, 0 }, { "PASSWORD", "C", 16, 0 }, { "NAME", "C", 50, 0 } }, , .T. , "user" )
      OrdCreate( "users", "user", "USER" )
      dbCloseArea()
   ELSEIF ! HB_FILEEXISTS( "users.cdx" )
      dbUseArea( .T. , , "users", , .F. , .F. )
      OrdCreate( "users", "user", "USER" )
      dbCloseArea()
   ENDIF

   IF ! HB_FILEEXISTS( "carts.dbf" )
      FErase( "carts.cdx" )
      dbCreate( "carts", { { "USER", "C", 16, 0 }, { "CODE", "C", 16, 0 }, { "AMOUNT", "N", 6, 0 }, { "TOTAL", "N", 9, 2 } }, , .T. , "cart" )
      OrdCreate( "carts", "user", "USER+CODE" )
      dbCloseArea()
   ELSEIF ! HB_FILEEXISTS( "carts.cdx" )
      dbUseArea( .T. , , "carts", , .F. , .F. )
      OrdCreate( "carts", "user", "USER+CODE" )
      dbCloseArea()
   ENDIF

   IF ! HB_FILEEXISTS( "items.dbf" )
      FErase( "items.cdx" )
      dbCreate( "items", { { "CODE", "C", 16, 0 }, { "TITLE", "C", 80, 0 }, { "PRICE", "N", 9, 2 } }, , .T. , "items" )
      OrdCreate( "items", "code", "CODE" )
      dbCloseArea()
   ELSEIF ! HB_FILEEXISTS( "item.cdx" )
      dbUseArea( .T. , , "items", , .F. , .F. )
      OrdCreate( "items", "code", "CODE" )
      dbCloseArea()
   ENDIF

   oLogAccess := UHttpdLog():New( "webapp_access.log" )

   IF ! oLogAccess:Add( "" )
      oLogAccess:Close()
      ? "Access log file open error " + hb_ntos( FError() )
      RETURN 0
   ENDIF

   oLogError := UHttpdLog():New( "webapp_error.log" )

   IF ! oLogError:Add( "" )
      oLogError:Close()
      oLogAccess:Close()
      ? "Error log file open error " + hb_ntos( FError() )
      RETURN 0
   ENDIF

   oServer := UHttpdNew()

   oServer:bLogAccess := {| m | oLogAccess:Add( m + hb_eol() ) }
   oServer:bLogError  := {| m | oLogError:Add( m + hb_eol() ) }
   oServer:bTrace     := {| ... | QOut( ... ) }

   oServer:nPort := 8002
   oServer:bIdle := { |o| iif( HB_FILEEXISTS( ".uhttpd.stop" ), ( FErase(".uhttpd.stop" ), o:Stop() ), NIL ) }


   aMap := {;
      "login"        => @proc_login(), ;
      "logout"       => @proc_logout(), ;
      "register"     => @proc_register(), ;
      "account"      => @proc_account(), ;
      "account/edit" => @proc_account_edit(), ;
      "main"         => @proc_main(), ;
      "shopping"     => @proc_shopping(), ;
      "cart"         => @proc_cart() }

   oServer:aMount := {;
      "/hello"   => { {|| UWrite( "Hello!" ) }, .F. }, ;
      "/info"    => { {|| UProcInfo() }, .F. }, ;
      "/files/*" => { {|x| UProcFiles( hb_dirBase() + "files/" + x, .F. ) }, .F. }, ;
      "/app/*"   => { {|x| UProcWidgets( x, aMap ) }, .T. }, ;
      "/*"       => { {|| URedirect( "/app/login" ) }, .F. } }

   ? "Listening on port:", oServer:nPort

   IF ! oServer:Run()
      oLogError:Close()
      oLogAccess:Close()
      ? "Server error:", oServer:cError
      RETURN 1
   ENDIF

   oLogError:Close()
   oLogAccess:Close()

   RETURN 0

STATIC FUNCTION proc_login( cMethod )

   LOCAL cUser, oM, oF, oG

   ? ProcName(), cMethod
   IF cMethod == "INIT"
      oM := UWMainNew()
      oM:Add( UWLabelNew( "", "errtxt", "color:red; font-weight:bold;" ) )
      oM:Add( oF := UWFormNew( "" ) )
      oF:Add( oG := UWLayoutGridNew() )
      oG:Add( UWHtmlNew( "User" ), 1, 1 )
      oG:Add( UWInputNew( "user" ), 1, 2 )
      oG:Add( UWHtmlNew( "Password" ), 2, 1 )
      oG:Add( UWPasswordNew( "password" ), 2, 2 )
      oG:Add( UWSubmitNew( "submit", "Login" ), 3, 2 )
      oM:Add( UWHtmlNew( ULink("Register", "register" ) ) )
   ELSEIF cMethod == "POST"
      dbUseArea( .T. , , "users", "users", .T. , .T. )
      OrdSetFocus( "user" )
      cUser := PadR( hb_HGetDef( post, "user", "" ), 16 )
      IF !Empty( cUser ) .AND. dbSeek( cUser, .F. ) .AND. ! Deleted() .AND. ;
            PadR( hb_HGetDef( post, "password", "" ), 16 ) == FIELD->PASSWORD
         session[ "loggedin" ] := cUser
         URedirect( "main" )
      ELSE
         URedirect( "login?err" )
         USessionDestroy()
      ENDIF
      dbCloseArea()
   ELSEIF cMethod == "GET"
      IF HB_HHasKey( get, "err" )
         UGetWidgetById( "errtxt" ):cText := "Invalid username or password!"
      ENDIF
      UWDefaultHandler( cMethod )
      USessionDestroy()
   ENDIF

   RETURN .T.

STATIC FUNCTION proc_register( cMethod )

   LOCAL cUser, cName, cPassword, cPassword2, oM, oF, oG

   ? ProcName(), cMethod
   IF cMethod == "INIT"
      oM := UWMainNew()
      oM:Add( UWLabelNew( "", "errtxt", "color:red; font-weight:bold;" ) )
      oM:Add( oF := UWFormNew( "" ) )
      oF:Add( oG := UWLayoutGridNew() )
      oG:Add( UWHtmlNew( "User name" ), 1, 1 )
      oG:Add( UWInputNew( "user",, "user" ), 1, 2 )
      oG:Add( UWHtmlNew( "Name" ), 2, 1 )
      oG:Add( UWInputNew( "name",, "name" ), 2, 2 )
      oG:Add( UWHtmlNew( "Password" ), 3, 1 )
      oG:Add( UWPasswordNew( "password" ), 3, 2 )
      oG:Add( UWHtmlNew( "Password again" ), 4, 1 )
      oG:Add( UWPasswordNew( "password2" ), 4, 2 )
      oG:Add( UWSubmitNew( "register", "Register" ), 5, 2 )
   ELSEIF cMethod == "POST"
      dbUseArea( .T. , , "users", "users", .T. , .F. )
      OrdSetFocus( "user" )
      cUser := hb_HGetDef( post, "user", "" )
      cName := hb_HGetDef( post, "name", "" )
      cPassword := hb_HGetDef( post, "password", "" )
      cPassword2 := hb_HGetDef( post, "password2", "" )
      UGetWidgetById( "user" ):cValue := cUser
      UGetWidgetById( "name" ):cValue := cName
      IF Empty( cUser ) .OR. Empty( cName ) .OR. Empty( cPassword ) .OR. Empty( cPassword2 )
         URedirect( "?err=1" )
      ELSEIF !( cPassword == cPassword2 )
         URedirect( "?err=2" )
      ELSEIF dbSeek( cUser, .F. )
         URedirect( "?err=3" )
      ELSE
         FLock()
         dbAppend()
         FIELD->USER := cUser
         FIELD->NAME := cName
         FIELD->PASSWORD := cPassword
         dbUnlock()
         session[ "loggedin" ] := cUser
         URedirect( "main" )
      ENDIF
      dbCloseArea()
   ELSEIF cMethod == "GET"
      IF HB_HHasKey( get, "err" )
         IF get[ "err" ] == "1"
            UGetWidgetById( "errtxt" ):cText := "All fields are required!"
         ELSEIF get[ "err" ] == "2"
            UGetWidgetById( "errtxt" ):cText := "Passwords does not match!"
         ELSEIF get[ "err" ] == "3"
            UGetWidgetById( "errtxt" ):cText := "This user already exists!"
         ENDIF
      ENDIF
      UWDefaultHandler( cMethod )
   ENDIF

   RETURN .T.

STATIC FUNCTION proc_account( cMethod )

   LOCAL oM, oG

   ? ProcName(), cMethod
   IF cMethod == "INIT"
      IF ! HB_HHasKey( session, "loggedin" );  URedirect( "/app/login" );  RETURN .F.
      ENDIF
      dbUseArea( .T. , , "users", "users", .T. , .F. )
      OrdSetFocus( "user" )
   ELSEIF cMethod == "GET"
      dbSeek( session[ "loggedin" ], .F. )
      /* Create object here because user name can be changed in account/edit */
      oM := UWMainNew()
      oM:Add( UWMenuNew():AddItem( "Shopping",   "shopping" ):AddItem( "Cart", "cart" ):AddItem( "Logout", "logout" ) )
      oM:Add( UWSeparatorNew() )
      oM:Add( oG := UWLayoutGridNew() )
      oG:Add( UWHtmlNew( "User name:" ), 1, 1 )
      oG:Add( UWHtmlNew( session[ "loggedin" ] ), 1, 2 )
      oG:Add( UWHtmlNew( "Name:" ), 2, 1 )
      oG:Add( UWHtmlNew( FIELD->NAME ), 2, 2 )
      oM:Add( UWHtmlNew( ULink("Edit", "account/edit" ) ) )
      UWDefaultHandler( cMethod )
   ELSEIF cMethod == "EXIT"
      users->( dbCloseArea() )
   ENDIF

   RETURN .T.

STATIC FUNCTION proc_account_edit( cMethod )

   LOCAL cName, cPassword, cPassword2, oM, oG, oF

   ? ProcName(), cMethod
   IF cMethod == "INIT"
      IF ! HB_HHasKey( session, "loggedin" );  URedirect( "/app/login" );  RETURN .F.
      ENDIF
      dbSeek( session[ "loggedin" ], .F. )
      oM := UWMainNew()
      oM:Add( UWLabelNew( "", "errtxt", "color:red; font-weight:bold;" ) )
      oM:Add( oF := UWFormNew( "" ) )
      oF:Add( oG := UWLayoutGridNew() )
      oG:Add( UWHtmlNew( "User name" ), 1, 1 )
      oG:Add( UWHtmlNew( session[ "loggedin" ] ), 1, 2 )
      oG:Add( UWHtmlNew( "Name" ), 2, 1 )
      oG:Add( UWInputNew( "name", RTrim( FIELD->NAME ), "name" ), 2, 2 )
      oG:Add( UWHtmlNew( "Password" ), 3, 1 )
      oG:Add( UWPasswordNew( "password" ), 3, 2 )
      oG:Add( UWHtmlNew( "Password again" ), 4, 1 )
      oG:Add( UWPasswordNew( "password2" ), 4, 2 )
      oG:Add( UWSubmitNew( "save", "Save" ), 5, 2 )
   ELSEIF cMethod == "POST"
      dbSeek( session[ "loggedin" ], .F. )
      cName := hb_HGetDef( post, "name", "" )
      cPassword := hb_HGetDef( post, "password", "" )
      cPassword2 := hb_HGetDef( post, "password2", "" )
      UGetWidgetById( "name" ):cValue := RTrim( cName )
      IF Empty( cName )
         URedirect( "?err=1" )
      ELSEIF ( ! Empty( cPassword ) .OR. ! Empty( cPassword2 ) ) .AND. ! ( cPassword == cPassword2 )
         URedirect( "?err=2" )
      ELSE
         FLock()
         FIELD->NAME := cName
         QOut( "PO DBAPPEND", Alias(), RecNo(), cName )
         IF ! Empty( cPassword )
            FIELD->PASSWORD := cPassword
         ENDIF
         dbUnlock()
         URedirect( "../account" )
      ENDIF
   ELSEIF cMethod == "GET"
      IF HB_HHasKey( get, "err" )
         IF get[ "err" ] == "1"
            UGetWidgetById( "errtxt" ):cText := "All fields are required!"
         ELSEIF get[ "err" ] == "2"
            UGetWidgetById( "errtxt" ):cText := "Passwords do not match!"
         ENDIF
      ENDIF
      UWDefaultHandler( cMethod )
   ELSEIF cMethod == "EXIT"
   ENDIF

   RETURN .T.

STATIC FUNCTION proc_main( cMethod )

   LOCAL oM

   ? ProcName(), cMethod
   IF cMethod == "INIT"
      IF ! HB_HHasKey( session, "loggedin" );  URedirect( "/app/login" );  RETURN .F.
      ENDIF
      oM := UWMainNew()
      oM:Add( UWMenuNew():AddItem( "Shopping",   "shopping" );
                         :AddItem( "Cart",       "cart" );
                         :AddItem( "My account", "account" );
                         :AddItem( "Logout",     "logout" ) )
      oM:Add( UWSeparatorNew() )
      oM:Add( UWLabelNew( "You can do shopping, or edit your cart using menu links above" ) )
   ELSEIF cMethod == "GET"
      UWDefaultHandler( cMethod )
   ENDIF

   RETURN .T.

STATIC FUNCTION proc_shopping( cMethod )

   LOCAL oM, oW, nT, cCode

   ? ProcName(), cMethod
   IF cMethod == "INIT"
      IF ! HB_HHasKey( session, "loggedin" );  URedirect( "/app/login" );  RETURN .F.
      ENDIF
      oM := UWMainNew()
      oM:Add( UWMenuNew():AddItem( "Cart", "cart" ):AddItem( "My account", "account" ):AddItem( "Logout", "logout" ) )
      oM:Add( UWSeparatorNew() )
      oM:Add( UWLabelNew( "", "cartsum" ) )

      dbUseArea( .T. , , "carts", "carts", .T. , .F. )
      OrdSetFocus( "user" )
      ORDSCOPE( 0, session[ "loggedin" ] )
      ORDSCOPE( 1, session[ "loggedin" ] )
      dbUseArea( .T. , , "items", "items", .T. , .T. )
      OrdSetFocus( "code" )
      oW := UWBrowseNew( "1" )
      oW:AddColumn( 101, "Item No.",    "CODE" )
      oW:AddColumn( 102, "Title",       "TITLE" )
      oW:AddColumn( 103, "Price",       "PRICE" )
      oW:AddColumn( 104, "",            {|| ULink( "Add to cart", "?add=" + RTrim( FIELD->CODE ) ) }, .T. )
      oM:Add( oW )
   ELSEIF cMethod == "GET"
      IF HB_HHasKey( get, "add" )
         cCode := PadR( get[ "add" ], 16 )
         IF items->( dbSeek( cCode ) ) .AND. carts->( FLock() )
            IF ! carts->( dbSeek( session[ "loggedin" ] + cCode ) )
               carts->( dbAppend() )
               carts->USER := session[ "loggedin" ]
               carts->CODE := cCode
            ENDIF
            carts->AMOUNT += 1
            carts->TOTAL += items->PRICE
            carts->( dbUnlock() )
         ENDIF
         URedirect( "shopping" )
         RETURN .T.
      ENDIF
      nT := 0
      carts->( dbEval( {|| nT += FIELD->TOTAL } ) )
      UGetWidgetById( "cartsum" ):cText := "Your cart is worth: " + hb_ntos( nT )
      UWDefaultHandler( cMethod )
   ELSEIF cMethod == "EXIT"
      items->( dbCloseArea() )
      carts->( dbCloseArea() )
   ENDIF

   RETURN .T.

STATIC FUNCTION proc_cart( cMethod )

   LOCAL oM, oW, nT, cCode

   ? ProcName(), cMethod
   IF cMethod == "INIT"
      IF ! HB_HHasKey( session, "loggedin" );  URedirect( "/app/login" );  RETURN .F.
      ENDIF
      oM := UWMainNew()
      oM:Add( UWMenuNew():AddItem( "Shopping", "shopping" ):AddItem( "My account", "account" ):AddItem( "Logout", "logout" ) )
      oM:Add( UWSeparatorNew() )
      oM:Add( UWLabelNew( "", "cartsum" ) )

      dbUseArea( .T. , , "items", "items", .T. , .T. )
      OrdSetFocus( "code" )
      dbUseArea( .T. , , "carts", "carts", .T. , .F. )
      OrdSetFocus( "user" )
      ORDSCOPE( 0, session[ "loggedin" ] )
      ORDSCOPE( 1, session[ "loggedin" ] )
      oW := UWBrowseNew( "1" )
      oW:AddColumn( 101, "Item No.",    "CODE" )
      oW:AddColumn( 102, "Title",       {|| items->( dbSeek(carts->CODE, .F. ), FIELD->TITLE ) } )
      oW:AddColumn( 103, "Amount",      "AMOUNT" )
      oW:AddColumn( 104, "Total",       "TOTAL" )
      oW:AddColumn( 104, "",            {|| ULink( "Delete", "?del=" + RTrim( FIELD->CODE ) ) }, .T. )
      oM:Add( oW )
   ELSEIF cMethod == "GET"
      IF HB_HHasKey( get, "del" )
         cCode := PadR( get[ "del" ], 16 )
         IF items->( dbSeek( cCode ) ) .AND. carts->( FLock() )
            IF carts->( dbSeek( session[ "loggedin" ] + cCode ) )
               carts->( dbDelete() )
               carts->USER := ""
               carts->CODE := cCode
            ENDIF
            carts->( dbUnlock() )
         ENDIF
         URedirect( "cart" )
         RETURN .T.
      ENDIF
      nT := 0
      carts->( dbEval( {|| nT += FIELD->TOTAL } ) )
      UGetWidgetById( "cartsum" ):cText := "Your cart is worth: " + hb_ntos( nT )
      UWDefaultHandler( cMethod )
   ELSEIF cMethod == "EXIT"
      items->( dbCloseArea() )
      carts->( dbCloseArea() )
   ENDIF

   RETURN .T.

STATIC FUNCTION proc_logout( cMethod )

   LOCAL oM

   ? ProcName(), cMethod
   IF cMethod == "INIT"
      IF ! HB_HHasKey( session, "loggedin" );  URedirect( "/app/login" );  RETURN .F.
      ENDIF
      oM := UWMainNew()
      oM:Add( UWMenuNew():AddItem( "Login", "login" ) )
      oM:Add( UWSeparatorNew() )
      oM:Add( UWLabelNew( "Your session is ended." ) )
   ELSEIF cMethod == "GET"
      UWDefaultHandler( cMethod )
      USessionDestroy()
   ENDIF

   RETURN .T.
