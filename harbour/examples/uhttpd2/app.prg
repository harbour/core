/*
 * $Id$
 */

#include "hbclass.ch"
#include "common.ch"
#include "fileio.ch"

#define CR_LF    (CHR(13)+CHR(10))

REQUEST DBFCDX

MEMVAR server, get, post, cookie, session

STATIC s_aMap

FIELD USER, NAME, PASSWORD, CODE, PRICE, TOTAL, TITLE

FUNC main()
LOCAL oServer

  IF HB_ARGCHECK("help")
    ? "Usage: app [options]"
    ? "Options:"
    ? "  //help               Print help"
    ? "  //stop               Stop running server"
    RETURN 0
  ENDIF

  IF HB_ARGCHECK("stop")
    HB_MEMOWRIT(".uhttpd.stop", "")
    RETURN 0
  ELSE
    FERASE(".uhttpd.stop")
  ENDIF

  RDDSETDEFAULT("DBFCDX")
  SET(_SET_DATEFORMAT, "yyyy-mm-dd")

  IF ! HB_FILEEXISTS("users.dbf")
    FERASE("users.cdx")
    DBCREATE("users", {{"USER", "C", 16, 0}, {"PASSWORD", "C", 16, 0}, {"NAME", "C", 50, 0}},, .T., "user")
    OrdCreate("users", "user", "USER")
    DBCLOSEAREA()
  ELSEIF ! HB_FILEEXISTS("users.cdx")
    DBUSEAREA(.T.,, "users",, .F., .F.)
    OrdCreate("users", "user", "USER")
    DBCLOSEAREA()
  ENDIF

  IF ! HB_FILEEXISTS("carts.dbf")
    FERASE("carts.cdx")
    DBCREATE("carts", {{"USER", "C", 16, 0}, {"CODE", "C", 16, 0}, {"AMOUNT", "N", 6, 0}, {"TOTAL", "N", 9, 2}},, .T., "cart")
    OrdCreate("carts", "user", "USER+CODE")
    DBCLOSEAREA()
  ELSEIF ! HB_FILEEXISTS("carts.cdx")
    DBUSEAREA(.T.,, "carts",, .F., .F.)
    OrdCreate("carts", "user", "USER+CODE")
    DBCLOSEAREA()
  ENDIF

  IF ! HB_FILEEXISTS("items.dbf")
    FERASE("items.cdx")
    DBCREATE("items", {{"CODE", "C", 16, 0}, {"TITLE", "C", 80, 0}, {"PRICE", "N", 9, 2}},, .T., "items")
    OrdCreate("items", "code", "CODE")
    DBCLOSEAREA()
  ELSEIF ! HB_FILEEXISTS("item.cdx")
    DBUSEAREA(.T.,, "items",, .F., .F.)
    OrdCreate("items", "code", "CODE")
    DBCLOSEAREA()
  ENDIF

  oServer := UHttpdNew()

  oServer:nPort := 8002
  oServer:bIdle := {|o| IIF(HB_FILEEXISTS(".uhttpd.stop"), (FERASE(".uhttpd.stop"), o:Stop()), NIL)}


  s_aMap := {"login"        => @proc_login(), ;
             "logout"       => @proc_logout(), ;
             "register"     => @proc_register(), ;
             "account"      => @proc_account(), ;
             "account/edit" => @proc_account_edit(), ;
             "main"         => @proc_main(), ;
             "shopping"     => @proc_shopping(), ;
             "cart"         => @proc_cart()}

  oServer:aMount := ;
     {"/hello"  => {{|| UWrite("Hello!")}, .F.}, ;
      "/info"   => {{|| UProcInfo()}, .F.}, ;
      "/files/*"=> {{|x| UProcFiles( hb_dirBase() + "files/" + X, .F.)}, .F.}, ;
      "/app/*"  => {{|x| UProcWidgets(x, s_aMap)}, .T.}, ;
      "/*"      => {{|| URedirect("/app/login")}, .F.}}

  IF ! oServer:Run()
    ? "Server error:", oServer:cError
    RETURN 1
  ENDIF

RETURN 0


STATIC FUNC proc_login(cMethod)
LOCAL cUser, oM, oF, oG
  ? PROCNAME(), cMethod
  IF cMethod == "INIT"
    oM := UWMainNew()
    oM:Add( UWLabelNew("", "errtxt", "color:red; font-weight:bold;") )
    oM:Add( oF := UWFormNew("") )
    oF:Add( oG := UWLayoutGridNew() )
    oG:Add( UWHtmlNew("User"), 1, 1 )
    oG:Add( UWInputNew("user"), 1, 2 )
    oG:Add( UWHtmlNew("Password"), 2, 1 )
    oG:Add( UWPasswordNew("password"), 2, 2 )
    oG:Add( UWSubmitNew("submit", "Login"), 3, 2 )
    oM:Add( UWHtmlNew(ULink("Register", "register")) )
  ELSEIF cMethod == "POST"
    DBUSEAREA(.T.,, "users", "users", .T., .T.)
    OrdSetFocus("user")
    cUser := PADR(HGetDef(post, "user", ""), 16)
    IF !EMPTY(cUser) .AND. DBSEEK(cUser, .F.) .AND. ! DELETED() .AND. ;
       PADR(HGetDef(post, "password", ""), 16) == FIELD->PASSWORD
      session["loggedin"] := cUser
      URedirect("main")
    ELSE
      URedirect("login?err")
      USessionDestroy()
    ENDIF
    DBCLOSEAREA()
  ELSEIF cMethod == "GET"
    IF HB_HHasKey(get, "err")
      GetWidgetById("errtxt"):cText := "Invalid username or password!"
    ENDIF
    UWDefaultHandler(cMethod)
    USessionDestroy()
  ENDIF
RETURN .T.


STATIC FUNC proc_register(cMethod)
LOCAL cUser, cName, cPassword, cPassword2, oM, oF, oG
  ? PROCNAME(), cMethod
  IF cMethod == "INIT"
    oM := UWMainNew()
    oM:Add( UWLabelNew("", "errtxt", "color:red; font-weight:bold;") )
    oM:Add( oF := UWFormNew("") )
    oF:Add( oG := UWLayoutGridNew() )
    oG:Add( UWHtmlNew("User name"), 1, 1 )
    oG:Add( UWInputNew("user",, "user"), 1, 2 )
    oG:Add( UWHtmlNew("Name"), 2, 1 )
    oG:Add( UWInputNew("name",, "name"), 2, 2 )
    oG:Add( UWHtmlNew("Password"), 3, 1 )
    oG:Add( UWPasswordNew("password"), 3, 2 )
    oG:Add( UWHtmlNew("Password again"), 4, 1 )
    oG:Add( UWPasswordNew("password2"), 4, 2 )
    oG:Add( UWSubmitNew("register", "Register"), 5, 2 )
  ELSEIF cMethod == "POST"
    DBUSEAREA(.T.,, "users", "users", .T., .F.)
    OrdSetFocus("user")
    cUser := HGetDef(post, "user", "")
    cName := HGetDef(post, "name", "")
    cPassword := HGetDef(post, "password", "")
    cPassword2 := HGetDef(post, "password2", "")
    GetWidgetById("user"):cValue := cUser
    GetWidgetById("name"):cValue := cName
    IF EMPTY(cUser) .OR. EMPTY(cName) .OR. EMPTY(cPassword) .OR. EMPTY(cPassword2)
      URedirect("?err=1")
    ELSEIF !(cPassword == cPassword2)
      URedirect("?err=2")
    ELSEIF DBSEEK(cUser, .F.)
      URedirect("?err=3")
    ELSE
      FLOCK()
      DBAPPEND()
      USER := cUser
      NAME := cName
      PASSWORD := cPassword
      DBUNLOCK()
      session["loggedin"] := cUser
      URedirect("main")
    ENDIF
    DBCLOSEAREA()
  ELSEIF cMethod == "GET"
    IF HB_HHasKey(get, "err")
      IF get["err"] == "1"
        GetWidgetById("errtxt"):cText := "All fields are required!"
      ELSEIF get["err"] == "2"
        GetWidgetById("errtxt"):cText := "Passwords does not match!"
      ELSEIF get["err"] == "3"
        GetWidgetById("errtxt"):cText := "This user already exists!"
      ENDIF
    ENDIF
    UWDefaultHandler(cMethod)
  ENDIF
RETURN .T.


STATIC FUNC proc_account(cMethod)
LOCAL cUser, cName, oM, oG
  ? PROCNAME(), cMethod
  IF cMethod == "INIT"
    IF ! HB_HHasKey(session, "loggedin");  URedirect("/app/login");  RETURN .F.
    ENDIF
    DBUSEAREA(.T.,, "users", "users", .T., .F.)
    OrdSetFocus("user")
  ELSEIF cMethod == "GET"
    DBSEEK(session["loggedin"], .F.)
    /* Create object here because user name can be changed in account/edit */
    oM := UWMainNew()
    oM:Add( UWMenuNew():AddItem("Shopping",   "shopping"):AddItem("Cart", "cart"):AddItem("Logout", "logout") )
    oM:Add( UWSeparatorNew() )
    oM:Add( oG := UWLayoutGridNew() )
    oG:Add( UWHtmlNew("User name:"), 1, 1 )
    oG:Add( UWHtmlNew(session["loggedin"]), 1, 2 )
    oG:Add( UWHtmlNew("Name:"), 2, 1 )
    oG:Add( UWHtmlNew(NAME), 2, 2 )
    oM:Add( UWHtmlNew(ULink("Edit", "account/edit")) )
    UWDefaultHandler(cMethod)
  ELSEIF cMethod == "EXIT"
    users->(DBCLOSEAREA())
  ENDIF
RETURN .T.


STATIC FUNC proc_account_edit(cMethod)
LOCAL cName, cPassword, cPassword2, oM, oG, oF
  ? PROCNAME(), cMethod
  IF cMethod == "INIT"
    IF ! HB_HHasKey(session, "loggedin");  URedirect("/app/login");  RETURN .F.
    ENDIF
    DBSEEK(session["loggedin"], .F.)
    oM := UWMainNew()
    oM:Add( UWLabelNew("", "errtxt", "color:red; font-weight:bold;") )
    oM:Add( oF := UWFormNew("") )
    oF:Add( oG := UWLayoutGridNew() )
    oG:Add( UWHtmlNew("User name"), 1, 1 )
    oG:Add( UWHtmlNew(session["loggedin"]), 1, 2 )
    oG:Add( UWHtmlNew("Name"), 2, 1 )
    oG:Add( UWInputNew("name", TRIM(NAME), "name"), 2, 2 )
    oG:Add( UWHtmlNew("Password"), 3, 1 )
    oG:Add( UWPasswordNew("password"), 3, 2 )
    oG:Add( UWHtmlNew("Password again"), 4, 1 )
    oG:Add( UWPasswordNew("password2"), 4, 2 )
    oG:Add( UWSubmitNew("save", "Save"), 5, 2 )
  ELSEIF cMethod == "POST"
    DBSEEK(session["loggedin"], .F.)
    cName := HGetDef(post, "name", "")
    cPassword := HGetDef(post, "password", "")
    cPassword2 := HGetDef(post, "password2", "")
    GetWidgetById("name"):cValue := TRIM(cName)
    IF EMPTY(cName)
      URedirect("?err=1")
    ELSEIF (! EMPTY(cPassword) .OR. ! EMPTY(cPassword2)) .AND. ! (cPassword == cPassword2)
      URedirect("?err=2")
    ELSE
      FLOCK()
      NAME := cName
      QOUT("PO DBAPPEND", ALIAS(), RECNO(), cName)
      IF ! EMPTY(cPassword)
         PASSWORD := cPassword
      ENDIF
      DBUNLOCK()
      URedirect("../account")
    ENDIF
  ELSEIF cMethod == "GET"
    IF HB_HHasKey(get, "err")
      IF get["err"] == "1"
        GetWidgetById("errtxt"):cText := "All fields are required!"
      ELSEIF get["err"] == "2"
        GetWidgetById("errtxt"):cText := "Passwords does not match!"
      ENDIF
    ENDIF
    UWDefaultHandler(cMethod)
  ELSEIF cMethod == "EXIT"
  ENDIF
RETURN .T.


STATIC FUNC proc_main(cMethod)
LOCAL oM
  ? PROCNAME(), cMethod
  IF cMethod == "INIT"
    IF ! HB_HHasKey(session, "loggedin");  URedirect("/app/login");  RETURN .F.
    ENDIF
    oM := UWMainNew()
    oM:Add( UWMenuNew():AddItem("Shopping",   "shopping");
                       :AddItem("Cart",       "cart");
                       :AddItem("My account", "account");
                       :AddItem("Logout",     "logout") )
    oM:Add( UWSeparatorNew() )
    oM:Add( UWLabelNew("You can do shopping, or edit your cart using menu links above") )
  ELSEIF cMethod == "GET"
    UWDefaultHandler(cMethod)
  ENDIF
RETURN .T.


STATIC FUNC proc_shopping(cMethod)
LOCAL oM, oW, nT, cCode
  ? PROCNAME(), cMethod
  IF cMethod == "INIT"
    IF ! HB_HHasKey(session, "loggedin");  URedirect("/app/login");  RETURN .F.
    ENDIF
    oM := UWMainNew()
    oM:Add( UWMenuNew():AddItem("Cart", "cart"):AddItem("My account", "account"):AddItem("Logout", "logout") )
    oM:Add( UWSeparatorNew() )
    oM:Add( UWLabelNew("", "cartsum") )

    DBUSEAREA(.T.,, "carts", "carts", .T., .F.)
    OrdSetFocus("user")
    ORDSCOPE(0, session["loggedin"])
    ORDSCOPE(1, session["loggedin"])
    DBUSEAREA(.T.,, "items", "items", .T., .T.)
    OrdSetFocus("code")
    oW := UWBrowseNew("1")
    oW:AddColumn(101, "Item No.",    "CODE")
    oW:AddColumn(102, "Title",       "TITLE")
    oW:AddColumn(103, "Price",       "PRICE")
    oW:AddColumn(104, "",            {|| ULink("Add to cart", "?add=" + TRIM(CODE))}, .T.)
    oM:Add( oW )
  ELSEIF cMethod == "GET"
    IF HB_HHasKey(get, "add")
      cCode := PADR(get["add"], 16)
      IF items->(DBSEEK(cCode)) .AND. carts->(FLOCK())
        IF ! carts->(DBSEEK(session["loggedin"] + cCode))
          carts->(DBAPPEND())
          carts->USER := session["loggedin"]
          carts->CODE := cCode
        ENDIF
        carts->AMOUNT += 1
        carts->TOTAL += items->PRICE
        carts->(DBUNLOCK())
      ENDIF
      URedirect("shopping")
      RETURN .T.
    ENDIF
    nT := 0
    carts->(DBEVAL({|| nT += TOTAL}))
    GetWidgetById("cartsum"):cText := "Your cart is worth: " + LTRIM(STR(nT))
    UWDefaultHandler(cMethod)
  ELSEIF cMethod == "EXIT"
    items->(DBCLOSEAREA())
    carts->(DBCLOSEAREA())
  ENDIF
RETURN .T.


STATIC FUNC proc_cart(cMethod)
LOCAL oM, oW, nT, cCode
  ? PROCNAME(), cMethod
  IF cMethod == "INIT"
    IF ! HB_HHasKey(session, "loggedin");  URedirect("/app/login");  RETURN .F.
    ENDIF
    oM := UWMainNew()
    oM:Add( UWMenuNew():AddItem("Shopping", "shopping"):AddItem("My account", "account"):AddItem("Logout", "logout") )
    oM:Add( UWSeparatorNew() )
    oM:Add( UWLabelNew("", "cartsum") )

    DBUSEAREA(.T.,, "items", "items", .T., .T.)
    OrdSetFocus("code")
    DBUSEAREA(.T.,, "carts", "carts", .T., .F.)
    OrdSetFocus("user")
    ORDSCOPE(0, session["loggedin"])
    ORDSCOPE(1, session["loggedin"])
    oW := UWBrowseNew("1")
    oW:AddColumn(101, "Item No.",    "CODE")
    oW:AddColumn(102, "Title",       {|| items->(DBSEEK(carts->CODE, .F.), TITLE)})
    oW:AddColumn(103, "Amount",      "AMOUNT")
    oW:AddColumn(104, "Total",       "TOTAL")
    oW:AddColumn(104, "",            {|| ULink("Delete", "?del=" + TRIM(CODE))}, .T.)
    oM:Add( oW )
  ELSEIF cMethod == "GET"
    IF HB_HHasKey(get, "del")
      cCode := PADR(get["del"], 16)
      IF items->(DBSEEK(cCode)) .AND. carts->(FLOCK())
        IF carts->(DBSEEK(session["loggedin"] + cCode))
          carts->(DBDELETE())
          carts->USER := ""
          carts->CODE := cCode
        ENDIF
        carts->(DBUNLOCK())
      ENDIF
      URedirect("cart")
      RETURN .T.
    ENDIF
    nT := 0
    carts->(DBEVAL({|| nT += TOTAL}))
    GetWidgetById("cartsum"):cText := "Your cart is worth: " + LTRIM(STR(nT))
    UWDefaultHandler(cMethod)
  ELSEIF cMethod == "EXIT"
    items->(DBCLOSEAREA())
    carts->(DBCLOSEAREA())
  ENDIF
RETURN .T.


STATIC FUNC proc_logout(cMethod)
LOCAL oM
  ? PROCNAME(), cMethod
  IF cMethod == "INIT"
    IF ! HB_HHasKey(session, "loggedin");  URedirect("/app/login");  RETURN .F.
    ENDIF
    oM := UWMainNew()
    oM:Add( UWMenuNew():AddItem("Login", "login") )
    oM:Add( UWSeparatorNew() )
    oM:Add( UWLabelNew("Your session is ended.") )
  ELSEIF cMethod == "GET"
    UWDefaultHandler(cMethod)
    USessionDestroy()
  ENDIF
RETURN .T.
