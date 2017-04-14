/* Copyright 2016 Viktor Szakats (vszakats.net/harbour) */

#require "hbtip"

#include "simpleio.ch"

PROCEDURE Main( cURL )

   LOCAL oURL := TURL():New( hb_defaultValue( cURL, "https://user:passwd@example.com:443/mypages/mysite/page.html?avar=0&avar1=1" ) )

   ? "cAddress"  , oURL:cAddress
   ? "cProto"    , oURL:cProto
   ? "cUserid"   , oURL:cUserid
   ? "cPassword" , oURL:cPassword
   ? "cServer"   , oURL:cServer
   ? "cPath"     , oURL:cPath
   ? "cQuery"    , oURL:cQuery
   ? "cFile"     , oURL:cFile
   ? "nPort"     , oURL:nPort

   RETURN
