/* Copyright 2010 Viktor Szakats (vszakats.net/harbour) */
{
  var tst2 = new ActiveXObject("MyOleTimeServer");

  WScript.CreateObject("Wscript.Shell").Popup(">" + tst2.TIME() + "<");
}
