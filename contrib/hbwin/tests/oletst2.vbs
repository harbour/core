' Copyright 2010 Viktor Szakats (vszakats.net/harbour)

Dim tst2 : Set tst2 = WScript.CreateObject("MyOleTimeServer")

WScript.CreateObject("Wscript.Shell").Popup tst2.Time()
