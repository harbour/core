'
' $Id$
'

' Copyright 2010 Viktor Szakats (harbour syenar.hu)
' www - http://harbour-project.org
'
' See COPYING for licensing terms.

Dim tst2 : Set tst2 = WScript.CreateObject("MyOleTimeServer")

WScript.CreateObject("Wscript.Shell").Popup tst2.Time()
