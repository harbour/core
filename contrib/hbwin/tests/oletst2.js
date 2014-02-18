/*
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
 * www - http://harbour-project.org
 *
 * See COPYING.txt for licensing terms.
 */

{
   var tst2 = new ActiveXObject("MyOleTimeServer");

   WScript.CreateObject("Wscript.Shell").Popup(">" + tst2.TIME() + "<");
}
