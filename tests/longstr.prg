// Written by Eddie Runia <eddie@runia.com>
// www - http://harbour-project.org
//
// Placed in the public domain

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL c := "This is a very long string. This may seem silly however strings like this are still used. Not by good programmers though, but I've seen stuff like this used for Copyright messages and other long text. What is the point to all of this you'd say. Well I am coming to the point right now, the constant string is limited to 256 characters and this string is a lot bigger. Do you get my drift ? If there is somebody who has read this line upto the very end: Esto es un sombrero grande ridiculo." + Chr( 13 ) + Chr( 10 ) + "/" + Chr( 13 ) + Chr( 10 ) + "[;-)" + Chr( 13 ) + Chr( 10 ) + "\"

   ? c

   RETURN
