
#require "xhb"

#include "hbdll.ch"

IMPORT STATIC MessageBox( hWnd, cMsg, cText, nFlags ) FROM user32.dll EXPORTED AS MessageBoxA

PROCEDURE Main()

   ? MessageBox( 0, "Hello world!", "Harbour sez" )

   RETURN
