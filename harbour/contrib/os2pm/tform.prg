#include "common.ch"
#include "hbclass.ch"
#include "os2pm.ch"   // Needed to store some OS/2 PM constant values

CLASS TForm

   DATA      hWnd

   CLASSDATA lRegistered

   METHOD    New()

   METHOD    ShowModal()

ENDCLASS

METHOD New() CLASS TForm

   local hWndClient

   DEFAULT ::lRegistered TO .f.

   if ! ::lRegistered

      // Notice that this code may be moved to a method Register()
      // so we hide again the OS API details

      WinRegisterClass( "HB_TFORM",;
                        (CS_SIZEREDRAW + 0x2000001), 0 )
      ::lRegistered = .t.
   endif

   // Again this code may be moved to a method Create() to hide the
   // OS API details

   ::hWnd = WinCreateStdWindow( HWND_DESKTOP,;
                                WS_VISIBLE,;
                                (FCF_TITLEBAR + FCF_SYSMENU +;
                                FCF_SIZEBORDER + FCF_TASKLIST +;
                                FCF_MINMAX + FCF_SHELLPOSITION ),;
                                "HB_TFORM", "Harbour TForm",;
                                (WS_SYNCPAINT + WS_VISIBLE ),,,;
                                @hWndClient ) // Not used yet

return Self

METHOD ShowModal() CLASS TForm

   HB_PM_ShowModal()

return nil



