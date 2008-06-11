/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    DLL call demonstration.
 *
 * Copyright 2008 Viktor Szakats <harbour.01 syenar.hu>
 * www - http://www.harbour-project.org
 *
 */

#define MB_OK                       0x00000000
#define MB_OKCANCEL                 0x00000001
#define MB_ABORTRETRYIGNORE         0x00000002
#define MB_YESNOCANCEL              0x00000003
#define MB_YESNO                    0x00000004
#define MB_RETRYCANCEL              0x00000005
#define MB_CANCELTRYCONTINUE        0x00000006
#define MB_ICONHAND                 0x00000010
#define MB_ICONQUESTION             0x00000020
#define MB_ICONEXCLAMATION          0x00000030
#define MB_ICONASTERISK             0x00000040
#define MB_USERICON                 0x00000080
#define MB_DEFBUTTON2               0x00000100
#define MB_DEFBUTTON3               0x00000200
#define MB_DEFBUTTON4               0x00000300
#define MB_SYSTEMMODAL              0x00001000
#define MB_TASKMODAL                0x00002000
#define MB_HELP                     0x00004000
#define MB_NOFOCUS                  0x00008000
#define MB_SETFOREGROUND            0x00010000
#define MB_DEFAULT_DESKTOP_ONLY     0x00020000
#define MB_TOPMOST                  0x00040000
#define MB_RIGHT                    0x00080000
#define MB_RTLREADING               0x00100000

#define CSIDL_DESKTOP                   0x0000        // <desktop>
#define CSIDL_INTERNET                  0x0001        // Internet Explorer (icon on desktop)
#define CSIDL_PROGRAMS                  0x0002        // Start Menu\Programs
#define CSIDL_CONTROLS                  0x0003        // My Computer\Control Panel
#define CSIDL_PRINTERS                  0x0004        // My Computer\Printers
#define CSIDL_PERSONAL                  0x0005        // My Documents
#define CSIDL_FAVORITES                 0x0006        // <user name>\Favorites
#define CSIDL_STARTUP                   0x0007        // Start Menu\Programs\Startup
#define CSIDL_RECENT                    0x0008        // <user name>\Recent
#define CSIDL_SENDTO                    0x0009        // <user name>\SendTo
#define CSIDL_BITBUCKET                 0x000a        // <desktop>\Recycle Bin
#define CSIDL_STARTMENU                 0x000b        // <user name>\Start Menu
#define CSIDL_DESKTOPDIRECTORY          0x0010        // <user name>\Desktop
#define CSIDL_DRIVES                    0x0011        // My Computer
#define CSIDL_NETWORK                   0x0012        // Network Neighborhood
#define CSIDL_NETHOOD                   0x0013        // <user name>\nethood
#define CSIDL_FONTS                     0x0014        // windows\fonts
#define CSIDL_TEMPLATES                 0x0015
#define CSIDL_COMMON_STARTMENU          0x0016        // All Users\Start Menu
#define CSIDL_COMMON_PROGRAMS           0X0017        // All Users\Programs
#define CSIDL_COMMON_STARTUP            0x0018        // All Users\Startup
#define CSIDL_COMMON_DESKTOPDIRECTORY   0x0019        // All Users\Desktop
#define CSIDL_APPDATA                   0x001a        // <user name>\Application Data
#define CSIDL_PRINTHOOD                 0x001b        // <user name>\PrintHood
#define CSIDL_LOCAL_APPDATA             0x001c        // <user name>\Local Settings\Applicaiton Data (non roaming)
#define CSIDL_ALTSTARTUP                0x001d        // non localized startup
#define CSIDL_COMMON_ALTSTARTUP         0x001e        // non localized common startup
#define CSIDL_COMMON_FAVORITES          0x001f
#define CSIDL_INTERNET_CACHE            0x0020
#define CSIDL_COOKIES                   0x0021
#define CSIDL_HISTORY                   0x0022
#define CSIDL_COMMON_APPDATA            0x0023        // All Users\Application Data
#define CSIDL_WINDOWS                   0x0024        // GetWindowsDirectory()
#define CSIDL_SYSTEM                    0x0025        // GetSystemDirectory()
#define CSIDL_PROGRAM_FILES             0x0026        // C:\Program Files
#define CSIDL_MYPICTURES                0x0027        // C:\Program Files\My Pictures
#define CSIDL_PROFILE                   0x0028        // USERPROFILE
#define CSIDL_PROGRAM_FILES_COMMON      0x002b        // C:\Program Files\Common
#define CSIDL_COMMON_TEMPLATES          0x002d        // All Users\Templates
#define CSIDL_COMMON_DOCUMENTS          0x002e        // All Users\Documents
#define CSIDL_COMMON_ADMINTOOLS         0x002f        // All Users\Start Menu\Programs\Administrative Tools
#define CSIDL_ADMINTOOLS                0x0030        // <user name>\Start Menu\Programs\Administrative Tools

#define MAX_PATH 260

PROCEDURE Main()
   LOCAL hDLL
   LOCAL cData

   IF File( "pscript.dll" )
      hDLL := DllLoad( "pscript.dll" )
      cData := Space( 24 )
      DllCall( hDll, NIL, "PSGetVersion", @cData )
      ? ">" + cData + "<"
      DllUnload( hDLL )

      // ; Testing failure 1
      hDLL := DllLoad( "pscript.dll" )
      cData := Space( 24 )
      DllCall( hDll, NIL, "PSGet__Version", @cData )
      ? ">" + cData + "<"
      DllUnload( hDLL )
   ENDIF

   // ; Testing failure 2
   hDLL := DllLoad( "nothere.dll" )
   cData := Space( 24 )
   DllCall( hDll, NIL, "PSGetVersion", @cData )
   ? cData
   DllUnload( hDLL )

   ? "MsgBox:", DllCall( "user32.dll", NIL, "MessageBoxA", 0, "Hello world!", "Harbour sez", hb_bitOr( MB_OKCANCEL, MB_ICONEXCLAMATION, MB_HELP ) )

   IF File( "libcurl.dll" )
      hDLL := DllLoad( "libcurl.dll" )
      ? GetProcAddress( hDLL, "curl_version" )
      // ; This one doesn't work.
      ? CallDllTyped( 10 /* return string */, GetProcAddress( hDLL, "CURL_VERSION" ) )
      DllUnload( hDLL )
   ENDIF

   /* Force Windows not to show dragged windows contents */

   #define SPI_SETDRAGFULLWINDOWS 37

   ? "Full content drag: OFF"
   ? DllCall( "user32.dll", NIL, "SystemParametersInfo", SPI_SETDRAGFULLWINDOWS, 0, 0, 0 )
   Inkey( 0 )

   ? "Full content drag: ON"
   ? DllCall( "user32.dll", NIL, "SystemParametersInfo", SPI_SETDRAGFULLWINDOWS, 1, 0, 0 )
   Inkey( 0 )

   /* Get some standard Windows folders */

   hDLL := DllLoad( "shell32.dll" )
   cData := Space( MAX_PATH )
   ? CallDllBool( GetProcAddress( hDLL, "SHGetSpecialFolderPath" ), 0, @cData, CSIDL_APPDATA, 0 )
   ? cData
   ? CallDll( GetProcAddress( hDLL, "SHGetFolderPath" ), 0, CSIDL_ADMINTOOLS, 0, 0, @cData )
   ? cData
   DllUnload( hDLL )

   RETURN
