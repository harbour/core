/*
 * $Id$
 */


// hbwhat
// DateTimePicker functions

#define HB_OS_WIN_USED
#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0400

#include "hbwhat.h"

#include <windows.h>
#include <shlobj.h>
//#include <commctrl.h>

#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "winreg.h"

//-----------------------------------------------------------------------------

// SYNTAX:
// DatePicker_Create( nExStyle, nStyle, x, y, nWidth, nHeight, hWndParent, nId ) --> hDTP

HB_FUNC( VWN_DATETIME_CREATE )
{

   HB_RETWH( CreateWindowEx( ISNIL( 1 ) ? 0 : hb_parnl( 1 ) ,
                                    "SysDateTimePick32"   ,   // CLASSNAME
                                    0                     ,   // Window Name   // ????????
                                    (DWORD) hb_parnl( 2 ) ,   // nStyle
                                    hb_parni( 3 )         ,   // x
                                    hb_parni( 4 )         ,   // y
                                    hb_parni( 5 )         ,   // nWidth
                                    hb_parni( 6 )         ,   // nHeight
                                    (HWND) HB_PARWH( 7 )  ,   // hParent
                                    (HMENU) HB_PARWH( 8 ) ,   // hMenu
                                    GetModuleHandle(NULL) ,   // hInstance
                                    ISNIL( 9 ) ? NULL : (void *) HB_PARWH( 9 ) ) );   // lpParam
}

//-----------------------------------------------------------------------------


// Retrieves the handle to a date and time picker's (DTP) child month calendar control.
// You can use this macro or send the DTM_GETMONTHCAL message explicitly.
//
// SYNTAX:
// DateTime_GetMonthCal( hWnd ) --> hDTP
//
// #define DateTime_GetMonthCal(hdp) (HWND)SNDMSG(hdp, DTM_GETMONTHCAL, 0, 0)

HB_FUNC( VWN_DATETIME_GETMONTHCAL )
{
   /* NOTE: '(HWND)' is needed as a workaround for MinGW (4.32),
            where this is missing from the supplied headers. */
   HB_RETWH( (HWND) DateTime_GetMonthCal(
                                            (HWND) HB_PARWH( 1 )  // Handle to a DTP control
                                          ) );
}

//-----------------------------------------------------------------------------

// Retrieves the color for a given portion of the month calendar within a date and time
// picker (DTP) control. You can use this macro or send the DTM_GETMCCOLOR message explicitly
//
// SYNTAX:
// DateTime_GetMonthCalColor( hWndDP, nColor ) --> nCOLORREF
//
// #define DateTime_GetMonthCalColor(hdp, iColor) SNDMSG(hdp, DTM_GETMCCOLOR, iColor, 0)

HB_FUNC( VWN_DATETIME_GETMONTHCALCOLOR )
{

   hb_retnl( ( long ) DateTime_GetMonthCalColor(
                                            (HWND) HB_PARWH( 1 ),  // Handle to a DTP control
                                                   hb_parni( 2 )   // Value of type int specifying which month calendar color to retrieve.

                                             ) );
}

//-----------------------------------------------------------------------------

// Retrieves the font that the date and time picker (DTP) control's child month calendar
// control is currently using. You can use this macro or send the DTM_GETMCFONT message explicitly
//
// SYNTAX:
// DateTime_GetMonthCalFont( hWndDP ) --> hFont
//
// #define DateTime_GetMonthCalFont(hdp) SNDMSG(hdp, DTM_GETMCFONT, 0, 0)

HB_FUNC( VWN_DATETIME_GETMONTHCALFONT )
{
   // QUESTION: Doc seems to say this returns a HFONT, but it's not on 64-bit.
   hb_retnl( ( long ) DateTime_GetMonthCalFont(
                                            (HWND) HB_PARWH( 1 )   // Handle to a DTP control
                                             ) );
}

//-----------------------------------------------------------------------------

// #define DateTime_GetRange(hdp, rgst)  (DWORD)SNDMSG(hdp, DTM_GETRANGE, 0, (LPARAM)(rgst))
//
// Retrieves the current minimum and maximum allowable system times for a date and time picker
// (DTP) control. You can use this macro, or send the DTM_GETRANGE message explicitly
//
// SYNTAX:
// DateTime_GetRange( hWndDP ) --> aMinMaxDate
//
//

HB_FUNC( VWN_DATETIME_GETRANGE )
{
   LPSYSTEMTIME lpSysTimeArray = (SYSTEMTIME *) hb_xgrab( 2 * sizeof(SYSTEMTIME));
   PHB_ITEM aMinMaxDate, aMinDate, aMaxDate;
   DWORD dwRet;

   dwRet = DateTime_GetRange( (HWND) HB_PARWH( 1 ), (SYSTEMTIME *)lpSysTimeArray );

   if( ISBYREF( 2 ) )
      hb_stornl( dwRet, 2 );

   aMinMaxDate = hb_itemArrayNew( 2 );

   aMinDate = hb_itemArrayNew( 8 );
   hb_arraySetNL( aMinDate, 1, lpSysTimeArray[0].wYear );
   hb_arraySetNL( aMinDate, 2, lpSysTimeArray[0].wMonth );
   hb_arraySetNL( aMinDate, 3, lpSysTimeArray[0].wDayOfWeek );
   hb_arraySetNL( aMinDate, 4, lpSysTimeArray[0].wDay );
   hb_arraySetNL( aMinDate, 5, lpSysTimeArray[0].wHour );
   hb_arraySetNL( aMinDate, 6, lpSysTimeArray[0].wMinute );
   hb_arraySetNL( aMinDate, 7, lpSysTimeArray[0].wSecond );
   hb_arraySetNL( aMinDate, 8, lpSysTimeArray[0].wMilliseconds );

   hb_arraySet( aMinMaxDate, 1, aMinDate );


   aMaxDate = hb_itemArrayNew( 8 );
   hb_arraySetNL( aMaxDate, 1, lpSysTimeArray[1].wYear );
   hb_arraySetNL( aMaxDate, 2, lpSysTimeArray[1].wMonth );
   hb_arraySetNL( aMaxDate, 3, lpSysTimeArray[1].wDayOfWeek );
   hb_arraySetNL( aMaxDate, 4, lpSysTimeArray[1].wDay );
   hb_arraySetNL( aMaxDate, 5, lpSysTimeArray[1].wHour );
   hb_arraySetNL( aMaxDate, 6, lpSysTimeArray[1].wMinute );
   hb_arraySetNL( aMaxDate, 7, lpSysTimeArray[1].wSecond );
   hb_arraySetNL( aMaxDate, 8, lpSysTimeArray[1].wMilliseconds );

   hb_arraySet( aMinMaxDate, 2, aMaxDate );

   hb_itemReturnRelease( aMinMaxDate );
   hb_itemRelease( aMinDate );
   hb_itemRelease( aMaxDate );

   hb_xfree(lpSysTimeArray);
}

//-----------------------------------------------------------------------------

// #define DateTime_GetSystemtime(hdp, pst)    (DWORD)SNDMSG(hdp, DTM_GETSYSTEMTIME, 0, (LPARAM)(pst))
//
// Retrieves the currently selected time from a date and time picker (DTP)
// control and places it in a specified SYSTEMTIME structure.
// You can use this macro, or send the DTM_GETSYSTEMTIME message explicitly.
//
// SYNTAX:
// DateTime_GetSystemtime( hWndDP ) --> aSystemTime
//
//

HB_FUNC( VWN_DATETIME_GETSYSTEMTIME )
{
   SYSTEMTIME SysTime;
   long nRet;

   nRet = DateTime_GetSystemtime(
                      (HWND) HB_PARWH( 1 ),   // Handle to a DTP control
                      &SysTime              // Pointer to a SYSTEMTIME structure. If DTM_GETSYSTEMTIME returns
                                              // GDT_VALID, this structure will contain the system time.
                                              // Otherwise, it will not contain valid information.
                                              // This parameter must be a valid pointer; it cannot be NULL.
                                );

   if (nRet == GDT_VALID)  // Time is valid
   {
      PHB_ITEM aSysTime = hb_itemArrayNew( 8 );

      hb_arraySetNL( aSysTime, 1, SysTime.wYear );
      hb_arraySetNL( aSysTime, 2, SysTime.wMonth );
      hb_arraySetNL( aSysTime, 3, SysTime.wDayOfWeek );
      hb_arraySetNL( aSysTime, 4, SysTime.wDay );
      hb_arraySetNL( aSysTime, 5, SysTime.wHour );
      hb_arraySetNL( aSysTime, 6, SysTime.wMinute );
      hb_arraySetNL( aSysTime, 7, SysTime.wSecond );
      hb_arraySetNL( aSysTime, 8, SysTime.wMilliseconds );

      hb_itemReturnRelease( aSysTime );
   }
}

//-----------------------------------------------------------------------------

// #define DateTime_SetFormat(hdp, sz)  (BOOL)SNDMSG(hdp, DTM_SETFORMAT, 0, (LPARAM)(sz))
//
// Sets the display of a date and time picker (DTP) control based on a
// given format string. You can use this macro or send the DTM_SETFORMAT
// message explicitly.
//
// SYNTAX:
// DateTime_SetFormat( hWndDP, cFormat ) --> lOk
//
//

HB_FUNC( VWN_DATETIME_SETFORMAT )
{
   hb_retl( DateTime_SetFormat(
                      (HWND) HB_PARWH( 1 ),   // Handle to a DTP control
                   (LPCTSTR) hb_parcx( 2 )     // Pointer to a zero-terminated format string that defines
                                              // the desired display. Setting this parameter to NULL will
                                              // reset the control to the default format string for the current style.
                          )  );
}

//-----------------------------------------------------------------------------

// #define DateTime_SetMonthCalColor(hdp, iColor, clr) SNDMSG(hdp, DTM_SETMCCOLOR, iColor, clr)
//
// Sets the color for a given portion of the month calendar within a date
// and time picker (DTP) control. You can use this macro or send the DTM_SETMCCOLOR
// message explicitly.
//
// SYNTAX:
// DateTime_SetMonthCalColor( hWndDP, iColor, nColorRef ) --> nOldColorRef
//
//

HB_FUNC( VWN_DATETIME_SETMONTHCALCOLOR )
{
   hb_retnl( ( long ) DateTime_SetMonthCalColor(
                      (HWND) HB_PARWH( 1 ),   // Handle to a DTP control
                             hb_parni( 2 ),   // Value of type int specifying which month calendar color to set.
                  (COLORREF) hb_parnl( 3 )    // COLORREF value that represents the color that will be set for the specified area of the month calendar.
                                    )  );
}

//-----------------------------------------------------------------------------

// #define DateTime_SetMonthCalFont(hdp, hfont, fRedraw) SNDMSG(hdp, DTM_SETMCFONT, (WPARAM)(hfont), (LPARAM)(fRedraw))
//
// Sets the font to be used by the date and time picker (DTP) control's child
// month calendar control. You can use this macro or explicitly send the DTM_SETMCFONT message.
//
// SYNTAX:
// DateTime_SetMonthCalFont( hWndDP, hFont, lRedraw ) --> NIL
//
//

HB_FUNC( VWN_DATETIME_SETMONTHCALFONT )
{

   DateTime_SetMonthCalFont(
                      (HWND) HB_PARWH( 1 ),   // Handle to a DTP control
                     (HFONT) HB_PARWH( 2 ),   // Handle to the font that will be set.
                      (BOOL) hb_parl( 3 )     // Specifies whether the control should be redrawn
                                              // immediately upon setting the font. Setting this
                                              // parameter to TRUE causes the control to redraw itself.
                           );

}

//-----------------------------------------------------------------------------

// #define DateTime_SetSystemtime(hdp, gd, pst)    (BOOL)SNDMSG(hdp, DTM_SETSYSTEMTIME, (WPARAM)(gd), (LPARAM)(pst))
//
// Sets a date and time picker (DTP) control to a given date and time.
// You can use this macro or send the DTM_SETSYSTEMTIME message explicitly.
//
// SYNTAX:
// DateTime_SetSystemtime( hWndDP, nFlag, aSystemTime ) --> lOk
// or
// DateTime_SetSystemTime( hWndDP, nFlag, SYSTEMTIME.value ) -> lSuccess
//
//

HB_FUNC( VWN_DATETIME_SETSYSTEMTIME )
{
   SYSTEMTIME SysTime, *lpSysTime ;

   if ( ISARRAY( 3 ) ) // array
   {
      SysTime.wYear         = (WORD)  hb_parni( 3, 1 );
      SysTime.wMonth        = (WORD)  hb_parni( 3, 2 );
      SysTime.wDayOfWeek    = (WORD)  hb_parni( 3, 3 );
      SysTime.wDay          = (WORD)  hb_parni( 3, 4 );
      SysTime.wHour         = (WORD)  hb_parni( 3, 5 );
      SysTime.wMinute       = (WORD)  hb_parni( 3, 6 );
      SysTime.wSecond       = (WORD)  hb_parni( 3, 7 );
      SysTime.wMilliseconds = (WORD)  hb_parni( 3, 8 );
      lpSysTime = &SysTime;
   }
   else
   {
     if ( ISCHAR(2) )  // xHarbour structure
     {
        lpSysTime =( SYSTEMTIME *) hb_parc( 3 ); //hb_param( 3, HB_IT_STRING)->item.asString.value;
     }
     else
     {
      hb_retl(0);
      return;
     }
   }

   hb_retl( DateTime_SetSystemtime(
                      (HWND) HB_PARWH( 1 ) ,   // Handle to a DTP control
                      (DWORD) hb_parnl( 2 ),   // Value that specifies the action that should be performed.
                      lpSysTime                // Pointer to SYSTEMTIME structures
                               ) );
}
