/*
 * $Id$
 */


// hbwhat
// Metafile functions

#define HB_OS_WIN_USED
#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0400

#include "hbwhat.h"

#include <windows.h>
#include <shlobj.h>
//#include <commctrl.h>

#include "hbapiitm.h"
#include "hbapiitm.h"
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"

extern PHB_ITEM Rect2Array( RECT *rc  );
extern BOOL Array2Rect(PHB_ITEM aRect, RECT *rc );
extern PHB_ITEM Point2Array( POINT *pt  );
extern BOOL Array2Point(PHB_ITEM aPoint, POINT *pt );


// Under developmemnt !



//////////////////////
//  Standard
//////////////////////


//-----------------------------------------------------------------------------
// WINGDIAPI HDC WINAPI CreateMetaFileA( IN LPCSTR);


HB_FUNC( VWN_CREATEMETAFILE )
{
   HB_RETWH( CreateMetaFile( (LPCSTR) hb_parcx( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI HMETAFILE WINAPI CopyMetaFileA( IN HMETAFILE, IN LPCSTR);


HB_FUNC( VWN_COPYMETAFILE )
{
   HB_RETWH( CopyMetaFile( (HMETAFILE) HB_PARWH( 1 ),
                                   (LPCSTR) hb_parcx( 2 )
                                   ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI HMETAFILE WINAPI CloseMetaFile( IN HDC);


HB_FUNC( VWN_CLOSEMETAFILE )
{
   HB_RETWH( CloseMetaFile( (HDC) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI DeleteMetaFile( IN HMETAFILE);


HB_FUNC( VWN_DELETEMETAFILE )
{
   hb_retl( DeleteMetaFile( (HMETAFILE) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI HMETAFILE WINAPI GetMetaFileA( IN LPCSTR);


HB_FUNC( VWN_GETMETAFILE )
{
   HB_RETWH( GetMetaFile( (LPCSTR) hb_parcx( 1 ) ) );
}






//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI PlayMetaFile(IN HDC, IN HMETAFILE);


HB_FUNC( VWN_PLAYMETAFILE )
{
   hb_retl( PlayMetaFile( (HDC) HB_PARWH( 1 ), (HMETAFILE) HB_PARWH( 2 ) ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI UINT WINAPI GetMetaFileBitsEx( IN HMETAFILE, IN UINT, OUT LPVOID);

/*

HB_FUNC( VWN_GETMETAFILEBITSEX )
{
   LPVOID    lpVoid    ;

   // Your code goes here

   hb_retni( GetMetaFileBitsEx( (HMETAFILE) HB_PARWH( 1 ),
                                (UINT) hb_parni( 2 )     ,
                                lpVoid
                                ) );
}

*/



//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI EnumMetaFile( IN HDC, IN HMETAFILE, IN MFENUMPROC, IN LPARAM);

/*

HB_FUNC( VWN_ENUMMETAFILE )
{
   MFENUMPROC mfEnumProc ;
   LPARAM     lParam     ;

   // Your code goes here

   hb_retl( EnumMetaFile( (HDC) HB_PARWH( 1 )      ,
                          (HMETAFILE) HB_PARWH( 2 ),
                          mfEnumProc               ,
                          lParam
                          ) );
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI HMETAFILE WINAPI SetMetaFileBitsEx(IN UINT, IN CONST BYTE *);

/*

HB_FUNC( VWN_SETMETAFILEBITSEX )
{
   CONST BYTE ;

   // Your code goes here

   HB_RETWH( SetMetaFileBitsEx( (UINT) hb_parni( 1 ), &BYTE ) );
}

*/


////////////////////////
//  Enhanced
////////////////////////



//-----------------------------------------------------------------------------
// WINGDIAPI HDC WINAPI CreateEnhMetaFile( IN HDC, IN LPCSTR, IN CONST RECT *, IN LPCSTR);


HB_FUNC( VWN_CREATEENHMETAFILE )
{
   RECT rc ;

   if ( ISARRAY(3) && Array2Rect( hb_param(3,HB_IT_ARRAY), &rc ))
      HB_RETWH( CreateEnhMetaFile( (HDC) HB_PARWH( 1 )  ,
                                           (LPCSTR) hb_parcx( 2 ),
                                           &rc                  ,
                                           ISNIL(4) ? NULL : (LPCSTR) hb_parcx( 4 )
                                        ) );


}


//-----------------------------------------------------------------------------
// WINGDIAPI HENHMETAFILE WINAPI GetEnhMetaFileA( IN LPCSTR);

HB_FUNC( VWN_GETENHMETAFILE )
{
   HB_RETWH( GetEnhMetaFile( (LPCSTR) hb_parcx( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI GetMetaRgn( IN HDC, IN HRGN);

HB_FUNC( VWN_GETMETARGN )
{
   hb_retni( GetMetaRgn( (HDC) HB_PARWH( 1 ), (HRGN) HB_PARWH( 2 ) ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI SetMetaRgn(IN HDC);


HB_FUNC( VWN_SETMETARGN )
{
   hb_retni( SetMetaRgn( (HDC) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------

HB_FUNC( VWN_CLOSEENHMETAFILE )
{
   HB_RETWH( CloseEnhMetaFile( (HDC) HB_PARWH(1) ) );
}



//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI DeleteEnhMetaFile( IN HENHMETAFILE);


HB_FUNC( VWN_DELETEENHMETAFILE )
{
   hb_retl( DeleteEnhMetaFile( (HENHMETAFILE) HB_PARWH( 1 ) ) );
}



//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI PlayEnhMetaFile( IN HDC, IN HENHMETAFILE, IN CONST RECT *);

HB_FUNC( VWN_PLAYENHMETAFILE )
{
   RECT  rc ;

   if ( ISARRAY(3) && Array2Rect( hb_param(3,HB_IT_ARRAY), &rc ))
      hb_retl( PlayEnhMetaFile( (HDC) HB_PARWH( 1 )         ,
                                (HENHMETAFILE) HB_PARWH( 2 ),
                                &rc
                             ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI HENHMETAFILE WINAPI CopyEnhMetaFileA( IN HENHMETAFILE, IN LPCSTR);


HB_FUNC( VWN_COPYENHMETAFILEA )
{
   HB_RETWH( CopyEnhMetaFileA( (HENHMETAFILE) HB_PARWH( 1 ),
                                      (LPCSTR) hb_parcx( 2 )
                                      ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI UINT WINAPI GetWinMetaFileBits( IN HENHMETAFILE, IN UINT, OUT LPBYTE, IN INT, IN HDC);


// Syntax
// GetWinMetafileByts( hMeta, nMapMode,hRefDC) -> Buffer, or NIL

HB_FUNC( VWN_GETWINMETAFILEBITS )
{
   BYTE  *Buffer ;

   UINT nBytes ;

   nBytes = GetWinMetaFileBits( (HENHMETAFILE) HB_PARWH( 1 ),
                                 0 , NULL, hb_parni( 2 ), (HDC) HB_PARWH( 3 ) );

   if ( nBytes )
   {
       Buffer = (BYTE *) hb_xgrab( nBytes);

       if ( GetWinMetaFileBits( (HENHMETAFILE) HB_PARWH( 1 ) ,
                                 nBytes  , Buffer            ,
                                 hb_parni( 2 )               ,
                                 (HDC) HB_PARWH( 3 )
                                 ) )
           hb_retclen( ( char *)Buffer, nBytes );

       hb_xfree(Buffer);

   }
}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI PlayEnhMetaFileRecord( IN HDC, IN LPHANDLETABLE, IN CONST ENHMETARECORD *, IN UINT);

/*

HB_FUNC( VWN_PLAYENHMETAFILERECORD )
{
   LPHANDLETABLE lpHandleTable ;
   ENHMETARECORD  emfr ;

   // Your code goes here

   hb_retl( PlayEnhMetaFileRecord( (HDC) HB_PARWH( 1 ) ,
                                   lpHandleTable       ,
                                   &emfr               ,
                                   (UINT) hb_parni( 4 )
                                   ) );
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI UINT WINAPI GetEnhMetaFileDescriptionA( IN HENHMETAFILE, IN UINT, OUT LPSTR );


HB_FUNC( VWN_GETENHMETAFILEDESCRIPTION )
{
   hb_retni( GetEnhMetaFileDescription( (HENHMETAFILE) HB_PARWH( 1 ),
                                         (UINT) hb_parni( 2 )        ,
                                         (LPSTR) hb_parcx( 3 )
                                         ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI PlayMetaFileRecord( IN HDC, IN LPHANDLETABLE, IN LPMETARECORD, IN UINT);

/*

HB_FUNC( VWN_PLAYMETAFILERECORD )
{
   LPHANDLETABLE lpHandleTable ;
   LPMETARECORD  lpMetaRecord  ;

   // Your code goes here

   hb_retl( PlayMetaFileRecord( (HDC) HB_PARWH( 1 ) ,
                                lpHandleTable       ,
                                lpMetaRecord        ,
                                (UINT) hb_parni( 4 )
                                ) );
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI HENHMETAFILE WINAPI SetEnhMetaFileBits( IN UINT, IN CONST BYTE *);

/*

HB_FUNC( VWN_SETENHMETAFILEBITS )
{
   CONST BYTE ;

   // Your code goes here

   HB_RETWH( SetEnhMetaFileBits( (UINT) hb_parni( 1 ), &BYTE ) );
}

*/


//-----------------------------------------------------------------------------
// WINGDIAPI HENHMETAFILE WINAPI SetWinMetaFileBits( IN UINT, IN CONST BYTE *, IN HDC, IN CONST METAFILEPICT *);

/*

HB_FUNC( VWN_SETWINMETAFILEBITS )
{
   CONST BYTE         ;
   CONST METAFILEPICT ;

   // Your code goes here

   HB_RETWH( SetWinMetaFileBits( (UINT) hb_parni( 1 ),
                                        &BYTE               ,
                                        (HDC) HB_PARWH( 3 ) ,
                                        &&METAFILEPICT
                                        ) );
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI EnumEnhMetaFile( IN HDC, IN HENHMETAFILE, IN ENHMFENUMPROC, IN LPVOID, IN CONST RECT *);

/*

HB_FUNC( VWN_ENUMENHMETAFILE )
{
   ENHMFENUMPROC enhmfEnumProc ;
   LPVOID        lpVoid        ;


   RECT  rc ;

   if ( ISARRAY(5) && Array2Rect( hb_param(5,HB_IT_ARRAY), &rc ))

   hb_retl( EnumEnhMetaFile( (HDC) HB_PARWH( 1 )         ,
                             (HENHMETAFILE) HB_PARWH( 2 ),
                             enhmfEnumProc               ,
                             lpVoid                      ,
                             &rc
                             ) );
}

*/







//-----------------------------------------------------------------------------
// WINGDIAPI UINT WINAPI GetEnhMetaFileBits( IN HENHMETAFILE, IN UINT, OUT LPBYTE);

/*

HB_FUNC( VWN_GETENHMETAFILEBITS )
{
   LPBYTE       lpByte       ;

   // Your code goes here

   hb_retni( GetEnhMetaFileBits( (HENHMETAFILE) HB_PARWH( 1 ),
                                 (UINT) hb_parni( 2 )        ,
                                 lpByte
                                 ) );
}

*/


//-----------------------------------------------------------------------------
// WINGDIAPI UINT WINAPI GetEnhMetaFileHeader( IN HENHMETAFILE, IN UINT, OUT LPENHMETAHEADER );

/*

HB_FUNC( VWN_GETENHMETAFILEHEADER )
{
   LPENHMETAHEADER lPenhMetaHeader ;

   // Your code goes here

   hb_retni( GetEnhMetaFileHeader( (HENHMETAFILE) HB_PARWH( 1 ),
                                   (UINT) hb_parni( 2 )        ,
                                   lPenhMetaHeader
                                   ) );
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI UINT WINAPI GetEnhMetaFilePaletteEntries( IN HENHMETAFILE, IN UINT, OUT LPPALETTEENTRY );

/*

HB_FUNC( VWN_GETENHMETAFILEPALETTEENTRIES )
{
   LPPALETTEENTRY lpPaletteEntry ;

   // Your code goes here

   hb_retni( GetEnhMetaFilePaletteEntries( (HENHMETAFILE) HB_PARWH( 1 ),
                                           (UINT) hb_parni( 2 )        ,
                                           lpPaletteEntry
                                           ) );
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI UINT WINAPI GetEnhMetaFilePixelFormat( IN HENHMETAFILE, IN UINT, OUT PIXELFORMATDESCRIPTOR *);

/*

HB_FUNC( VWN_GETENHMETAFILEPIXELFORMAT )
{
   PIXELFORMATDESCRIPTOR PixelFormatdescriptor ;

   // Your code goes here

   hb_retni( GetEnhMetaFilePixelFormat( (HENHMETAFILE) HB_PARWH( 1 ),
                                        (UINT) hb_parni( 2 )        ,
                                        &PixelFormatdescriptor
                                        ) );
}

*/
