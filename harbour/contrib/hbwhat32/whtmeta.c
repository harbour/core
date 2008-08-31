/*
 * $Id$
 */


// What32
// Metafile functions

#define HB_OS_WIN_32_USED
#define _WIN32_WINNT   0x0400

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


HB_FUNC( CREATEMETAFILE )
{
   hb_retnl( (LONG) CreateMetaFile( (LPCSTR) hb_parcx( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINGDIAPI HMETAFILE WINAPI CopyMetaFileA( IN HMETAFILE, IN LPCSTR);


HB_FUNC( COPYMETAFILE )
{
   hb_retnl( (LONG) CopyMetaFile( (HMETAFILE) hb_parnl( 1 ),
                                   (LPCSTR) hb_parcx( 2 )
                                   ) ) ;
}

//-----------------------------------------------------------------------------
// WINGDIAPI HMETAFILE WINAPI CloseMetaFile( IN HDC);


HB_FUNC( CLOSEMETAFILE )
{
   hb_retnl( (LONG) CloseMetaFile( (HDC) hb_parnl( 1 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI DeleteMetaFile( IN HMETAFILE);


HB_FUNC( DELETEMETAFILE )
{
   hb_retl( DeleteMetaFile( (HMETAFILE) hb_parnl( 1 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINGDIAPI HMETAFILE WINAPI GetMetaFileA( IN LPCSTR);


HB_FUNC( GETMETAFILE )
{
   hb_retnl( (LONG) GetMetaFile( (LPCSTR) hb_parcx( 1 ) ) ) ;
}






//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI PlayMetaFile(IN HDC, IN HMETAFILE);


HB_FUNC( PLAYMETAFILE )
{
   hb_retl( PlayMetaFile( (HDC) hb_parnl( 1 ), (HMETAFILE) hb_parnl( 2 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINGDIAPI UINT WINAPI GetMetaFileBitsEx( IN HMETAFILE, IN UINT, OUT LPVOID);

/*

HB_FUNC( GETMETAFILEBITSEX )
{
   LPVOID    lpVoid    ;

   // Your code goes here

   hb_retni( GetMetaFileBitsEx( (HMETAFILE) hb_parnl( 1 ),
                                (UINT) hb_parni( 2 )     ,
                                lpVoid
                                ) ) ;
}

*/



//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI EnumMetaFile( IN HDC, IN HMETAFILE, IN MFENUMPROC, IN LPARAM);

/*

HB_FUNC( ENUMMETAFILE )
{
   MFENUMPROC mfEnumProc ;
   LPARAM     lParam     ;

   // Your code goes here

   hb_retl( EnumMetaFile( (HDC) hb_parnl( 1 )      ,
                          (HMETAFILE) hb_parnl( 2 ),
                          mfEnumProc               ,
                          lParam
                          ) ) ;
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI HMETAFILE WINAPI SetMetaFileBitsEx(IN UINT, IN CONST BYTE *);

/*

HB_FUNC( SETMETAFILEBITSEX )
{
   CONST BYTE ;

   // Your code goes here

   hb_retnl( (LONG) SetMetaFileBitsEx( (UINT) hb_parni( 1 ), &BYTE ) ) ;
}

*/


////////////////////////
//  Enhanced
////////////////////////



//-----------------------------------------------------------------------------
// WINGDIAPI HDC WINAPI CreateEnhMetaFile( IN HDC, IN LPCSTR, IN CONST RECT *, IN LPCSTR);


HB_FUNC( CREATEENHMETAFILE )
{
   RECT rc ;

   if ( ISARRAY(3) && Array2Rect( hb_param(3,HB_IT_ARRAY), &rc ))
      hb_retnl( (LONG) CreateEnhMetaFile( (HDC) hb_parnl( 1 )  ,
                                           (LPCSTR) hb_parcx( 2 ),
                                           &rc                  ,
                                           ISNIL(4) ? NULL : (LPCSTR) hb_parcx( 4 )
                                        ) ) ;


}


//-----------------------------------------------------------------------------
// WINGDIAPI HENHMETAFILE WINAPI GetEnhMetaFileA( IN LPCSTR);

HB_FUNC( GETENHMETAFILE )
{
   hb_retnl( (LONG) GetEnhMetaFile( (LPCSTR) hb_parcx( 1 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI GetMetaRgn( IN HDC, IN HRGN);

HB_FUNC( GETMETARGN )
{
   hb_retni( GetMetaRgn( (HDC) hb_parnl( 1 ), (HRGN) hb_parnl( 2 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI SetMetaRgn(IN HDC);


HB_FUNC( SETMETARGN )
{
   hb_retni( SetMetaRgn( (HDC) hb_parnl( 1 ) ) ) ;
}


//-----------------------------------------------------------------------------

HB_FUNC( CLOSEENHMETAFILE )
{
   hb_retnl( (LONG) CloseEnhMetaFile( (HDC) hb_parnl(1) ) );
}



//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI DeleteEnhMetaFile( IN HENHMETAFILE);


HB_FUNC( DELETEENHMETAFILE )
{
   hb_retl( DeleteEnhMetaFile( (HENHMETAFILE) hb_parnl( 1 ) ) ) ;
}



//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI PlayEnhMetaFile( IN HDC, IN HENHMETAFILE, IN CONST RECT *);

HB_FUNC( PLAYENHMETAFILE )
{
   RECT  rc ;

   if ( ISARRAY(3) && Array2Rect( hb_param(3,HB_IT_ARRAY), &rc ))
      hb_retl( PlayEnhMetaFile( (HDC) hb_parnl( 1 )         ,
                                (HENHMETAFILE) hb_parnl( 2 ),
                                &rc
                             ) ) ;
}


//-----------------------------------------------------------------------------
// WINGDIAPI HENHMETAFILE WINAPI CopyEnhMetaFileA( IN HENHMETAFILE, IN LPCSTR);


HB_FUNC( COPYENHMETAFILEA )
{
   hb_retnl( (LONG) CopyEnhMetaFileA( (HENHMETAFILE) hb_parnl( 1 ),
                                      (LPCSTR) hb_parcx( 2 )
                                      ) ) ;
}

//-----------------------------------------------------------------------------
// WINGDIAPI UINT WINAPI GetWinMetaFileBits( IN HENHMETAFILE, IN UINT, OUT LPBYTE, IN INT, IN HDC);


// Syntax
// GetWinMetafileByts( hMeta, nMapMode,hRefDC) -> Buffer, or NIL

HB_FUNC( GETWINMETAFILEBITS )
{
   BYTE  *Buffer ;

   UINT nBytes ;

   nBytes = GetWinMetaFileBits( (HENHMETAFILE) hb_parnl( 1 ),
                                 0 , NULL, hb_parni( 2 ), (HDC) hb_parnl( 3 ) ) ;

   if ( nBytes )
   {
       Buffer = (BYTE *) hb_xgrab( nBytes) ;

       if ( GetWinMetaFileBits( (HENHMETAFILE) hb_parnl( 1 ) ,
                                 nBytes  , Buffer            ,
                                 hb_parni( 2 )               ,
                                 (HDC) hb_parnl( 3 )
                                 ) )
           hb_retclen( ( char *)Buffer, nBytes );

       hb_xfree(Buffer);

   }
}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI PlayEnhMetaFileRecord( IN HDC, IN LPHANDLETABLE, IN CONST ENHMETARECORD *, IN UINT);

/*

HB_FUNC( PLAYENHMETAFILERECORD )
{
   LPHANDLETABLE lpHandleTable ;
   ENHMETARECORD  emfr ;

   // Your code goes here

   hb_retl( PlayEnhMetaFileRecord( (HDC) hb_parnl( 1 ) ,
                                   lpHandleTable       ,
                                   &emfr               ,
                                   (UINT) hb_parni( 4 )
                                   ) ) ;
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI UINT WINAPI GetEnhMetaFileDescriptionA( IN HENHMETAFILE, IN UINT, OUT LPSTR );


HB_FUNC( GETENHMETAFILEDESCRIPTION )
{
   hb_retni( GetEnhMetaFileDescription( (HENHMETAFILE) hb_parnl( 1 ),
                                         (UINT) hb_parni( 2 )        ,
                                         (LPSTR) hb_parcx( 3 )
                                         ) ) ;
}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI PlayMetaFileRecord( IN HDC, IN LPHANDLETABLE, IN LPMETARECORD, IN UINT);

/*

HB_FUNC( PLAYMETAFILERECORD )
{
   LPHANDLETABLE lpHandleTable ;
   LPMETARECORD  lpMetaRecord  ;

   // Your code goes here

   hb_retl( PlayMetaFileRecord( (HDC) hb_parnl( 1 ) ,
                                lpHandleTable       ,
                                lpMetaRecord        ,
                                (UINT) hb_parni( 4 )
                                ) ) ;
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI HENHMETAFILE WINAPI SetEnhMetaFileBits( IN UINT, IN CONST BYTE *);

/*

HB_FUNC( SETENHMETAFILEBITS )
{
   CONST BYTE ;

   // Your code goes here

   hb_retnl( (LONG) SetEnhMetaFileBits( (UINT) hb_parni( 1 ), &BYTE ) ) ;
}

*/


//-----------------------------------------------------------------------------
// WINGDIAPI HENHMETAFILE WINAPI SetWinMetaFileBits( IN UINT, IN CONST BYTE *, IN HDC, IN CONST METAFILEPICT *);

/*

HB_FUNC( SETWINMETAFILEBITS )
{
   CONST BYTE         ;
   CONST METAFILEPICT ;

   // Your code goes here

   hb_retnl( (LONG) SetWinMetaFileBits( (UINT) hb_parni( 1 ),
                                        &BYTE               ,
                                        (HDC) hb_parnl( 3 ) ,
                                        &&METAFILEPICT
                                        ) ) ;
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI EnumEnhMetaFile( IN HDC, IN HENHMETAFILE, IN ENHMFENUMPROC, IN LPVOID, IN CONST RECT *);

/*

HB_FUNC( ENUMENHMETAFILE )
{
   ENHMFENUMPROC enhmfEnumProc ;
   LPVOID        lpVoid        ;


   RECT  rc ;

   if ( ISARRAY(5) && Array2Rect( hb_param(5,HB_IT_ARRAY), &rc ))

   hb_retl( EnumEnhMetaFile( (HDC) hb_parnl( 1 )         ,
                             (HENHMETAFILE) hb_parnl( 2 ),
                             enhmfEnumProc               ,
                             lpVoid                      ,
                             &rc
                             ) ) ;
}

*/







//-----------------------------------------------------------------------------
// WINGDIAPI UINT WINAPI GetEnhMetaFileBits( IN HENHMETAFILE, IN UINT, OUT LPBYTE);

/*

HB_FUNC( GETENHMETAFILEBITS )
{
   LPBYTE       lpByte       ;

   // Your code goes here

   hb_retni( GetEnhMetaFileBits( (HENHMETAFILE) hb_parnl( 1 ),
                                 (UINT) hb_parni( 2 )        ,
                                 lpByte
                                 ) ) ;
}

*/


//-----------------------------------------------------------------------------
// WINGDIAPI UINT WINAPI GetEnhMetaFileHeader( IN HENHMETAFILE, IN UINT, OUT LPENHMETAHEADER );

/*

HB_FUNC( GETENHMETAFILEHEADER )
{
   LPENHMETAHEADER lPenhMetaHeader ;

   // Your code goes here

   hb_retni( GetEnhMetaFileHeader( (HENHMETAFILE) hb_parnl( 1 ),
                                   (UINT) hb_parni( 2 )        ,
                                   lPenhMetaHeader
                                   ) ) ;
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI UINT WINAPI GetEnhMetaFilePaletteEntries( IN HENHMETAFILE, IN UINT, OUT LPPALETTEENTRY );

/*

HB_FUNC( GETENHMETAFILEPALETTEENTRIES )
{
   LPPALETTEENTRY lpPaletteEntry ;

   // Your code goes here

   hb_retni( GetEnhMetaFilePaletteEntries( (HENHMETAFILE) hb_parnl( 1 ),
                                           (UINT) hb_parni( 2 )        ,
                                           lpPaletteEntry
                                           ) ) ;
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI UINT WINAPI GetEnhMetaFilePixelFormat( IN HENHMETAFILE, IN UINT, OUT PIXELFORMATDESCRIPTOR *);

/*

HB_FUNC( GETENHMETAFILEPIXELFORMAT )
{
   PIXELFORMATDESCRIPTOR PixelFormatdescriptor ;

   // Your code goes here

   hb_retni( GetEnhMetaFilePixelFormat( (HENHMETAFILE) hb_parnl( 1 ),
                                        (UINT) hb_parni( 2 )        ,
                                        &PixelFormatdescriptor
                                        ) ) ;
}

*/

