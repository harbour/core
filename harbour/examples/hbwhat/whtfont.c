/*
 * $Id$
 */


// hbwhat
// Font functions




#define HB_OS_WIN_USED
#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0400

#include "hbwhat.h"

#include <windows.h>
#include "hbapiitm.h"
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
//#include "hbapiitm.h"

extern PHB_ITEM Rect2Array( RECT *rc  );
extern BOOL Array2Rect(PHB_ITEM aRect, RECT *rc );
extern PHB_ITEM Point2Array( POINT *pt  );
extern BOOL Array2Point(PHB_ITEM aPoint, POINT *pt );

int CALLBACK GenericCallbackProc( LONG param1, LONG param2, int wParam, LPARAM lParam );
int CALLBACK GenericCallblockProc( LONG param1, LONG param2, int wParam, LPARAM lParam );

//-----------------------------------------------------------------------------
// WINGDIAPI HFONT WINAPI CreateFontA( IN int, IN int, IN int, IN int, IN int, IN DWORD, IN DWORD, IN DWORD, IN DWORD, IN DWORD, IN DWORD, IN DWORD, IN DWORD, IN LPCSTR);

// provide default 0 to first 5 parameters

HB_FUNC( VWN_CREATEFONT )
{

   if ( ISARRAY(1))
   {
      HB_RETWH( CreateFont( hb_parni( 1, 1 )         ,  // nHeight
                                    hb_parni( 1, 2 )         ,  // nWidth
                                    hb_parni( 1, 3 )         ,  // nEscapement
                                    hb_parni( 1, 4 )         ,  // nOrientation
                                    hb_parni( 1, 5 )         ,  // fnWeight
                                    (DWORD) hb_parnl( 1, 6 ) ,  // fdwItalic
                                    (DWORD) hb_parnl( 1, 7 ) ,  // fdwUnderline
                                    (DWORD) hb_parnl( 1, 8 ) ,  // fdwStrikeOut
                                    (DWORD) hb_parnl( 1, 9 ) ,  // fdwCharSet
                                    (DWORD) hb_parnl( 1, 10 ),  // fdwOutputPrecision
                                    (DWORD) hb_parnl( 1, 11 ),  // fdwClipPrecision
                                    (DWORD) hb_parnl( 1, 12 ),  // fdwQuality
                                    (DWORD) hb_parnl( 1, 13 ),  // fdwPitchAndFamily
                                    (LPCSTR) hb_parcx( 1, 14 )   // lpszFace
                                   ) );

   }
   else
   {
      HB_RETWH( CreateFont( ISNIL(1) ? 0 : hb_parni( 1 )         ,  // nHeight
                                    ISNIL(2) ? 0 : hb_parni( 2 )         ,  // nWidth
                                    ISNIL(3) ? 0 : hb_parni( 3 )         ,  // nEscapement
                                    ISNIL(4) ? 0 : hb_parni( 4 )         ,  // nOrientation
                                    ISNIL(5) ? 0 : hb_parni( 5 )         ,  // fnWeight
                                    (DWORD) hb_parnl( 6 ) ,  // fdwItalic
                                    (DWORD) hb_parnl( 7 ) ,  // fdwUnderline
                                    (DWORD) hb_parnl( 8 ) ,  // fdwStrikeOut
                                    (DWORD) hb_parnl( 9 ) ,  // fdwCharSet
                                    (DWORD) hb_parnl( 10 ),  // fdwOutputPrecision
                                    (DWORD) hb_parnl( 11 ),  // fdwClipPrecision
                                    (DWORD) hb_parnl( 12 ),  // fdwQuality
                                    (DWORD) hb_parnl( 13 ),  // fdwPitchAndFamily
                                    (LPCSTR) hb_parcx( 14 )   // lpszFace
                                   ) );

   }

}

//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI AddFontResourceA(IN LPCSTR);

HB_FUNC( VWN_ADDFONTRESOURCE )
{
   hb_retni( AddFontResource( (LPCSTR) hb_parcx( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI HFONT WINAPI CreateFontIndirectA( IN CONST LOGFONTA *);

HB_FUNC( VWN_CREATEFONTINDIRECT )
{
   LOGFONT *lf = (LOGFONT * ) hb_parc( 1 ); //hb_param( 1, HB_IT_STRING )->item.asString.value;

   HB_RETWH( CreateFontIndirect( lf ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI HFONT WINAPI CreateFontIndirectEx( IN CONST ENUMLOGFONTEXDVA *);

// No info

/*

HB_FUNC( VWN_CREATEFONTINDIRECTEX )
{
   ENUMLOGFONTEXDVA

   // Your code goes here

   HB_RETWH( CreateFontIndirectEx( &ENUMLOGFONTEXDVA ) );
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI CreateScalableFontResourceA( IN DWORD, IN LPCSTR, IN LPCSTR, IN LPCSTR);


HB_FUNC( VWN_CREATESCALABLEFONTRESOURCE )
{
   hb_retl( CreateScalableFontResource( (DWORD) hb_parnl( 1 ),
                                         (LPCSTR) hb_parcx( 2 ),
                                         (LPCSTR) hb_parcx( 3 ),
                                         (LPCSTR) hb_parcx( 4 )
                                         ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI EnumFontFamiliesA( IN HDC, IN LPCSTR, IN FONTENUMPROCA, IN LPARAM);

// syntax
// EnumFontFamilies(hDC,cTypeFace,codeBlock) -> HBFuncLastReturnValue or NIL if problem


HB_FUNC( VWN_ENUMFONTFAMILIES )
{
   LPARAM        lParam        ;

   if ( ISBLOCK( 3 ) )
   {
     lParam = (LPARAM) (PHB_ITEM ) hb_param( 3, HB_IT_BLOCK );


     hb_retni( EnumFontFamilies( (HDC) HB_PARWH( 1 )  ,
                                (LPCSTR) hb_parcx( 2 ),
                                (FONTENUMPROC) GenericCallblockProc  ,
                                lParam
                                ) );
   }
   else
     OutputDebugString("EnumFontFamilies(): No codeblock");


}



//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI EnumFontFamiliesExA( IN HDC, IN LPLOGFONTA, IN FONTENUMPROCA, IN LPARAM, IN DWORD);

// syntax
// EnumFontFamiliesEx(hDC,LogFontStruct,codeBlock) -> HBFuncLastReturnValue or NIL if problem

HB_FUNC( VWN_ENUMFONTFAMILIESEX )
{
   LOGFONT *LogFont = (LOGFONT * ) hb_parc( 2 ); //hb_param( 2, HB_IT_STRING )->item.asString.value ;
   LPARAM  lParam  ;

   if ( ISBLOCK( 3 ) )
   {
     lParam = (LPARAM) (PHB_ITEM ) hb_param( 3, HB_IT_BLOCK );



   hb_retni( EnumFontFamiliesEx( (HDC) HB_PARWH( 1 )  ,
                                  LogFont            ,
                                  (FONTENUMPROC) GenericCallblockProc  ,
                                  lParam               ,
                                  0
                                  ) );

  }
   else
     OutputDebugString("EnumFontFamiliesEx(): No codeblock");

}





//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI EnumFontsA( IN HDC, IN LPCSTR, IN FONTENUMPROCA, IN LPARAM);

// syntax
// EnumFonts(hDC,cTypeFace,codeBlock) -> HBFuncLastReturnValue or NIL if problem

HB_FUNC( VWN_ENUMFONTS )
{
   LPARAM lParam ;

   if ( ISBLOCK( 3 ) )
   {
     lParam = (LPARAM) (PHB_ITEM ) hb_param( 3, HB_IT_BLOCK );

     hb_retni( EnumFonts( (HDC) HB_PARWH( 1 )  ,
                         (LPCSTR) hb_parcx( 2 ),
                         (FONTENUMPROC) GenericCallblockProc  ,
                         lParam
                         ) );
   }
   else
     OutputDebugString("EnumFonts(): No codeblock");

}


/*

//-----------------------------------------------------------------------------

//using function name

int CALLBACK GenericCallbackProc( LONG param1, LONG param2, int wParam, LPARAM lParam )
{

   PHB_DYNS pSymTest ;
   long int res;

   pSymTest = hb_dynsymFind( (char *) lParam );

   if ( pSymTest )
   {
      hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushNil();
      hb_vmPushLong( (LONG ) param1 );
      hb_vmPushLong( (LONG ) param2 );
      hb_vmPushLong( (LONG ) wParam );
      hb_vmPushLong( (LONG ) lParam );
      hb_vmDo( 4 );
      res = hb_itemGetNL( (PHB_ITEM) hb_param( -1, HB_IT_ANY ) );

      return res;
   }
   else // shouldn't happen
      return( 0 );

}
*/


//-----------------------------------------------------------------------------

// using a codeblock
int CALLBACK GenericCallblockProc( LONG param1, LONG param2, int wParam, LPARAM lParam )
{
   PHB_ITEM pItem ;
   long int res   ;
   static PHB_DYNS s_pEval = NULL;

   if( s_pEval == NULL )
   {
      s_pEval = hb_dynsymFind( "__EVAL" );
   }

   pItem = (PHB_ITEM ) lParam ;

   if ( pItem )
   {
      hb_vmPush(s_pEval);//s_pEval->pSymbol;
      hb_vmPush(pItem);

      hb_vmPushLong( (LONG ) param1 );
      hb_vmPushLong( (LONG ) param2 );
      hb_vmPushLong( (LONG ) wParam );
      hb_vmPushLong( (LONG ) lParam );

      hb_vmSend( 4 );
      res = hb_itemGetNL( (PHB_ITEM) hb_param( -1, HB_IT_ANY ) );

      return res;
   }
   else // shouldn't happen
   {
      return( 0 );
   }
}

//-----------------------------------------------------------------------------

// WINGDIAPI DWORD WINAPI GetFontData( IN HDC, IN DWORD, IN DWORD, OUT LPVOID, IN DWORD);

// syntax
// GetFontData(hDC, nTable, dwOffset,[@cBuffer]


HB_FUNC( VWN_GETFONTDATA )
{
   char * cBuffer = NULL;
   DWORD dwRet ;
   if ( ! ISNIL( 5 ) && ( hb_parnl( 5 ) > 0 ) )
      cBuffer = (char *) hb_xgrab( hb_parnl(5));

   dwRet = GetFontData( (HDC) HB_PARWH( 1 )  ,
                                 (DWORD) hb_parnl( 2 ),
                                 (DWORD) hb_parnl( 3 ),
                                 ( ISNIL( 5 ) || ( hb_parnl( 5 ) <= 0 ) ) ? NULL :cBuffer  ,
                                 (DWORD) ISNIL( 5 ) ? 0 : hb_parnl( 5 )
                      );

   hb_retnl( (LONG) dwRet );

   if ( ! ISNIL( 5 ) && ( hb_parnl( 5 ) > 0 ) )
   {
      hb_storclen(cBuffer, dwRet, 4 );
      hb_xfree( cBuffer );
   }

}


//-----------------------------------------------------------------------------
// WINGDIAPI DWORD WINAPI GetFontLanguageInfo( IN HDC );


HB_FUNC( VWN_GETFONTLANGUAGEINFO )
{
   hb_retnl( (LONG) GetFontLanguageInfo( (HDC) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI DWORD WINAPI GetFontUnicodeRanges( IN HDC, OUT LPGLYPHSET);

// no prototype ?

/*

HB_FUNC( VWN_GETFONTUNICODERANGES )
{
   LPGLYPHSET lpglyphSet ;

   // Your code goes here

   hb_retnl( (LONG) GetFontUnicodeRanges( (HDC) HB_PARWH( 1 ), lpglyphSet ) );
}

*/


//-----------------------------------------------------------------------------
// WINGDIAPI HANDLE WINAPI AddFontMemResourceEx( IN PVOID, IN DWORD, IN PVOID , IN DWORD*);

// no info

/*

HB_FUNC( VWN_ADDFONTMEMRESOURCEEX )
{
   PVOID pVoid1 ;
   PVOID pVoid2 ;

   // Your code goes here

   hb_retnl( (LONG) AddFontMemResourceEx( pVoid1               ,
                                          (DWORD) hb_parnl( 2 ),
                                          pVoid2               ,
                                          (DWORD) hb_parnl( 4 )
                                          ) );
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI AddFontResourceExA( IN LPCSTR, IN DWORD, IN PVOID);

// no info

/*

HB_FUNC( VWN_ADDFONTRESOURCEEXA )
{
   PVOID  pVoid  ;

   // Your code goes here

   hb_retni( AddFontResourceExA( (LPCSTR) hb_parcx( 1 ),
                                 (DWORD) hb_parnl( 2 ),
                                 pVoid
                                 ) );
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI RemoveFontMemResourceEx( IN HANDLE);

// no prototype ?

/*
HB_FUNC( VWN_REMOVEFONTMEMRESOURCEEX )
{
   hb_retl( RemoveFontMemResourceExA( (HANDLE) HB_PARWH( 1 ) ) );
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI RemoveFontResourceA(IN LPCSTR);


HB_FUNC( VWN_REMOVEFONTRESOURCE )
{
   hb_retl( RemoveFontResource( (LPCSTR) hb_parcx( 1 ) ) );
}



//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI RemoveFontResourceExA( IN LPCSTR, IN DWORD, IN PVOID);

// no info

/*

HB_FUNC( VWN_REMOVEFONTRESOURCEEXA )
{
   PVOID  pVoid  ;

   // Your code goes here

   hb_retl( RemoveFontResourceExA( (LPCSTR) hb_parcx( 1 ),
                                   (DWORD) hb_parnl( 2 ),
                                   pVoid
                                   ) );
}

*/
