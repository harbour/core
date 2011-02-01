/*
 * $Id$
 */

/*
 * Copyright 2008 Pritpal Bedi <pritpal@vouchcac.com>
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option )
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/ ).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*----------------------------------------------------------------------*
 *                                                                      *
 *                      HaruLib (x)Harbour Wrappers                     *
 *                                   .                                  *
 *                        http://www.libharu.org/                       *
 *                http://sourceforge.net/projects/libharu/              *
 *                                                                      *
 *                   Pritpal Bedi <pritpal@hotmail.com>                 *
 *                                                                      *
 *----------------------------------------------------------------------*/

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"

#include "hpdf.h"

#define HB_HPDF_VERS( ma, mi, re ) ( HPDF_MAJOR_VERSION >= ma && HPDF_MINOR_VERSION >= mi && HPDF_BUGFIX_VERSION >= re )

static HB_GARBAGE_FUNC( HPDF_Doc_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && * ph )
   {
      /* Destroy the object */
      HPDF_Free( ( HPDF_Doc ) * ph );

      /* set pointer to NULL to avoid multiple freeing */
      * ph = NULL;
   }
}

static const HB_GC_FUNCS s_gcHPDF_DocFuncs =
{
   HPDF_Doc_release,
   hb_gcDummyMark
};

static HPDF_Doc HPDF_Doc_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gcHPDF_DocFuncs, iParam );

   return ph ? ( HPDF_Doc ) * ph : NULL;
}

/*----------------------------------------------------------------------*/
/*
     Most of the functions return hStatus == HPDF_OK or ERROR Code
*/
/*----------------------------------------------------------------------*/
/* HPdf_New() -> hDoc
*/
HB_FUNC( HPDF_NEW )
{
   void ** ph = ( void ** ) hb_gcAllocate( sizeof( HPDF_Doc ), &s_gcHPDF_DocFuncs );

   * ph = ( void * ) HPDF_New( NULL, NULL );

   hb_retptrGC( ph );
}
/*----------------------------------------------------------------------*/
/* HPdf_Free( hDoc ) -> NIL
*/
HB_FUNC( HPDF_FREE )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gcHPDF_DocFuncs, 1 );

   if( ph && * ph )
   {
      /* Destroy the object */
      HPDF_Free( ( HPDF_Doc ) * ph );

      /* set pointer to NULL to avoid multiple freeing */
      * ph = NULL;
   }
}
/*----------------------------------------------------------------------*/
/* HPdf_NewDoc( hDoc ) -> hStatus
*/
HB_FUNC( HPDF_NEWDOC )
{
   hb_retnl( ( long ) HPDF_NewDoc( HPDF_Doc_par( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_FreeDoc( hNewDoc ) -> NIL
*/
HB_FUNC( HPDF_FREEDOC )
{
   HPDF_FreeDoc( HPDF_Doc_par( 1 ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_FreeDocAll() -> NIL
*/
HB_FUNC( HPDF_FREEDOCALL )
{
   HPDF_FreeDocAll( HPDF_Doc_par( 1 ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_SaveToFile( hDoc, cFileToSave ) -> hStatus
*/
HB_FUNC( HPDF_SAVETOFILE )
{
   char * pszFree;
   const char * pszFileName = hb_fsNameConv( hb_parcx( 2 ), &pszFree );

   hb_retnl( ( long ) HPDF_SaveToFile( HPDF_Doc_par( 1 ), pszFileName ) );

   if( pszFree )
      hb_xfree( pszFree );
}
/*----------------------------------------------------------------------*/
/* HPdf_SaveToStream( hDoc ) -> hStatus
*/
HB_FUNC( HPDF_SAVETOSTREAM )
{
   hb_retnl( ( long ) HPDF_SaveToStream( HPDF_Doc_par( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_GetStreamSize( hDoc ) -> nSize
*/
HB_FUNC( HPDF_GETSTREAMSIZE )
{
   hb_retnl( ( long ) HPDF_GetStreamSize( HPDF_Doc_par( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_ReadFromStream( hDoc, @cBuffer ) -> nBytesRead
*/
HB_FUNC( HPDF_READFROMSTREAM )
{
   HPDF_UINT32 size = ( HPDF_UINT32 ) hb_parclen( 2 );
   HPDF_BYTE * buffer;

   if( size < 1024 )
      size = 1024;

   buffer = ( HPDF_BYTE * ) hb_xgrab( size + 1 );

   hb_retnl( ( long ) HPDF_ReadFromStream( HPDF_Doc_par( 1 ), buffer, &size ) );

   if( ! hb_storclen_buffer( ( char * ) buffer, size, 2 ) )
      hb_xfree( buffer );
}
/*----------------------------------------------------------------------*/
/* HPdf_ResetStream( hDoc ) -> hStatus
*/
HB_FUNC( HPDF_RESETSTREAM )
{
   hb_retnl( ( long ) HPDF_ResetStream( HPDF_Doc_par( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_HasDoc( hDoc ) -> lHasDoc
*/
HB_FUNC( HPDF_HASDOC )
{
   hb_retl( HPDF_HasDoc( HPDF_Doc_par( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_SetErrorHandler( hDoc, procErrHandler ) -> hStatus
*/
HB_FUNC( HPDF_SETERRORHANDLER )
{
   /* TOFIX: This should be extended to pass a wrapper which calls a
             user defined codeblock. */

   hb_retnl( ( long ) HPDF_SetErrorHandler( HPDF_Doc_par( 1 ), ( HPDF_Error_Handler ) hb_parptr( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_GetError( hDoc ) -> nErrorCode
*/
HB_FUNC( HPDF_GETERROR )
{
   hb_retnl( ( long ) HPDF_GetError( HPDF_Doc_par( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_ResetError( hDoc ) -> NIL
*/
HB_FUNC( HPDF_RESETERROR )
{
   HPDF_ResetError( HPDF_Doc_par( 1 ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_SetPagesConfiguration( hDoc, nPagePerPages ) -> hStatus
*/
HB_FUNC( HPDF_SETPAGESCONFIGURATION )
{
   hb_retnl( ( long ) HPDF_SetPagesConfiguration( HPDF_Doc_par( 1 ), hb_parni( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_SetPageLayout( hDoc, nLayout ) -> hStatus
        nLayout ==
   HPDF_PAGE_LAYOUT_SINGLE             0
   HPDF_PAGE_LAYOUT_ONE_COLUMN         1
   HPDF_PAGE_LAYOUT_TWO_COLUMN_LEFT    2
   HPDF_PAGE_LAYOUT_TWO_COLUMN_RIGHT   3
   HPDF_PAGE_LAYOUT_EOF                4
*/
HB_FUNC( HPDF_SETPAGELAYOUT )
{
   hb_retnl( ( long ) HPDF_SetPageLayout( HPDF_Doc_par( 1 ), ( HPDF_PageLayout ) hb_parni( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_GetPageLayout( hDoc ) -> nLayout
*/
HB_FUNC( HPDF_GETPAGELAYOUT )
{
   hb_retni( ( int ) HPDF_GetPageLayout( HPDF_Doc_par( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_SetPageMode( hDoc, nPageMode ) -> hStatus
       nPageMode ==
   HPDF_PAGE_MODE_USE_NONE     0  Display the document with neither outline nor thumbnail.
   HPDF_PAGE_MODE_USE_OUTLINE  1  Display the document with outline pane.
   HPDF_PAGE_MODE_USE_THUMBS   2  Display the document with thumbnail pane.
   HPDF_PAGE_MODE_FULL_SCREEN  3  Display the document with full screen mode.
   HPDF_PAGE_MODE_EOF          4
*/
HB_FUNC( HPDF_SETPAGEMODE )
{
   hb_retnl( ( long ) HPDF_SetPageMode( HPDF_Doc_par( 1 ), ( HPDF_PageMode ) hb_parni( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_GetPageMode( hDoc ) -> nPageMode
*/
HB_FUNC( HPDF_GETPAGEMODE )
{
   hb_retni( ( int ) HPDF_GetPageMode( HPDF_Doc_par( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_SetOpenAction( hDoc, hDestn ) -> hStatus
*/
HB_FUNC( HPDF_SETOPENACTION )
{
   hb_retnl( ( long ) HPDF_SetOpenAction( HPDF_Doc_par( 1 ), ( HPDF_Destination ) hb_parptr( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_GetCurrentPage( hDoc ) -> hPage
*/
HB_FUNC( HPDF_GETCURRENTPAGE )
{
   hb_retptr( ( void * ) HPDF_GetCurrentPage( HPDF_Doc_par( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_AddPage( hDoc ) -> hPage
*/
HB_FUNC( HPDF_ADDPAGE )
{
   hb_retptr( ( void  * ) HPDF_AddPage( HPDF_Doc_par( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_InsertPage( hDoc, hPage ) -> hPageInserted  : Just before hPage
*/
HB_FUNC( HPDF_INSERTPAGE )
{
   hb_retptr( ( void * ) HPDF_InsertPage( HPDF_Doc_par( 1 ), ( HPDF_Page ) hb_parptr( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_GetFont( hDoc, cFontName, cEncoding ) -> hFont
*/
HB_FUNC( HPDF_GETFONT )
{
   hb_retptr( ( void * ) HPDF_GetFont( HPDF_Doc_par( 1 ), hb_parc( 2 ), hb_parc( 3 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_LoadTypeIFontFromFile( hDoc, cAFMFileName, cPFA_PFBFileName ) -> cFontName
*/
HB_FUNC( HPDF_LOADTYPE1FONTFROMFILE )
{
   char * pszFree1;
   const char * pszFileName1 = hb_fsNameConv( hb_parcx( 2 ), &pszFree1 );
   char * pszFree2;
   const char * pszFileName2 = hb_fsNameConv( hb_parcx( 3 ), &pszFree2 );

   hb_retc( HPDF_LoadType1FontFromFile( HPDF_Doc_par( 1 ), pszFileName1, pszFileName2 ) );

   if( pszFree1 )
      hb_xfree( pszFree1 );

   if( pszFree2 )
      hb_xfree( pszFree2 );
}
/*----------------------------------------------------------------------*/
/* HPdf_LoadTTFontFromFile( hDoc, cTTFontFileName, lEmbed ) -> cFontName
*/
HB_FUNC( HPDF_LOADTTFONTFROMFILE )
{
   char * pszFree;
   const char * pszFileName = hb_fsNameConv( hb_parcx( 2 ), &pszFree );

   hb_retc( HPDF_LoadTTFontFromFile( HPDF_Doc_par( 1 ), pszFileName, hb_parl( 3 ) ? HPDF_TRUE : HPDF_FALSE ) );

   if( pszFree )
      hb_xfree( pszFree );
}
/*----------------------------------------------------------------------*/
/* HPdf_LoadTTFontFromFile2( hDoc, cTTFontFileName, nIndexInFile, lEmbed ) -> cFontName
*/
HB_FUNC( HPDF_LOADTTFONTFROMFILE2 )
{
   char * pszFree;
   const char * pszFileName = hb_fsNameConv( hb_parcx( 2 ), &pszFree );

   hb_retc( HPDF_LoadTTFontFromFile2( HPDF_Doc_par( 1 ), pszFileName, hb_parni( 3 ), hb_parl( 4 ) ? HPDF_TRUE : HPDF_FALSE ) );

   if( pszFree )
      hb_xfree( pszFree );
}
/*----------------------------------------------------------------------*/
/* HPdf_AddPageLabel( hDoc, nPageNum, nPgNoStyle, nFirstPageInRange, cPrefixToLabel ) -> hStatus
       nPgNoStyle
   HPDF_PAGE_NUM_STYLE_DECIMAL         1   Page label is displayed by Arabic numerals.
   HPDF_PAGE_NUM_STYLE_UPPER_ROMAN     2   Page label is displayed by Uppercase roman numerals.
   HPDF_PAGE_NUM_STYLE_LOWER_ROMAN     3   Page label is displayed by Lowercase roman numerals.
   HPDF_PAGE_NUM_STYLE_UPPER_LETTERS   4   Page label is displayed by Uppercase letters (using A to Z).
   HPDF_PAGE_NUM_STYLE_LOWER_LETTERS   5   Page label is displayed by Lowercase letters (using a to z).
*/
HB_FUNC( HPDF_ADDPAGELABEL )
{
   hb_retnl( ( long ) HPDF_AddPageLabel( HPDF_Doc_par( 1 ), hb_parni( 2 ), ( HPDF_PageNumStyle ) hb_parni( 3 ), hb_parni( 4 ), hb_parc( 5 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_UseJPFonts( hDoc ) -> hStatus
*/
HB_FUNC( HPDF_USEJPFONTS )
{
   hb_retnl( ( long ) HPDF_UseJPFonts( HPDF_Doc_par( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_UseKRFonts( hDoc ) -> hStatus
*/
HB_FUNC( HPDF_USEKRFONTS )
{
   hb_retnl( ( long ) HPDF_UseKRFonts( HPDF_Doc_par( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_UseCNSFonts( hDoc ) -> hStatus
*/
HB_FUNC( HPDF_USECNSFONTS )
{
   hb_retnl( ( long ) HPDF_UseCNSFonts( HPDF_Doc_par( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_UseCNTFonts( hDoc ) -> hStatus
*/
HB_FUNC( HPDF_USECNTFONTS )
{
   hb_retnl( ( long ) HPDF_UseCNTFonts( HPDF_Doc_par( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_CreateExtGState( hDoc ) -> hExtGState
*/
HB_FUNC( HPDF_CREATEEXTGSTATE )
{
   hb_retptr( ( void * ) HPDF_CreateExtGState( HPDF_Doc_par( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_CreateOutline( hDoc, hParentOutline, cTitle, hEncoder ) -> hOutline
*/
HB_FUNC( HPDF_CREATEOUTLINE )
{
   hb_retptr( ( void * ) HPDF_CreateOutline( HPDF_Doc_par( 1 ), ( HPDF_Outline ) hb_parptr( 2 ), hb_parc( 3 ), ( HPDF_Encoder ) hb_parptr( 4 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_GetEncoder( hDoc, cEncoding ) -> hEncoder
*/
HB_FUNC( HPDF_GETENCODER )
{
   hb_retptr( ( void * ) HPDF_GetEncoder( HPDF_Doc_par( 1 ), hb_parc( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_GetCurrentEncoder( hDoc ) -> hEncoder
*/
HB_FUNC( HPDF_GETCURRENTENCODER )
{
   hb_retptr( ( void * ) HPDF_GetCurrentEncoder( HPDF_Doc_par( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_SetCurrentEncoder( hDoc, hEncoder ) -> hStatus
*/
HB_FUNC( HPDF_SETCURRENTENCODER )
{
   hb_retnl( ( long ) HPDF_SetCurrentEncoder( HPDF_Doc_par( 1 ), hb_parc( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_UseJPEncodings( hDoc ) -> hStatus
*/
HB_FUNC( HPDF_USEJPENCODINGS )
{
   hb_retnl( ( long ) HPDF_UseJPEncodings( HPDF_Doc_par( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_UseKREncodings( hDoc ) -> hStatus
*/
HB_FUNC( HPDF_USEKRENCODINGS )
{
   hb_retnl( ( long ) HPDF_UseKREncodings( HPDF_Doc_par( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_UseCNSEncodings( hDoc ) -> hStatus
*/
HB_FUNC( HPDF_USECNSENCODINGS )
{
   hb_retnl( ( long ) HPDF_UseCNSEncodings( HPDF_Doc_par( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_UseCNTEncodings( hDoc ) -> hStatus
*/
HB_FUNC( HPDF_USECNTENCODINGS )
{
   hb_retnl( ( long ) HPDF_UseCNTEncodings( HPDF_Doc_par( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_LoadPngImageFromFile( hDoc, cPNGFileName ) -> hImage
*/
HB_FUNC( HPDF_LOADPNGIMAGEFROMFILE )
{
   hb_retptr( ( void * ) HPDF_LoadPngImageFromFile( HPDF_Doc_par( 1 ), hb_parc( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_LoadPngImageFromFile2( hDoc, cPNGFileName ) -> hImage
*/
HB_FUNC( HPDF_LOADPNGIMAGEFROMFILE2 )
{
   hb_retptr( ( void * ) HPDF_LoadPngImageFromFile2( HPDF_Doc_par( 1 ), hb_parc( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_LoadRawImageFromFile( hDoc, cImageFileName, nWidth, nHeight, nColorSpace ) -> hImage
       nColorSpace
   HPDF_CS_DEVICE_GRAY
   HPDF_CS_DEVICE_RGB
   HPDF_CS_DEVICE_CMYK
*/
HB_FUNC( HPDF_LOADRAWIMAGEFROMFILE )
{
   hb_retptr( ( void * ) HPDF_LoadRawImageFromFile( HPDF_Doc_par( 1 ), hb_parc( 2 ), hb_parni( 3 ), hb_parni( 4 ), ( HPDF_ColorSpace ) hb_parni( 5 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_LoadRawImageFromMem( hDoc, cBuffer, nWidth, nHeight, nColorSpace, nBitsPerComponents ) -> hImage
*/
HB_FUNC( HPDF_LOADRAWIMAGEFROMMEM )
{
   hb_retptr( ( void * ) HPDF_LoadRawImageFromMem( HPDF_Doc_par( 1 ), ( HPDF_BYTE * ) hb_parc( 2 ), hb_parni( 3 ), hb_parni( 4 ), ( HPDF_ColorSpace ) hb_parni( 5 ), hb_parni( 6 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_LoadJPEGImageFromFile( hDoc, cHPEGFileName ) -> hImage
*/
HB_FUNC( HPDF_LOADJPEGIMAGEFROMFILE )
{
   hb_retptr( ( void * ) HPDF_LoadJpegImageFromFile( HPDF_Doc_par( 1 ), hb_parc( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_SetInfoAttr( hDoc, nInfoType, cInfo ) -> hStatus
       nInfoType ==
   HPDF_INFO_AUTHOR
   HPDF_INFO_CREATOR
   HPDF_INFO_TITLE
   HPDF_INFO_SUBJECT
   HPDF_INFO_KEYWORDS
*/
HB_FUNC( HPDF_SETINFOATTR )
{
   hb_retnl( ( long ) HPDF_SetInfoAttr( HPDF_Doc_par( 1 ), ( HPDF_InfoType ) hb_parni( 2 ), hb_parc( 3 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_GetInfoAttr( hDoc, nInfoType ) -> cInfo
*/
HB_FUNC( HPDF_GETINFOATTR )
{
   hb_retc( HPDF_GetInfoAttr( HPDF_Doc_par( 1 ), ( HPDF_InfoType ) hb_parni( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_SetInfoDateAttr( hDoc, nInfoType, aDateValues ) -> hStatus
       nInfoType ==
   HPDF_INFO_CREATION_DATE
   HPDF_INFO_MOD_DATE
*/
HB_FUNC( HPDF_SETINFODATEATTR )
{
   HPDF_Date date;

   memset( &date, 0, sizeof( date ) );

   date.year    = hb_parvni( 3, 1 );
   date.month   = hb_parvni( 3, 2 );
   date.day     = hb_parvni( 3, 3 );
   date.hour    = hb_parvni( 3, 4 );
   date.minutes = hb_parvni( 3, 5 );
   date.seconds = hb_parvni( 3, 6 );
   date.ind     = ' ';

   hb_retnl( ( long ) HPDF_SetInfoDateAttr( HPDF_Doc_par( 1 ), ( HPDF_InfoType ) hb_parni( 2 ), date ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_SetPassword( hDoc, cOwnerPassword = NO NIL, cUserPassword = CANBE NIL ) -> hStatus
*/
HB_FUNC( HPDF_SETPASSWORD )
{
   hb_retnl( ( long ) HPDF_SetPassword( HPDF_Doc_par( 1 ), hb_parc( 2 ), hb_parc( 3 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_SetPermission( hDoc, nPermission ) -> hStatus
       nPermission ==
   HPDF_ENABLE_READ      1   user can read the document.
   HPDF_ENABLE_PRINT     2   user can print the document.
   HPDF_ENABLE_EDIT_ALL  3   user can edit the contents of the document other than annotations, form fields.
   HPDF_ENABLE_COPY      4   user can copy the text and the graphics of the document.
   HPDF_ENABLE_EDIT      5   user can add or modify the annotations and form fields of the document.
*/
HB_FUNC( HPDF_SETPERMISSION )
{
   hb_retnl( ( long ) HPDF_SetPermission( HPDF_Doc_par( 1 ), hb_parni( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_SetEncryptionMode( hDoc, nEncMode, nKeyLen ) -> hStatus
       nEncMode ==
   HPDF_ENCRYPT_R2    1   Use "Revision 2" algorithm.
                             The length of key is automatically set to 5(40bit).
   HPDF_ENCRYPT_R3    2   Use "Revision 3" algorithm.
                             Between 5(40bit) and 16(128bit) can be specified for length of the key
*/
HB_FUNC( HPDF_SETENCRYPTIONMODE )
{
   hb_retnl( ( long ) HPDF_SetEncryptionMode( HPDF_Doc_par( 1 ), ( HPDF_EncryptMode ) hb_parni( 2 ), hb_parni( 3 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_SetCompressionMode( hDoc, nCompMode ) -> hStatus
       nCompMode ==
   HPDF_COMP_NONE         1    All contents are not compressed.
   HPDF_COMP_TEXT         2    Compress the contents stream of the page.
   HPDF_COMP_IMAGE        3    Compress the streams of the image objects.
   HPDF_COMP_METADATA     4    Other stream datas (fonts, cmaps and so on)  are compressed.
   HPDF_COMP_ALL          5    All stream datas are compressed. (The same as "HPDF_COMP_TEXT | HPDF_COMP_IMAGE | HPDF_COMP_METADATA")
*/
HB_FUNC( HPDF_SETCOMPRESSIONMODE )
{
   hb_retnl( ( long ) HPDF_SetCompressionMode( HPDF_Doc_par( 1 ), hb_parni( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*                           Page Handling                              */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/* HPdf_Page_SetWidth( hPage, nWidth ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SETWIDTH )
{
   hb_retnl( ( long ) HPDF_Page_SetWidth( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_SetHeight( hPage, nHeight ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SETHEIGHT )
{
   hb_retnl( ( long ) HPDF_Page_SetHeight( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_SetSize( hPage, nSize, nOrientation = 1 Portrait, 2 Landscape ) -> hStatus
       nSize ==
   HPDF_PAGE_SIZE_LETTER         1      8½ x 11 (Inches) 612 x 792
   HPDF_PAGE_SIZE_LEGAL          2      8½ x 14 (Inches) 612 x 1008
   HPDF_PAGE_SIZE_A3             3      297 × 420 (mm) 841.89 x 1199.551
   HPDF_PAGE_SIZE_A4             4      210 × 297 (mm)  595.276 x 841.89
   HPDF_PAGE_SIZE_A5             5      148 × 210 (mm) 419.528 x 595.276
   HPDF_PAGE_SIZE_B4             6      250 × 353 (mm)  708.661 x 1000.63
   HPDF_PAGE_SIZE_B5             7      176 × 250 (mm) 498.898 x 708.661
   HPDF_PAGE_SIZE_EXECUTIVE      8      7½ x 10½ (Inches) 522 x 756
   HPDF_PAGE_SIZE_US4x6          9      4 x 6 (Inches) 288 x 432
   HPDF_PAGE_SIZE_US4x8         10      4 x 8 (Inches) 288 x 576
   HPDF_PAGE_SIZE_US5x7         11      5 x 7 (Inches) 360 x 504
   HPDF_PAGE_SIZE_COMM10        12      4.125 x 9.5 (Inches) 297x 684
*/
HB_FUNC( HPDF_PAGE_SETSIZE )
{
   hb_retnl( ( long ) HPDF_Page_SetSize( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_PageSizes ) hb_parni( 2 ), ( HPDF_PageDirection ) hb_parni( 3 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_SetRotate( hPage, nAngle = 0-360 ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SETROTATE )
{
   hb_retnl( ( long ) HPDF_Page_SetRotate( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_UINT16 ) hb_parni( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetWidth( hPage ) -> nWidth
*/
HB_FUNC( HPDF_PAGE_GETWIDTH )
{
   hb_retnd( ( double ) HPDF_Page_GetWidth( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetHeight( hPage ) -> nHeight
*/
HB_FUNC( HPDF_PAGE_GETHEIGHT )
{
   hb_retnd( ( double ) HPDF_Page_GetHeight( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_CreateDestination( hPage ) -> hDestn
*/
HB_FUNC( HPDF_PAGE_CREATEDESTINATION )
{
   hb_retptr( ( void * ) HPDF_Page_CreateDestination( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_CreateAnnot( hPage, aRect[nLeft,nTop,nRight,nBottom], cText, cEncoder ) -> nHandle
*/
HB_FUNC( HPDF_PAGE_CREATETEXTANNOT )
{
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retptr( HPDF_Page_CreateTextAnnot( ( HPDF_Page ) hb_parptr( 1 ), rc, hb_parc( 3 ), ( HPDF_Encoder ) hb_parptr( 4 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_CreateLinkAnnot( hPage, aRect, hDestn ) -> nHandle
*/
HB_FUNC( HPDF_PAGE_CREATELINKANNOT )
{
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retptr( HPDF_Page_CreateLinkAnnot( ( HPDF_Page ) hb_parptr( 1 ), rc, ( HPDF_Destination ) hb_parptr( 3 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_CreateURILinkAnnot( hPage, aRect, cURI ) -> nHandle
*/
HB_FUNC( HPDF_PAGE_CREATEURILINKANNOT )
{
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retptr( HPDF_Page_CreateURILinkAnnot( ( HPDF_Page ) hb_parptr( 1 ), rc, hb_parc( 3 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_TextWidth( hPage, cText ) -> nTextWidth
*/
HB_FUNC( HPDF_PAGE_TEXTWIDTH )
{
   hb_retnd( ( double ) HPDF_Page_TextWidth( ( HPDF_Page ) hb_parptr( 1 ), hb_parc( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_MeasureText( hPage, cText, nWidth, lWordWrap ) -> nByteLenOfTextToFitWidth
*/
HB_FUNC( HPDF_PAGE_MEASURETEXT )
{
   hb_retnl( ( long ) HPDF_Page_MeasureText( ( HPDF_Page ) hb_parptr( 1 ), hb_parc( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), hb_parl( 4 ) ? HPDF_TRUE : HPDF_FALSE, NULL ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetMode( hPage ) -> nGraphicMode
*/
HB_FUNC( HPDF_PAGE_GETGMODE )
{
   hb_retnl( ( long ) HPDF_Page_GetGMode( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetCurrentPos( hPage ) -> aCurPos[ nX, nY ]
*/
HB_FUNC( HPDF_PAGE_GETCURRENTPOS )
{
   HPDF_Point pt;
   PHB_ITEM info = hb_itemArrayNew( 2 );

   HPDF_Page_GetCurrentPos2( ( HPDF_Page ) hb_parptr( 1 ), &pt );

   hb_arraySetND( info, 1, pt.x );
   hb_arraySetND( info, 2, pt.y );

   hb_itemReturnRelease( info );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetCurrentTextPos( hPage ) -> aCurTextPos[ nX, nY ]
*/
HB_FUNC( HPDF_PAGE_GETCURRENTTEXTPOS )
{
   HPDF_Point pt;
   PHB_ITEM info = hb_itemArrayNew( 2 );

   HPDF_Page_GetCurrentTextPos2( ( HPDF_Page ) hb_parptr( 1 ), &pt );

   hb_arraySetND( info, 1, pt.x );
   hb_arraySetND( info, 2, pt.y );

   hb_itemReturnRelease( info );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetCurrentFont( hPage ) -> hFont
*/
HB_FUNC( HPDF_PAGE_GETCURRENTFONT )
{
   hb_retptr( ( void * ) HPDF_Page_GetCurrentFont( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetCurrentFontSize( hPage ) -> nFontSize
*/
HB_FUNC( HPDF_PAGE_GETCURRENTFONTSIZE )
{
   hb_retnd( ( double ) HPDF_Page_GetCurrentFontSize( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetTransMatrix( hPage ) -> aMatrix[ ]
*/
HB_FUNC( HPDF_PAGE_GETTRANSMATRIX )
{
   HPDF_TransMatrix matrix;
   PHB_ITEM info = hb_itemArrayNew( 6 );

   matrix = HPDF_Page_GetTransMatrix( ( HPDF_Page ) hb_parptr( 1 ) );

   hb_arraySetND( info, 1, matrix.a );
   hb_arraySetND( info, 2, matrix.b );
   hb_arraySetND( info, 3, matrix.c );
   hb_arraySetND( info, 4, matrix.d );
   hb_arraySetND( info, 5, matrix.x );
   hb_arraySetND( info, 6, matrix.y );

   hb_itemReturnRelease( info );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetLineWidth( hPage ) -> nLineWidth
*/
HB_FUNC( HPDF_PAGE_GETLINEWIDTH )
{
   hb_retnd( ( double ) HPDF_Page_GetLineWidth( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetLineCap( hPage ) -> nLineCapStyle
*/
HB_FUNC( HPDF_PAGE_GETLINECAP )
{
   hb_retnl( ( long ) HPDF_Page_GetLineCap( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetLineJoin( hPage ) -> nLineJoinStyle
*/
HB_FUNC( HPDF_PAGE_GETLINEJOIN )
{
   hb_retnl( ( long ) HPDF_Page_GetLineJoin( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetMiterLimit( hPage ) -> nMiterLimit
*/
HB_FUNC( HPDF_PAGE_GETMITERLIMIT )
{
   hb_retnd( ( double ) HPDF_Page_GetMiterLimit( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetDash( hPage ) -> aDash
*/
HB_FUNC( HPDF_PAGE_GETDASH )
{
   HPDF_DashMode dash;
   PHB_ITEM info = hb_itemArrayNew( 10 );

   dash = HPDF_Page_GetDash( ( HPDF_Page ) hb_parptr( 1 ) );

   hb_arraySetNI( info, 1, dash.ptn[0] );
   hb_arraySetNI( info, 2, dash.ptn[1] );
   hb_arraySetNI( info, 3, dash.ptn[2] );
   hb_arraySetNI( info, 4, dash.ptn[3] );
   hb_arraySetNI( info, 5, dash.ptn[4] );
   hb_arraySetNI( info, 6, dash.ptn[5] );
   hb_arraySetNI( info, 7, dash.ptn[6] );
   hb_arraySetNI( info, 8, dash.ptn[7] );
   hb_arraySetND( info, 9, dash.num_ptn );
   hb_arraySetND( info,10, dash.phase   );

   hb_itemReturnRelease( info );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetFlat( hPage ) -> nCurFlatness
*/
HB_FUNC( HPDF_PAGE_GETFLAT )
{
   hb_retnd( ( double ) HPDF_Page_GetFlat( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetCharSpace( hPage ) -> nCurCharSpace
*/
HB_FUNC( HPDF_PAGE_GETCHARSPACE )
{
   hb_retnd( ( double ) HPDF_Page_GetCharSpace( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetWordSpace( hPage ) -> nCurWordSpace
*/
HB_FUNC( HPDF_PAGE_GETWORDSPACE )
{
   hb_retnd( ( double ) HPDF_Page_GetWordSpace( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetHorizontalScalling( hPage ) -> nHorzScaling
*/
HB_FUNC( HPDF_PAGE_GETHORIZONTALSCALLING )
{
   hb_retnd( ( double ) HPDF_Page_GetHorizontalScalling( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetTextLeading( hPage ) -> nTextLeading
*/
HB_FUNC( HPDF_PAGE_GETTEXTLEADING )
{
   hb_retnd( ( double ) HPDF_Page_GetTextLeading( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetTextRenderingMode( hPage ) -> nTextRenderingMode
*/
HB_FUNC( HPDF_PAGE_GETTEXTRENDERINGMODE )
{
   hb_retnd( ( double ) HPDF_Page_GetTextRenderingMode( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetTextRise( hPage ) -> nTextRise
*/
HB_FUNC( HPDF_PAGE_GETTEXTRISE )
{
   hb_retnd( ( double ) HPDF_Page_GetTextRise( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetRGBFill( hPage ) -> aRGBFill[ nRed, nGreen, nBlue ]
*/
HB_FUNC( HPDF_PAGE_GETRGBFILL )
{
   HPDF_RGBColor rgb;
   PHB_ITEM info = hb_itemArrayNew( 3 );

   rgb = HPDF_Page_GetRGBFill( ( HPDF_Page ) hb_parptr( 1 ) );

   hb_arraySetND( info, 1, rgb.r );
   hb_arraySetND( info, 2, rgb.g );
   hb_arraySetND( info, 3, rgb.b );

   hb_itemReturnRelease( info );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetRGBStroke( hPage ) -> aRGBStroke[ nRed, nGreen, nBlue ]
*/
HB_FUNC( HPDF_PAGE_GETRGBSTROKE )
{
   HPDF_RGBColor rgb;
   PHB_ITEM info = hb_itemArrayNew( 3 );

   rgb = HPDF_Page_GetRGBStroke( ( HPDF_Page ) hb_parptr( 1 ) );

   hb_arraySetND( info, 1, rgb.r );
   hb_arraySetND( info, 2, rgb.g );
   hb_arraySetND( info, 3, rgb.b );

   hb_itemReturnRelease( info );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetCMYKFill( hPage ) -> aCMYKFill[ nC, nM, nY, nK ]
*/
HB_FUNC( HPDF_PAGE_GETCMYKFILL )
{
   HPDF_CMYKColor cmyk;
   PHB_ITEM info = hb_itemArrayNew( 4 );

   cmyk = HPDF_Page_GetCMYKFill( ( HPDF_Page ) hb_parptr( 1 ) );

   hb_arraySetND( info, 1, cmyk.c );
   hb_arraySetND( info, 2, cmyk.m );
   hb_arraySetND( info, 3, cmyk.y );
   hb_arraySetND( info, 4, cmyk.k );

   hb_itemReturnRelease( info );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetCMYKStroke( hPage ) -> aCMYKStroke[ nC, nM, nY, nK ]
*/
HB_FUNC( HPDF_PAGE_GETCMYKSTROKE )
{
   HPDF_CMYKColor cmyk;
   PHB_ITEM info = hb_itemArrayNew( 4 );

   cmyk = HPDF_Page_GetCMYKStroke( ( HPDF_Page ) hb_parptr( 1 ) );

   hb_arraySetND( info, 1, cmyk.c );
   hb_arraySetND( info, 2, cmyk.m );
   hb_arraySetND( info, 3, cmyk.y );
   hb_arraySetND( info, 4, cmyk.k );

   hb_itemReturnRelease( info );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetGrayFill( hPage ) -> nGrayFillValue
*/
HB_FUNC( HPDF_PAGE_GETGRAYFILL )
{
   hb_retnd( ( double ) HPDF_Page_GetGrayFill( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetGrayStroke( hPage ) -> nGrayStrokeValue
*/
HB_FUNC( HPDF_PAGE_GETGRAYSTROKE )
{
   hb_retnd( ( double ) HPDF_Page_GetGrayStroke( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetStrokingColorSpace( hPage ) -> nStrokingSpace
*/
HB_FUNC( HPDF_PAGE_GETSTROKINGCOLORSPACE )
{
   hb_retni( ( int ) HPDF_Page_GetStrokingColorSpace( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetFillingColorSpace( hPage ) -> nFillingColorSpace
*/
HB_FUNC( HPDF_PAGE_GETFILLINGCOLORSPACE )
{
   hb_retni( ( int ) HPDF_Page_GetFillingColorSpace( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetTextMatrix( hPage ) -> aMatrix[ ]
*/
HB_FUNC( HPDF_PAGE_GETTEXTMATRIX )
{
   HPDF_TransMatrix matrix;
   PHB_ITEM info = hb_itemArrayNew( 6 );

   matrix = HPDF_Page_GetTextMatrix( ( HPDF_Page ) hb_parptr( 1 ) ) ;

   hb_arraySetND( info, 1, matrix.a );
   hb_arraySetND( info, 2, matrix.b );
   hb_arraySetND( info, 3, matrix.c );
   hb_arraySetND( info, 4, matrix.d );
   hb_arraySetND( info, 5, matrix.x );
   hb_arraySetND( info, 6, matrix.y );

   hb_itemReturnRelease( info );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_GetGStateDepth( hPage ) -> nGStateDepth
*/
HB_FUNC( HPDF_PAGE_GETGSTATEDEPTH )
{
   hb_retni( ( int ) HPDF_Page_GetGStateDepth( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPdf_Page_SetSlideShow( hPage, nType, nDurationPerFrame, nTranstnTime = 1 Second ) -> hStatus
       nType ==
   HPDF_TS_WIPE_RIGHT
   HPDF_TS_WIPE_UP
   HPDF_TS_WIPE_LEFT
   HPDF_TS_WIPE_DOWN
   HPDF_TS_BARN_DOORS_HORIZONTAL_OUT
   HPDF_TS_BARN_DOORS_HORIZONTAL_IN
   HPDF_TS_BARN_DOORS_VERTICAL_OUT
   HPDF_TS_BARN_DOORS_VERTICAL_IN
   HPDF_TS_BOX_OUT
   HPDF_TS_BOX_IN
   HPDF_TS_BLINDS_HORIZONTAL
   HPDF_TS_BLINDS_VERTICAL
   HPDF_TS_DISSOLVE
   HPDF_TS_GLITTER_RIGHT
   HPDF_TS_GLITTER_DOWN
   HPDF_TS_GLITTER_TOP_LEFT_TO_BOTTOM_RIGHT
   HPDF_TS_REPLACE
*/
HB_FUNC( HPDF_PAGE_SETSLIDESHOW )
{
   hb_retnl( ( long ) HPDF_Page_SetSlideShow( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_TransitionStyle ) hb_parni( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ) ) );
}
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*                              GRAPHICS                                */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/* HPDF_Page_SetLineWidth( hPage, nLineWidth ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SETLINEWIDTH )
{
   hb_retnl( ( long ) HPDF_Page_SetLineWidth( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_SetLineCap( hPage, nLineCap ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SETLINECAP )
{
   hb_retnl( ( long ) HPDF_Page_SetLineCap( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_LineCap ) hb_parni( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_SetLineJoin( hPage, nLineJoin ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SETLINEJOIN )
{
   hb_retnl( ( long ) HPDF_Page_SetLineJoin( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_LineJoin ) hb_parni( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_SetMiterLimit( hPage, nMiterLimit ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SETMITERLIMIT )
{
   hb_retnl( ( long ) HPDF_Page_SetMiterLimit( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_SetDash( hPage, aDash, nNumPoints, nStartFrom ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SETDASH )
{
   HPDF_DashMode dash;
   int nPtns = hb_parni( 3 );
   int i;

   for ( i = 0; i < nPtns; i++ )
      dash.ptn[ i ] = ( HPDF_UINT16 ) hb_parvni( 2, i+1 );

   hb_retnl( ( long ) HPDF_Page_SetDash( ( HPDF_Page ) hb_parptr( 1 ), dash.ptn, nPtns, hb_parni( 4 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_SetExtGState( hPage, hGState ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SETEXTGSTATE )
{
   hb_retnl( ( long ) HPDF_Page_SetExtGState( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_ExtGState ) hb_parptr( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_GSave( hPage ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_GSAVE )
{
   hb_retnl( ( long ) HPDF_Page_GSave( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_GRestore( hPage ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_GRESTORE )
{
   hb_retnl( ( long ) HPDF_Page_GRestore( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_Concat( hPage, nA, nB, nC, nD, nX, nY ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_CONCAT )
{
   hb_retnl( ( long ) HPDF_Page_Concat( ( HPDF_Page ) hb_parptr( 1 ),
                                        ( HPDF_REAL ) hb_parnd( 2 ),
                                        ( HPDF_REAL ) hb_parnd( 3 ),
                                        ( HPDF_REAL ) hb_parnd( 4 ),
                                        ( HPDF_REAL ) hb_parnd( 5 ),
                                        ( HPDF_REAL ) hb_parnd( 6 ),
                                        ( HPDF_REAL ) hb_parnd( 7 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_MoveTo( hPage, nX, nY ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_MOVETO )
{
   hb_retnl( ( long ) HPDF_Page_MoveTo( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_LineTo( hPage, nX, nY ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_LINETO )
{
   hb_retnl( ( long ) HPDF_Page_LineTo( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_CurveTo( hPage, nX1, nY1, nX2, nY2, nX3, nY3  ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_CURVETO )
{
   hb_retnl( ( long ) HPDF_Page_CurveTo( ( HPDF_Page ) hb_parptr( 1 ),
                                         ( HPDF_REAL ) hb_parnd( 2 ),
                                         ( HPDF_REAL ) hb_parnd( 3 ),
                                         ( HPDF_REAL ) hb_parnd( 4 ),
                                         ( HPDF_REAL ) hb_parnd( 5 ),
                                         ( HPDF_REAL ) hb_parnd( 6 ),
                                         ( HPDF_REAL ) hb_parnd( 7 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_CurveTo2( hPage, nX2, nY2, nX3, nY3 ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_CURVETO2 )
{
   hb_retnl( ( long ) HPDF_Page_CurveTo2( ( HPDF_Page ) hb_parptr( 1 ),
                                          ( HPDF_REAL ) hb_parnd( 2 ),
                                          ( HPDF_REAL ) hb_parnd( 3 ),
                                          ( HPDF_REAL ) hb_parnd( 4 ),
                                          ( HPDF_REAL ) hb_parnd( 5 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_CurveTo3( hPage, nX1, nY1, nX3, nY3 ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_CURVETO3 )
{
   hb_retnl( ( long ) HPDF_Page_CurveTo3( ( HPDF_Page ) hb_parptr( 1 ),
                                          ( HPDF_REAL ) hb_parnd( 2 ),
                                          ( HPDF_REAL ) hb_parnd( 3 ),
                                          ( HPDF_REAL ) hb_parnd( 4 ),
                                          ( HPDF_REAL ) hb_parnd( 5 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_ClosePath( hPage ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_CLOSEPATH )
{
   hb_retnl( ( long ) HPDF_Page_ClosePath( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_Rectangle( hPage, nX, nY, nWidth, nHeight ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_RECTANGLE )
{
   hb_retnl( ( long ) HPDF_Page_Rectangle( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ), ( HPDF_REAL ) hb_parnd( 5 )  ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_Stroke( hPage ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_STROKE )
{
   hb_retnl( ( long ) HPDF_Page_Stroke( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_ClosePathStroke( hPage ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_CLOSEPATHSTROKE )
{
   hb_retnl( ( long ) HPDF_Page_ClosePathStroke( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_SetFontAndSize( hPage, hFont, nSize ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SETFONTANDSIZE )
{
   hb_retnl( ( long ) HPDF_Page_SetFontAndSize( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_Font ) hb_parptr( 2 ), ( HPDF_REAL ) hb_parnd( 3 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_BeginText( hPage ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_BEGINTEXT )
{
   hb_retnl( ( long ) HPDF_Page_BeginText( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_EndText( hPage ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_ENDTEXT )
{
   hb_retnl( ( long ) HPDF_Page_EndText( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_TextOut( hPage, nX, nY, cText ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_TEXTOUT )
{
   hb_retnl( ( long ) HPDF_Page_TextOut( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), hb_parc( 4 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_MoveTextPos( hPage, nX, nY ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_MOVETEXTPOS )
{
   hb_retnl( ( long ) HPDF_Page_MoveTextPos( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_ShowText( hPage, cText ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SHOWTEXT )
{
   hb_retnl( ( long ) HPDF_Page_ShowText( ( HPDF_Page ) hb_parptr( 1 ), hb_parc( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_Fill( hPage ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_FILL )
{
   hb_retnl( ( long ) HPDF_Page_Fill( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_Eofill( hPage ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_EOFILL )
{
   hb_retnl( ( long ) HPDF_Page_Eofill( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_FillStroke( hPage ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_FILLSTROKE )
{
   hb_retnl( ( long ) HPDF_Page_FillStroke( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_EofillStroke( hPage ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_EOFILLSTROKE )
{
   hb_retnl( ( long ) HPDF_Page_EofillStroke( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_ClosePathFillStroke( hPage ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_CLOSEPATHFILLSTROKE )
{
   hb_retnl( ( long ) HPDF_Page_ClosePathFillStroke( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_ClosePathEofillStroke( hPage ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_CLOSEPATHEOFILLSTROKE )
{
   hb_retnl( ( long ) HPDF_Page_ClosePathEofillStroke( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_EndPath( hPage ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_ENDPATH )
{
   hb_retnl( ( long ) HPDF_Page_EndPath( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_Clip( hPage ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_CLIP )
{
   hb_retnl( ( long ) HPDF_Page_Clip( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_Eoclip( hPage ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_EOCLIP )
{
   hb_retnl( ( long ) HPDF_Page_Eoclip( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_SetCharSpace( hPage, nSpaceWidth ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SETCHARSPACE )
{
   hb_retnl( ( long ) HPDF_Page_SetCharSpace( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_SetWordSpace( hPage, nSpaceWidth ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SETWORDSPACE )
{
   hb_retnl( ( long ) HPDF_Page_SetWordSpace( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_SetHorizontalScalling( hPage, nHorzScale ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SETHORIZONTALSCALLING )
{
   hb_retnl( ( long ) HPDF_Page_SetHorizontalScalling( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_SetTextLeading( hPage, nTextLeading ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SETTEXTLEADING )
{
   hb_retnl( ( long ) HPDF_Page_SetTextLeading( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_SetTextRenderingMode( hPage, nTextRenderingMode ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SETTEXTRENDERINGMODE )
{
   hb_retnl( ( long ) HPDF_Page_SetTextRenderingMode( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_TextRenderingMode ) hb_parni( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_SetTextRise( hPage, nTextRise ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SETTEXTRISE )
{
   hb_retnl( ( long ) HPDF_Page_SetTextRise( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_MoveTextPos2( hPage, nX, nY ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_MOVETEXTPOS2 )
{
   hb_retnl( ( long ) HPDF_Page_MoveTextPos2( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_SetTextMatrix( hPage ) -> hStatus          --------tobedone---------
*/
HB_FUNC( HPDF_PAGE_SETTEXTMATRIX )
{
   hb_retnl( ( long ) HPDF_Page_SetTextMatrix( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ), ( HPDF_REAL ) hb_parnd( 5 ), ( HPDF_REAL ) hb_parnd( 6 ), ( HPDF_REAL ) hb_parnd( 7 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_MoveToNextLine( hPage ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_MOVETONEXTLINE )
{
   hb_retnl( ( long ) HPDF_Page_MoveToNextLine( ( HPDF_Page ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_ShowTextNextLine( hPage, cText ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SHOWTEXTNEXTLINE )
{
   hb_retnl( ( long ) HPDF_Page_ShowTextNextLine( ( HPDF_Page ) hb_parptr( 1 ), hb_parc( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_ShowTextNextLineEx( hPage, nWordSpace, nCharSpace, cText ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SHOWTEXTNEXTLINEEX )
{
   hb_retnl( ( long ) HPDF_Page_ShowTextNextLineEx( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), hb_parc( 4 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_SetGrayFill( hPage, nGrayFill ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SETGRAYFILL )
{
   hb_retnl( ( long ) HPDF_Page_SetGrayFill( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_SetGrayStroke( hPage, nGrayStroke ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SETGRAYSTROKE )
{
   hb_retnl( ( long ) HPDF_Page_SetGrayStroke( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_SetRGBFill( hPage, nRGBRed, nRGBGreen, nRGBBlue ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SETRGBFILL )
{
   hb_retnl( ( long ) HPDF_Page_SetRGBFill( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_SetRGBStroke( hPage, nRGBRed, nRGBGreen, nRGBBlue ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SETRGBSTROKE )
{
   hb_retnl( ( long ) HPDF_Page_SetRGBStroke( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_SetCMYKFill( hPage, nC, nM, nY, nK ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SETCMYKFILL )
{
   hb_retnl( ( long ) HPDF_Page_SetCMYKFill( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ), ( HPDF_REAL ) hb_parnd( 5 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_SetCMYKStroke( hPage, nC, nM, nY, nK ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_SETCMYKSTROKE )
{
   hb_retnl( ( long ) HPDF_Page_SetCMYKStroke( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ), ( HPDF_REAL ) hb_parnd( 5 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_ExecuteXObject( hPage, hImage ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_EXECUTEXOBJECT )
{
   hb_retnl( ( long ) HPDF_Page_ExecuteXObject( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_Image ) hb_parptr( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_DrawImage( hPage, hImage, nX, nY, nWidth, nHeight ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_DRAWIMAGE )
{
   hb_retnl( ( long ) HPDF_Page_DrawImage( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_Image ) hb_parptr( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ), ( HPDF_REAL ) hb_parnd( 5 ), ( HPDF_REAL ) hb_parnd( 6 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_Circle( hPage, nX, nY, nRay ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_CIRCLE )
{
   hb_retnl( ( long ) HPDF_Page_Circle( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_Arc( hPage, nX, nY, nRay, nAngle1, nAngle2 ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_ARC )
{
   hb_retnl( ( long ) HPDF_Page_Arc( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ), ( HPDF_REAL ) hb_parnd( 5 ), ( HPDF_REAL ) hb_parnd( 6 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_Ellipse( hPage, nX, nY, nxRay, nyRay ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_ELLIPSE )
{
   hb_retnl( ( long ) HPDF_Page_Ellipse( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ), ( HPDF_REAL ) hb_parnd( 5 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Page_TextRect( hPage, nLeft, nTop, nRight, nBottom, cText, nAlign ) -> hStatus
*/
HB_FUNC( HPDF_PAGE_TEXTRECT )
{
   hb_retnl( ( long ) HPDF_Page_TextRect( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ), ( HPDF_REAL ) hb_parnd( 5 ), hb_parc( 6 ), ( HPDF_TextAlignment ) hb_parni( 7 ), NULL ) );
}
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*                              FONTS                                   */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/* HPDF_Font_GetFontName( hFont ) -> cFontName
*/
HB_FUNC( HPDF_FONT_GETFONTNAME )
{
   hb_retc( HPDF_Font_GetFontName( ( HPDF_Font ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Font_GetEncodingName( hFont ) -> cEncoding
*/
HB_FUNC( HPDF_FONT_GETENCODINGNAME )
{
   hb_retc( HPDF_Font_GetEncodingName( ( HPDF_Font ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Font_GetUnicodeWidth( hFont, hUnicode ) -> nCharWidth
*/
HB_FUNC( HPDF_FONT_GETUNICODEWIDTH )
{
   hb_retnl( ( long ) HPDF_Font_GetUnicodeWidth( ( HPDF_Font ) hb_parptr( 1 ), ( HPDF_UNICODE ) hb_parni( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Font_GetBBox( hFont ) -> aRect
*/
HB_FUNC( HPDF_FONT_GETBBOX )
{
   HPDF_Box rc;
   PHB_ITEM info = hb_itemArrayNew( 4 );

   rc =  HPDF_Font_GetBBox( ( HPDF_Font ) hb_parptr( 1 ) );

   hb_arraySetND( info, 1, rc.left   );
   hb_arraySetND( info, 2, rc.top    );
   hb_arraySetND( info, 3, rc.right  );
   hb_arraySetND( info, 4, rc.bottom );

   hb_itemReturnRelease( info );
}
/*----------------------------------------------------------------------*/
/* HPDF_Font_GetAscent( hFont ) -> nAscent
*/
HB_FUNC( HPDF_FONT_GETASCENT )
{
   hb_retni( ( int ) HPDF_Font_GetAscent( ( HPDF_Font ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Font_GetDescent( hFont ) -> nDescent
*/
HB_FUNC( HPDF_FONT_GETDESCENT )
{
   hb_retni( ( int ) HPDF_Font_GetDescent( ( HPDF_Font ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Font_GetXHeight( hFont ) -> nXHeight
*/
HB_FUNC( HPDF_FONT_GETXHEIGHT )
{
   hb_retnl( ( long ) HPDF_Font_GetXHeight( ( HPDF_Font ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Font_GetCapHeight( hFont ) -> nCapsHeight
*/
HB_FUNC( HPDF_FONT_GETCAPHEIGHT )
{
   hb_retnl( ( long ) HPDF_Font_GetCapHeight( ( HPDF_Font ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Font_TextWidth( hFont, cText, nWidth ) -> aTextWidth[ nNumChars, nNumWords, nWidth, nNumSpace ]
*/
HB_FUNC( HPDF_FONT_TEXTWIDTH )
{
   HPDF_TextWidth tw;
   PHB_ITEM info = hb_itemArrayNew( 4 );

   tw = HPDF_Font_TextWidth( ( HPDF_Font ) hb_parptr( 1 ), ( HPDF_BYTE * ) hb_parc( 2 ), hb_parni( 3 ) );

   hb_arraySetNI( info, 1, tw.numchars );
   hb_arraySetNI( info, 2, tw.numwords );
   hb_arraySetNI( info, 3, tw.width    );
   hb_arraySetNI( info, 4, tw.numspace );

   hb_itemReturnRelease( info );
}
/*----------------------------------------------------------------------*/
/* HPDF_Font_MeasureText( hFont, cText, nTextLen, nWidth, nFontSize, nCharSpace, nWordSpace, lWordWrap ) -> nByteLengthTobeIncludedInWidth
*/
HB_FUNC( HPDF_FONT_MEASURETEXT )
{
   hb_retni( HPDF_Font_MeasureText( ( HPDF_Font ) hb_parptr( 1 ),
                                    ( HPDF_BYTE * ) hb_parc( 2 ),
                                    hb_parni( 3 ),
                                    ( HPDF_REAL ) hb_parnd( 4 ),
                                    ( HPDF_REAL ) hb_parnd( 5 ),
                                    ( HPDF_REAL ) hb_parnd( 6 ),
                                    ( HPDF_REAL ) hb_parnd( 7 ),
                                    hb_parl( 8 ) ? HPDF_TRUE : HPDF_FALSE,
                                    NULL ) );
}
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*                              ENCODING                                */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/* HPDF_Encoder_GetType( hEncoder ) -> nEncoderType
       nEncoderType ==
   HPDF_ENCODER_TYPE_SINGLE_BYTE      1    This encoder is an encoder for single byte characters.
   HPDF_ENCODER_TYPE_DOUBLE_BYTE      2    This encoder is an encoder for multi byte characters.
   HPDF_ENCODER_TYPE_UNINITIALIZED    3    This encoder is uninitialized. (May be it is an encoder for multi byte characters.)
   HPDF_ENCODER_UNKNOWN               4    Invalid encoder.
*/
HB_FUNC( HPDF_ENCODER_GETTYPE )
{
   hb_retni( ( int ) HPDF_Encoder_GetType( ( HPDF_Encoder ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Encoder_GetByteType( hEncoder, cText, nIndex ) -> nByteType
       nByteType
   HPDF_BYTE_TYPE_SINGLE     1     Single byte character.
   HPDF_BYTE_TYPE_LEAD       2     Lead byte of a double-byte character.
   HPDF_BYTE_TYPE_TRIAL      3     Trailing byte of a double-byte character.
   HPDF_BYTE_TYPE_UNKNOWN    4     Invalid encoder or cannot judge the byte type.
*/
HB_FUNC( HPDF_ENCODER_GETBYTETYPE )
{
   hb_retni( ( int ) HPDF_Encoder_GetByteType( ( HPDF_Encoder ) hb_parptr( 1 ), hb_parc( 2 ), hb_parni( 3 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Encoder_GetUnicode( hEncoder, nCode ) -> nUnicode
*/
HB_FUNC( HPDF_ENCODER_GETUNICODE )
{
   hb_retni( ( int ) HPDF_Encoder_GetUnicode( ( HPDF_Encoder ) hb_parptr( 1 ), ( HPDF_UINT16 ) hb_parni( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Encoder_GetWritingMode( hEncoder ) -> nWriteMode
       nWriteMode ==
   HPDF_WMODE_HORIZONTAL    1    horizontal writing mode.
   HPDF_WMODE_VERTICAL      2    vertical writing mode;
*/
HB_FUNC( HPDF_ENCODER_GETWRITINGMODE )
{
   hb_retni( ( int ) HPDF_Encoder_GetWritingMode( ( HPDF_Encoder ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*                             ANNOTATION                               */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/* HPDF_LinkAnnot_SetHighlightMode( hAnnot, nHilightMode ) -> hStatus
       nHilightMode ==
   HPDF_ANNOT_NO_HIGHTLIGHT       1     No highlighting.
   HPDF_ANNOT_INVERT_BOX          2     Invert the contents of the area of annotation.
   HPDF_ANNOT_INVERT_BORDER       3     Invert the annotation’s border.
   HPDF_ANNOT_DOWN_APPEARANCE     4     Dent the annotation.
*/
HB_FUNC( HPDF_LINKANNOT_SETHIGHLIGHTMODE )
{
   hb_retnl( ( long ) HPDF_LinkAnnot_SetHighlightMode( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_AnnotHighlightMode ) hb_parni( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_LinkAnnot_SetBorderStyle( hAnnot, nWidth, nDashOn, nDashOff ) -> hStatus
*/
HB_FUNC( HPDF_LINKANNOT_SETBORDERSTYLE )
{
   hb_retnl( ( long ) HPDF_LinkAnnot_SetBorderStyle( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_UINT16 ) hb_parni( 3 ), ( HPDF_UINT16 ) hb_parni( 4 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_TextAnnot_SetIcon( hAnnot, nIconID ) -> hStatus
       nIconID
   HPDF_ANNOT_ICON_COMMENT
   HPDF_ANNOT_ICON_KEY
   HPDF_ANNOT_ICON_NOTE
   HPDF_ANNOT_ICON_HELP
   HPDF_ANNOT_ICON_NEW_PARAGRAPH
   HPDF_ANNOT_ICON_PARAGRAPH
   HPDF_ANNOT_ICON_INSERT
*/
HB_FUNC( HPDF_TEXTANNOT_SETICON )
{
   hb_retnl( ( long ) HPDF_TextAnnot_SetIcon( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_AnnotIcon ) hb_parni( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_TextAnnot_SetOpened( hAnnot, lOpened ) -> hStatus
*/
HB_FUNC( HPDF_TEXTANNOT_SETOPENED )
{
   hb_retnl( ( long ) HPDF_TextAnnot_SetOpened( ( HPDF_Annotation ) hb_parptr( 1 ), hb_parl( 2 ) ? HPDF_TRUE : HPDF_FALSE ) );
}
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*                                OUTLINE                               */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/* HPDF_Outline_SetOpened( hOutline, lShowOpened ) -> hStatus
*/
HB_FUNC( HPDF_OUTLINE_SETOPENED )
{
   hb_retnl( ( long ) HPDF_Outline_SetOpened( ( HPDF_Outline ) hb_parptr( 1 ), hb_parl( 2 ) ? HPDF_TRUE : HPDF_FALSE ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Outline_SetDestination( hOutline, hDestn ) -> hStatus
*/
HB_FUNC( HPDF_OUTLINE_SETDESTINATION )
{
   hb_retnl( ( long ) HPDF_Outline_SetDestination( ( HPDF_Outline ) hb_parptr( 1 ), ( HPDF_Destination ) hb_parptr( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*                              DESTINATION                             */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/* HPDF_Destination_SetXYZ( hDestn, nLeft, nTop, nZoom ) -> hStatus
*/
HB_FUNC( HPDF_DESTINATION_SETXYZ )
{
   hb_retnl( ( long ) HPDF_Destination_SetXYZ( ( HPDF_Destination ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Destination_SetFit( hDestn ) -> hStatus
*/
HB_FUNC( HPDF_DESTINATION_SETFIT )
{
   hb_retnl( ( long ) HPDF_Destination_SetFit( ( HPDF_Destination ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Destination_SetFitH( hDestn, nTop ) -> hStatus
*/
HB_FUNC( HPDF_DESTINATION_SETFITH )
{
   hb_retnl( ( long ) HPDF_Destination_SetFitH( ( HPDF_Destination ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Destination_SetFitV( hDestn, nLeft ) -> hStatus
*/
HB_FUNC( HPDF_DESTINATION_SETFITV )
{
   hb_retnl( ( long ) HPDF_Destination_SetFitV( ( HPDF_Destination ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Destination_SetFitR( hDestn, nLeft, nBottom, nRight, nTop ) -> hStatus
*/
HB_FUNC( HPDF_DESTINATION_SETFITR )
{
   hb_retnl( ( long ) HPDF_Destination_SetFitR( ( HPDF_Destination ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ), ( HPDF_REAL ) hb_parnd( 5 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Destination_SetFitB( hDestn ) -> hStatus
*/
HB_FUNC( HPDF_DESTINATION_SETFITB )
{
   hb_retnl( ( long ) HPDF_Destination_SetFitB( ( HPDF_Destination ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Destination_SetFitBH( hDestn, nTop ) -> hStatus
*/
HB_FUNC( HPDF_DESTINATION_SETFITBH )
{
   hb_retnl( ( long ) HPDF_Destination_SetFitBH( ( HPDF_Destination ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Destination_SetFitBV( hDestn, nTop ) -> hStatus
*/
HB_FUNC( HPDF_DESTINATION_SETFITBV )
{
   hb_retnl( ( long ) HPDF_Destination_SetFitBV( ( HPDF_Destination ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*                                IMAGE                                 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/* HPDF_Image_GetSize( hImage ) -> aSize[ nW, nH ]
*/
HB_FUNC( HPDF_IMAGE_GETSIZE )
{
   HPDF_Point pt;
   PHB_ITEM info = hb_itemArrayNew( 2 );

   pt = HPDF_Image_GetSize( ( HPDF_Image ) hb_parptr( 1 ) );

   hb_arraySetND( info, 1, pt.x );
   hb_arraySetND( info, 2, pt.y );

   hb_itemReturnRelease( info );
}
/*----------------------------------------------------------------------*/
/* HPDF_Image_GetWidth( hImage ) -> nWidth
*/
HB_FUNC( HPDF_IMAGE_GETWIDTH )
{
   hb_retni( HPDF_Image_GetWidth( ( HPDF_Image ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Image_GetHeight( hImage ) -> nHeight
*/
HB_FUNC( HPDF_IMAGE_GETHEIGHT )
{
   hb_retni( HPDF_Image_GetHeight( ( HPDF_Image ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Image_GetBitsPerComponent( hImage ) -> nBitsPerComponent
*/
HB_FUNC( HPDF_IMAGE_GETBITSPERCOMPONENT )
{
   hb_retni( HPDF_Image_GetBitsPerComponent( ( HPDF_Image ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Image_GetColorSpace( hImage ) -> nColorSpace
*/
HB_FUNC( HPDF_IMAGE_GETCOLORSPACE )
{
   hb_retc( HPDF_Image_GetColorSpace( ( HPDF_Image ) hb_parptr( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Image_SetColorMask( hImage, nRGB_R_Min, nRGB_R_Max, nRGB_G_Min, nRGB_G_Max, nRGB_B_Min, nRGB_B_Max )
*/
HB_FUNC( HPDF_IMAGE_SETCOLORMASK )
{
   hb_retnl( ( long ) HPDF_Image_SetColorMask( ( HPDF_Image ) hb_parptr( 1 ),
                                               hb_parni( 2 ),
                                               hb_parni( 3 ),
                                               hb_parni( 4 ),
                                               hb_parni( 5 ),
                                               hb_parni( 6 ),
                                               hb_parni( 7 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_Image_SetMaskImage( hImage, hImageMask ) -> hStatus
*/
HB_FUNC( HPDF_IMAGE_SETMASKIMAGE )
{
   hb_retnl( ( long ) HPDF_Image_SetMaskImage( ( HPDF_Image ) hb_parptr( 1 ), ( HPDF_Image ) hb_parptr( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*                               ExtGState                              */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/* HPDF_ExtGState_SetAlphaStroke( hGState, nValue ) -> hStatus
*/
HB_FUNC( HPDF_EXTGSTATE_SETALPHASTROKE )
{
   hb_retnl( ( long ) HPDF_ExtGState_SetAlphaStroke( ( HPDF_ExtGState ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_ExtGState_SetAlphaFill( hGState, nValue ) -> hStatus
*/
HB_FUNC( HPDF_EXTGSTATE_SETALPHAFILL )
{
   hb_retnl( ( long ) HPDF_ExtGState_SetAlphaFill( ( HPDF_ExtGState ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* HPDF_ExtGState_SetBlendMode( hGState, nBlendMode ) -> hStatus
      nBlendMode ==
   HPDF_BM_NORMAL
   HPDF_BM_MULTIPLY
   HPDF_BM_SCREEN
   HPDF_BM_OVERLAY
   HPDF_BM_DARKEN
   HPDF_BM_LIGHTEN
   HPDF_BM_COLOR_DODGE
   HPDF_BM_COLOR_BUM
   HPDF_BM_HARD_LIGHT
   HPDF_BM_SOFT_LIGHT
   HPDF_BM_DIFFERENCE
   HPDF_BM_EXCLUSHON
*/
HB_FUNC( HPDF_EXTGSTATE_SETBLENDMODE )
{
   hb_retnl( ( long ) HPDF_ExtGState_SetBlendMode( ( HPDF_ExtGState ) hb_parptr( 1 ), ( HPDF_BlendMode ) hb_parni( 2 ) ) );
}
/*----------------------------------------------------------------------*/

HB_FUNC( HPDF_VERSION_TEXT )
{
   hb_retc_const( HPDF_VERSION_TEXT );
}
/*----------------------------------------------------------------------*/
//                    New Functions in LibHaru 2.2.0
/*----------------------------------------------------------------------*/
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_GetContents   (HPDF_Doc   pdf,
                   HPDF_BYTE  *buf,
                 HPDF_UINT32  *size);
*/
HB_FUNC( HPDF_GETCONTENTS )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_UINT32 size = ( HPDF_UINT32 ) hb_parclen( 2 );
   HPDF_BYTE * buffer;

   if( size < 1024 )
      size = 1024;

   buffer = ( HPDF_BYTE * ) hb_xgrab( size + 1 );

   hb_retnl( ( long ) HPDF_GetContents( HPDF_Doc_par( 1 ), buffer, &size ) );

   if( ! hb_storclen_buffer( ( char * ) buffer, size, 2 ) )
      hb_xfree( buffer );
#else
   hb_storc( NULL, 2 );
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_CheckError  (HPDF_Error   error);
*/
HB_FUNC( HPDF_CHECKERROR )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_CheckError( ( HPDF_Error ) hb_parptr( 1 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_Page_SetZoom  (HPDF_Page     page,
                    HPDF_REAL     zoom);
*/
HB_FUNC( HPDF_PAGE_SETZOOM )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_Page_SetZoom( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_Annotation)
HPDF_Page_CreateFreeTextAnnot  (HPDF_Page       page,
                        HPDF_Rect       rect,
                        const char     *text,
                        HPDF_Encoder    encoder);
*/
HB_FUNC( HPDF_PAGE_CREATEFREETEXTANNOT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retptr( HPDF_Page_CreateFreeTextAnnot( ( HPDF_Page ) hb_parptr( 1 ), rc, hb_parc( 3 ), ( HPDF_Encoder ) hb_parptr( 4 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
HPDF_EXPORT(HPDF_Annotation)
HPDF_Page_CreateLineAnnot  (HPDF_Page       page,
                     const char     *text,
                     HPDF_Encoder    encoder);
*/
HB_FUNC( HPDF_PAGE_CREATELINEANNOT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retptr( HPDF_Page_CreateLineAnnot( ( HPDF_Page ) hb_parptr( 1 ), hb_parc( 2 ), ( HPDF_Encoder ) hb_parptr( 3 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
HPDF_Annotation
HPDF_Page_CreateTextMarkupAnnot (HPDF_Page     page,
                        HPDF_Rect      rect,
                        const char     *text,
                        HPDF_Encoder   encoder,
                        HPDF_AnnotType subType);
*/
HB_FUNC( HPDF_PAGE_CREATETEXTMARKUPANNOT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retptr( HPDF_Page_CreateTextMarkupAnnot( ( HPDF_Page ) hb_parptr( 1 ), rc, hb_parc( 3 ), ( HPDF_Encoder ) hb_parptr( 4 ), ( HPDF_AnnotType ) hb_parni( 5 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
HPDF_EXPORT(HPDF_Annotation)
HPDF_Page_CreateHighlightAnnot  (HPDF_Page   page,
                        HPDF_Rect    rect,
                        const char   *text,
                        HPDF_Encoder encoder);
*/
HB_FUNC( HPDF_PAGE_CREATEHIGHLIGHTANNOT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retptr( HPDF_Page_CreateHighlightAnnot( ( HPDF_Page ) hb_parptr( 1 ), rc, hb_parc( 3 ), ( HPDF_Encoder ) hb_parptr( 4 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
HPDF_EXPORT(HPDF_Annotation)
HPDF_Page_CreateUnderlineAnnot (HPDF_Page    page,
                        HPDF_Rect    rect,
                        const char   *text,
                        HPDF_Encoder encoder);
*/
HB_FUNC( HPDF_PAGE_CREATEUNDERLINEANNOT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retptr( HPDF_Page_CreateUnderlineAnnot( ( HPDF_Page ) hb_parptr( 1 ), rc, hb_parc( 3 ), ( HPDF_Encoder ) hb_parptr( 4 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
HPDF_EXPORT(HPDF_Annotation)
HPDF_Page_CreateSquigglyAnnot  (HPDF_Page    page,
                        HPDF_Rect    rect,
                        const char   *text,
                        HPDF_Encoder encoder);
*/
HB_FUNC( HPDF_PAGE_CREATESQUIGGLYANNOT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retptr( HPDF_Page_CreateSquigglyAnnot( ( HPDF_Page ) hb_parptr( 1 ), rc, hb_parc( 3 ), ( HPDF_Encoder ) hb_parptr( 4 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
HPDF_EXPORT(HPDF_Annotation)
HPDF_Page_CreateStrikeOutAnnot  (HPDF_Page   page,
                        HPDF_Rect    rect,
                        const char   *text,
                        HPDF_Encoder encoder);
*/
HB_FUNC( HPDF_PAGE_CREATESTRIKEOUTANNOT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retptr( HPDF_Page_CreateStrikeOutAnnot( ( HPDF_Page ) hb_parptr( 1 ), rc, hb_parc( 3 ), ( HPDF_Encoder ) hb_parptr( 4 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
HPDF_EXPORT(HPDF_Annotation)
HPDF_Page_CreatePopupAnnot  ( HPDF_Page    page,
                        HPDF_Rect          rect,
                        HPDF_Annotation      parent);
*/
HB_FUNC( HPDF_PAGE_CREATEPOPUPANNOT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retptr( HPDF_Page_CreatePopupAnnot( ( HPDF_Page ) hb_parptr( 1 ), rc, ( HPDF_Annotation ) hb_parptr( 3 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
HPDF_EXPORT(HPDF_Annotation)
HPDF_Page_CreateStampAnnot  (   HPDF_Page           page,
                        HPDF_Rect           rect,
                        HPDF_StampAnnotName name,
                        const char*         text,
                        HPDF_Encoder      encoder);
*/
HB_FUNC( HPDF_PAGE_CREATESTAMPANNOT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retptr( HPDF_Page_CreateStampAnnot( ( HPDF_Page ) hb_parptr( 1 ), rc, ( HPDF_StampAnnotName ) hb_parni( 3 ), hb_parc( 4 ), ( HPDF_Encoder ) hb_parptr( 5 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
HPDF_EXPORT(HPDF_Annotation)
HPDF_Page_CreateSquareAnnot (HPDF_Page          page,
                      HPDF_Rect          rect,
                      const char         *text,
                      HPDF_Encoder       encoder);
*/
HB_FUNC( HPDF_PAGE_CREATESQUAREANNOT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retptr( HPDF_Page_CreateSquareAnnot( ( HPDF_Page ) hb_parptr( 1 ), rc, hb_parc( 3 ), ( HPDF_Encoder ) hb_parptr( 4 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
HPDF_EXPORT(HPDF_Annotation)
HPDF_Page_CreateCircleAnnot (HPDF_Page          page,
                      HPDF_Rect          rect,
                      const char         *text,
                      HPDF_Encoder       encoder);
*/
HB_FUNC( HPDF_PAGE_CREATECIRCLEANNOT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retptr( HPDF_Page_CreateCircleAnnot( ( HPDF_Page ) hb_parptr( 1 ), rc, hb_parc( 3 ), ( HPDF_Encoder ) hb_parptr( 4 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_Annot_SetRGBColor (HPDF_Annotation annot, HPDF_RGBColor color);
*/
HB_FUNC( HPDF_ANNOT_SETRGBCOLOR )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_RGBColor rgb;

   rgb.r = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rgb.g = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rgb.b = ( HPDF_REAL ) hb_parvnd( 2, 3 );

   hb_retnl( ( long ) HPDF_Annot_SetRGBColor( ( HPDF_Annotation ) hb_parptr( 1 ), rgb ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_Annot_SetCMYKColor (HPDF_Annotation annot, HPDF_CMYKColor color);
*/
HB_FUNC( HPDF_ANNOT_SETCMYKCOLOR )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_CMYKColor cmyk;

   cmyk.c = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   cmyk.m = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   cmyk.y = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   cmyk.k = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retnl( ( long ) HPDF_Annot_SetCMYKColor( ( HPDF_Annotation ) hb_parptr( 1 ), cmyk ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_Annot_SetGrayColor (HPDF_Annotation annot, HPDF_REAL color);
*/
HB_FUNC( HPDF_ANNOT_SETGRAYCOLOR )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_Annot_SetGrayColor( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_Annot_SetNoColor (HPDF_Annotation annot);
*/
HB_FUNC( HPDF_ANNOT_SETNOCOLOR )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_Annot_SetNoColor( ( HPDF_Annotation ) hb_parptr( 1 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_MarkupAnnot_SetTitle (HPDF_Annotation annot, const char* name);
*/
HB_FUNC( HPDF_MARKUPANNOT_SETTITLE )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_MarkupAnnot_SetTitle( ( HPDF_Annotation ) hb_parptr( 1 ), hb_parc( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_MarkupAnnot_SetSubject (HPDF_Annotation annot, const char* name);
*/
HB_FUNC( HPDF_MARKUPANNOT_SETSUBJECT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_MarkupAnnot_SetSubject( ( HPDF_Annotation ) hb_parptr( 1 ), hb_parc( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_MarkupAnnot_SetCreationDate (HPDF_Annotation annot, HPDF_Date value);
*/
HB_FUNC( HPDF_MARKUPANNOT_SETCREATIONDATE )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Date date;

   memset( &date, 0, sizeof( date ) );

   date.year    = hb_parvni( 2, 1 );
   date.month   = hb_parvni( 2, 2 );
   date.day     = hb_parvni( 2, 3 );
   date.hour    = hb_parvni( 2, 4 );
   date.minutes = hb_parvni( 2, 5 );
   date.seconds = hb_parvni( 2, 6 );
   date.ind     = ' ';

   hb_retnl( ( long ) HPDF_MarkupAnnot_SetCreationDate( ( HPDF_Annotation ) hb_parptr( 1 ), date ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_MarkupAnnot_SetTransparency (HPDF_Annotation annot, HPDF_REAL value);
*/
HB_FUNC( HPDF_MARKUPANNOT_SETTRANSPARENCY )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_MarkupAnnot_SetTransparency( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_MarkupAnnot_SetIntent (HPDF_Annotation  annot, HPDF_AnnotIntent  intent);
*/
HB_FUNC( HPDF_MARKUPANNOT_SETINTENT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_MarkupAnnot_SetIntent( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_AnnotIntent ) hb_parni( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_MarkupAnnot_SetPopup (HPDF_Annotation annot, HPDF_Annotation popup);
*/
HB_FUNC( HPDF_MARKUPANNOT_SETPOPUP )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_MarkupAnnot_SetPopup( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_Annotation ) hb_parptr( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_MarkupAnnot_SetRectDiff (HPDF_Annotation  annot, HPDF_Rect  rect);
*/
HB_FUNC( HPDF_MARKUPANNOT_SETRECTDIFF )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retnl( ( long ) HPDF_MarkupAnnot_SetRectDiff( ( HPDF_Annotation ) hb_parptr( 1 ), rc ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_MarkupAnnot_SetCloudEffect (HPDF_Annotation  annot, HPDF_INT cloudIntensity);
*/
HB_FUNC( HPDF_MARKUPANNOT_SETCLOUDEFFECT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_MarkupAnnot_SetCloudEffect( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_INT ) hb_parni( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_MarkupAnnot_SetInteriorRGBColor (HPDF_Annotation  annot, HPDF_RGBColor color);
*/
HB_FUNC( HPDF_MARKUPANNOT_SETINTERIORRGBCOLOR )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_RGBColor rgb;

   rgb.r = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rgb.g = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rgb.b = ( HPDF_REAL ) hb_parvnd( 2, 3 );

   hb_retnl( ( long ) HPDF_MarkupAnnot_SetInteriorRGBColor( ( HPDF_Annotation ) hb_parptr( 1 ), rgb ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_MarkupAnnot_SetInteriorCMYKColor (HPDF_Annotation  annot, HPDF_CMYKColor color);
*/
HB_FUNC( HPDF_MARKUPANNOT_SETINTERIORCMYKCOLOR )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_CMYKColor cmyk;

   cmyk.c = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   cmyk.m = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   cmyk.y = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   cmyk.k = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retnl( ( long ) HPDF_MarkupAnnot_SetInteriorCMYKColor( ( HPDF_Annotation ) hb_parptr( 1 ), cmyk ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_MarkupAnnot_SetInteriorGrayColor (HPDF_Annotation  annot, HPDF_REAL color);
*/
HB_FUNC( HPDF_MARKUPANNOT_SETINTERIORGRAYCOLOR )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_MarkupAnnot_SetInteriorGrayColor( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_MarkupAnnot_SetInteriorTransparent (HPDF_Annotation  annot);
*/
HB_FUNC( HPDF_MARKUPANNOT_SETINTERIORTRANSPARENT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_MarkupAnnot_SetInteriorTransparent( ( HPDF_Annotation ) hb_parptr( 1 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_TextMarkupAnnot_SetQuadPoints ( HPDF_Annotation annot, HPDF_Point lb, HPDF_Point rb, HPDF_Point rt, HPDF_Point lt);
*/
HB_FUNC( HPDF_TEXTMARKUPANNOT_SETQUADPOINTS )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Point p1;
   HPDF_Point p2;
   HPDF_Point p3;
   HPDF_Point p4;

   p1.x = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   p1.y = ( HPDF_REAL ) hb_parvnd( 2, 2 );

   p2.x = ( HPDF_REAL ) hb_parvnd( 3, 1 );
   p2.y = ( HPDF_REAL ) hb_parvnd( 3, 2 );

   p3.x = ( HPDF_REAL ) hb_parvnd( 4, 1 );
   p3.y = ( HPDF_REAL ) hb_parvnd( 4, 2 );

   p4.x = ( HPDF_REAL ) hb_parvnd( 5, 1 );
   p4.y = ( HPDF_REAL ) hb_parvnd( 5, 2 );

   hb_retnl( ( long ) HPDF_TextMarkupAnnot_SetQuadPoints( ( HPDF_Annotation ) hb_parptr( 1 ), p1, p2, p3, p4 ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_Annot_Set3DView  ( HPDF_MMgr mmgr,
                   HPDF_Annotation   annot,
                   HPDF_Annotation   annot3d,
                   HPDF_Dict         view);
*/
HB_FUNC( HPDF_ANNOT_SET3DVIEW )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_Annot_Set3DView( ( HPDF_MMgr ) hb_parptr( 1 ), ( HPDF_Annotation ) hb_parptr( 2 ), ( HPDF_Annotation ) hb_parptr( 3 ), ( HPDF_Dict ) hb_parptr( 4 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_PopupAnnot_SetOpened  (HPDF_Annotation   annot,
                            HPDF_BOOL         opened);
*/
HB_FUNC( HPDF_POPUPANNOT_SETOPENED )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_PopupAnnot_SetOpened( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_BOOL ) hb_parl( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_FreeTextAnnot_SetLineEndingStyle (HPDF_Annotation annot, HPDF_LineAnnotEndingStyle startStyle, HPDF_LineAnnotEndingStyle endStyle);
*/
HB_FUNC( HPDF_FREETEXTANNOT_SETLINEENDINGSTYLE )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_FreeTextAnnot_SetLineEndingStyle( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_LineAnnotEndingStyle ) hb_parni( 2 ), ( HPDF_LineAnnotEndingStyle ) hb_parni( 3 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_FreeTextAnnot_Set3PointCalloutLine (HPDF_Annotation annot, HPDF_Point startPoint, HPDF_Point kneePoint, HPDF_Point endPoint);
*/
HB_FUNC( HPDF_FREETEXTANNOT_SET3POINTCALLOUTLINE )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Point p1;
   HPDF_Point p2;
   HPDF_Point p3;

   p1.x = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   p1.y = ( HPDF_REAL ) hb_parvnd( 2, 2 );

   p2.x = ( HPDF_REAL ) hb_parvnd( 3, 1 );
   p2.y = ( HPDF_REAL ) hb_parvnd( 3, 2 );

   p3.x = ( HPDF_REAL ) hb_parvnd( 4, 1 );
   p3.y = ( HPDF_REAL ) hb_parvnd( 4, 2 );

   hb_retnl( ( long ) HPDF_FreeTextAnnot_Set3PointCalloutLine( ( HPDF_Annotation ) hb_parptr( 1 ), p1, p2, p3 ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_FreeTextAnnot_Set2PointCalloutLine (HPDF_Annotation annot, HPDF_Point startPoint, HPDF_Point endPoint);
*/
HB_FUNC( HPDF_FREETEXTANNOT_SET2POINTCALLOUTLINE )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Point p1;
   HPDF_Point p2;

   p1.x = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   p1.y = ( HPDF_REAL ) hb_parvnd( 2, 2 );

   p2.x = ( HPDF_REAL ) hb_parvnd( 3, 1 );
   p2.y = ( HPDF_REAL ) hb_parvnd( 3, 2 );

   hb_retnl( ( long ) HPDF_FreeTextAnnot_Set2PointCalloutLine( ( HPDF_Annotation ) hb_parptr( 1 ), p1, p2 ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_FreeTextAnnot_SetDefaultStyle (HPDF_Annotation  annot, const char* style);
*/
HB_FUNC( HPDF_FREETEXTANNOT_SETDEFAULTSTYLE )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_FreeTextAnnot_SetDefaultStyle( ( HPDF_Annotation ) hb_parptr( 1 ), hb_parc( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_LineAnnot_SetPosition (HPDF_Annotation annot,
                     HPDF_Point startPoint, HPDF_LineAnnotEndingStyle startStyle,
                     HPDF_Point endPoint, HPDF_LineAnnotEndingStyle endStyle);
*/
HB_FUNC( HPDF_LINEANNOT_SETPOSITION )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Point p1;
   HPDF_Point p2;

   p1.x = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   p1.y = ( HPDF_REAL ) hb_parvnd( 2, 2 );

   p2.x = ( HPDF_REAL ) hb_parvnd( 4, 1 );
   p2.y = ( HPDF_REAL ) hb_parvnd( 4, 2 );

   hb_retnl( ( long ) HPDF_LineAnnot_SetPosition( ( HPDF_Annotation ) hb_parptr( 1 ), p1, ( HPDF_LineAnnotEndingStyle ) hb_parni( 3 ), p2, ( HPDF_LineAnnotEndingStyle ) hb_parni( 5 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_LineAnnot_SetLeader (HPDF_Annotation annot, HPDF_INT leaderLen, HPDF_INT leaderExtLen, HPDF_INT leaderOffsetLen);
*/
HB_FUNC( HPDF_LINEANNOT_SETLEADER )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_LineAnnot_SetLeader( ( HPDF_Annotation ) hb_parptr( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_LineAnnot_SetCaption (HPDF_Annotation annot, HPDF_BOOL showCaption, HPDF_LineAnnotCapPosition position, HPDF_INT horzOffset, HPDF_INT vertOffset);
*/
HB_FUNC( HPDF_LINEANNOT_SETCAPTION )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_LineAnnot_SetCaption( ( HPDF_Annotation ) hb_parptr( 1 ), hb_parl( 2 ), ( HPDF_LineAnnotCapPosition ) hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_Annotation_SetBorderStyle  (HPDF_Annotation  annot,
                                 HPDF_BSSubtype   subtype,
                                 HPDF_REAL        width,
                                 HPDF_UINT16      dash_on,
                                 HPDF_UINT16      dash_off,
                                 HPDF_UINT16      dash_phase);
*/
HB_FUNC( HPDF_ANNOTATION_SETBORDERSTYLE )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_Annotation_SetBorderStyle( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_BSSubtype ) hb_parni( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_UINT16 ) hb_parni( 4 ), ( HPDF_UINT16 ) hb_parni( 5 ), ( HPDF_UINT16 ) hb_parni( 6 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_Dict)
HPDF_Page_Create3DView (HPDF_Page       page,
                        HPDF_U3D        u3d,
                        HPDF_Annotation   annot3d,
                        const char *name);
*/
HB_FUNC( HPDF_PAGE_CREATE3DVIEW )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retptr( ( HPDF_Dict ) HPDF_Page_Create3DView( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_U3D ) hb_parptr( 2 ), ( HPDF_Annotation ) hb_parptr( 3 ), hb_parc( 4 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
HPDF_EXPORT(HPDF_Image)
HPDF_LoadPngImageFromMem  (HPDF_Doc     pdf,
                    const HPDF_BYTE    *buffer,
                          HPDF_UINT     size);
*/
HB_FUNC( HPDF_LOADPNGIMAGEFROMMEM )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_UINT size = ( HPDF_UINT ) hb_parclen( 2 );
   HPDF_BYTE * buffer;

   buffer = ( HPDF_BYTE * ) hb_xgrab( size + 1 );

   hb_retptr( ( HPDF_Image ) HPDF_LoadPngImageFromMem( HPDF_Doc_par( 1 ), buffer, size ) );

   if( ! hb_storclen_buffer( ( char * ) buffer, size, 2 ) )
      hb_xfree( buffer );
#else
   hb_storc( NULL, 2 );
   hb_retptr( NULL );
#endif
}
/*
HPDF_EXPORT(HPDF_Image)
HPDF_LoadJpegImageFromMem   (HPDF_Doc      pdf,
                      const HPDF_BYTE     *buffer,
                            HPDF_UINT      size);
*/
HB_FUNC( HPDF_LOADJPEGIMAGEFROMMEM )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_UINT size = ( HPDF_UINT ) hb_parclen( 2 );
   HPDF_BYTE * buffer;

   buffer = ( HPDF_BYTE * ) hb_xgrab( size + 1 );

   hb_retptr( ( HPDF_Image ) HPDF_LoadJpegImageFromMem( HPDF_Doc_par( 1 ), buffer, size ) );

   if( ! hb_storclen_buffer( ( char * ) buffer, size, 2 ) )
      hb_xfree( buffer );
#else
   hb_storc( NULL, 2 );
   hb_retptr( NULL );
#endif
}
/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_Image_AddSMask  (HPDF_Image    image,
                      HPDF_Image    smask);
*/
HB_FUNC( HPDF_IMAGE_ADDSMASK )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_Image_AddSMask( ( HPDF_Image ) hb_parptr( 1 ), ( HPDF_Image ) hb_parptr( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
HPDF_EXPORT(HPDF_EmbeddedFile)
HPDF_AttachFile  (HPDF_Doc    pdf,
                  const char *file);
*/
HB_FUNC( HPDF_ATTACHFILE )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retptr( ( HPDF_EmbeddedFile ) HPDF_AttachFile( HPDF_Doc_par( 1 ), hb_parc( 2 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
HPDF_EXPORT(HPDF_OutputIntent)
HPDF_ICC_LoadIccFromMem (HPDF_Doc   pdf,
                        HPDF_MMgr   mmgr,
                        HPDF_Stream iccdata,
                        HPDF_Xref   xref,
                        int         numcomponent);
*/
HB_FUNC( HPDF_ICC_LOADICCFROMMEM )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retptr( ( HPDF_OutputIntent ) HPDF_ICC_LoadIccFromMem( HPDF_Doc_par( 1 ), ( HPDF_MMgr ) hb_parptr( 2 ), ( HPDF_Stream ) hb_parptr( 3 ), ( HPDF_Xref ) hb_parptr( 4 ), hb_parni( 5 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
HPDF_EXPORT(HPDF_OutputIntent)
HPDF_LoadIccProfileFromFile  (HPDF_Doc  pdf,
                            const char* icc_file_name,
                                   int  numcomponent);
*/
HB_FUNC( HPDF_LOADICCPROFILEFROMFILE )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retptr( ( HPDF_OutputIntent ) HPDF_LoadIccProfileFromFile( HPDF_Doc_par( 1 ), hb_parc( 2 ), hb_parni( 3 ) ) );
#else
   hb_retptr( NULL );
#endif
}

/*
HPDF_STATUS
HPDF_PDFA_SetPDFAConformance (HPDF_Doc pdf,HPDF_PDFAType pdfatype)
*/
HB_FUNC( HPDF_PDFA_SETPDFACONFORMANCE )
{
/* TOFIX: These functions are not exported from libharu. Until we find
          out it is design decision or bug, I excluded them from dynamic
          builds. [vszakats] */
#if HB_HPDF_VERS( 2, 2, 0 ) && ! defined( HB_DYNLIB )
   hb_retnl( HPDF_PDFA_SetPDFAConformance( HPDF_Doc_par( 1 ), ( HPDF_PDFAType ) hb_parni( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}

/*----------------------------------------------------------------------*/

/*
HPDF_EXPORT(HPDF_STATUS)
HPDF_UseUTFEncodings   (HPDF_Doc   pdf);
*/
HB_FUNC( HPDF_USEUTFENCODINGS )
{
#if HB_HPDF_VERS( 2, 3, 0 )
   hb_retnl( HPDF_UseUTFEncodings( HPDF_Doc_par( 1 ) ) );
#else
   hb_retnl( -1 );
#endif
}
