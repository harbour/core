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
 * along with this software; see the file COPYING.txt.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/ ).
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

#include "hbhpdf.h"

static HB_GARBAGE_FUNC( hb_HPDF_Doc_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */
      HPDF_Free( ( HPDF_Doc ) * ph );

      /* set pointer to NULL to avoid multiple freeing */
      *ph = NULL;
   }
}

static const HB_GC_FUNCS s_gcHPDF_DocFuncs =
{
   hb_HPDF_Doc_release,
   hb_gcDummyMark
};

HPDF_Doc hb_HPDF_Doc_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gcHPDF_DocFuncs, iParam );

   return ph ? ( HPDF_Doc ) * ph : NULL;
}

/*
     Most of the functions return hStatus == HPDF_OK or ERROR Code
 */

/* HPDF_New() -> hDoc
 */
HB_FUNC( HPDF_NEW )
{
   void ** ph = ( void ** ) hb_gcAllocate( sizeof( HPDF_Doc ), &s_gcHPDF_DocFuncs );

   *ph = ( void * ) HPDF_New( NULL, NULL );

   hb_retptrGC( ph );
}

/* HPDF_Free( hDoc ) -> NIL
 */
HB_FUNC( HPDF_FREE )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gcHPDF_DocFuncs, 1 );

   if( ph && *ph )
   {
      /* Destroy the object */
      HPDF_Free( ( HPDF_Doc ) * ph );

      /* set pointer to NULL to avoid multiple freeing */
      *ph = NULL;
   }
}

/* HPDF_NewDoc( hDoc ) -> hStatus
 */
HB_FUNC( HPDF_NEWDOC )
{
   hb_retnl( ( long ) HPDF_NewDoc( hb_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_FreeDoc( hNewDoc ) -> NIL
 */
HB_FUNC( HPDF_FREEDOC )
{
   HPDF_FreeDoc( hb_HPDF_Doc_par( 1 ) );
}

/* HPDF_FreeDocAll() -> NIL
 */
HB_FUNC( HPDF_FREEDOCALL )
{
   HPDF_FreeDocAll( hb_HPDF_Doc_par( 1 ) );
}

/* HPDF_SaveToFile( hDoc, cFileToSave ) -> hStatus
 */
HB_FUNC( HPDF_SAVETOFILE )
{
   char *       pszFree;
   const char * pszFileName = hb_fsNameConv( hb_parcx( 2 ), &pszFree );

   hb_retnl( ( long ) HPDF_SaveToFile( hb_HPDF_Doc_par( 1 ), pszFileName ) );

   if( pszFree )
      hb_xfree( pszFree );
}

/* HPDF_SaveToStream( hDoc ) -> hStatus
 */
HB_FUNC( HPDF_SAVETOSTREAM )
{
   hb_retnl( ( long ) HPDF_SaveToStream( hb_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_GetStreamSize( hDoc ) -> nSize
 */
HB_FUNC( HPDF_GETSTREAMSIZE )
{
   hb_retnl( ( long ) HPDF_GetStreamSize( hb_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_ReadFromStream( hDoc, @cBuffer ) -> nBytesRead
 */
HB_FUNC( HPDF_READFROMSTREAM )
{
   HPDF_UINT32 size = ( HPDF_UINT32 ) hb_parclen( 2 );
   HPDF_BYTE * buffer;

   if( size < 1024 )
      size = 1024;

   buffer = ( HPDF_BYTE * ) hb_xgrab( size + 1 );

   hb_retnl( ( long ) HPDF_ReadFromStream( hb_HPDF_Doc_par( 1 ), buffer, &size ) );

   if( ! hb_storclen_buffer( ( char * ) buffer, size, 2 ) )
      hb_xfree( buffer );
}

/* HPDF_ResetStream( hDoc ) -> hStatus
 */
HB_FUNC( HPDF_RESETSTREAM )
{
   hb_retnl( ( long ) HPDF_ResetStream( hb_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_HasDoc( hDoc ) -> lHasDoc
 */
HB_FUNC( HPDF_HASDOC )
{
   hb_retl( HPDF_HasDoc( hb_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_SetErrorHandler( hDoc, procErrHandler ) -> hStatus
 */
HB_FUNC( HPDF_SETERRORHANDLER )
{
   /* TOFIX: This should be extended to pass a wrapper which calls a
             user defined codeblock. */

   hb_retnl( ( long ) HPDF_SetErrorHandler( hb_HPDF_Doc_par( 1 ), ( HPDF_Error_Handler ) hb_parptr( 2 ) ) );
}

/* HPDF_GetError( hDoc ) -> nErrorCode
 */
HB_FUNC( HPDF_GETERROR )
{
   hb_retnl( ( long ) HPDF_GetError( hb_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_GetErrorDetail( hDoc ) -> nErrorCode
 */
HB_FUNC( HPDF_GETERRORDETAIL )
{
   hb_retnl( ( long ) HPDF_GetErrorDetail( hb_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_ResetError( hDoc ) -> NIL
 */
HB_FUNC( HPDF_RESETERROR )
{
   HPDF_ResetError( hb_HPDF_Doc_par( 1 ) );
}

/* HPDF_SetPagesConfiguration( hDoc, nPagePerPages ) -> hStatus
 */
HB_FUNC( HPDF_SETPAGESCONFIGURATION )
{
   hb_retnl( ( long ) HPDF_SetPagesConfiguration( hb_HPDF_Doc_par( 1 ), hb_parni( 2 ) ) );
}

/* HPDF_SetPageLayout( hDoc, nLayout ) -> hStatus
        nLayout ==
   HPDF_PAGE_LAYOUT_SINGLE             0
   HPDF_PAGE_LAYOUT_ONE_COLUMN         1
   HPDF_PAGE_LAYOUT_TWO_COLUMN_LEFT    2
   HPDF_PAGE_LAYOUT_TWO_COLUMN_RIGHT   3
   HPDF_PAGE_LAYOUT_EOF                4
 */
HB_FUNC( HPDF_SETPAGELAYOUT )
{
   hb_retnl( ( long ) HPDF_SetPageLayout( hb_HPDF_Doc_par( 1 ), ( HPDF_PageLayout ) hb_parni( 2 ) ) );
}

/* HPDF_GetPageLayout( hDoc ) -> nLayout
 */
HB_FUNC( HPDF_GETPAGELAYOUT )
{
   hb_retni( ( int ) HPDF_GetPageLayout( hb_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_SetPageMode( hDoc, nPageMode ) -> hStatus
       nPageMode ==
   HPDF_PAGE_MODE_USE_NONE     0  Display the document with neither outline nor thumbnail.
   HPDF_PAGE_MODE_USE_OUTLINE  1  Display the document with outline pane.
   HPDF_PAGE_MODE_USE_THUMBS   2  Display the document with thumbnail pane.
   HPDF_PAGE_MODE_FULL_SCREEN  3  Display the document with full screen mode.
   HPDF_PAGE_MODE_EOF          4
 */
HB_FUNC( HPDF_SETPAGEMODE )
{
   hb_retnl( ( long ) HPDF_SetPageMode( hb_HPDF_Doc_par( 1 ), ( HPDF_PageMode ) hb_parni( 2 ) ) );
}

/* HPDF_GetPageMode( hDoc ) -> nPageMode
 */
HB_FUNC( HPDF_GETPAGEMODE )
{
   hb_retni( ( int ) HPDF_GetPageMode( hb_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_SetOpenAction( hDoc, hDestn ) -> hStatus
 */
HB_FUNC( HPDF_SETOPENACTION )
{
   hb_retnl( ( long ) HPDF_SetOpenAction( hb_HPDF_Doc_par( 1 ), ( HPDF_Destination ) hb_parptr( 2 ) ) );
}

/* HPDF_GetCurrentPage( hDoc ) -> hPage
 */
HB_FUNC( HPDF_GETCURRENTPAGE )
{
   hb_retptr( ( void * ) HPDF_GetCurrentPage( hb_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_AddPage( hDoc ) -> hPage
 */
HB_FUNC( HPDF_ADDPAGE )
{
   hb_retptr( ( void  * ) HPDF_AddPage( hb_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_InsertPage( hDoc, hPage ) -> hPageInserted  : Just before hPage
 */
HB_FUNC( HPDF_INSERTPAGE )
{
   hb_retptr( ( void * ) HPDF_InsertPage( hb_HPDF_Doc_par( 1 ), ( HPDF_Page ) hb_parptr( 2 ) ) );
}

/* HPDF_GetFont( hDoc, cFontName, cEncoding ) -> hFont
 */
HB_FUNC( HPDF_GETFONT )
{
   hb_retptr( ( void * ) HPDF_GetFont( hb_HPDF_Doc_par( 1 ), hb_parc( 2 ), hb_parc( 3 ) ) );
}

/* HPDF_AddPageLabel( hDoc, nPageNum, nPgNoStyle, nFirstPageInRange, cPrefixToLabel ) -> hStatus
       nPgNoStyle
   HPDF_PAGE_NUM_STYLE_DECIMAL         1   Page label is displayed by Arabic numerals.
   HPDF_PAGE_NUM_STYLE_UPPER_ROMAN     2   Page label is displayed by Uppercase roman numerals.
   HPDF_PAGE_NUM_STYLE_LOWER_ROMAN     3   Page label is displayed by Lowercase roman numerals.
   HPDF_PAGE_NUM_STYLE_UPPER_LETTERS   4   Page label is displayed by Uppercase letters (using A to Z).
   HPDF_PAGE_NUM_STYLE_LOWER_LETTERS   5   Page label is displayed by Lowercase letters (using a to z).
 */
HB_FUNC( HPDF_ADDPAGELABEL )
{
   hb_retnl( ( long ) HPDF_AddPageLabel( hb_HPDF_Doc_par( 1 ), hb_parni( 2 ), ( HPDF_PageNumStyle ) hb_parni( 3 ), hb_parni( 4 ), hb_parc( 5 ) ) );
}

/* HPDF_CreateExtGState( hDoc ) -> hExtGState
 */
HB_FUNC( HPDF_CREATEEXTGSTATE )
{
   hb_retptr( ( void * ) HPDF_CreateExtGState( hb_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_CreateOutline( hDoc, hParentOutline, cTitle, hEncoder ) -> hOutline
 */
HB_FUNC( HPDF_CREATEOUTLINE )
{
   hb_retptr( ( void * ) HPDF_CreateOutline( hb_HPDF_Doc_par( 1 ), ( HPDF_Outline ) hb_parptr( 2 ), hb_parc( 3 ), ( HPDF_Encoder ) hb_parptr( 4 ) ) );
}

/* HPDF_GetEncoder( hDoc, cEncoding ) -> hEncoder
 */
HB_FUNC( HPDF_GETENCODER )
{
   hb_retptr( ( void * ) HPDF_GetEncoder( hb_HPDF_Doc_par( 1 ), hb_parc( 2 ) ) );
}

/* HPDF_GetCurrentEncoder( hDoc ) -> hEncoder
 */
HB_FUNC( HPDF_GETCURRENTENCODER )
{
   hb_retptr( ( void * ) HPDF_GetCurrentEncoder( hb_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_SetCurrentEncoder( hDoc, hEncoder ) -> hStatus
 */
HB_FUNC( HPDF_SETCURRENTENCODER )
{
   hb_retnl( ( long ) HPDF_SetCurrentEncoder( hb_HPDF_Doc_par( 1 ), hb_parc( 2 ) ) );
}

/* HPDF_SetInfoAttr( hDoc, nInfoType, cInfo ) -> hStatus
       nInfoType ==
   HPDF_INFO_AUTHOR
   HPDF_INFO_CREATOR
   HPDF_INFO_TITLE
   HPDF_INFO_SUBJECT
   HPDF_INFO_KEYWORDS
 */
HB_FUNC( HPDF_SETINFOATTR )
{
   HPDF_Doc doc = hb_HPDF_Doc_par( 1 );

   if( doc )
      hb_retnl( ( long ) HPDF_SetInfoAttr( doc, ( HPDF_InfoType ) hb_parni( 2 ), hb_parc( 3 ) ) );
   else
      hb_retnl( HB_HPDF_BADPARAM );
}

/* HPDF_GetInfoAttr( hDoc, nInfoType ) -> cInfo
 */
HB_FUNC( HPDF_GETINFOATTR )
{
   HPDF_Doc doc = hb_HPDF_Doc_par( 1 );

   if( doc )
      hb_retc( HPDF_GetInfoAttr( doc, ( HPDF_InfoType ) hb_parni( 2 ) ) );
   else
      hb_retc_null();
}

/* HPDF_SetInfoDateAttr( hDoc, nInfoType, aDateValues ) -> hStatus
       nInfoType ==
   HPDF_INFO_CREATION_DATE
   HPDF_INFO_MOD_DATE
 */
HB_FUNC( HPDF_SETINFODATEATTR )
{
   HPDF_Doc doc = hb_HPDF_Doc_par( 1 );

   if( doc )
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

      hb_retnl( ( long ) HPDF_SetInfoDateAttr( doc, ( HPDF_InfoType ) hb_parni( 2 ), date ) );
   }
   else
      hb_retnl( HB_HPDF_BADPARAM );
}

/* HPDF_SetPassword( hDoc, cOwnerPassword = NO NIL, cUserPassword = CANBE NIL ) -> hStatus
 */
HB_FUNC( HPDF_SETPASSWORD )
{
   hb_retnl( ( long ) HPDF_SetPassword( hb_HPDF_Doc_par( 1 ), hb_parc( 2 ), hb_parc( 3 ) ) );
}

/* HPDF_SetPermission( hDoc, nPermission ) -> hStatus
       nPermission ==
   HPDF_ENABLE_READ      1   user can read the document.
   HPDF_ENABLE_PRINT     2   user can print the document.
   HPDF_ENABLE_EDIT_ALL  3   user can edit the contents of the document other than annotations, form fields.
   HPDF_ENABLE_COPY      4   user can copy the text and the graphics of the document.
   HPDF_ENABLE_EDIT      5   user can add or modify the annotations and form fields of the document.
 */
HB_FUNC( HPDF_SETPERMISSION )
{
   hb_retnl( ( long ) HPDF_SetPermission( hb_HPDF_Doc_par( 1 ), hb_parni( 2 ) ) );
}

/* HPDF_SetEncryptionMode( hDoc, nEncMode, nKeyLen ) -> hStatus
       nEncMode ==
   HPDF_ENCRYPT_R2    1   Use "Revision 2" algorithm.
                             The length of key is automatically set to 5(40bit).
   HPDF_ENCRYPT_R3    2   Use "Revision 3" algorithm.
                             Between 5(40bit) and 16(128bit) can be specified for length of the key
 */
HB_FUNC( HPDF_SETENCRYPTIONMODE )
{
   hb_retnl( ( long ) HPDF_SetEncryptionMode( hb_HPDF_Doc_par( 1 ), ( HPDF_EncryptMode ) hb_parni( 2 ), hb_parni( 3 ) ) );
}

/* HPDF_SetCompressionMode( hDoc, nCompMode ) -> hStatus
       nCompMode ==
   HPDF_COMP_NONE         1    All contents are not compressed.
   HPDF_COMP_TEXT         2    Compress the contents stream of the page.
   HPDF_COMP_IMAGE        3    Compress the streams of the image objects.
   HPDF_COMP_METADATA     4    Other stream datas (fonts, cmaps and so on)  are compressed.
   HPDF_COMP_ALL          5    All stream datas are compressed. (The same as "HPDF_COMP_TEXT | HPDF_COMP_IMAGE | HPDF_COMP_METADATA")
 */
HB_FUNC( HPDF_SETCOMPRESSIONMODE )
{
   hb_retnl( ( long ) HPDF_SetCompressionMode( hb_HPDF_Doc_par( 1 ), hb_parni( 2 ) ) );
}

/*----------------------------------------------------------------------*/
/*                           Page Handling                              */
/*----------------------------------------------------------------------*/

/* HPDF_Page_SetWidth( hPage, nWidth ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SETWIDTH )
{
   HPDF_Page page = ( HPDF_Page ) hb_parptr( 1 );

   if( page )
      hb_retnl( ( long ) HPDF_Page_SetWidth( page, ( HPDF_REAL ) hb_parnd( 2 ) ) );
   else
      hb_retnl( HB_HPDF_BADPARAM );
}

/* HPDF_Page_SetHeight( hPage, nHeight ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SETHEIGHT )
{
   HPDF_Page page = ( HPDF_Page ) hb_parptr( 1 );

   if( page )
      hb_retnl( ( long ) HPDF_Page_SetHeight( page, ( HPDF_REAL ) hb_parnd( 2 ) ) );
   else
      hb_retnl( HB_HPDF_BADPARAM );
}

/* HPDF_Page_SetSize( hPage, nSize, nOrientation = 1 Portrait, 2 Landscape ) -> hStatus
       nSize ==
   HPDF_PAGE_SIZE_LETTER         1      8.5 x 11 (Inches) 612 x 792
   HPDF_PAGE_SIZE_LEGAL          2      8.5 x 14 (Inches) 612 x 1008
   HPDF_PAGE_SIZE_A3             3      297 x 420 (mm) 841.89 x 1199.551
   HPDF_PAGE_SIZE_A4             4      210 x 297 (mm)  595.276 x 841.89
   HPDF_PAGE_SIZE_A5             5      148 x 210 (mm) 419.528 x 595.276
   HPDF_PAGE_SIZE_B4             6      250 x 353 (mm)  708.661 x 1000.63
   HPDF_PAGE_SIZE_B5             7      176 x 250 (mm) 498.898 x 708.661
   HPDF_PAGE_SIZE_EXECUTIVE      8      7.5 x 10.5 (Inches) 522 x 756
   HPDF_PAGE_SIZE_US4x6          9      4 x 6 (Inches) 288 x 432
   HPDF_PAGE_SIZE_US4x8         10      4 x 8 (Inches) 288 x 576
   HPDF_PAGE_SIZE_US5x7         11      5 x 7 (Inches) 360 x 504
   HPDF_PAGE_SIZE_COMM10        12      4.125 x 9.5 (Inches) 297x 684
 */
HB_FUNC( HPDF_PAGE_SETSIZE )
{
   hb_retnl( ( long ) HPDF_Page_SetSize( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_PageSizes ) hb_parni( 2 ), ( HPDF_PageDirection ) hb_parni( 3 ) ) );
}

/* HPDF_Page_SetRotate( hPage, nAngle = 0-360 ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SETROTATE )
{
   hb_retnl( ( long ) HPDF_Page_SetRotate( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_UINT16 ) hb_parni( 2 ) ) );
}

/* HPDF_Page_GetWidth( hPage ) -> nWidth
 */
HB_FUNC( HPDF_PAGE_GETWIDTH )
{
   hb_retnd( ( double ) HPDF_Page_GetWidth( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_GetHeight( hPage ) -> nHeight
 */
HB_FUNC( HPDF_PAGE_GETHEIGHT )
{
   hb_retnd( ( double ) HPDF_Page_GetHeight( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_CreateDestination( hPage ) -> hDestn
 */
HB_FUNC( HPDF_PAGE_CREATEDESTINATION )
{
   hb_retptr( ( void * ) HPDF_Page_CreateDestination( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_CreateAnnot( hPage, aRect[nLeft,nTop,nRight,nBottom], cText, cEncoder ) -> nHandle
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

/* HPDF_Page_CreateLinkAnnot( hPage, aRect, hDestn ) -> nHandle
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

/* HPDF_Page_CreateURILinkAnnot( hPage, aRect, cURI ) -> nHandle
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

/* HPDF_Page_TextWidth( hPage, cText ) -> nTextWidth
 */
HB_FUNC( HPDF_PAGE_TEXTWIDTH )
{
   hb_retnd( ( double ) HPDF_Page_TextWidth( ( HPDF_Page ) hb_parptr( 1 ), hb_parc( 2 ) ) );
}

/* HPDF_Page_MeasureText( hPage, cText, nWidth, lWordWrap ) -> nByteLenOfTextToFitWidth
 */
HB_FUNC( HPDF_PAGE_MEASURETEXT )
{
   hb_retnl( ( long ) HPDF_Page_MeasureText( ( HPDF_Page ) hb_parptr( 1 ), hb_parc( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), hb_parl( 4 ) ? HPDF_TRUE : HPDF_FALSE, NULL ) );
}

/* HPDF_Page_GetMode( hPage ) -> nGraphicMode
 */
HB_FUNC( HPDF_PAGE_GETGMODE )
{
   hb_retnl( ( long ) HPDF_Page_GetGMode( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_GetCurrentPos( hPage ) -> aCurPos[ nX, nY ]
 */
HB_FUNC( HPDF_PAGE_GETCURRENTPOS )
{
   HPDF_Point pt;
   PHB_ITEM   info = hb_itemArrayNew( 2 );

   HPDF_Page_GetCurrentPos2( ( HPDF_Page ) hb_parptr( 1 ), &pt );

   hb_arraySetND( info, 1, pt.x );
   hb_arraySetND( info, 2, pt.y );

   hb_itemReturnRelease( info );
}

/* HPDF_Page_GetCurrentTextPos( hPage ) -> aCurTextPos[ nX, nY ]
 */
HB_FUNC( HPDF_PAGE_GETCURRENTTEXTPOS )
{
   HPDF_Point pt;
   PHB_ITEM   info = hb_itemArrayNew( 2 );

   HPDF_Page_GetCurrentTextPos2( ( HPDF_Page ) hb_parptr( 1 ), &pt );

   hb_arraySetND( info, 1, pt.x );
   hb_arraySetND( info, 2, pt.y );

   hb_itemReturnRelease( info );
}

/* HPDF_Page_GetCurrentFont( hPage ) -> hFont
 */
HB_FUNC( HPDF_PAGE_GETCURRENTFONT )
{
   hb_retptr( ( void * ) HPDF_Page_GetCurrentFont( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_GetCurrentFontSize( hPage ) -> nFontSize
 */
HB_FUNC( HPDF_PAGE_GETCURRENTFONTSIZE )
{
   hb_retnd( ( double ) HPDF_Page_GetCurrentFontSize( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_GetTransMatrix( hPage ) -> aMatrix[ ]
 */
HB_FUNC( HPDF_PAGE_GETTRANSMATRIX )
{
   HPDF_TransMatrix matrix;
   PHB_ITEM         info = hb_itemArrayNew( 6 );

   matrix = HPDF_Page_GetTransMatrix( ( HPDF_Page ) hb_parptr( 1 ) );

   hb_arraySetND( info, 1, matrix.a );
   hb_arraySetND( info, 2, matrix.b );
   hb_arraySetND( info, 3, matrix.c );
   hb_arraySetND( info, 4, matrix.d );
   hb_arraySetND( info, 5, matrix.x );
   hb_arraySetND( info, 6, matrix.y );

   hb_itemReturnRelease( info );
}

/* HPDF_Page_GetLineWidth( hPage ) -> nLineWidth
 */
HB_FUNC( HPDF_PAGE_GETLINEWIDTH )
{
   hb_retnd( ( double ) HPDF_Page_GetLineWidth( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_GetLineCap( hPage ) -> nLineCapStyle
 */
HB_FUNC( HPDF_PAGE_GETLINECAP )
{
   hb_retnl( ( long ) HPDF_Page_GetLineCap( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_GetLineJoin( hPage ) -> nLineJoinStyle
 */
HB_FUNC( HPDF_PAGE_GETLINEJOIN )
{
   hb_retnl( ( long ) HPDF_Page_GetLineJoin( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_GetMiterLimit( hPage ) -> nMiterLimit
 */
HB_FUNC( HPDF_PAGE_GETMITERLIMIT )
{
   hb_retnd( ( double ) HPDF_Page_GetMiterLimit( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_GetDash( hPage ) -> aDash
 */
HB_FUNC( HPDF_PAGE_GETDASH )
{
   HPDF_DashMode dash;
   PHB_ITEM      info = hb_itemArrayNew( 10 );

   dash = HPDF_Page_GetDash( ( HPDF_Page ) hb_parptr( 1 ) );

   hb_arraySetNI( info, 1, dash.ptn[ 0 ] );
   hb_arraySetNI( info, 2, dash.ptn[ 1 ] );
   hb_arraySetNI( info, 3, dash.ptn[ 2 ] );
   hb_arraySetNI( info, 4, dash.ptn[ 3 ] );
   hb_arraySetNI( info, 5, dash.ptn[ 4 ] );
   hb_arraySetNI( info, 6, dash.ptn[ 5 ] );
   hb_arraySetNI( info, 7, dash.ptn[ 6 ] );
   hb_arraySetNI( info, 8, dash.ptn[ 7 ] );
   hb_arraySetND( info, 9, dash.num_ptn );
   hb_arraySetND( info, 10, dash.phase   );

   hb_itemReturnRelease( info );
}

/* HPDF_Page_GetFlat( hPage ) -> nCurFlatness
 */
HB_FUNC( HPDF_PAGE_GETFLAT )
{
   hb_retnd( ( double ) HPDF_Page_GetFlat( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_GetCharSpace( hPage ) -> nCurCharSpace
 */
HB_FUNC( HPDF_PAGE_GETCHARSPACE )
{
   hb_retnd( ( double ) HPDF_Page_GetCharSpace( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_GetWordSpace( hPage ) -> nCurWordSpace
 */
HB_FUNC( HPDF_PAGE_GETWORDSPACE )
{
   hb_retnd( ( double ) HPDF_Page_GetWordSpace( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_GetHorizontalScalling( hPage ) -> nHorzScaling
 */
HB_FUNC( HPDF_PAGE_GETHORIZONTALSCALLING )
{
   hb_retnd( ( double ) HPDF_Page_GetHorizontalScalling( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_GetTextLeading( hPage ) -> nTextLeading
 */
HB_FUNC( HPDF_PAGE_GETTEXTLEADING )
{
   hb_retnd( ( double ) HPDF_Page_GetTextLeading( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_GetTextRenderingMode( hPage ) -> nTextRenderingMode
 */
HB_FUNC( HPDF_PAGE_GETTEXTRENDERINGMODE )
{
   hb_retnd( ( double ) HPDF_Page_GetTextRenderingMode( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_GetTextRise( hPage ) -> nTextRise
 */
HB_FUNC( HPDF_PAGE_GETTEXTRISE )
{
   hb_retnd( ( double ) HPDF_Page_GetTextRise( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_GetRGBFill( hPage ) -> aRGBFill[ nRed, nGreen, nBlue ]
 */
HB_FUNC( HPDF_PAGE_GETRGBFILL )
{
   HPDF_RGBColor rgb;
   PHB_ITEM      info = hb_itemArrayNew( 3 );

   rgb = HPDF_Page_GetRGBFill( ( HPDF_Page ) hb_parptr( 1 ) );

   hb_arraySetND( info, 1, rgb.r );
   hb_arraySetND( info, 2, rgb.g );
   hb_arraySetND( info, 3, rgb.b );

   hb_itemReturnRelease( info );
}

/* HPDF_Page_GetRGBStroke( hPage ) -> aRGBStroke[ nRed, nGreen, nBlue ]
 */
HB_FUNC( HPDF_PAGE_GETRGBSTROKE )
{
   HPDF_RGBColor rgb;
   PHB_ITEM      info = hb_itemArrayNew( 3 );

   rgb = HPDF_Page_GetRGBStroke( ( HPDF_Page ) hb_parptr( 1 ) );

   hb_arraySetND( info, 1, rgb.r );
   hb_arraySetND( info, 2, rgb.g );
   hb_arraySetND( info, 3, rgb.b );

   hb_itemReturnRelease( info );
}

/* HPDF_Page_GetCMYKFill( hPage ) -> aCMYKFill[ nC, nM, nY, nK ]
 */
HB_FUNC( HPDF_PAGE_GETCMYKFILL )
{
   HPDF_CMYKColor cmyk;
   PHB_ITEM       info = hb_itemArrayNew( 4 );

   cmyk = HPDF_Page_GetCMYKFill( ( HPDF_Page ) hb_parptr( 1 ) );

   hb_arraySetND( info, 1, cmyk.c );
   hb_arraySetND( info, 2, cmyk.m );
   hb_arraySetND( info, 3, cmyk.y );
   hb_arraySetND( info, 4, cmyk.k );

   hb_itemReturnRelease( info );
}

/* HPDF_Page_GetCMYKStroke( hPage ) -> aCMYKStroke[ nC, nM, nY, nK ]
 */
HB_FUNC( HPDF_PAGE_GETCMYKSTROKE )
{
   HPDF_CMYKColor cmyk;
   PHB_ITEM       info = hb_itemArrayNew( 4 );

   cmyk = HPDF_Page_GetCMYKStroke( ( HPDF_Page ) hb_parptr( 1 ) );

   hb_arraySetND( info, 1, cmyk.c );
   hb_arraySetND( info, 2, cmyk.m );
   hb_arraySetND( info, 3, cmyk.y );
   hb_arraySetND( info, 4, cmyk.k );

   hb_itemReturnRelease( info );
}

/* HPDF_Page_GetGrayFill( hPage ) -> nGrayFillValue
 */
HB_FUNC( HPDF_PAGE_GETGRAYFILL )
{
   hb_retnd( ( double ) HPDF_Page_GetGrayFill( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_GetGrayStroke( hPage ) -> nGrayStrokeValue
 */
HB_FUNC( HPDF_PAGE_GETGRAYSTROKE )
{
   hb_retnd( ( double ) HPDF_Page_GetGrayStroke( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_GetStrokingColorSpace( hPage ) -> nStrokingSpace
 */
HB_FUNC( HPDF_PAGE_GETSTROKINGCOLORSPACE )
{
   hb_retni( ( int ) HPDF_Page_GetStrokingColorSpace( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_GetFillingColorSpace( hPage ) -> nFillingColorSpace
 */
HB_FUNC( HPDF_PAGE_GETFILLINGCOLORSPACE )
{
   hb_retni( ( int ) HPDF_Page_GetFillingColorSpace( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_GetTextMatrix( hPage ) -> aMatrix[ ]
 */
HB_FUNC( HPDF_PAGE_GETTEXTMATRIX )
{
   HPDF_TransMatrix matrix;
   PHB_ITEM         info = hb_itemArrayNew( 6 );

   matrix = HPDF_Page_GetTextMatrix( ( HPDF_Page ) hb_parptr( 1 ) );

   hb_arraySetND( info, 1, matrix.a );
   hb_arraySetND( info, 2, matrix.b );
   hb_arraySetND( info, 3, matrix.c );
   hb_arraySetND( info, 4, matrix.d );
   hb_arraySetND( info, 5, matrix.x );
   hb_arraySetND( info, 6, matrix.y );

   hb_itemReturnRelease( info );
}

/* HPDF_Page_GetGStateDepth( hPage ) -> nGStateDepth
 */
HB_FUNC( HPDF_PAGE_GETGSTATEDEPTH )
{
   hb_retni( ( int ) HPDF_Page_GetGStateDepth( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_SetSlideShow( hPage, nType, nDurationPerFrame, nTranstnTime = 1 Second ) -> hStatus
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
/*                              GRAPHICS                                */
/*----------------------------------------------------------------------*/

/* HPDF_Page_SetLineWidth( hPage, nLineWidth ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SETLINEWIDTH )
{
   hb_retnl( ( long ) HPDF_Page_SetLineWidth( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}

/* HPDF_Page_SetLineCap( hPage, nLineCap ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SETLINECAP )
{
   hb_retnl( ( long ) HPDF_Page_SetLineCap( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_LineCap ) hb_parni( 2 ) ) );
}

/* HPDF_Page_SetLineJoin( hPage, nLineJoin ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SETLINEJOIN )
{
   hb_retnl( ( long ) HPDF_Page_SetLineJoin( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_LineJoin ) hb_parni( 2 ) ) );
}

/* HPDF_Page_SetMiterLimit( hPage, nMiterLimit ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SETMITERLIMIT )
{
   hb_retnl( ( long ) HPDF_Page_SetMiterLimit( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}

/* HPDF_Page_SetDash( hPage, aDash, nNumPoints, nStartFrom ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SETDASH )
{
   HPDF_DashMode dash;
   int nPtns = hb_parni( 3 );
   int i;

   for( i = 0; i < nPtns; i++ )
      dash.ptn[ i ] = ( HPDF_UINT16 ) hb_parvni( 2, i + 1 );

   hb_retnl( ( long ) HPDF_Page_SetDash( ( HPDF_Page ) hb_parptr( 1 ), dash.ptn, nPtns, hb_parni( 4 ) ) );
}

/* HPDF_Page_SetExtGState( hPage, hGState ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SETEXTGSTATE )
{
   hb_retnl( ( long ) HPDF_Page_SetExtGState( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_ExtGState ) hb_parptr( 2 ) ) );
}

/* HPDF_Page_GSave( hPage ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_GSAVE )
{
   hb_retnl( ( long ) HPDF_Page_GSave( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_GRestore( hPage ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_GRESTORE )
{
   hb_retnl( ( long ) HPDF_Page_GRestore( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

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

/* HPDF_Page_MoveTo( hPage, nX, nY ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_MOVETO )
{
   hb_retnl( ( long ) HPDF_Page_MoveTo( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ) ) );
}

/* HPDF_Page_LineTo( hPage, nX, nY ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_LINETO )
{
   hb_retnl( ( long ) HPDF_Page_LineTo( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ) ) );
}

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

/* HPDF_Page_ClosePath( hPage ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_CLOSEPATH )
{
   hb_retnl( ( long ) HPDF_Page_ClosePath( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_Rectangle( hPage, nX, nY, nWidth, nHeight ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_RECTANGLE )
{
   hb_retnl( ( long ) HPDF_Page_Rectangle( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ), ( HPDF_REAL ) hb_parnd( 5 ) ) );
}

/* HPDF_Page_Stroke( hPage ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_STROKE )
{
   hb_retnl( ( long ) HPDF_Page_Stroke( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_ClosePathStroke( hPage ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_CLOSEPATHSTROKE )
{
   hb_retnl( ( long ) HPDF_Page_ClosePathStroke( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_SetFontAndSize( hPage, hFont, nSize ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SETFONTANDSIZE )
{
   hb_retnl( ( long ) HPDF_Page_SetFontAndSize( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_Font ) hb_parptr( 2 ), ( HPDF_REAL ) hb_parnd( 3 ) ) );
}

/* HPDF_Page_BeginText( hPage ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_BEGINTEXT )
{
   hb_retnl( ( long ) HPDF_Page_BeginText( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_EndText( hPage ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_ENDTEXT )
{
   hb_retnl( ( long ) HPDF_Page_EndText( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_TextOut( hPage, nX, nY, cText ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_TEXTOUT )
{
   hb_retnl( ( long ) HPDF_Page_TextOut( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), hb_parc( 4 ) ) );
}

/* HPDF_Page_MoveTextPos( hPage, nX, nY ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_MOVETEXTPOS )
{
   hb_retnl( ( long ) HPDF_Page_MoveTextPos( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ) ) );
}

/* HPDF_Page_ShowText( hPage, cText ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SHOWTEXT )
{
   hb_retnl( ( long ) HPDF_Page_ShowText( ( HPDF_Page ) hb_parptr( 1 ), hb_parc( 2 ) ) );
}

/* HPDF_Page_Fill( hPage ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_FILL )
{
   hb_retnl( ( long ) HPDF_Page_Fill( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_Eofill( hPage ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_EOFILL )
{
   hb_retnl( ( long ) HPDF_Page_Eofill( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_FillStroke( hPage ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_FILLSTROKE )
{
   hb_retnl( ( long ) HPDF_Page_FillStroke( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_EofillStroke( hPage ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_EOFILLSTROKE )
{
   hb_retnl( ( long ) HPDF_Page_EofillStroke( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_ClosePathFillStroke( hPage ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_CLOSEPATHFILLSTROKE )
{
   hb_retnl( ( long ) HPDF_Page_ClosePathFillStroke( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_ClosePathEofillStroke( hPage ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_CLOSEPATHEOFILLSTROKE )
{
   hb_retnl( ( long ) HPDF_Page_ClosePathEofillStroke( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_EndPath( hPage ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_ENDPATH )
{
   hb_retnl( ( long ) HPDF_Page_EndPath( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_Clip( hPage ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_CLIP )
{
   hb_retnl( ( long ) HPDF_Page_Clip( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_Eoclip( hPage ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_EOCLIP )
{
   hb_retnl( ( long ) HPDF_Page_Eoclip( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_SetCharSpace( hPage, nSpaceWidth ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SETCHARSPACE )
{
   hb_retnl( ( long ) HPDF_Page_SetCharSpace( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}

/* HPDF_Page_SetWordSpace( hPage, nSpaceWidth ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SETWORDSPACE )
{
   hb_retnl( ( long ) HPDF_Page_SetWordSpace( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}

/* HPDF_Page_SetHorizontalScalling( hPage, nHorzScale ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SETHORIZONTALSCALLING )
{
   hb_retnl( ( long ) HPDF_Page_SetHorizontalScalling( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}

/* HPDF_Page_SetTextLeading( hPage, nTextLeading ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SETTEXTLEADING )
{
   hb_retnl( ( long ) HPDF_Page_SetTextLeading( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}

/* HPDF_Page_SetTextRenderingMode( hPage, nTextRenderingMode ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SETTEXTRENDERINGMODE )
{
   hb_retnl( ( long ) HPDF_Page_SetTextRenderingMode( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_TextRenderingMode ) hb_parni( 2 ) ) );
}

/* HPDF_Page_SetTextRise( hPage, nTextRise ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SETTEXTRISE )
{
   hb_retnl( ( long ) HPDF_Page_SetTextRise( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}

/* HPDF_Page_MoveTextPos2( hPage, nX, nY ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_MOVETEXTPOS2 )
{
   hb_retnl( ( long ) HPDF_Page_MoveTextPos2( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ) ) );
}

/* HPDF_Page_SetTextMatrix( hPage ) -> hStatus          --------tobedone---------
 */
HB_FUNC( HPDF_PAGE_SETTEXTMATRIX )
{
   hb_retnl( ( long ) HPDF_Page_SetTextMatrix( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ), ( HPDF_REAL ) hb_parnd( 5 ), ( HPDF_REAL ) hb_parnd( 6 ), ( HPDF_REAL ) hb_parnd( 7 ) ) );
}

/* HPDF_Page_MoveToNextLine( hPage ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_MOVETONEXTLINE )
{
   hb_retnl( ( long ) HPDF_Page_MoveToNextLine( ( HPDF_Page ) hb_parptr( 1 ) ) );
}

/* HPDF_Page_ShowTextNextLine( hPage, cText ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SHOWTEXTNEXTLINE )
{
   hb_retnl( ( long ) HPDF_Page_ShowTextNextLine( ( HPDF_Page ) hb_parptr( 1 ), hb_parc( 2 ) ) );
}

/* HPDF_Page_ShowTextNextLineEx( hPage, nWordSpace, nCharSpace, cText ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SHOWTEXTNEXTLINEEX )
{
   hb_retnl( ( long ) HPDF_Page_ShowTextNextLineEx( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), hb_parc( 4 ) ) );
}

/* HPDF_Page_SetGrayFill( hPage, nGrayFill ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SETGRAYFILL )
{
   hb_retnl( ( long ) HPDF_Page_SetGrayFill( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}

/* HPDF_Page_SetGrayStroke( hPage, nGrayStroke ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SETGRAYSTROKE )
{
   hb_retnl( ( long ) HPDF_Page_SetGrayStroke( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}

/* HPDF_Page_SetRGBFill( hPage, nRGBRed, nRGBGreen, nRGBBlue ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SETRGBFILL )
{
   hb_retnl( ( long ) HPDF_Page_SetRGBFill( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ) ) );
}

/* HPDF_Page_SetRGBStroke( hPage, nRGBRed, nRGBGreen, nRGBBlue ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SETRGBSTROKE )
{
   hb_retnl( ( long ) HPDF_Page_SetRGBStroke( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ) ) );
}

/* HPDF_Page_SetCMYKFill( hPage, nC, nM, nY, nK ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SETCMYKFILL )
{
   hb_retnl( ( long ) HPDF_Page_SetCMYKFill( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ), ( HPDF_REAL ) hb_parnd( 5 ) ) );
}

/* HPDF_Page_SetCMYKStroke( hPage, nC, nM, nY, nK ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_SETCMYKSTROKE )
{
   hb_retnl( ( long ) HPDF_Page_SetCMYKStroke( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ), ( HPDF_REAL ) hb_parnd( 5 ) ) );
}

/* HPDF_Page_ExecuteXObject( hPage, hImage ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_EXECUTEXOBJECT )
{
   hb_retnl( ( long ) HPDF_Page_ExecuteXObject( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_Image ) hb_parptr( 2 ) ) );
}

/* HPDF_Page_DrawImage( hPage, hImage, nX, nY, nWidth, nHeight ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_DRAWIMAGE )
{
   hb_retnl( ( long ) HPDF_Page_DrawImage( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_Image ) hb_parptr( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ), ( HPDF_REAL ) hb_parnd( 5 ), ( HPDF_REAL ) hb_parnd( 6 ) ) );
}

/* HPDF_Page_Circle( hPage, nX, nY, nRay ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_CIRCLE )
{
   hb_retnl( ( long ) HPDF_Page_Circle( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ) ) );
}

/* HPDF_Page_Arc( hPage, nX, nY, nRay, nAngle1, nAngle2 ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_ARC )
{
   hb_retnl( ( long ) HPDF_Page_Arc( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ), ( HPDF_REAL ) hb_parnd( 5 ), ( HPDF_REAL ) hb_parnd( 6 ) ) );
}

/* HPDF_Page_Ellipse( hPage, nX, nY, nxRay, nyRay ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_ELLIPSE )
{
   hb_retnl( ( long ) HPDF_Page_Ellipse( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ), ( HPDF_REAL ) hb_parnd( 5 ) ) );
}

/* HPDF_Page_TextRect( hPage, nLeft, nTop, nRight, nBottom, cText, nAlign ) -> hStatus
 */
HB_FUNC( HPDF_PAGE_TEXTRECT )
{
   hb_retnl( ( long ) HPDF_Page_TextRect( ( HPDF_Page ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ), ( HPDF_REAL ) hb_parnd( 5 ), hb_parc( 6 ), ( HPDF_TextAlignment ) hb_parni( 7 ), NULL ) );
}

/*----------------------------------------------------------------------*/
/*                              FONTS                                   */
/*----------------------------------------------------------------------*/

/* HPDF_Font_GetFontName( hFont ) -> cFontName
 */
HB_FUNC( HPDF_FONT_GETFONTNAME )
{
   hb_retc( HPDF_Font_GetFontName( ( HPDF_Font ) hb_parptr( 1 ) ) );
}

/* HPDF_Font_GetEncodingName( hFont ) -> cEncoding
 */
HB_FUNC( HPDF_FONT_GETENCODINGNAME )
{
   hb_retc( HPDF_Font_GetEncodingName( ( HPDF_Font ) hb_parptr( 1 ) ) );
}

/* HPDF_Font_GetUnicodeWidth( hFont, hUnicode ) -> nCharWidth
 */
HB_FUNC( HPDF_FONT_GETUNICODEWIDTH )
{
   hb_retnl( ( long ) HPDF_Font_GetUnicodeWidth( ( HPDF_Font ) hb_parptr( 1 ), ( HPDF_UNICODE ) hb_parni( 2 ) ) );
}

/* HPDF_Font_GetBBox( hFont ) -> aRect
 */
HB_FUNC( HPDF_FONT_GETBBOX )
{
   HPDF_Box rc;
   PHB_ITEM info = hb_itemArrayNew( 4 );

   rc = HPDF_Font_GetBBox( ( HPDF_Font ) hb_parptr( 1 ) );

   hb_arraySetND( info, 1, rc.left   );
   hb_arraySetND( info, 2, rc.top    );
   hb_arraySetND( info, 3, rc.right  );
   hb_arraySetND( info, 4, rc.bottom );

   hb_itemReturnRelease( info );
}

/* HPDF_Font_GetAscent( hFont ) -> nAscent
 */
HB_FUNC( HPDF_FONT_GETASCENT )
{
   hb_retni( ( int ) HPDF_Font_GetAscent( ( HPDF_Font ) hb_parptr( 1 ) ) );
}

/* HPDF_Font_GetDescent( hFont ) -> nDescent
 */
HB_FUNC( HPDF_FONT_GETDESCENT )
{
   hb_retni( ( int ) HPDF_Font_GetDescent( ( HPDF_Font ) hb_parptr( 1 ) ) );
}

/* HPDF_Font_GetXHeight( hFont ) -> nXHeight
 */
HB_FUNC( HPDF_FONT_GETXHEIGHT )
{
   hb_retnl( ( long ) HPDF_Font_GetXHeight( ( HPDF_Font ) hb_parptr( 1 ) ) );
}

/* HPDF_Font_GetCapHeight( hFont ) -> nCapsHeight
 */
HB_FUNC( HPDF_FONT_GETCAPHEIGHT )
{
   hb_retnl( ( long ) HPDF_Font_GetCapHeight( ( HPDF_Font ) hb_parptr( 1 ) ) );
}

/* HPDF_Font_TextWidth( hFont, cText, nWidth ) -> aTextWidth[ nNumChars, nNumWords, nWidth, nNumSpace ]
 */
HB_FUNC( HPDF_FONT_TEXTWIDTH )
{
   HPDF_TextWidth tw;
   PHB_ITEM       info = hb_itemArrayNew( 4 );

   tw = HPDF_Font_TextWidth( ( HPDF_Font ) hb_parptr( 1 ), ( HPDF_BYTE * ) hb_parc( 2 ), hb_parni( 3 ) );

   hb_arraySetNI( info, 1, tw.numchars );
   hb_arraySetNI( info, 2, tw.numwords );
   hb_arraySetNI( info, 3, tw.width    );
   hb_arraySetNI( info, 4, tw.numspace );

   hb_itemReturnRelease( info );
}

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
/*                              ENCODING                                */
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

/* HPDF_Encoder_GetUnicode( hEncoder, nCode ) -> nUnicode
 */
HB_FUNC( HPDF_ENCODER_GETUNICODE )
{
   hb_retni( ( int ) HPDF_Encoder_GetUnicode( ( HPDF_Encoder ) hb_parptr( 1 ), ( HPDF_UINT16 ) hb_parni( 2 ) ) );
}

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
/*                                OUTLINE                               */
/*----------------------------------------------------------------------*/

/* HPDF_Outline_SetOpened( hOutline, lShowOpened ) -> hStatus
 */
HB_FUNC( HPDF_OUTLINE_SETOPENED )
{
   hb_retnl( ( long ) HPDF_Outline_SetOpened( ( HPDF_Outline ) hb_parptr( 1 ), hb_parl( 2 ) ? HPDF_TRUE : HPDF_FALSE ) );
}

/* HPDF_Outline_SetDestination( hOutline, hDestn ) -> hStatus
 */
HB_FUNC( HPDF_OUTLINE_SETDESTINATION )
{
   hb_retnl( ( long ) HPDF_Outline_SetDestination( ( HPDF_Outline ) hb_parptr( 1 ), ( HPDF_Destination ) hb_parptr( 2 ) ) );
}

/*----------------------------------------------------------------------*/
/*                              DESTINATION                             */
/*----------------------------------------------------------------------*/

/* HPDF_Destination_SetXYZ( hDestn, nLeft, nTop, nZoom ) -> hStatus
 */
HB_FUNC( HPDF_DESTINATION_SETXYZ )
{
   hb_retnl( ( long ) HPDF_Destination_SetXYZ( ( HPDF_Destination ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ) ) );
}

/* HPDF_Destination_SetFit( hDestn ) -> hStatus
 */
HB_FUNC( HPDF_DESTINATION_SETFIT )
{
   hb_retnl( ( long ) HPDF_Destination_SetFit( ( HPDF_Destination ) hb_parptr( 1 ) ) );
}

/* HPDF_Destination_SetFitH( hDestn, nTop ) -> hStatus
 */
HB_FUNC( HPDF_DESTINATION_SETFITH )
{
   hb_retnl( ( long ) HPDF_Destination_SetFitH( ( HPDF_Destination ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}

/* HPDF_Destination_SetFitV( hDestn, nLeft ) -> hStatus
 */
HB_FUNC( HPDF_DESTINATION_SETFITV )
{
   hb_retnl( ( long ) HPDF_Destination_SetFitV( ( HPDF_Destination ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}

/* HPDF_Destination_SetFitR( hDestn, nLeft, nBottom, nRight, nTop ) -> hStatus
 */
HB_FUNC( HPDF_DESTINATION_SETFITR )
{
   hb_retnl( ( long ) HPDF_Destination_SetFitR( ( HPDF_Destination ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_REAL ) hb_parnd( 4 ), ( HPDF_REAL ) hb_parnd( 5 ) ) );
}

/* HPDF_Destination_SetFitB( hDestn ) -> hStatus
 */
HB_FUNC( HPDF_DESTINATION_SETFITB )
{
   hb_retnl( ( long ) HPDF_Destination_SetFitB( ( HPDF_Destination ) hb_parptr( 1 ) ) );
}

/* HPDF_Destination_SetFitBH( hDestn, nTop ) -> hStatus
 */
HB_FUNC( HPDF_DESTINATION_SETFITBH )
{
   hb_retnl( ( long ) HPDF_Destination_SetFitBH( ( HPDF_Destination ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}

/* HPDF_Destination_SetFitBV( hDestn, nTop ) -> hStatus
 */
HB_FUNC( HPDF_DESTINATION_SETFITBV )
{
   hb_retnl( ( long ) HPDF_Destination_SetFitBV( ( HPDF_Destination ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}

/*----------------------------------------------------------------------*/
/*                               ExtGState                              */
/*----------------------------------------------------------------------*/

/* HPDF_ExtGState_SetAlphaStroke( hGState, nValue ) -> hStatus
 */
HB_FUNC( HPDF_EXTGSTATE_SETALPHASTROKE )
{
   hb_retnl( ( long ) HPDF_ExtGState_SetAlphaStroke( ( HPDF_ExtGState ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}

/* HPDF_ExtGState_SetAlphaFill( hGState, nValue ) -> hStatus
 */
HB_FUNC( HPDF_EXTGSTATE_SETALPHAFILL )
{
   hb_retnl( ( long ) HPDF_ExtGState_SetAlphaFill( ( HPDF_ExtGState ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
}

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

HB_FUNC( HPDF_VERSION_TEXT )
{
   hb_retc_const( HPDF_VERSION_TEXT );
}

/*----------------------------------------------------------------------*/
/*                    New Functions in LibHaru 2.2.0                    */
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

   hb_retnl( ( long ) HPDF_GetContents( hb_HPDF_Doc_par( 1 ), buffer, &size ) );

   if( ! hb_storclen_buffer( ( char * ) buffer, size, 2 ) )
      hb_xfree( buffer );
#else
   hb_storc( NULL, 2 );
   hb_retnl( HB_HPDF_NOTSUPPORTED );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_CheckError  (HPDF_Error   error);
 */
HB_FUNC( HPDF_CHECKERROR )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Error error = ( HPDF_Error ) hb_parptr( 1 );

   if( error )
      hb_retnl( ( long ) HPDF_CheckError( error ) );
   else
      hb_retnl( HB_HPDF_BADPARAM );
#else
   hb_retnl( HB_HPDF_NOTSUPPORTED );
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
   hb_retnl( HB_HPDF_NOTSUPPORTED );
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
   HPDF_EXPORT(HPDF_EmbeddedFile)
   HPDF_AttachFile  (HPDF_Doc    pdf,
                  const char *file);
 */
HB_FUNC( HPDF_ATTACHFILE )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retptr( ( HPDF_EmbeddedFile ) HPDF_AttachFile( hb_HPDF_Doc_par( 1 ), hb_parc( 2 ) ) );
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
   HPDF_MMgr mmgr = ( HPDF_MMgr ) hb_parptr( 2 );

   if( mmgr )
      hb_retptr( ( HPDF_OutputIntent ) HPDF_ICC_LoadIccFromMem( hb_HPDF_Doc_par( 1 ), mmgr, ( HPDF_Stream ) hb_parptr( 3 ), ( HPDF_Xref ) hb_parptr( 4 ), hb_parni( 5 ) ) );
   else
      hb_retptr( NULL );
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
   hb_retptr( ( HPDF_OutputIntent ) HPDF_LoadIccProfileFromFile( hb_HPDF_Doc_par( 1 ), hb_parc( 2 ), hb_parni( 3 ) ) );
#else
   hb_retptr( NULL );
#endif
}

/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_UseUTFEncodings   (HPDF_Doc   pdf);
 */
HB_FUNC( HPDF_USEUTFENCODINGS )
{
#if HB_HPDF_VERS( 2, 3, 0 )
   hb_retnl( HPDF_UseUTFEncodings( hb_HPDF_Doc_par( 1 ) ) );
#else
   hb_retnl( HB_HPDF_NOTSUPPORTED );
#endif
}

HB_FUNC( HB_HPDF_VERSION )
{
   hb_storni( HPDF_MAJOR_VERSION, 1 );
   hb_storni( HPDF_MINOR_VERSION, 2 );
   hb_storni( HPDF_BUGFIX_VERSION, 3 );
}
