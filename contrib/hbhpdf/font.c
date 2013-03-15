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
 * along with this software; see the file COPYING.txt.   If not, write to
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

#include "hbhpdf.h"

/* HPDF_LoadTypeIFontFromFile( hDoc, cAFMFileName, cPFA_PFBFileName ) -> cFontName
 */
HB_FUNC( HPDF_LOADTYPE1FONTFROMFILE )
{
   char *       pszFree1;
   const char * pszFileName1 = hb_fsNameConv( hb_parcx( 2 ), &pszFree1 );
   char *       pszFree2;
   const char * pszFileName2 = hb_fsNameConv( hb_parcx( 3 ), &pszFree2 );

   hb_retc( HPDF_LoadType1FontFromFile( hb_HPDF_Doc_par( 1 ), pszFileName1, pszFileName2 ) );

   if( pszFree1 )
      hb_xfree( pszFree1 );

   if( pszFree2 )
      hb_xfree( pszFree2 );
}

/* HPDF_LoadTTFontFromFile( hDoc, cTTFontFileName, lEmbed ) -> cFontName
 */
HB_FUNC( HPDF_LOADTTFONTFROMFILE )
{
   char *       pszFree;
   const char * pszFileName = hb_fsNameConv( hb_parcx( 2 ), &pszFree );

   hb_retc( HPDF_LoadTTFontFromFile( hb_HPDF_Doc_par( 1 ), pszFileName, hb_parl( 3 ) ? HPDF_TRUE : HPDF_FALSE ) );

   if( pszFree )
      hb_xfree( pszFree );
}

/* HPDF_LoadTTFontFromFile2( hDoc, cTTFontFileName, nIndexInFile, lEmbed ) -> cFontName
 */
HB_FUNC( HPDF_LOADTTFONTFROMFILE2 )
{
   char *       pszFree;
   const char * pszFileName = hb_fsNameConv( hb_parcx( 2 ), &pszFree );

   hb_retc( HPDF_LoadTTFontFromFile2( hb_HPDF_Doc_par( 1 ), pszFileName, hb_parni( 3 ), hb_parl( 4 ) ? HPDF_TRUE : HPDF_FALSE ) );

   if( pszFree )
      hb_xfree( pszFree );
}
