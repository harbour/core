/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * expat API - Harbour interface
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
   Author James Clark:
      http://www.jclark.com/xml/
   Using Expat:
      http://www.xml.com/pub/a/1999/09/expat/index.html
      http://www.xml.com/lpt/a/47
 */

/*
   TODO:
      XML_SetExternalEntityRefHandler()
      XML_SetExternalEntityRefHandlerArg()
      XML_ExternalEntityParserCreate()
      XML_SetElementDeclHandler()
         and related functions/constants
 */

#include "expat.h"

#define HB_EXPAT_VERS( ma, mi, mu )  ( XML_MAJOR_VERSION > ma || ( XML_MAJOR_VERSION == ma && ( XML_MINOR_VERSION > mi || ( XML_MINOR_VERSION == mi && XML_MICRO_VERSION >= mu ) ) ) )

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapistr.h"
#include "hbapierr.h"
#include "hbvm.h"

#include "hbexpat.ch"

#define _VAR_xUserData                      0
#define _VAR_xEncodingHandlerData           1
#define _VAR_bStartElementHandler           2
#define _VAR_bEndElementHandler             3
#define _VAR_bCharacterDataHandler          4
#define _VAR_bProcessingInstructionHandler  5
#define _VAR_bCommentHandler                6
#define _VAR_bStartCdataSectionHandler      7
#define _VAR_bEndCdataSectionHandler        8
#define _VAR_bDefaultHandler                9
#define _VAR_bDefaultHandlerExpand          10
#define _VAR_bSkippedEntityHandler          11
#define _VAR_bUnknownEncodingHandler        12
#define _VAR_bStartNamespaceDeclHandler     13
#define _VAR_bEndNamespaceDeclHandler       14
#define _VAR_bXmlDeclHandler                15
#define _VAR_bStartDoctypeDeclHandler       16
#define _VAR_bEndDoctypeDeclHandler         17
#define _VAR_bAttlistDeclHandler            18
#define _VAR_bEntityDeclHandler             19
#define _VAR_bUnparsedEntityDeclHandler     20
#define _VAR_bNotationDeclHandler           21
#define _VAR_bNotStandaloneHandler          22
#define _VAR_LEN_                           23

typedef struct _HB_EXPAT
{
   XML_Parser parser;
   PHB_ITEM   pVar[ _VAR_LEN_ ];
} HB_EXPAT, * PHB_EXPAT;

/* Skeleton wrapper for all single handler setters */
#define HB_EXPAT_SETHANDLER( _nameu_, _name_ ) \
   HB_FUNC( XML_SET##_nameu_ ) \
   { \
      if( PHB_EXPAT_is( 1 ) ) \
      { \
         PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 ); \
         \
         hb_expat_setvar( hb_expat, _VAR_b##_name_, hb_param( 2, HB_IT_BLOCK | HB_IT_SYMBOL ) ); \
         \
         XML_Set##_name_/* do not delete this */ ( hb_expat->parser, hb_expat->pVar[ _VAR_b##_name_ ] ? hb_expat_##_name_ : NULL ); \
         \
         hb_ret(); \
      } \
      else \
         hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS ); \
   }

/* Global initialization/deinitialization */
/* -------------------------------------- */

static void * XMLCALL hb_expat_xgrab( size_t size )
{
   return hb_xgrab( size );
}

static void XMLCALL hb_expat_xfree( void * p )
{
   if( p )
      hb_xfree( p );
}

static void * XMLCALL hb_expat_xrealloc( void * p, size_t size )
{
   return hb_xrealloc( p, size );
}

/* Callbacks */
/* --------- */

/* Common */

static void hb_expat_hnd_void( int nHnd, void * userdata )
{
   PHB_EXPAT hb_expat = ( PHB_EXPAT ) userdata;

   if( hb_expat && hb_expat->pVar[ nHnd ] )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPushEvalSym();
         hb_vmPush( hb_expat->pVar[ nHnd ] );
         hb_vmPush( hb_expat->pVar[ _VAR_xUserData ] );
         hb_vmSend( 1 );

         hb_vmRequestRestore();
      }
   }
}

static void hb_expat_hnd_C( int nHnd, void * userdata, const XML_Char * par1 )
{
   PHB_EXPAT hb_expat = ( PHB_EXPAT ) userdata;

   if( hb_expat && hb_expat->pVar[ nHnd ] )
   {
      if( hb_vmRequestReenter() )
      {
         PHB_ITEM pUserData = hb_itemNew( hb_expat->pVar[ _VAR_xUserData ] );
         PHB_ITEM pPar1     = hb_itemPutStrUTF8( NULL, par1 );

         hb_evalBlock( hb_expat->pVar[ nHnd ], pUserData, pPar1, NULL );

         hb_itemRelease( pPar1 );
         hb_itemRelease( pUserData );

         hb_vmRequestRestore();
      }
   }
}

static void hb_expat_hnd_CLen( int nHnd, void * userdata, const XML_Char * par1, int par1len )
{
   PHB_EXPAT hb_expat = ( PHB_EXPAT ) userdata;

   if( hb_expat && hb_expat->pVar[ nHnd ] )
   {
      if( hb_vmRequestReenter() )
      {
         PHB_ITEM pUserData = hb_itemNew( hb_expat->pVar[ _VAR_xUserData ] );
         PHB_ITEM pPar1     = hb_itemPutStrLenUTF8( NULL, par1, par1len );

         hb_evalBlock( hb_expat->pVar[ nHnd ], pUserData, pPar1, NULL );

         hb_itemRelease( pPar1 );
         hb_itemRelease( pUserData );

         hb_vmRequestRestore();
      }
   }
}

/* Specific */

static void XMLCALL hb_expat_StartElementHandler( void * userdata, const XML_Char * name, const XML_Char ** atts )
{
   PHB_EXPAT hb_expat = ( PHB_EXPAT ) userdata;

   if( hb_expat && hb_expat->pVar[ _VAR_bStartElementHandler ] )
   {
      if( hb_vmRequestReenter() )
      {
         PHB_ITEM pUserData = hb_itemNew( hb_expat->pVar[ _VAR_xUserData ] );
         PHB_ITEM pElement  = hb_itemPutStrUTF8( NULL, name );
         PHB_ITEM pAttr;

         if( atts )
         {
            PHB_ITEM pTempItem = hb_itemNew( NULL );
            HB_ISIZ  nPos;
            HB_ISIZ  nLen = 0;

            for( nPos = 0; atts[ nPos ]; nPos += 2 )
               ++nLen;

            pAttr = hb_itemArrayNew( nLen );

            for( nPos = 0; atts[ nPos ]; nPos += 2 )
            {
               hb_arrayNew( pTempItem, 2 );

               hb_arraySetStrUTF8( pTempItem, 1, atts[ nPos ] );
               hb_arraySetStrUTF8( pTempItem, 2, atts[ nPos + 1 ] );

               hb_arraySetForward( pAttr, ( nPos >> 1 ) + 1, pTempItem );
            }

            hb_itemRelease( pTempItem );
         }
         else
            pAttr = hb_itemArrayNew( 0 );

         hb_evalBlock( hb_expat->pVar[ _VAR_bStartElementHandler ], pUserData, pElement, pAttr, NULL );

         hb_itemRelease( pAttr );
         hb_itemRelease( pElement );
         hb_itemRelease( pUserData );

         hb_vmRequestRestore();
      }
   }
}

static void XMLCALL hb_expat_EndElementHandler( void * userdata, const XML_Char * name )
{
   hb_expat_hnd_C( _VAR_bEndElementHandler, userdata, name );
}

static void XMLCALL hb_expat_CharacterDataHandler( void * userdata, const XML_Char * s, int len )
{
   hb_expat_hnd_CLen( _VAR_bCharacterDataHandler, userdata, s, len );
}

static void XMLCALL hb_expat_ProcessingInstructionHandler( void * userdata, const XML_Char * target, const XML_Char * data )
{
   PHB_EXPAT hb_expat = ( PHB_EXPAT ) userdata;

   if( hb_expat && hb_expat->pVar[ _VAR_bProcessingInstructionHandler ] )
   {
      if( hb_vmRequestReenter() )
      {
         PHB_ITEM pUserData = hb_itemNew( hb_expat->pVar[ _VAR_xUserData ] );
         PHB_ITEM pTarget   = hb_itemPutStrUTF8( NULL, target );
         PHB_ITEM pData     = hb_itemPutStrUTF8( NULL, data );

         hb_evalBlock( hb_expat->pVar[ _VAR_bProcessingInstructionHandler ], pUserData, pTarget, pData, NULL );

         hb_itemRelease( pData );
         hb_itemRelease( pTarget );
         hb_itemRelease( pUserData );

         hb_vmRequestRestore();
      }
   }
}

static void XMLCALL hb_expat_CommentHandler( void * userdata, const XML_Char * data )
{
   hb_expat_hnd_C( _VAR_bCommentHandler, userdata, data );
}

static void XMLCALL hb_expat_StartCdataSectionHandler( void * userdata )
{
   hb_expat_hnd_void( _VAR_bStartCdataSectionHandler, userdata );
}

static void XMLCALL hb_expat_EndCdataSectionHandler( void * userdata )
{
   hb_expat_hnd_void( _VAR_bEndCdataSectionHandler, userdata );
}

static void XMLCALL hb_expat_DefaultHandler( void * userdata, const XML_Char * s, int len )
{
   hb_expat_hnd_CLen( _VAR_bDefaultHandler, userdata, s, len );
}

static void XMLCALL hb_expat_DefaultHandlerExpand( void * userdata, const XML_Char * s, int len )
{
   hb_expat_hnd_CLen( _VAR_bDefaultHandlerExpand, userdata, s, len );
}

static void XMLCALL hb_expat_SkippedEntityHandler( void * userdata,
                                                   const XML_Char * entityName,
                                                   int is_parameter_entity )
{
   PHB_EXPAT hb_expat = ( PHB_EXPAT ) userdata;

   if( hb_expat && hb_expat->pVar[ _VAR_bSkippedEntityHandler ] )
   {
      if( hb_vmRequestReenter() )
      {
         PHB_ITEM pUserData = hb_itemNew( hb_expat->pVar[ _VAR_xUserData ] );
         PHB_ITEM pPar1     = hb_itemPutStrUTF8( NULL, entityName );
         PHB_ITEM pPar2     = hb_itemPutL( NULL, is_parameter_entity );

         hb_evalBlock( hb_expat->pVar[ _VAR_bSkippedEntityHandler ], pUserData, pPar1, pPar2, NULL );

         hb_itemRelease( pPar2 );
         hb_itemRelease( pPar1 );
         hb_itemRelease( pUserData );

         hb_vmRequestRestore();
      }
   }
}

static int XMLCALL hb_expat_UnknownEncodingHandler( void * userdata,
                                                    const XML_Char * name,
                                                    XML_Encoding * info )
{
   PHB_EXPAT hb_expat = ( PHB_EXPAT ) userdata;
   int       iResult  = XML_STATUS_ERROR;

   if( hb_expat && hb_expat->pVar[ _VAR_bUnknownEncodingHandler ] )
   {
      if( hb_vmRequestReenter() )
      {
         PHB_ITEM pEncData = hb_itemNew( hb_expat->pVar[ _VAR_xEncodingHandlerData ] );
         PHB_ITEM pPar1    = hb_itemPutStrUTF8( NULL, name );
         PHB_ITEM pPar2    = hb_itemArrayNew( HB_SIZEOFARRAY( info->map ) );

         hb_evalBlock( hb_expat->pVar[ _VAR_bUnknownEncodingHandler ], pEncData, pPar1, pPar2, NULL );

         iResult = hb_parni( -1 );

         if( iResult == XML_STATUS_OK )
         {
            HB_UINT tmp;

            for( tmp = 0; tmp < HB_SIZEOFARRAY( info->map ); ++tmp )
               info->map[ tmp ] = hb_arrayGetNI( pPar2, tmp + 1 );

            /* NOTE: Not supported by wrapper layer yet. */
            info->data    = NULL;
            info->convert = NULL;
            info->release = NULL;
         }

         hb_itemRelease( pPar2 );
         hb_itemRelease( pPar1 );
         hb_itemRelease( pEncData );

         hb_vmRequestRestore();
      }
   }

   return iResult;
}

static void XMLCALL hb_expat_StartNamespaceDeclHandler( void * userdata,
                                                        const XML_Char * prefix,
                                                        const XML_Char * uri )
{
   PHB_EXPAT hb_expat = ( PHB_EXPAT ) userdata;

   if( hb_expat && hb_expat->pVar[ _VAR_bStartNamespaceDeclHandler ] )
   {
      if( hb_vmRequestReenter() )
      {
         PHB_ITEM pUserData = hb_itemNew( hb_expat->pVar[ _VAR_xUserData ] );
         PHB_ITEM pPar1     = hb_itemPutStrUTF8( NULL, prefix );
         PHB_ITEM pPar2     = hb_itemPutStrUTF8( NULL, uri );

         hb_evalBlock( hb_expat->pVar[ _VAR_bStartNamespaceDeclHandler ], pUserData, pPar1, pPar2, NULL );

         hb_itemRelease( pPar2 );
         hb_itemRelease( pPar1 );
         hb_itemRelease( pUserData );

         hb_vmRequestRestore();
      }
   }
}

static void XMLCALL hb_expat_EndNamespaceDeclHandler( void * userdata, const XML_Char * prefix )
{
   hb_expat_hnd_C( _VAR_bEndNamespaceDeclHandler, userdata, prefix );
}

static void XMLCALL hb_expat_XmlDeclHandler( void * userdata,
                                             const XML_Char * version,
                                             const XML_Char * encoding,
                                             int standalone )
{
   PHB_EXPAT hb_expat = ( PHB_EXPAT ) userdata;

   if( hb_expat && hb_expat->pVar[ _VAR_bXmlDeclHandler ] )
   {
      if( hb_vmRequestReenter() )
      {
         PHB_ITEM pUserData = hb_itemNew( hb_expat->pVar[ _VAR_xUserData ] );
         PHB_ITEM pPar1     = version ? hb_itemPutStrUTF8( NULL, version ) : hb_itemNew( NULL );
         PHB_ITEM pPar2     = encoding ? hb_itemPutStrUTF8( NULL, encoding ) : hb_itemNew( NULL );
         PHB_ITEM pPar3     = hb_itemPutNI( NULL, standalone );

         hb_evalBlock( hb_expat->pVar[ _VAR_bXmlDeclHandler ], pUserData, pPar1, pPar2, pPar3, NULL );

         hb_itemRelease( pPar3 );
         hb_itemRelease( pPar2 );
         hb_itemRelease( pPar1 );
         hb_itemRelease( pUserData );

         hb_vmRequestRestore();
      }
   }
}

static void XMLCALL hb_expat_StartDoctypeDeclHandler( void * userdata,
                                                      const XML_Char * doctypeName,
                                                      const XML_Char * sysid,
                                                      const XML_Char * pubid,
                                                      int has_internal_subset )
{
   PHB_EXPAT hb_expat = ( PHB_EXPAT ) userdata;

   if( hb_expat && hb_expat->pVar[ _VAR_bStartDoctypeDeclHandler ] )
   {
      if( hb_vmRequestReenter() )
      {
         PHB_ITEM pUserData = hb_itemNew( hb_expat->pVar[ _VAR_xUserData ] );
         PHB_ITEM pPar1     = hb_itemPutStrUTF8( NULL, doctypeName );
         PHB_ITEM pPar2     = sysid ? hb_itemPutStrUTF8( NULL, sysid ) : hb_itemNew( NULL );
         PHB_ITEM pPar3     = pubid ? hb_itemPutStrUTF8( NULL, pubid ) : hb_itemNew( NULL );
         PHB_ITEM pPar4     = hb_itemPutL( NULL, has_internal_subset );

         hb_evalBlock( hb_expat->pVar[ _VAR_bStartDoctypeDeclHandler ], pUserData, pPar1, pPar2, pPar3, pPar4, NULL );

         hb_itemRelease( pPar4 );
         hb_itemRelease( pPar3 );
         hb_itemRelease( pPar2 );
         hb_itemRelease( pPar1 );
         hb_itemRelease( pUserData );

         hb_vmRequestRestore();
      }
   }
}

static void XMLCALL hb_expat_EndDoctypeDeclHandler( void * userdata )
{
   hb_expat_hnd_void( _VAR_bEndDoctypeDeclHandler, userdata );
}

static void XMLCALL hb_expat_AttlistDeclHandler( void * userdata,
                                                 const XML_Char * elname,
                                                 const XML_Char * attname,
                                                 const XML_Char * att_type,
                                                 const XML_Char * dflt,
                                                 int isrequired )
{
   PHB_EXPAT hb_expat = ( PHB_EXPAT ) userdata;

   if( hb_expat && hb_expat->pVar[ _VAR_bAttlistDeclHandler ] )
   {
      if( hb_vmRequestReenter() )
      {
         PHB_ITEM pUserData = hb_itemNew( hb_expat->pVar[ _VAR_xUserData ] );
         PHB_ITEM pPar1     = hb_itemPutStrUTF8( NULL, elname );
         PHB_ITEM pPar2     = hb_itemPutStrUTF8( NULL, attname );
         PHB_ITEM pPar3     = hb_itemPutStrUTF8( NULL, att_type );
         PHB_ITEM pPar4     = dflt ? hb_itemPutStrUTF8( NULL, dflt ) : hb_itemNew( NULL );
         PHB_ITEM pPar5     = hb_itemPutL( NULL, isrequired );

         hb_evalBlock( hb_expat->pVar[ _VAR_bAttlistDeclHandler ], pUserData, pPar1, pPar2, pPar3, pPar4, pPar5, NULL );

         hb_itemRelease( pPar5 );
         hb_itemRelease( pPar4 );
         hb_itemRelease( pPar3 );
         hb_itemRelease( pPar2 );
         hb_itemRelease( pPar1 );
         hb_itemRelease( pUserData );

         hb_vmRequestRestore();
      }
   }
}

static void XMLCALL hb_expat_EntityDeclHandler( void * userdata,
                                                const XML_Char * entityName,
                                                int is_parameter_entity,
                                                const XML_Char * value,
                                                int value_length,
                                                const XML_Char * base,
                                                const XML_Char * systemId,
                                                const XML_Char * publicId,
                                                const XML_Char * notationName )
{
   PHB_EXPAT hb_expat = ( PHB_EXPAT ) userdata;

   if( hb_expat && hb_expat->pVar[ _VAR_bEntityDeclHandler ] )
   {
      if( hb_vmRequestReenter() )
      {
         PHB_ITEM pUserData = hb_itemNew( hb_expat->pVar[ _VAR_xUserData ] );
         PHB_ITEM pPar1     = hb_itemPutStrUTF8( NULL, entityName );
         PHB_ITEM pPar2     = hb_itemPutL( NULL, is_parameter_entity );
         PHB_ITEM pPar3     = value ? hb_itemPutStrLenUTF8( NULL, value, value_length ) : hb_itemNew( NULL );
         PHB_ITEM pPar4     = base ? hb_itemPutStrUTF8( NULL, base ) : hb_itemNew( NULL );
         PHB_ITEM pPar5     = systemId ? hb_itemPutStrUTF8( NULL, systemId ) : hb_itemNew( NULL );
         PHB_ITEM pPar6     = publicId ? hb_itemPutStrUTF8( NULL, publicId ) : hb_itemNew( NULL );
         PHB_ITEM pPar7     = notationName ? hb_itemPutStrUTF8( NULL, notationName ) : hb_itemNew( NULL );

         hb_evalBlock( hb_expat->pVar[ _VAR_bEntityDeclHandler ], pUserData, pPar1, pPar2, pPar3, pPar4, pPar5, pPar6, pPar7, NULL );

         hb_itemRelease( pPar7 );
         hb_itemRelease( pPar6 );
         hb_itemRelease( pPar5 );
         hb_itemRelease( pPar4 );
         hb_itemRelease( pPar3 );
         hb_itemRelease( pPar2 );
         hb_itemRelease( pPar1 );
         hb_itemRelease( pUserData );

         hb_vmRequestRestore();
      }
   }
}

#if 0
static void XMLCALL hb_expat_UnparsedEntityDeclHandler( void * userdata,
                                                        const XML_Char * entityName,
                                                        const XML_Char * base,
                                                        const XML_Char * systemId,
                                                        const XML_Char * publicId,
                                                        const XML_Char * notationName )
{
   PHB_EXPAT hb_expat = ( PHB_EXPAT ) userdata;

   if( hb_expat && hb_expat->pVar[ _VAR_bUnparsedEntityDeclHandler ] )
   {
      if( hb_vmRequestReenter() )
      {
         PHB_ITEM pUserData = hb_itemNew( hb_expat->pVar[ _VAR_xUserData ] );
         PHB_ITEM pPar1     = hb_itemPutStrUTF8( NULL, entityName );
         PHB_ITEM pPar2     = hb_itemPutStrUTF8( NULL, base );
         PHB_ITEM pPar3     = hb_itemPutStrUTF8( NULL, systemId );
         PHB_ITEM pPar4     = hb_itemPutStrUTF8( NULL, publicId );
         PHB_ITEM pPar5     = hb_itemPutStrUTF8( NULL, notationName );

         hb_evalBlock( hb_expat->pVar[ _VAR_bUnparsedEntityDeclHandler ], pUserData, pPar1, pPar2, pPar3, pPar4, pPar5, NULL );

         hb_itemRelease( pPar5 );
         hb_itemRelease( pPar4 );
         hb_itemRelease( pPar3 );
         hb_itemRelease( pPar2 );
         hb_itemRelease( pPar1 );
         hb_itemRelease( pUserData );

         hb_vmRequestRestore();
      }
   }
}
#endif

static void XMLCALL hb_expat_NotationDeclHandler( void * userdata,
                                                  const XML_Char * notationName,
                                                  const XML_Char * base,
                                                  const XML_Char * systemId,
                                                  const XML_Char * publicId )
{
   PHB_EXPAT hb_expat = ( PHB_EXPAT ) userdata;

   if( hb_expat && hb_expat->pVar[ _VAR_bNotationDeclHandler ] )
   {
      if( hb_vmRequestReenter() )
      {
         PHB_ITEM pUserData = hb_itemNew( hb_expat->pVar[ _VAR_xUserData ] );
         PHB_ITEM pPar1     = hb_itemPutStrUTF8( NULL, notationName );
         PHB_ITEM pPar2     = base ? hb_itemPutStrUTF8( NULL, base ) : hb_itemNew( NULL );
         PHB_ITEM pPar3     = systemId ? hb_itemPutStrUTF8( NULL, systemId ) : hb_itemNew( NULL );
         PHB_ITEM pPar4     = publicId ? hb_itemPutStrUTF8( NULL, publicId ) : hb_itemNew( NULL );

         hb_evalBlock( hb_expat->pVar[ _VAR_bNotationDeclHandler ], pUserData, pPar1, pPar2, pPar3, pPar4, NULL );

         hb_itemRelease( pPar4 );
         hb_itemRelease( pPar3 );
         hb_itemRelease( pPar2 );
         hb_itemRelease( pPar1 );
         hb_itemRelease( pUserData );

         hb_vmRequestRestore();
      }
   }
}

static int XMLCALL hb_expat_NotStandaloneHandler( void * userdata )
{
   PHB_EXPAT hb_expat = ( PHB_EXPAT ) userdata;

   int iResult = XML_STATUS_ERROR;

   if( hb_expat && hb_expat->pVar[ _VAR_bNotStandaloneHandler ] )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPushEvalSym();
         hb_vmPush( hb_expat->pVar[ _VAR_bNotStandaloneHandler ] );
         hb_vmPush( hb_expat->pVar[ _VAR_xUserData ] );
         hb_vmSend( 1 );

         iResult = hb_parni( -1 );

         hb_vmRequestRestore();
      }
   }

   return iResult;
}

/* Constructor/Destructor */
/* ---------------------- */

static void PHB_EXPAT_free( PHB_EXPAT hb_expat, HB_BOOL bFree )
{
   HB_UINT tmp;

   for( tmp = 0; tmp < HB_SIZEOFARRAY( hb_expat->pVar ); ++tmp )
   {
      if( hb_expat->pVar[ tmp ] )
      {
         hb_itemRelease( hb_expat->pVar[ tmp ] );
         hb_expat->pVar[ tmp ] = NULL;
      }
   }

   if( bFree )
   {
      XML_SetUserData( hb_expat->parser, NULL );
      XML_ParserFree( hb_expat->parser );
      hb_xfree( hb_expat );
   }
}

static HB_GARBAGE_FUNC( PHB_EXPAT_release )
{
   PHB_EXPAT * hb_expat_ptr = ( PHB_EXPAT * ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( hb_expat_ptr && *hb_expat_ptr )
   {
      PHB_EXPAT hb_expat = *hb_expat_ptr;

      /* set pointer to NULL to avoid multiple freeing */
      *hb_expat_ptr = NULL;

      /* Destroy the object */
      PHB_EXPAT_free( hb_expat, HB_TRUE );
   }
}

static HB_GARBAGE_FUNC( PHB_EXPAT_mark )
{
   PHB_EXPAT * hb_expat_ptr = ( PHB_EXPAT * ) Cargo;

   if( hb_expat_ptr && *hb_expat_ptr )
   {
      PHB_EXPAT hb_expat = *hb_expat_ptr;
      HB_UINT   tmp;

      for( tmp = 0; tmp < HB_SIZEOFARRAY( hb_expat->pVar ); ++tmp )
      {
         if( hb_expat->pVar[ tmp ] )
            hb_gcMark( hb_expat->pVar[ tmp ] );
      }
   }
}

static const HB_GC_FUNCS s_gcEXPATFuncs =
{
   PHB_EXPAT_release,
   PHB_EXPAT_mark
};

static void * PHB_EXPAT_is( int iParam )
{
   return hb_parptrGC( &s_gcEXPATFuncs, iParam );
}

static PHB_EXPAT PHB_EXPAT_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gcEXPATFuncs, iParam );

   return ph ? ( PHB_EXPAT ) *ph : NULL;
}

static void hb_expat_setvar( PHB_EXPAT hb_expat, int iHandler, PHB_ITEM pBlock )
{
   if( hb_expat->pVar[ iHandler ] )
   {
      hb_itemRelease( hb_expat->pVar[ iHandler ] );
      hb_expat->pVar[ iHandler ] = NULL;
   }

   if( pBlock )
   {
      hb_expat->pVar[ iHandler ] = hb_itemNew( pBlock );
      /* unlock the item so GC will not mark them as used */
      hb_gcUnlock( hb_expat->pVar[ iHandler ] );
   }
}

/* Harbour interface */
/* ----------------- */

HB_FUNC( XML_PARSERCREATE )
{
   void ** ph = ( void ** ) hb_gcAllocate( sizeof( PHB_EXPAT ), &s_gcEXPATFuncs );

   XML_Parser parser;
   XML_Memory_Handling_Suite ms;

   void * hEncoding;
   void * hSep;

   ms.malloc_fcn  = hb_expat_xgrab;
   ms.realloc_fcn = hb_expat_xrealloc;
   ms.free_fcn    = hb_expat_xfree;

   parser = XML_ParserCreate_MM( hb_parstr_utf8( 1, &hEncoding, NULL ),
                                 &ms,
                                 hb_parstr_utf8( 2, &hSep, NULL ) );

   hb_strfree( hSep );
   hb_strfree( hEncoding );

   if( parser )
   {
      PHB_EXPAT hb_expat = ( PHB_EXPAT ) hb_xgrab( sizeof( HB_EXPAT ) );

      memset( hb_expat, 0, sizeof( HB_EXPAT ) );
      hb_expat->parser = parser;

      XML_SetUserData( hb_expat->parser, hb_expat );

      *ph = hb_expat;
   }
   else
      *ph = NULL;

   hb_retptrGC( ph );
}

HB_FUNC( XML_PARSERRESET )
{
   if( PHB_EXPAT_is( 1 ) )
   {
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );

      if( hb_expat )
      {
         void * hEncoding;

         PHB_EXPAT_free( hb_expat, HB_FALSE );

         XML_ParserReset( hb_expat->parser,
                          hb_parstr_utf8( 1, &hEncoding, NULL ) );

         hb_strfree( hEncoding );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_PARSERFREE )
{
   if( PHB_EXPAT_is( 1 ) )
   {
      void ** ph = ( void ** ) hb_parptrGC( &s_gcEXPATFuncs, 1 );

      if( ph && *ph )
      {
         PHB_EXPAT hb_expat = ( PHB_EXPAT ) *ph;

         /* set pointer to NULL to avoid multiple freeing */
         *ph = NULL;

         /* Destroy the object */
         PHB_EXPAT_free( hb_expat, HB_TRUE );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_SETUSERDATA )
{
   if( PHB_EXPAT_is( 1 ) )
   {
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );

      hb_expat_setvar( hb_expat, _VAR_xUserData, hb_param( 2, HB_IT_ANY ) );

      hb_ret();
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_GETUSERDATA )
{
   if( PHB_EXPAT_is( 1 ) )
   {
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );

      hb_itemReturnRelease( hb_itemNew( hb_expat->pVar[ _VAR_xUserData ] ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_SETELEMENTHANDLER )
{
   if( PHB_EXPAT_is( 1 ) )
   {
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );

      hb_expat_setvar( hb_expat, _VAR_bStartElementHandler, hb_param( 2, HB_IT_BLOCK | HB_IT_SYMBOL ) );
      hb_expat_setvar( hb_expat, _VAR_bEndElementHandler, hb_param( 3, HB_IT_BLOCK | HB_IT_SYMBOL ) );

      XML_SetElementHandler( hb_expat->parser,
                             hb_expat->pVar[ _VAR_bStartElementHandler ] ? hb_expat_StartElementHandler : NULL,
                             hb_expat->pVar[ _VAR_bEndElementHandler ] ? hb_expat_EndElementHandler : NULL );

      hb_ret();
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_SETCDATASECTIONHANDLER )
{
   if( PHB_EXPAT_is( 1 ) )
   {
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );

      hb_expat_setvar( hb_expat, _VAR_bStartCdataSectionHandler, hb_param( 2, HB_IT_BLOCK | HB_IT_SYMBOL ) );
      hb_expat_setvar( hb_expat, _VAR_bEndCdataSectionHandler, hb_param( 3, HB_IT_BLOCK | HB_IT_SYMBOL ) );

      XML_SetCdataSectionHandler( hb_expat->parser,
                                  hb_expat->pVar[ _VAR_bStartCdataSectionHandler ] ? hb_expat_StartCdataSectionHandler : NULL,
                                  hb_expat->pVar[ _VAR_bEndCdataSectionHandler ] ? hb_expat_EndCdataSectionHandler : NULL );

      hb_ret();
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_SETNAMESPACEDECLHANDLER )
{
   if( PHB_EXPAT_is( 1 ) )
   {
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );

      hb_expat_setvar( hb_expat, _VAR_bStartNamespaceDeclHandler, hb_param( 2, HB_IT_BLOCK | HB_IT_SYMBOL ) );
      hb_expat_setvar( hb_expat, _VAR_bEndNamespaceDeclHandler, hb_param( 3, HB_IT_BLOCK | HB_IT_SYMBOL ) );

      XML_SetNamespaceDeclHandler( hb_expat->parser,
                                   hb_expat->pVar[ _VAR_bStartNamespaceDeclHandler ] ? hb_expat_StartNamespaceDeclHandler : NULL,
                                   hb_expat->pVar[ _VAR_bEndNamespaceDeclHandler ] ? hb_expat_EndNamespaceDeclHandler : NULL );

      hb_ret();
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_SETUNKNOWNENCODINGHANDLER )
{
   if( PHB_EXPAT_is( 1 ) )
   {
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );

      hb_expat_setvar( hb_expat, _VAR_bUnknownEncodingHandler, hb_param( 2, HB_IT_BLOCK | HB_IT_SYMBOL ) );
      hb_expat_setvar( hb_expat, _VAR_xEncodingHandlerData, hb_param( 3, HB_IT_ANY ) );

      XML_SetUnknownEncodingHandler( hb_expat->parser,
                                     hb_expat->pVar[ _VAR_bUnknownEncodingHandler ] ? hb_expat_UnknownEncodingHandler : NULL,
                                     hb_expat );

      hb_ret();
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* ; */

HB_FUNC( XML_PARSE )
{
   if( PHB_EXPAT_is( 1 ) )
   {
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );

      hb_retni( XML_Parse( hb_expat->parser, hb_parcx( 2 ), ( int ) hb_parclen( 2 ), ( int ) hb_parl( 3 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_GETERRORCODE )
{
   if( PHB_EXPAT_is( 1 ) )
   {
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );

      hb_retni( ( int ) XML_GetErrorCode( hb_expat->parser ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_ERRORSTRING )
{
   hb_retc( XML_ErrorString( ( enum XML_Error ) hb_parni( 1 ) ) );
}

HB_FUNC( XML_GETCURRENTBYTEINDEX )
{
   if( PHB_EXPAT_is( 1 ) )
   {
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );

      hb_retns( XML_GetCurrentByteIndex( hb_expat->parser ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_GETCURRENTLINENUMBER )
{
   if( PHB_EXPAT_is( 1 ) )
   {
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );

      hb_retns( XML_GetCurrentLineNumber( hb_expat->parser ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_GETCURRENTCOLUMNNUMBER )
{
   if( PHB_EXPAT_is( 1 ) )
   {
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );

      hb_retns( XML_GetCurrentColumnNumber( hb_expat->parser ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_GETCURRENTBYTECOUNT )
{
   if( PHB_EXPAT_is( 1 ) )
   {
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );

      hb_retni( XML_GetCurrentByteCount( hb_expat->parser ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_SETBASE )
{
   if( PHB_EXPAT_is( 1 ) )
   {
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );
      void *    hBase;

      hb_retni( ( int ) XML_SetBase( hb_expat->parser, hb_parstr_utf8( 1, &hBase, NULL ) ) );

      hb_strfree( hBase );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_GETBASE )
{
   if( PHB_EXPAT_is( 1 ) )
   {
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );

      hb_retstr_utf8( XML_GetBase( hb_expat->parser ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_GETSPECIFIEDATTRIBUTECOUNT )
{
   if( PHB_EXPAT_is( 1 ) )
   {
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );

      hb_retni( XML_GetSpecifiedAttributeCount( hb_expat->parser ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_GETIDATTRIBUTEINDEX )
{
   if( PHB_EXPAT_is( 1 ) )
   {
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );

      hb_retni( XML_GetIdAttributeIndex( hb_expat->parser ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_SETENCODING )
{
   if( PHB_EXPAT_is( 1 ) )
   {
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );
      void *    hEncoding;

      hb_retni( ( int ) XML_SetEncoding( hb_expat->parser,
                                         hb_parstr_utf8( 1, &hEncoding, NULL ) ) );

      hb_strfree( hEncoding );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_SETPARAMENTITYPARSING )
{
   if( PHB_EXPAT_is( 1 ) )
   {
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );

      hb_retni( XML_SetParamEntityParsing( hb_expat->parser, ( enum XML_ParamEntityParsing ) hb_parni( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_USEFOREIGNDTD )
{
   if( PHB_EXPAT_is( 1 ) )
   {
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );

      hb_retni( ( int ) XML_UseForeignDTD( hb_expat->parser, ( XML_Bool ) hb_parl( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_SETRETURNNSTRIPLET )
{
   if( PHB_EXPAT_is( 1 ) )
   {
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );

      XML_SetReturnNSTriplet( hb_expat->parser, hb_parni( 2 ) );

      hb_ret();
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_DEFAULTCURRENT )
{
   if( PHB_EXPAT_is( 1 ) )
   {
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );

      XML_DefaultCurrent( hb_expat->parser );

      hb_ret();
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_STOPPARSER )
{
   if( PHB_EXPAT_is( 1 ) )
   {
#if HB_EXPAT_VERS( 1, 95, 8 )
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );

      hb_retni( ( int ) XML_StopParser( hb_expat->parser, ( XML_Bool ) hb_parl( 2 ) ) );
#else
      hb_retni( HB_XML_ERROR_NOT_IMPLEMENTED_ );
#endif
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_RESUMEPARSER )
{
   if( PHB_EXPAT_is( 1 ) )
   {
#if HB_EXPAT_VERS( 1, 95, 8 )
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );

      hb_retni( ( int ) XML_ResumeParser( hb_expat->parser ) );
#else
      hb_retni( HB_XML_ERROR_NOT_IMPLEMENTED_ );
#endif
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_GETPARSINGSTATUS )
{
   if( PHB_EXPAT_is( 1 ) )
   {
#if HB_EXPAT_VERS( 1, 95, 8 )
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );
      XML_ParsingStatus status;

      XML_GetParsingStatus( hb_expat->parser, &status );

      hb_storni( ( int ) status.parsing, 2 );
      hb_storl( ( HB_BOOL ) status.finalBuffer, 3 );
#else
      hb_storni( -1, 2 );
      hb_storl( HB_FALSE, 3 );
#endif
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_SETHASHSALT )
{
   if( PHB_EXPAT_is( 1 ) )
   {
#if HB_EXPAT_VERS( 2, 1, 0 )
      PHB_EXPAT hb_expat = PHB_EXPAT_par( 1 );

      hb_retni( XML_SetHashSalt( hb_expat->parser, ( unsigned long ) hb_parnint( 2 ) ) );
#else
      hb_retni( 0 );
#endif
   }
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XML_EXPATVERSION )
{
   hb_retc( XML_ExpatVersion() );
}

HB_FUNC( XML_EXPATVERSIONINFO )
{
   XML_Expat_Version var = XML_ExpatVersionInfo();

   hb_storni( var.major, 1 );
   hb_storni( var.minor, 2 );
   hb_storni( var.micro, 3 );
}

HB_FUNC( HB_XML_EXPATVERSIONINFO )
{
   hb_storni( XML_MAJOR_VERSION, 1 );
   hb_storni( XML_MINOR_VERSION, 2 );
   hb_storni( XML_MICRO_VERSION, 3 );
}

HB_EXPAT_SETHANDLER( STARTELEMENTHANDLER         , StartElementHandler          )
HB_EXPAT_SETHANDLER( ENDELEMENTHANDLER           , EndElementHandler            )
HB_EXPAT_SETHANDLER( CHARACTERDATAHANDLER        , CharacterDataHandler         )
HB_EXPAT_SETHANDLER( PROCESSINGINSTRUCTIONHANDLER, ProcessingInstructionHandler )
HB_EXPAT_SETHANDLER( COMMENTHANDLER              , CommentHandler               )
HB_EXPAT_SETHANDLER( STARTCDATASECTIONHANDLER    , StartCdataSectionHandler     )
HB_EXPAT_SETHANDLER( ENDCDATASECTIONHANDLER      , EndCdataSectionHandler       )
HB_EXPAT_SETHANDLER( DEFAULTHANDLER              , DefaultHandler               )
HB_EXPAT_SETHANDLER( DEFAULTHANDLEREXPAND        , DefaultHandlerExpand         )
HB_EXPAT_SETHANDLER( SKIPPEDENTITYHANDLER        , SkippedEntityHandler         )
HB_EXPAT_SETHANDLER( STARTNAMESPACEDECLHANDLER   , StartNamespaceDeclHandler    )
HB_EXPAT_SETHANDLER( ENDNAMESPACEDECLHANDLER     , EndNamespaceDeclHandler      )
HB_EXPAT_SETHANDLER( XMLDECLHANDLER              , XmlDeclHandler               )
HB_EXPAT_SETHANDLER( STARTDOCTYPEDECLHANDLER     , StartDoctypeDeclHandler      )
HB_EXPAT_SETHANDLER( ENDDOCTYPEDECLHANDLER       , EndDoctypeDeclHandler        )
HB_EXPAT_SETHANDLER( ATTLISTDECLHANDLER          , AttlistDeclHandler           )
HB_EXPAT_SETHANDLER( ENTITYDECLHANDLER           , EntityDeclHandler            )
#if 0 /* Obsolete */
HB_EXPAT_SETHANDLER( UNPARSEDENTITYDECLHANDLER   , UnparsedEntityDeclHandler    )
#endif
HB_EXPAT_SETHANDLER( NOTATIONDECLHANDLER         , NotationDeclHandler          )
HB_EXPAT_SETHANDLER( NOTSTANDALONEHANDLER        , NotStandaloneHandler         )
