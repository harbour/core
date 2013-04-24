/*
 * xHarbour Project source code:
 * HBXML - XML DOM oriented routines
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 *    See also MXML library related copyright below
 *
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
 * MXML (Mini XML) Library related copyright notice.
 * (referring to Harbour/xHarbour version).
 *
 * This source file contains a modified version of MXML (Mini XML)
 * library, developed by Giancarlo Niccolai. MXML is released under
 * LGPL license; this modified version (called HBXML) is released under
 * GPL with HARBOUR exception. HBXML license does not extends into
 * MXML; HBXML and any modification to HBXML is to be considered as
 * a part of Harbour or xHarbour projects, as it is modified to
 * be specifically working in the context of the compiler's RTL.
 *
 * Original MXML lib can be obtained requesting it at
 * Giancarlo Niccolai <giancarlo@niccolai.org>
 */

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbapicls.h"
#include "hbapifs.h"

#include "hbvm.h"

#include "hbxml.h"

/*******************************************
   Static declarations
 ********************************************/

/* Node oriented operations */
static PHB_ITEM mxml_node_new( PHB_ITEM pDoc );
static PHB_ITEM mxml_node_clone( PHB_ITEM tg );
static PHB_ITEM mxml_node_clone_tree( PHB_ITEM tg );
static void mxml_node_unlink( PHB_ITEM tag );

static void mxml_node_insert_before( PHB_ITEM tg, PHB_ITEM node );
static void mxml_node_insert_after( PHB_ITEM tg, PHB_ITEM node );
static void mxml_node_insert_below( PHB_ITEM tg, PHB_ITEM node );
static void mxml_node_add_below( PHB_ITEM tg, PHB_ITEM node );

static MXML_STATUS mxml_node_read( MXML_REFIL * data, PHB_ITEM node, PHB_ITEM doc, int style );
static MXML_STATUS mxml_node_write( MXML_OUTPUT * out, PHB_ITEM pNode, int style );

/* Attribute oriented operations */
static MXML_STATUS mxml_attribute_read( MXML_REFIL * data, PHB_ITEM doc, PHB_ITEM pNode, PHBXML_ATTRIBUTE dest, int style );
static MXML_STATUS mxml_attribute_write( MXML_OUTPUT * out, PHBXML_ATTRIBUTE attr, int style );

/* Refil routines */
/* Currently not used */
#if 0
static MXML_REFIL * mxml_refil_new( MXML_REFIL_FUNC func, char * buf, HB_ISIZ buflen, HB_ISIZ bufsize );
static void mxml_refil_destory( MXML_REFIL * ref );
#endif
static MXML_STATUS mxml_refil_setup( MXML_REFIL * ref, MXML_REFIL_FUNC func,
                                     char * buf, HB_ISIZ buflen, HB_ISIZ bufsize );

static int mxml_refil_getc( MXML_REFIL * ref );
#define mxml_refil_ungetc( ref, ch )  ref->sparechar = ch

/* Currently not used */
#if 0
static void mxml_refill_from_stream_func( MXML_REFIL * ref );
#endif

static void mxml_refill_from_handle_func( MXML_REFIL * ref );

/* Output routines */
/* Currently not used */
#if 0
static MXML_OUTPUT * mxml_output_new( MXML_OUTPUT_FUNC func, int node_count );
static void mxml_output_destroy( MXML_OUTPUT * out );
static MXML_STATUS mxml_output_string( MXML_OUTPUT * out, const char * s );
#endif

static MXML_STATUS mxml_output_setup( MXML_OUTPUT * out, MXML_OUTPUT_FUNC func, int node_count );
static MXML_STATUS mxml_output_char( MXML_OUTPUT * out, int c );
static MXML_STATUS mxml_output_string_len( MXML_OUTPUT * out, const char * s, HB_ISIZ len );
static MXML_STATUS mxml_output_string_escape( MXML_OUTPUT * out, const char * s );
static MXML_STATUS mxml_output_string( MXML_OUTPUT * out, const char * s );

static void mxml_output_func_to_handle( MXML_OUTPUT * out, const char * s, HB_ISIZ len );
static void mxml_output_func_to_sgs( MXML_OUTPUT * out, const char * s, HB_ISIZ len );

/* Self growing string routines */
static MXML_SGS * mxml_sgs_new( void );
static void mxml_sgs_destroy( MXML_SGS * sgs );
static char * mxml_sgs_extract( MXML_SGS * sgs );
static MXML_STATUS mxml_sgs_append_char( MXML_SGS * sgs, char c );
static MXML_STATUS mxml_sgs_append_string_len( MXML_SGS * sgs, const char * s, HB_ISIZ slen );
/* Currently not used */
#if 0
static MXML_STATUS mxml_sgs_append_string( MXML_SGS * sgs, char * s );
#endif

/* Error description */
static const char * mxml_error_desc( MXML_ERROR_CODE code );


/********************************************
   HB-MXML glue code
 *********************************************/

/* This is just a shortcut */
static void hbxml_set_doc_status( MXML_REFIL * ref, PHB_ITEM doc, PHB_ITEM pNode, int status, int error )
{
   PHB_ITEM pNumber = hb_itemPutNI( NULL, 1 );

   hb_objSendMsg( doc, "_NSTATUS", 1, pNumber );
   hb_itemPutNI( pNumber, error );
   hb_objSendMsg( doc, "_NERROR", 1, pNumber );
   hb_objSendMsg( doc, "_OERRORNODE", 1, pNode );
   hb_itemRelease( pNumber );

   /* ref->status is often used as an error marker even if the error wasn't from i/o */
   ref->status = ( MXML_STATUS ) status;
   ref->error  = ( MXML_ERROR_CODE ) error;
}

static void hbxml_doc_new_line( PHB_ITEM pDoc )
{
   PHB_ITEM pNumber;

   hb_objSendMsg( pDoc, "NLINE", 0 );
   pNumber = hb_itemPutNI( NULL, hb_parni( -1 ) + 1 );
   hb_objSendMsg( pDoc, "_NLINE", 1, pNumber );
   hb_itemRelease( pNumber );
}

static void hbxml_doc_new_node( PHB_ITEM pDoc, int amount )
{
   PHB_ITEM pNumber;

   hb_objSendMsg( pDoc, "NNODECOUNT", 0 );
   pNumber = hb_itemPutNI( NULL, hb_parni( -1 ) + amount );
   hb_objSendMsg( pDoc, "_NNODECOUNT", 1, pNumber );
   hb_itemRelease( pNumber );

   /* TODO: launch a callback */
}

/***********************************************************
   HBXML lib
   Attribute oriented routines
 ************************************************************/

static MXML_STATUS mxml_attribute_read( MXML_REFIL * ref, PHB_ITEM pDoc, PHB_ITEM pNode, PHBXML_ATTRIBUTE pDest, int style )
{
   int        chr, quotechr = '"';
   MXML_SGS * buf_name;
   MXML_SGS * buf_attrib;
   int        iStatus   = 0;
   HB_ISIZ    iPosAmper = 0;
   HB_ISIZ    iLenName, iLenAttrib;

   buf_name   = mxml_sgs_new();
   buf_attrib = mxml_sgs_new();
   HB_SYMBOL_UNUSED( style );

   while( iStatus < 6 )
   {
      chr = mxml_refil_getc( ref );
      if( chr == MXML_EOF )
         break;

      switch( iStatus )
      {
         /* begin */
         case 0:

            switch( chr )
            {
               case MXML_LINE_TERMINATOR:
                  hbxml_doc_new_line( pDoc );
                  break;
               /* We repeat line terminator here for portability */
               case MXML_SOFT_LINE_TERMINATOR:
                  break;
               case ' ':
               case '\t':
                  break;
               /* no attributes found */
               case '>':
               case '/':
                  mxml_sgs_destroy( buf_name );
                  mxml_sgs_destroy( buf_attrib );
                  return MXML_STATUS_DONE;

               default:
                  if( HB_ISALPHA( chr ) )
                  {
                     if( mxml_sgs_append_char( buf_name, ( char ) chr ) != MXML_STATUS_OK )
                     {
                        hbxml_set_doc_status( ref, pDoc, pNode,
                                              MXML_STATUS_MALFORMED, MXML_ERROR_NAMETOOLONG );
                        return MXML_STATUS_MALFORMED;
                     }
                     iStatus = 1;
                  }
                  else
                  {
                     hbxml_set_doc_status( ref, pDoc, pNode, MXML_STATUS_MALFORMED, MXML_ERROR_INVATT );
                     return MXML_STATUS_MALFORMED;
                  }
            }
            break;

         /* scanning for a name */
         case 1:
            if( HB_ISALNUM( chr ) || chr == '_' || chr == '-' || chr == ':' )
            {
               if( mxml_sgs_append_char( buf_name, ( char ) chr ) != MXML_STATUS_OK )
               {
                  hbxml_set_doc_status( ref, pDoc, pNode,
                                        MXML_STATUS_MALFORMED, MXML_ERROR_NAMETOOLONG );
                  return MXML_STATUS_MALFORMED;
               }
            }
            else if( chr == MXML_LINE_TERMINATOR )
            {
               hbxml_doc_new_line( pDoc );
               iStatus = 2; /* waiting for a '=' */
            }
            /* We repeat line terminator here for portability */
            else if( chr == ' ' || chr == '\t' || chr == '\n' || chr == '\r' )
               iStatus = 2;
            else if( chr == '=' )
               iStatus = 3;
            else
            {
               hbxml_set_doc_status( ref, pDoc, pNode, MXML_STATUS_MALFORMED, MXML_ERROR_MALFATT );
               return MXML_STATUS_MALFORMED;
            }
            break;

         /* waiting for '=' */
         case 2:
            if( chr == '=' )
               iStatus = 3;
            else if( chr == MXML_LINE_TERMINATOR )
               hbxml_doc_new_line( pDoc );
            /* We repeat line terminator here for portability */
            else if( chr == ' ' || chr == '\t' || chr == '\n' || chr == '\r' )
            {
            }
            else
            {
               hbxml_set_doc_status( ref, pDoc, pNode, MXML_STATUS_MALFORMED, MXML_ERROR_MALFATT );
               return MXML_STATUS_MALFORMED;
            }
            break;

         /* waiting for ' or " */
         case 3:
            if( chr == '\'' || chr == '"' )
            {
               iStatus  = 4;
               quotechr = chr;
            }
            else if( chr == MXML_LINE_TERMINATOR )
            {
               hbxml_doc_new_line( pDoc );
            }
            /* We repeat line terminator here for portability */
            else if( chr == ' ' || chr == '\t' || chr == '\n' || chr == '\r' )
            {
            }
            else
            {
               hbxml_set_doc_status( ref, pDoc, pNode, MXML_STATUS_MALFORMED, MXML_ERROR_MALFATT );
               return MXML_STATUS_MALFORMED;
            }
            break;

         /* scanning the attribute content ( until next quotechr ) */
         case 4:
            if( chr == quotechr )
               iStatus = 6;
            else if( chr == '&' && ! ( style & MXML_STYLE_NOESCAPE ) )
            {
               iStatus   = 5;
               iPosAmper = buf_attrib->length;
               mxml_sgs_append_char( buf_attrib, ( char ) chr ); /* we'll need it */
            }
            else if( chr == MXML_LINE_TERMINATOR )
            {
               hbxml_doc_new_line( pDoc );
               if( mxml_sgs_append_char( buf_attrib, ( char ) chr ) != MXML_STATUS_OK )
               {
                  hbxml_set_doc_status( ref, pDoc, pNode,
                                        MXML_STATUS_MALFORMED, MXML_ERROR_ATTRIBTOOLONG );
                  return MXML_STATUS_MALFORMED;
               }
            }
            /* We repeat line terminator here for portability */
            else
            {
               if( mxml_sgs_append_char( buf_attrib, ( char ) chr ) != MXML_STATUS_OK )
               {
                  hbxml_set_doc_status( ref, pDoc, pNode, MXML_STATUS_MALFORMED, MXML_ERROR_ATTRIBTOOLONG );
                  return MXML_STATUS_MALFORMED;
               }
            }
            break;

         case 5:
            if( chr == ';' )
            {
               HB_ISIZ iAmpLen = buf_attrib->length - iPosAmper - 2;
               /* Taking the address of a sgs buffer is legal */
               char * bp = buf_attrib->buffer + iPosAmper + 1;

               if( iAmpLen <= 0 )
               {
                  /* error! - we have "&;" */
                  hbxml_set_doc_status( ref, pDoc, pNode, MXML_STATUS_MALFORMED, MXML_ERROR_WRONGENTITY );
                  return MXML_STATUS_MALFORMED;
               }

               iStatus = 4;

               /* we see if we have a predef entity (also known as escape) */
               if( strncmp( bp, "amp", iAmpLen ) == 0 )
                  chr = '&';
               else if( strncmp( bp, "lt", iAmpLen ) == 0 )
                  chr = '<';
               else if( strncmp( bp, "gt", iAmpLen ) == 0 )
                  chr = '>';
               else if( strncmp( bp, "quot", iAmpLen ) == 0 )
                  chr = '"';
               else if( strncmp( bp, "apos", iAmpLen ) == 0 )
                  chr = '\'';
               else if( *bp == '#' )
               {
                  if( *( ++bp ) == 'x' ) /* Hexadecimal */
                     chr = ( ( *( bp + 1 ) - '0' ) << 4 ) + ( *( bp + 2 ) - '0' );
                  else                   /* Decimal */
                     chr = atoi( bp );
               }
               /** Reducing an SGS length is legal */
               buf_attrib->length = iPosAmper;
               mxml_sgs_append_char( buf_attrib, ( char ) chr );
            }
            else if( ! ( HB_ISALPHA( chr ) || HB_ISDIGIT( chr ) || chr == '#' ) )
            {
               /* error - we have something like &amp &amp */
               hbxml_set_doc_status( ref, pDoc, pNode, MXML_STATUS_MALFORMED, MXML_ERROR_WRONGENTITY );
               return MXML_STATUS_MALFORMED;
            }
            else
            {
               if( mxml_sgs_append_char( buf_attrib, ( char ) chr ) != MXML_STATUS_OK )
               {
                  hbxml_set_doc_status( ref, pDoc, pNode, MXML_STATUS_MALFORMED, MXML_ERROR_ATTRIBTOOLONG );
                  return MXML_STATUS_MALFORMED;
               }
            }
            break;
      }
   }

   if( ref->status != MXML_STATUS_OK )
      return ref->status;

   if( iStatus < 6 )
   {
      hbxml_set_doc_status( ref, pDoc, pNode, MXML_STATUS_MALFORMED, MXML_ERROR_MALFATT );
      mxml_sgs_destroy( buf_name );
      mxml_sgs_destroy( buf_attrib );
      return MXML_STATUS_MALFORMED;
   }

   /* time to create the attribute */
   iLenName   = buf_name->length;
   iLenAttrib = buf_attrib->length;

   pDest->pName  = hb_itemPutCL( pDest->pName, mxml_sgs_extract( buf_name ), iLenName );
   pDest->pValue = hb_itemPutCL( pDest->pValue, mxml_sgs_extract( buf_attrib ), iLenAttrib );

   mxml_sgs_destroy( buf_name );
   mxml_sgs_destroy( buf_attrib );

   return MXML_STATUS_OK;
}

static MXML_STATUS mxml_attribute_write( MXML_OUTPUT * out, PHBXML_ATTRIBUTE pAttr, int style )
{
   const char * name = hb_itemGetCPtr( pAttr->pName );

   mxml_output_string_len( out, name, hb_itemGetCLen( pAttr->pName ) );
   mxml_output_char( out, '=' );
   mxml_output_char( out, '"' );

   if( ! ( style & MXML_STYLE_NOESCAPE ) )
      mxml_output_string_escape( out, hb_itemGetCPtr( pAttr->pValue ) );
   else
      mxml_output_string_len( out, hb_itemGetCPtr( pAttr->pValue ),
                              hb_itemGetCLen( pAttr->pValue ) );

   mxml_output_char( out, '"' );

   return out->status;
}



/***********************************************************
   HBXML lib
   Item (node) routines
***********************************************************/


static PHB_ITEM mxml_node_new( PHB_ITEM pDoc )
{
   PHB_ITEM pNode;
   PHB_DYNS pExecSym;

   pExecSym = hb_dynsymGetCase( "TXMLNODE" );
   hb_vmPushDynSym( pExecSym );
   hb_vmPushNil();
   hb_vmDo( 0 );

   /* The node is in the return */
   hb_objSendMsg( hb_param( -1, HB_IT_ANY ), "NEW", 0 );
   pNode = hb_itemNew( hb_param( -1, HB_IT_ANY ) );

   /* Sets also current node line begin value, if the node is from a document */
   if( pDoc != NULL )
   {
      hb_objSendMsg( pDoc, "NLINE", 0 );
      hb_objSendMsg( pNode, "_NBEGINLINE", 1, hb_param( -1, HB_IT_ANY ) );
   }
   /* Else, leaves nBeginLine at 0 */

   return pNode;
}

/**
 * The unlink function is used to detach a node from the UPPER and PARENT hyerarcy.
 * The node is "removed" so that siblings node are "squished", and possible parents
 * are informed of the changes, so that they are able to get a new child to start
 * the tree structure under them. The childs of the unlinked nodes are NOT unlinked,
 * thus remains attached to the node: is like removing a branch with all its leaves.
 *
 */

static void mxml_node_unlink( PHB_ITEM pNode )
{
   PHB_ITEM pPrev, pNext, pParent, pNil;

   pNil = hb_itemNew( NULL );

   hb_objSendMsg( pNode, "OPREV", 0 );
   pPrev = hb_itemNew( hb_param( -1, HB_IT_ANY ) );

   hb_objSendMsg( pNode, "ONEXT", 0 );
   pNext = hb_itemNew( hb_param( -1, HB_IT_ANY ) );

   hb_objSendMsg( pNode, "OPARENT", 0 );
   pParent = hb_itemNew( hb_param( -1, HB_IT_ANY ) );

   /* Detaching from previous */
   if( ! HB_IS_NIL( pPrev ) )
   {
      hb_objSendMsg( pPrev, "_ONEXT", 1, pNext );
      hb_objSendMsg( pNode, "_ONEXT", 1, pNil );
   }

   /* Detaching from Next */
   if( ! HB_IS_NIL( pNext ) )
   {
      hb_objSendMsg( pNext, "_OPREV", 1, pPrev );
      hb_objSendMsg( pNode, "_OPREV", 1, pNil );
   }

   /* Detaching from parent */
   if( ! HB_IS_NIL( pParent ) )
   {
      /* Eventually set the next node as first child */
      if( HB_IS_NIL( pPrev ) ) /* was I the first node? */
         hb_objSendMsg( pParent, "_OCHILD", 1, pNext );
      hb_objSendMsg( pNode, "_OPARENT", 1, pNil );
   }

   hb_itemRelease( pPrev );
   hb_itemRelease( pNext );
   hb_itemRelease( pParent );
   hb_itemRelease( pNil );
}

HB_FUNC( HBXML_NODE_UNLINK )
{
   mxml_node_unlink( hb_param( 1, HB_IT_OBJECT ) );
}
/****************************************************************/

static void mxml_node_insert_before( PHB_ITEM pTg, PHB_ITEM pNode )
{
   PHB_ITEM pParent;

   /* Move tg->prev into node->prev */
   hb_objSendMsg( pTg, "OPREV", 0 );
   hb_objSendMsg( pNode, "_OPREV", 1, hb_param( -1, HB_IT_ANY ) );

   /* tg->prev is now pnode! */
   hb_objSendMsg( pTg, "_OPREV", 1, pNode );

   /* pNode->next is TG */
   hb_objSendMsg( pNode, "_ONEXT", 1, pTg );

   /* pNode->parent is the same as tg */
   hb_objSendMsg( pTg, "OPARENT", 0 );
   pParent = hb_itemNew( hb_param( -1, HB_IT_ANY ) );
   hb_objSendMsg( pNode, "_OPARENT", 1, pParent );

   /* if the parent is not null, and if it's child was tg, we must update to node */
   if( ! HB_IS_NIL( pParent ) )
   {
      hb_objSendMsg( pParent, "OCHILD", 0 );
      if( hb_arrayId( hb_param( -1, HB_IT_ANY ) ) == hb_arrayId( pTg ) )
      {
         hb_objSendMsg( pParent, "_OCHILD", 1, pNode );
      }
   }

   hb_itemRelease( pParent );
}

HB_FUNC( HBXML_NODE_INSERT_BEFORE )
{
   mxml_node_insert_before( hb_param( 1, HB_IT_OBJECT ), hb_param( 2, HB_IT_OBJECT ) );
}

static void mxml_node_insert_after( PHB_ITEM pTg, PHB_ITEM pNode )
{
   /* Move tg->next into node->next */
   hb_objSendMsg( pTg, "ONEXT", 0 );
   hb_objSendMsg( pNode, "_ONEXT", 1, hb_param( -1, HB_IT_ANY ) );

   /* tg->NEXT is now pnode! */
   hb_objSendMsg( pTg, "_ONEXT", 1, pNode );

   /* pNode->prev is TG */
   hb_objSendMsg( pNode, "_OPREV", 1, pTg );

   /* pNode->parent is the same as tg */
   hb_objSendMsg( pTg, "OPARENT", 0 );
   hb_objSendMsg( pNode, "_OPARENT", 1, hb_param( -1, HB_IT_ANY ) );
}

HB_FUNC( HBXML_NODE_INSERT_AFTER )
{
   mxml_node_insert_after( hb_param( 1, HB_IT_OBJECT ), hb_param( 2, HB_IT_OBJECT ) );
}

/**
 * Creates a new tree level, so that the given node is added between
 * tg and its former children. Former children of pNode are discarded
 */

static void mxml_node_insert_below( PHB_ITEM pTg, PHB_ITEM pNode )
{
   PHB_ITEM pChild;

   /* Move tg->child into node->child */
   hb_objSendMsg( pTg, "OCHILD", 0 );
   pChild = hb_itemNew( hb_param( -1, HB_IT_ANY ) );
   hb_objSendMsg( pNode, "_OCHILD", 1, pChild );

   /* Parent of pNode is now TG */
   hb_objSendMsg( pNode, "_OPARENT", 1, pTg );

   /* All children parents are moved to pNode */
   while( ! HB_IS_NIL( pChild ) )
   {
      hb_objSendMsg( pChild, "_OPARENT", 1, pNode );
      hb_objSendMsg( pChild, "ONEXT", 0 );
      hb_itemMove( pChild, hb_param( -1, HB_IT_ANY ) );
   }
   hb_itemRelease( pChild );
}


HB_FUNC( HBXML_NODE_INSERT_BELOW )
{
   mxml_node_insert_below( hb_param( 1, HB_IT_OBJECT ), hb_param( 2, HB_IT_OBJECT ) );
}
/**
 * Adds a node to the bottom of the children list of tg.
 */

static void mxml_node_add_below( PHB_ITEM pTg, PHB_ITEM pNode )
{
   PHB_ITEM pChild;

   /* Parent of pNode is now TG */
   hb_objSendMsg( pNode, "_OPARENT", 1, pTg );

   /* Get the TG child */
   hb_objSendMsg( pTg, "OCHILD", 0 );
   pChild = hb_itemNew( hb_param( -1, HB_IT_ANY ) );

   if( ! HB_IS_NIL( pChild ) )
   {
      /* Scanning up to the last child */
      for(;; )
      {
         hb_objSendMsg( pChild, "ONEXT", 0 );
         if( HB_IS_NIL( hb_param( -1, HB_IT_ANY ) ) )
            break;
         hb_itemMove( pChild, hb_param( -1, HB_IT_ANY ) );
      }
      /* linking the child with pnode */
      hb_objSendMsg( pChild, "_ONEXT", 1, pNode );
      hb_objSendMsg( pNode, "_OPREV", 1, pChild );
   }
   else
   {
      /* if we are the first child, we notify pTg of this */
      hb_objSendMsg( pTg, "_OCHILD", 1, pNode );
   }
   hb_itemRelease( pChild );
}

HB_FUNC( HBXML_NODE_ADD_BELOW )
{
   mxml_node_add_below( hb_param( 1, HB_IT_OBJECT ), hb_param( 2, HB_IT_OBJECT ) );
}
/**
 * Clones a node, but it does not sets the parent, nor the siblings;
 * this clone is "floating" out of the tree hierarcy.
 */

static PHB_ITEM mxml_node_clone( PHB_ITEM pTg )
{
   /* Node is not from a real document, so is right to leave nBeginLine at 0 */
   PHB_ITEM pNode = mxml_node_new( NULL );
   PHB_ITEM pArrayClone;

   /* sets clone type */
   hb_objSendMsg( pTg, "NTYPE", 0 );
   hb_objSendMsg( pNode, "_NTYPE", 1, hb_param( -1, HB_IT_ANY ) );

   /* sets clone name */
   hb_objSendMsg( pTg, "CNAME", 0 );
   hb_objSendMsg( pNode, "_CNAME", 1, hb_param( -1, HB_IT_ANY ) );

   /* sets clone data */
   hb_objSendMsg( pTg, "CDATA", 0 );
   hb_objSendMsg( pNode, "_CDATA", 1, hb_param( -1, HB_IT_ANY ) );

   /* clone attributes */
   hb_objSendMsg( pTg, "AATTRIBUTES", 0 );
   pArrayClone = hb_arrayClone( hb_param( -1, HB_IT_ANY ) );
   hb_objSendMsg( pNode, "_AATTRIBUTES", 1, pArrayClone );
   hb_itemRelease( pArrayClone );

   return pNode;
}

HB_FUNC( HBXML_NODE_CLONE )
{
   hb_itemReturnRelease( mxml_node_clone( hb_param( 1, HB_IT_OBJECT ) ) );
}

/**
 * Clones a node and all its subtree, but it does not sets the parent, nor the siblings;
 * this clone is "floating" out of the tree hierarcy.
 */

static PHB_ITEM mxml_node_clone_tree( PHB_ITEM pTg )
{
   PHB_ITEM pClone = mxml_node_clone( pTg );
   PHB_ITEM pNode;

   /* Get the TG child */
   hb_objSendMsg( pTg, "OCHILD", 0 );
   pNode = hb_itemNew( hb_param( -1, HB_IT_ANY ) );

   while( ! HB_IS_NIL( pNode ) )
   {
      PHB_ITEM pSubTree;

      pSubTree = mxml_node_clone_tree( pNode );
      /* the subtree is the child of the clone */
      hb_objSendMsg( pClone, "_OCHILD", 1, pSubTree );

      /* the parent of the subtree is the clone */
      hb_objSendMsg( pSubTree, "_OPARENT", 1, pClone );

      /* go to the next node */
      hb_objSendMsg( pNode, "ONEXT", 0 );
      hb_itemMove( pNode, hb_param( -1, HB_IT_ANY ) );
   }
   hb_itemRelease( pNode );

   return pClone;
}

HB_FUNC( HBXML_NODE_CLONE_TREE )
{
   hb_itemReturnRelease( mxml_node_clone_tree( hb_param( 1, HB_IT_OBJECT ) ) );
}

/* reads a data node */
static void mxml_node_read_data( MXML_REFIL * ref, PHB_ITEM pNode, PHB_ITEM doc, int iStyle )
{
   char *   buf        = ( char * ) MXML_ALLOCATOR( MXML_ALLOC_BLOCK );
   int      iAllocated = MXML_ALLOC_BLOCK;
   int      iPos       = 0;
   int      chr;
   PHB_ITEM pItem;
   int      iStatus = 0, iPosAmper = 0;

   chr = mxml_refil_getc( ref );
   while( chr != MXML_EOF )
   {

      /* still in a data element */
      if( chr != '<' )
      {

         /* verify entity or escape */
         if( chr == '&' && ! ( iStyle & MXML_STYLE_NOESCAPE ) )
         {
            if( iStatus == 0 )
            {
               iStatus   = 1;
               iPosAmper = iPos;
            }
            else
            {
               /* error - we have something like &amp &amp */
               MXML_DELETOR( buf );
               hbxml_set_doc_status( ref, doc, pNode,
                                     MXML_STATUS_MALFORMED, MXML_ERROR_UNCLOSEDENTITY );
               return;
            }
         }

         /* rightful closing of an entity */
         if( chr == ';' && iStatus == 1 )
         {
            int    iAmpLen = iPos - iPosAmper - 2;
            char * bp      = buf + iPosAmper + 1;

            if( iAmpLen <= 0 )
            {
               /* error! - we have "&;" */
               MXML_DELETOR( buf );
               hbxml_set_doc_status( ref, doc, pNode,
                                     MXML_STATUS_MALFORMED, MXML_ERROR_WRONGENTITY );
               return;
            }

            iStatus = 0;

            /* we see if we have a predef entity (also known as escape) */
            if( strncmp( bp, "amp", iAmpLen ) == 0 )
               chr = '&';
            else if( strncmp( bp, "lt", iAmpLen ) == 0 )
               chr = '<';
            else if( strncmp( bp, "gt", iAmpLen ) == 0 )
               chr = '>';
            else if( strncmp( bp, "quot", iAmpLen ) == 0 )
               chr = '"';
            else if( strncmp( bp, "apos", iAmpLen ) == 0 )
               chr = '\'';

            /* if yes, we must put it at the place of the amper, and restart
               from there */
            if( chr != ';' )
               iPos = iPosAmper;
         }

         if( chr == MXML_LINE_TERMINATOR )
         {
            hbxml_doc_new_line( doc );
         }

         buf[ iPos++ ] = ( char ) chr;

         if( iPos >= iAllocated )
         {
            iAllocated += MXML_ALLOC_BLOCK;
            buf         = ( char * ) MXML_REALLOCATOR( buf, iAllocated );
         }

      }
      else
      {
         mxml_refil_ungetc( ref, chr );
         break;
      }

      chr = mxml_refil_getc( ref );
   }

   if( iStatus != 0 )
   {
      MXML_DELETOR( buf );
      hbxml_set_doc_status( ref, doc, pNode, MXML_STATUS_MALFORMED, MXML_ERROR_UNCLOSEDENTITY );
      return;
   }

   if( ref->status != MXML_STATUS_OK )
   {
      MXML_DELETOR( buf );
      hbxml_set_doc_status( ref, doc, pNode, ref->status, ref->error );
      return;
   }

   /* trimming unneded spaces */
   while( iPos > 1 && HB_ISSPACE( ( HB_BYTE ) buf[ iPos - 1 ] ) )
      iPos--;

   buf[ iPos ] = 0;

   pItem = hb_itemPutNI( NULL, MXML_TYPE_DATA );
   hb_objSendMsg( pNode, "_NTYPE", 1, pItem );

   if( iAllocated > iPos + 1 )
      buf = ( char * ) MXML_REALLOCATOR( buf, iPos + 1 );

   hb_itemPutCL( pItem, buf, iPos );
   hb_objSendMsg( pNode, "_CDATA", 1, pItem );
   hb_itemRelease( pItem );
   MXML_DELETOR( buf );
}

static MXML_STATUS mxml_node_read_name( MXML_REFIL * ref, PHB_ITEM pNode, PHB_ITEM doc )
{
   PHB_ITEM pItem;
   char *   buf;
   int      iAllocated;
   int      iPos = 0;
   int      chr;
   int      iStatus = 0;

   buf        = ( char * ) MXML_ALLOCATOR( MXML_ALLOC_BLOCK );
   iAllocated = MXML_ALLOC_BLOCK;

   while( iStatus < 2 )
   {
      chr = mxml_refil_getc( ref );
      if( chr == MXML_EOF )
         break;

      switch( iStatus )
      {
         case 0:
            if( HB_ISALPHA( chr ) )
            {
               /* can't cause reallocations */
               buf[ iPos++ ] = ( char ) chr;
               iStatus       = 1;
            }
            else
            {
               MXML_DELETOR( buf );
               hbxml_set_doc_status( ref, doc, pNode, MXML_STATUS_MALFORMED, MXML_ERROR_INVNODE );
               return MXML_STATUS_MALFORMED;
            }
            break;

         case 1:
            if( HB_ISALNUM( chr ) || chr == '_' || chr == '-' || chr == ':' )
            {
               /* can't cause reallocations */
               buf[ iPos++ ] = ( char ) chr;
            }
            else if( chr == '>' || chr == ' ' || chr == '/' || chr == '\r' ||
                     chr == '\t' || chr == '\n' )
            {
               mxml_refil_ungetc( ref, chr );
               iStatus = 2;
            }
            else
            {
               MXML_DELETOR( buf );
               hbxml_set_doc_status( ref, doc, pNode, MXML_STATUS_MALFORMED, MXML_ERROR_INVNODE );
               return MXML_STATUS_MALFORMED;
            }
            break;
      }

      if( iPos >= iAllocated )
      {
         iAllocated += MXML_ALLOC_BLOCK;
         buf         = ( char * ) MXML_REALLOCATOR( buf, iAllocated );
         if( ! buf )
         {
            hbxml_set_doc_status( ref, doc, pNode, MXML_STATUS_MALFORMED, MXML_ERROR_NAMETOOLONG );
            return MXML_STATUS_MALFORMED;
         }
      }
   }

   if( ref->status != MXML_STATUS_OK )
   {
      MXML_DELETOR( buf );
      hbxml_set_doc_status( ref, doc, pNode, ref->status, ref->error );
      return ref->status;
   }

   buf[ iPos ] = 0;
   if( iAllocated > iPos + 1 )
      buf = ( char * ) MXML_REALLOCATOR( buf, iPos + 1 );

   pItem = hb_itemPutCL( NULL, buf, iPos );
   hb_objSendMsg( pNode, "_CNAME", 1, pItem );
   hb_itemRelease( pItem );
   MXML_DELETOR( buf );

   return MXML_STATUS_OK;
}

static MXML_STATUS mxml_node_read_attributes( MXML_REFIL * ref,
                                              PHB_ITEM pNode, PHB_ITEM doc, int style )
{
   HBXML_ATTRIBUTE hbAttr;
   PHB_ITEM        attributes;
   PHB_ITEM        hbName;
   PHB_ITEM        hbValue;
   MXML_STATUS     ret;

   hbName     = hb_itemNew( NULL );
   hbValue    = hb_itemNew( NULL );
   attributes = hb_hashNew( NULL );

   hbAttr.pName  = hbName;
   hbAttr.pValue = hbValue;

   ret = mxml_attribute_read( ref, doc, pNode, &hbAttr, style );
   while( ret == MXML_STATUS_OK )
   {
      hb_hashAdd( attributes, hbAttr.pName, hbAttr.pValue );
      ret = mxml_attribute_read( ref, doc, pNode, &hbAttr, style );
   }

   hb_objSendMsg( pNode, "_AATTRIBUTES", 1, attributes );
   /* Error already set. */
#if 0
   if( ref->status != MXML_STATUS_OK )
   {
      hbxml_set_doc_status( ref, doc, pNode, ref->status, ref->error );
   }
#endif
   hb_itemRelease( attributes );
   hb_itemRelease( hbValue );
   hb_itemRelease( hbName );

   return ref->status;
}

static void mxml_node_read_directive( MXML_REFIL * ref, PHB_ITEM pNode, PHB_ITEM doc )
{
   char * buf        = ( char * ) MXML_ALLOCATOR( MXML_ALLOC_BLOCK );
   int    iAllocated = MXML_ALLOC_BLOCK;
   int    iPos       = 0;
   int    chr;

   if( mxml_node_read_name( ref, pNode, doc ) == MXML_STATUS_OK )
   {
      chr = mxml_refil_getc( ref );
      while( chr != MXML_EOF && chr != '>' )
      {
         if( iPos > 0 || ( chr != ' ' && chr != '\t' && chr != '\r' && chr != '\n' ) )
            buf[ iPos++ ] = ( char ) chr;

         if( iPos >= iAllocated )
         {
            iAllocated += MXML_ALLOC_BLOCK;
            buf         = ( char * ) MXML_REALLOCATOR( buf, iAllocated );
         }

         if( chr == MXML_LINE_TERMINATOR )
            hbxml_doc_new_line( doc );

         chr = mxml_refil_getc( ref );
      }

      if( ref->status == MXML_STATUS_OK )
      {
         PHB_ITEM pItem = hb_itemPutNI( NULL, MXML_TYPE_DIRECTIVE );
         buf[ iPos ] = 0;
         hb_objSendMsg( pNode, "_NTYPE", 1, pItem );
         if( iAllocated > iPos + 1 )
            buf = ( char * ) MXML_REALLOCATOR( buf, iPos + 1 );

         hb_itemPutCL( pItem, buf, iPos );
         hb_objSendMsg( pNode, "_CDATA", 1, pItem );
         hb_itemRelease( pItem );
      }
      else
      {
         hbxml_set_doc_status( ref, doc, pNode, ref->status, ref->error );
      }
   }
   MXML_DELETOR( buf );
}

static void mxml_node_read_pi( MXML_REFIL * ref, PHB_ITEM pNode, PHB_ITEM doc )
{
   int    iPos = 0, iAllocated;
   int    chr;
   char * buf;
   int    iStatus = 0;

   /* let's read the xml PI instruction */
   if( mxml_node_read_name( ref, pNode, doc ) != MXML_STATUS_OK )
      return;

   /* and then we'll put all the "data" into the data member, up to ?> */

   buf        = ( char * ) MXML_ALLOCATOR( MXML_ALLOC_BLOCK );
   iAllocated = MXML_ALLOC_BLOCK;

   while( iStatus < 2 )
   {
      chr = mxml_refil_getc( ref );
      if( chr == MXML_EOF )
         break;

      switch( iStatus )
      {
         /* scanning for ?> */
         case 0:
            if( chr == MXML_LINE_TERMINATOR )
            {
               hbxml_doc_new_line( doc );
               buf[ iPos++ ] = ( char ) chr;
            }
            else if( chr == '?' )
               iStatus = 1;
            else
            {
               if( iPos > 0 || ( chr != ' ' && chr != '\n' ) )
                  buf[ iPos++ ] = ( char ) chr;
            }
            break;

         case 1:
            if( chr == '>' )
               iStatus = 2;
            else
            {
               iStatus       = 0;
               buf[ iPos++ ] = '?';
               mxml_refil_ungetc( ref, chr );
            }
            break;
      }

      if( iPos == iAllocated )
      {
         iAllocated += MXML_ALLOC_BLOCK;
         buf         = ( char * ) MXML_REALLOCATOR( buf, iAllocated );
      }
   }

   if( ref->status == MXML_STATUS_OK )
   {
      PHB_ITEM pItem = hb_itemPutNI( NULL, MXML_TYPE_PI );
      buf[ iPos ] = 0;
      hb_objSendMsg( pNode, "_NTYPE", 1, pItem );
      if( iAllocated > iPos + 1 )
         buf = ( char * ) MXML_REALLOCATOR( buf, iPos + 1 );

      hb_itemPutCL( pItem, buf, iPos );
      hb_objSendMsg( pNode, "_CDATA", 1, pItem );
      hb_itemRelease( pItem );
   }
   else
   {
      hbxml_set_doc_status( ref, doc, pNode, ref->status, ref->error );
   }
   MXML_DELETOR( buf );
}

static void mxml_node_read_tag( MXML_REFIL * ref, PHB_ITEM pNode, PHB_ITEM doc,
                                int style )
{
   char     chr;
   PHB_ITEM pItem;

   pItem = hb_itemPutNI( NULL, MXML_TYPE_TAG );
   hb_objSendMsg( pNode, "_NTYPE", 1, pItem );
   hb_itemRelease( pItem );

   if( mxml_node_read_name( ref, pNode, doc ) == MXML_STATUS_OK )
   {
      if( mxml_node_read_attributes( ref, pNode, doc, style ) != MXML_STATUS_OK )
         return;
   }

   /* if the list of attributes terminates with a '/', the last '>' is
      left unread. This means the current node is complete. */
   chr = ( char ) mxml_refil_getc( ref );
   if( ref->status == MXML_STATUS_OK && chr != '>' )
   {
      mxml_refil_ungetc( ref, chr );
      /* recurse */
      if( mxml_node_read( ref, pNode, doc, style ) != MXML_STATUS_OK )
         return;
   }
   else if( ref->status != MXML_STATUS_OK )
      hbxml_set_doc_status( ref, doc, pNode, ref->status, ref->error );

   /* else the node is complete */
}

static void mxml_node_read_comment( MXML_REFIL * ref, PHB_ITEM pNode, PHB_ITEM doc )
{
   int      iPos = 0, iAllocated;
   int      chr;
   char *   buf;
   int      iStatus = 0;
   PHB_ITEM pItem;

   pItem = hb_itemPutNI( NULL, MXML_TYPE_COMMENT );
   hb_objSendMsg( pNode, "_NTYPE", 1, pItem );

   /* we'll put all the comment into the data member, up to -> */

   buf        = ( char * ) MXML_ALLOCATOR( MXML_ALLOC_BLOCK );
   iAllocated = MXML_ALLOC_BLOCK;

   while( iStatus < 3 )
   {
      chr = mxml_refil_getc( ref );
      if( chr == MXML_EOF )
         break;

      switch( iStatus )
      {
         /* scanning for -> */
         case 0:
            if( chr == MXML_LINE_TERMINATOR )
            {
               hbxml_doc_new_line( doc );
               buf[ iPos++ ] = ( char ) chr;
            }
            else if( chr == '-' )
               iStatus = 1;
            else
               buf[ iPos++ ] = ( char ) chr;
            break;

         case 1:
            if( chr == '-' )
               iStatus = 2;
            else
            {
               iStatus       = 0;
               buf[ iPos++ ] = '-';
               mxml_refil_ungetc( ref, chr );
            }
            break;

         case 2:
            if( chr == '>' )
               iStatus = 3;
            else
            {
               iStatus       = 0;
               buf[ iPos++ ] = '-';
               mxml_refil_ungetc( ref, chr );
            }
            break;
      }

      if( iPos == iAllocated )
      {
         iAllocated += MXML_ALLOC_BLOCK;
         buf         = ( char * ) MXML_REALLOCATOR( buf, iAllocated );
      }
   }

   if( ref->status == MXML_STATUS_OK )
   {
      buf[ iPos ] = 0;
      if( iAllocated > iPos + 1 )
         buf = ( char * ) MXML_REALLOCATOR( buf, iPos + 1 );

      hb_itemPutCL( pItem, buf, iPos );
      hb_objSendMsg( pNode, "_CDATA", 1, pItem );
   }
   else
   {
      hbxml_set_doc_status( ref, doc, pNode, ref->status, ref->error );
   }
   hb_itemRelease( pItem );
   MXML_DELETOR( buf );
}

static void mxml_node_read_cdata( MXML_REFIL * ref, PHB_ITEM pNode, PHB_ITEM pDoc )
{
   int      iPos = 0, iAllocated;
   int      chr;
   char *   buf;
   int      iStatus = 0;
   PHB_ITEM pItem;

   pItem = hb_itemPutNI( NULL, MXML_TYPE_CDATA );
   hb_objSendMsg( pNode, "_NTYPE", 1, pItem );

   /* we'll put all the cdata into the data member, up to ]]>
      however, we are not still sure that this is really a cdata.
      we must finish to read it: */

   while( iStatus < 6 )
   {
      chr = mxml_refil_getc( ref );
      if( chr == MXML_EOF )
         break;

      if( chr == MXML_LINE_TERMINATOR )
      {
         hbxml_doc_new_line( pDoc );
         /* but is an error, so */
         iStatus = 100;
      }

      switch( iStatus )
      {
         /* scanning for C */
         case 0:
            if( chr == 'C' )
               iStatus = 1;
            else
               iStatus = 100;  /* error */
            break;

         case 1:
            if( chr == 'D' )
               iStatus = 2;
            else
               iStatus = 100;
            break;

         case 2:
            if( chr == 'A' )
               iStatus = 3;
            else
               iStatus = 100;
            break;

         case 3:
            if( chr == 'T' )
               iStatus = 4;
            else
               iStatus = 100;
            break;

         case 4:
            if( chr == 'A' )
               iStatus = 5;
            else
               iStatus = 100;
            break;

         case 5:
            if( chr == '[' )
               iStatus = 6;
            else
               iStatus = 100;
            break;
      }
   }

   if( iStatus == 100 )
   {
      hbxml_set_doc_status( ref, pDoc, pNode,
                            MXML_STATUS_MALFORMED, MXML_ERROR_INVNODE );
   }
   else
   {
      iStatus = 0;

      buf        = ( char * ) MXML_ALLOCATOR( MXML_ALLOC_BLOCK );
      iAllocated = MXML_ALLOC_BLOCK;

      /* now we can read the node */
      while( iStatus < 3 )
      {
         chr = mxml_refil_getc( ref );
         if( chr == MXML_EOF )
            break;

         switch( iStatus )
         {
            /* scanning for -> */
            case 0:
               if( chr == MXML_LINE_TERMINATOR )
               {
                  hbxml_doc_new_line( pDoc );
                  buf[ iPos++ ] = ( char ) chr;
               }
               else if( chr == ']' )
                  iStatus = 1;
               else
                  buf[ iPos++ ] = ( char ) chr;
               break;

            case 1:
               if( chr == ']' )
                  iStatus = 2;
               else
               {
                  iStatus       = 0;
                  buf[ iPos++ ] = ']';
                  mxml_refil_ungetc( ref, chr );
               }
               break;

            case 2:
               if( chr == '>' )
                  iStatus = 3;
               else
               {
                  iStatus       = 0;
                  buf[ iPos++ ] = ']';
                  mxml_refil_ungetc( ref, chr );
               }
               break;

         }

         if( iPos == iAllocated )
         {
            iAllocated += MXML_ALLOC_BLOCK;
            buf         = ( char * ) MXML_REALLOCATOR( buf, iAllocated );
         }
      }

      if( ref->status == MXML_STATUS_OK )
      {
         buf[ iPos ] = 0;
         if( iAllocated > iPos + 1 )
            buf = ( char * ) MXML_REALLOCATOR( buf, iPos + 1 );

         hb_itemPutCL( pItem, buf, iPos );
         hb_objSendMsg( pNode, "_CDATA", 1, pItem );
      }
      else
      {
         hbxml_set_doc_status( ref, pDoc, pNode, ref->status, ref->error );
      }
      MXML_DELETOR( buf );
   }
   hb_itemRelease( pItem );
}

/* checking closing tag */
static int mxml_node_read_closing( MXML_REFIL * ref, PHB_ITEM pNode, PHB_ITEM doc )
{
   char *  buf;
   HB_ISIZ iPos = 0;
   int     chr;
   HB_ISIZ iLen;

   hb_objSendMsg( pNode, "CNAME", 0 );
   iLen = hb_parclen( -1 ) + 1;
   buf  = ( char * ) MXML_ALLOCATOR( iLen );

   chr = mxml_refil_getc( ref );
   while( chr != MXML_EOF && chr != '>' && iPos < iLen )
   {
      buf[ iPos++ ] = ( char ) chr;
      chr = mxml_refil_getc( ref );
   }

   if( ref->status != MXML_STATUS_OK )
   {
      MXML_DELETOR( buf );
      hbxml_set_doc_status( ref, doc, pNode, ref->status, ref->error );
      return ref->status;
   }

   if( chr != '>' || iPos == iLen || ( strncmp( hb_parcx( -1 ), buf, iLen - 1 ) != 0 ) )
   {
      MXML_DELETOR( buf );
      hbxml_set_doc_status( ref, doc, pNode, MXML_STATUS_MALFORMED, MXML_ERROR_UNCLOSED );
      return MXML_STATUS_MALFORMED;
   }
   /* all fine */
   MXML_DELETOR( buf );
   return MXML_STATUS_OK;
}

static MXML_STATUS mxml_node_read( MXML_REFIL * ref, PHB_ITEM pNode, PHB_ITEM doc, int style )
{
   PHB_ITEM node;

   int chr;
   /* Stateful machine status */
   int iStatus = 0;

   while( iStatus >= 0 )
   {
      chr = mxml_refil_getc( ref );
      if( chr == MXML_EOF )
         break;

      if( ref->status != MXML_STATUS_OK )
      {
         hbxml_set_doc_status( ref, doc, pNode, MXML_STATUS_MALFORMED, MXML_ERROR_INVNODE );
         return MXML_STATUS_MALFORMED;
      }
      /* resetting new node foundings */
      node = NULL;

      switch( iStatus )
      {
         case 0:  /* outside nodes */
            switch( chr )
            {
               case MXML_LINE_TERMINATOR: hbxml_doc_new_line( doc ); break;
               /* We repeat line terminator here for portability */
               case MXML_SOFT_LINE_TERMINATOR: break;
               case ' ': case '\t': break;
               case '<': iStatus = 1; break;
               default:  /* it is a data node */
                  mxml_refil_ungetc( ref, chr );
                  node = mxml_node_new( doc );
                  mxml_node_read_data( ref, node, doc, style );
            }
            break;

         case 1: /* inside a node, first character */
            if( chr == '/' )
            {
               /* This can be met only inside current tag */
               iStatus = -1; /* done */
            }
            else if( chr == '!' )
               iStatus = 2;
            else if( chr == '?' )
            {
               node = mxml_node_new( doc );
               mxml_node_read_pi( ref, node, doc );
            }
            else if( HB_ISALPHA( chr ) )
            {
               mxml_refil_ungetc( ref, chr );
               node = mxml_node_new( doc );
               mxml_node_read_tag( ref, node, doc, style );
            }
            else
            {
               hbxml_set_doc_status( ref, doc, pNode, MXML_STATUS_MALFORMED, MXML_ERROR_INVNODE );
               return MXML_STATUS_MALFORMED;
            }
            break;

         case 2: /* inside a possible comment (<!-/<!?) */
            if( chr == '-' )
            {
               iStatus = 3;
            }
            else if( HB_ISALPHA( chr ) )
            {
               node = mxml_node_new( doc );
               mxml_refil_ungetc( ref, chr );
               mxml_node_read_directive( ref, node, doc );
            }
            else if( chr == '[' )
            {
               node = mxml_node_new( doc );
               mxml_node_read_cdata( ref, node, doc );
            }
            else
            {
               hbxml_set_doc_status( ref, doc, pNode, MXML_STATUS_MALFORMED, MXML_ERROR_INVNODE );
               return MXML_STATUS_MALFORMED;
            }
            break;

         case 3:
            if( chr == '-' )
            {
               node = mxml_node_new( doc );
               mxml_node_read_comment( ref, node, doc );
            }
            else
            {
               hbxml_set_doc_status( ref, doc, pNode, MXML_STATUS_MALFORMED, MXML_ERROR_INVNODE );
               return MXML_STATUS_MALFORMED;
            }
            break;
      }

      /* have I to add a node below our structure ? */
      if( node != NULL )
      {
         if( ref->status == MXML_STATUS_OK )
         {
            mxml_node_add_below( pNode, node );
            hb_itemRelease( node );
            /* beginning again - a new node is born */
            hbxml_doc_new_node( doc, 1 );
            iStatus = 0;
         }
         else
         {
            /* Error is already set in ref->status; it can't be an hard error, we catch it before.*/
            hb_itemRelease( node );
            /* node will be destroyed by GC when needed */
            return ref->status;
         }
      }

   }

   /* We can't have errors here; we would have been already returned */

   if( iStatus == -1 )  /* ARE WE DONE ? */
   {
      PHB_ITEM child_node, data_node = NULL;

      /* Time to close current node. We must verify:
         1) If the closing tag is coherent with the opened tag name.
         2) If the tag has just one data node as child.
         if we have only one data node among the children, the data
         node is destroyed and the data element is moved to the
         "data" field of current node, to simplify the tree structure
         in the most common config oriented XML files.
       */

      /* I/O error or malformed error during closing? */
      mxml_node_read_closing( ref, pNode, doc );
      if( ref->status != MXML_STATUS_OK )
         return ref->status;

      /* checking for data nodes */
      hb_objSendMsg( pNode, "OCHILD", 0 );
      child_node = hb_itemNew( hb_param( -1, HB_IT_ANY ) );

      while( ! HB_IS_NIL( child_node ) )
      {
         hb_objSendMsg( child_node, "NTYPE", 0 );
         if( hb_parni( -1 ) == MXML_TYPE_DATA )
         {
            /* first data node ? */
            if( data_node == NULL )
               data_node = hb_itemNew( child_node );
            /* ... or have we more than a data node? */
            else
            {
               hb_itemRelease( data_node );
               data_node = NULL;
               break;
            }
         }
         hb_objSendMsg( child_node, "ONEXT", 0 );
         hb_itemMove( child_node, hb_param( -1, HB_IT_ANY ) );
      }

      if( data_node )
      {
         hb_objSendMsg( data_node, "CDATA", 0 );
         hb_objSendMsg( pNode, "_CDATA", 1, hb_param( -1, HB_IT_ANY ) );

         mxml_node_unlink( data_node );
         /* garbage will take care of destroying it */
         hbxml_doc_new_node( doc, -1 );
         hb_itemRelease( data_node );
      }
      hb_itemRelease( child_node );
   }

   return MXML_STATUS_OK;
}

static void mxml_node_write_attributes( MXML_OUTPUT * out, PHB_ITEM pAttr, int style )
{
   HB_SIZE iLen = hb_hashLen( pAttr );
   HB_SIZE i;
   HBXML_ATTRIBUTE hbAttr;

   for( i = 1; i <= iLen; i++ )
   {
      mxml_output_char( out, ' ' );
      hbAttr.pName  = hb_hashGetKeyAt( pAttr, i );
      hbAttr.pValue = hb_hashGetValueAt( pAttr, i );

      mxml_attribute_write( out, &hbAttr, style );
   }
}

static void mxml_node_file_indent( MXML_OUTPUT * out, int depth, int style )
{
   int i;

   for( i = 0; i < depth; i++ )
   {
      if( style & MXML_STYLE_TAB )
         mxml_output_char( out, '\t' );
      else if( style & MXML_STYLE_THREESPACES )
         mxml_output_string_len( out, "   ", 3 );
      else
         mxml_output_char( out, ' ' );
   }
}

static MXML_STATUS mxml_node_write( MXML_OUTPUT * out, PHB_ITEM pNode, int style )
{
   PHB_ITEM pChild, pItem;
   int      depth      = 0;
   int      mustIndent = 0;

   pChild = hb_itemNew( NULL );
   pItem  = hb_itemNew( NULL );

   if( style & MXML_STYLE_INDENT )
   {
      hb_objSendMsg( pNode, "DEPTH", 0 );
      depth = hb_parni( -1 ) - 1;
      mxml_node_file_indent( out, depth, style );
   }

   hb_objSendMsg( pNode, "NTYPE", 0 );
   switch( hb_parni( -1 ) )
   {
      case MXML_TYPE_TAG:

         mxml_output_char( out, '<' );
         hb_objSendMsg( pNode, "CNAME", 0 );
         mxml_output_string_len( out,
                                 hb_parc( -1 ), hb_parclen( -1 ) );

         hb_objSendMsg( pNode, "AATTRIBUTES", 0 );
         mxml_node_write_attributes( out, hb_param( -1, HB_IT_ANY ), style );

         hb_objSendMsg( pNode, "CDATA", 0 );
         /* itemcopy should not be applied to strings, as it rises the
            holders, and we don't want this */
         hb_itemMove( pItem, hb_param( -1, HB_IT_ANY ) );
         hb_objSendMsg( pNode, "OCHILD", 0 );
         hb_itemMove( pChild, hb_param( -1, HB_IT_ANY ) );

         if( HB_IS_NIL( pItem ) && HB_IS_NIL( pChild ) )
         {
            mxml_output_string_len( out, "/>", 2 );
            mxml_output_string( out, hb_conNewLine() );
         }
         else
         {
            mxml_output_char( out, '>' );

            if( ! HB_IS_NIL( pChild ) )
            {
               mustIndent = 1;
               mxml_output_string( out, hb_conNewLine() );

               while( ! HB_IS_NIL( pChild ) )
               {
                  mxml_node_write( out, pChild, style );
                  hb_objSendMsg( pChild, "ONEXT", 0 );
                  hb_itemMove( pChild, hb_param( -1, HB_IT_ANY ) );
               }
            }

            if( ! HB_IS_NIL( pItem ) )
            {
               if( mustIndent && ( style & MXML_STYLE_INDENT ) )
                  mxml_node_file_indent( out, depth + 1, style );

               if( ! ( style & MXML_STYLE_NOESCAPE ) )
                  mxml_output_string_escape( out, hb_itemGetCPtr( pItem ) );
               else
                  mxml_output_string_len( out, hb_itemGetCPtr( pItem ),
                                          hb_itemGetCLen( pItem ) );
            }

            if( mustIndent && ( style & MXML_STYLE_INDENT ) )
               mxml_node_file_indent( out, depth, style );

            mxml_output_string_len( out, "</", 2 );
            hb_objSendMsg( pNode, "CNAME", 0 );
            mxml_output_string_len( out, hb_parc( -1 ), hb_parclen( -1 ) );
            mxml_output_char( out, '>' );
            mxml_output_string( out, hb_conNewLine() );
         }
         break;

      case MXML_TYPE_COMMENT:
         mxml_output_string_len( out, "<!-- ", 5 );
         hb_objSendMsg( pNode, "CDATA", 0 );
         mxml_output_string_len( out, hb_parc( -1 ), hb_parclen( -1 ) );
         mxml_output_string_len( out, " -->", 4 );
         mxml_output_string( out, hb_conNewLine() );
         break;

      case MXML_TYPE_CDATA:
         mxml_output_string_len( out, "<![CDATA[ ", 9 );
         hb_objSendMsg( pNode, "CDATA", 0 );
         mxml_output_string_len( out, hb_parc( -1 ), hb_parclen( -1 ) );
         mxml_output_string_len( out, " ]]>", 4 );
         mxml_output_string( out, hb_conNewLine() );
         break;

      case MXML_TYPE_DATA:
         hb_objSendMsg( pNode, "CDATA", 0 );
         if( ! ( style & MXML_STYLE_NOESCAPE ) )
            mxml_output_string_escape( out, hb_parcx( -1 ) );
         else
            mxml_output_string_len( out, hb_parcx( -1 ), hb_parclen( -1 ) );
         mxml_output_string( out, hb_conNewLine() );
         break;

      case MXML_TYPE_DIRECTIVE:
         mxml_output_string_len( out, "<!", 2 );
         hb_objSendMsg( pNode, "CNAME", 0 );
         mxml_output_string_len( out, hb_parcx( -1 ), hb_parclen( -1 ) );

         hb_objSendMsg( pNode, "CDATA", 0 );

         if( HB_IS_STRING( hb_param( -1, HB_IT_ANY ) ) )
         {
            mxml_output_char( out, ' ' );
            mxml_output_string_len( out, hb_parcx( -1 ), hb_parclen( -1 ) );
         }
         mxml_output_char( out, '>' );
         mxml_output_string( out, hb_conNewLine() );
         break;

      case MXML_TYPE_PI:
         mxml_output_string_len( out, "<?", 2 );
         hb_objSendMsg( pNode, "CNAME", 0 );
         mxml_output_string_len( out, hb_parcx( -1 ), hb_parclen( -1 ) );
         hb_objSendMsg( pNode, "CDATA", 0 );

         if( HB_IS_STRING( hb_param( -1, HB_IT_ANY ) ) )
         {
            mxml_output_char( out, ' ' );
            mxml_output_string_len( out, hb_parcx( -1 ), hb_parclen( -1 ) );
         }
         mxml_output_string_len( out, "?>", 2 );
         mxml_output_string( out, hb_conNewLine() );
         break;

      case MXML_TYPE_DOCUMENT:
         hb_objSendMsg( pNode, "OCHILD", 0 );
         hb_itemMove( pChild, hb_param( -1, HB_IT_ANY ) );

         while( ! HB_IS_NIL( pChild ) )
         {
            mxml_node_write( out, pChild, style );
            hb_objSendMsg( pChild, "ONEXT", 0 );
            hb_itemMove( pChild, hb_param( -1, HB_IT_ANY ) );
         }
         mxml_output_string( out, hb_conNewLine() );
         break;
   }

   hb_itemRelease( pItem );
   hb_itemRelease( pChild );

   if( out->status != MXML_STATUS_OK )
      return out->status;

   /* just for progress indicators */
   out->node_done++;

   return MXML_STATUS_OK;
}


/***********************************************************
   HBXML lib
   Virtual stream input/output routines
***********************************************************/

/**
 * Creates a new output object
 * In this case, the func member is required.
 * Node count is optional, but highly wanted for progress indicators.
 */
/* Currently not used */
#if 0
static MXML_OUTPUT * mxml_output_new( MXML_OUTPUT_FUNC func, int node_count )
{
   MXML_OUTPUT * ret = ( MXML_OUTPUT * ) MXML_ALLOCATOR( sizeof( MXML_OUTPUT ) );

   if( ret == NULL )
      return NULL;

   if( mxml_output_setup( ret, func, node_count ) == MXML_STATUS_OK )
      return ret;

   MXML_DELETOR( ret );
   return NULL;
}
#endif

/**
 * Sets up output parameters.
 * In this case, the func member is required.
 * Node count is optional, but highly wanted for progress indicators.
 */

static MXML_STATUS mxml_output_setup( MXML_OUTPUT * out, MXML_OUTPUT_FUNC func, int node_count )
{
   if( func == NULL )
      return MXML_STATUS_ERROR;

   out->output_func = func;
   out->node_count  = node_count;
   out->node_done   = 0;

   out->status = MXML_STATUS_OK;
   out->error  = MXML_ERROR_NONE;
   return MXML_STATUS_ERROR;
}

/* Currently not used */
#if 0
static void mxml_output_destroy( MXML_OUTPUT * out )
{
   MXML_DELETOR( out );
}
#endif

/**********************************************/
/* output functions                           */

static MXML_STATUS mxml_output_char( MXML_OUTPUT * out, int c )
{
   char chr = ( char ) c;

   out->output_func( out, &chr, 1 );
   return out->status;
}

static MXML_STATUS mxml_output_string_len( MXML_OUTPUT * out, const char * s, HB_ISIZ len )
{
   out->output_func( out, s, len );
   return out->status;
}

/* Currently not used */
#if 0
static MXML_STATUS mxml_output_string( MXML_OUTPUT * out, const char * s )
{
   return mxml_output_string_len( out, s, strlen( s ) );
}
#endif

static MXML_STATUS mxml_output_string( MXML_OUTPUT * out, const char * s )
{
   out->output_func( out, s, strlen( s ) );
   return out->status;
}


static MXML_STATUS mxml_output_string_escape( MXML_OUTPUT * out, const char * s )
{
   while( *s )
   {
      switch( *s )
      {
         case '"': mxml_output_string_len( out, "&quot;", 6 ); break;
         case '\'': mxml_output_string_len( out, "&apos;", 6 ); break;
         case '&': mxml_output_string_len( out, "&amp;", 5 ); break;
         case '<': mxml_output_string_len( out, "&lt;", 4 ); break;
         case '>': mxml_output_string_len( out, "&gt;", 4 ); break;
         default: mxml_output_char( out, *s );
      }

      if( out->status != MXML_STATUS_OK )
         break;
      s++;
   }

   return out->status;
}

/**
 * Useful function to output to file handles
 */
static void mxml_output_func_to_handle( MXML_OUTPUT * out, const char * s, HB_ISIZ len )
{
   HB_FHANDLE fh = out->u.hFile;
   HB_ISIZ    olen;

   olen = hb_fsWriteLarge( fh, s, len );

   if( olen < len )
   {
      out->status = MXML_STATUS_ERROR;
      out->error  = MXML_ERROR_IO;
   }
}

/**
 * Useful function to output to self growing strings
 */
static void mxml_output_func_to_sgs( MXML_OUTPUT * out, const char * s, HB_ISIZ len )
{
   MXML_SGS * sgs = ( MXML_SGS * ) out->u.vPtr;

   MXML_STATUS stat;

   if( len == 1 )
      stat = mxml_sgs_append_char( sgs, *s );
   else
      stat = mxml_sgs_append_string_len( sgs, s, len );

   if( stat != MXML_STATUS_OK )
   {
      out->status = MXML_STATUS_ERROR;
      out->error  = MXML_ERROR_NOMEM;
   }
}


/***********************************************************
   HBXML lib
   Refiller routines
***********************************************************/


/**
 * Creates a new refiller object.
 * If buf is null, then buflen is ignored and set to 0; the first retrival
 * of a character will then lead to refil func calling.
 * If the function is null, once the data has been read the reader returns
 * eof. If both func and buf are NULL, the creation fails, and the function
 * retunrs NULL.
 */
#if 0
static MXML_REFIL * mxml_refil_new( MXML_REFIL_FUNC func, char * buf, HB_ISIZ buflen,
                                    HB_ISIZ bufsize )
{
   MXML_REFIL * ret = ( MXML_REFIL * ) MXML_ALLOCATOR( sizeof( MXML_REFIL ) );

   if( ret == NULL )
      return NULL;

   if( mxml_refil_setup( ret, func, buf, buflen, bufsize ) == MXML_STATUS_OK )
      return ret;

   MXML_DELETOR( ret );
   return NULL;
}
#endif

/**
 * Sets up refiller parameters.
 * If buf is null, then buflen is ignored and set to 0; the first retrival
 * of a character will then lead to refil func calling. Bufsize is the size
 * of the allocated memory, while buflen is the count of currently valid
 * characters in that buffer.
 * If the function is null, once the data has been read the reader returns
 * eof. If both func and buf are NULL, the function fails and returns
 * MXML_STATUS_ERROR. On success, returns MXML_STATUS_OK.
 * Notice: ref->data member is left to fill to the
 * calling program, if this is needed.
 */

static MXML_STATUS mxml_refil_setup( MXML_REFIL * ref, MXML_REFIL_FUNC func,
                                     char * buf, HB_ISIZ buflen, HB_ISIZ bufsize )
{
   if( buf == NULL && func == NULL )
      return MXML_STATUS_ERROR;

   ref->refil_func = func;
   ref->buffer     = ( HB_BYTE * ) buf;

   ref->status = MXML_STATUS_OK;
   ref->error  = MXML_ERROR_NONE;

   if( buf == NULL )
      ref->buflen = ref->bufsize = 0;
   else
   {
      ref->buflen  = buflen;
      ref->bufsize = bufsize;
   }

   ref->bufpos = 0;

   /* stream length is left for the program to implement progress indicators */
   ref->streamlen = 0;
   ref->streampos = 0;

   /* theese are for ungetc operations */
   ref->sparechar = MXML_EOF;

   /* data is left to fill for the program */
   return MXML_STATUS_OK;
}

/* Currently not used. */
#if 0
static void mxml_refil_destroy( MXML_REFIL * ref )
{
   MXML_DELETOR( ref );
}
#endif

static int mxml_refil_getc( MXML_REFIL * ref )
{
   if( ref->sparechar != MXML_EOF )
   {
      int chr = ref->sparechar;
      ref->sparechar = MXML_EOF;
      return chr;
   }

   if( ref->bufpos >= ref->buflen )
   {
      if( ref->refil_func != NULL )
      {
         ref->refil_func( ref );
         if( ref->status != MXML_STATUS_OK || ref->buflen == 0 )
            return MXML_EOF;
      }
      else
         return MXML_EOF;
   }

   return ref->buffer[ ref->bufpos++ ];
}

/* implemented as a macro */
#if 0
void mxml_refil_ungetc( MXML_REFIL * ref, int chr )
{
   ref->sparechar = chr;
}
#endif

/**
 * Useful "fill" function that reads from a file handle
 */

static void mxml_refill_from_handle_func( MXML_REFIL * ref )
{
   HB_FHANDLE fh = ( HB_FHANDLE ) ref->u.hFile;
   HB_ISIZ    len;

   len = hb_fsReadLarge( fh, ref->buffer, ref->bufsize );

   if( len == -1 )
   {
      ref->status = MXML_STATUS_ERROR;
      ref->error  = MXML_ERROR_IO;
   }
   else
   {
      ref->buflen = len;
      ref->bufpos = 0;
   }
}



/********************************************************
   HBXML lib
   Self growing string routines
 *********************************************************/

/**
 * Creates a new self growing string, with buffer set to
 * minimal buffer length
 */
static MXML_SGS * mxml_sgs_new()
{
   MXML_SGS * ret = ( MXML_SGS * ) MXML_ALLOCATOR( sizeof( MXML_SGS ) );

   if( ret == NULL )
      return NULL;

   ret->buffer = ( char * ) MXML_ALLOCATOR( MXML_ALLOC_BLOCK );
   if( ret->buffer == NULL )
   {
      MXML_DELETOR( ret );
      return NULL;
   }

   ret->allocated = MXML_ALLOC_BLOCK;
   ret->length    = 0;

   return ret;
}

static void mxml_sgs_destroy( MXML_SGS * sgs )
{
   if( sgs->buffer != NULL )
      MXML_DELETOR( sgs->buffer );

   MXML_DELETOR( sgs );
}

/****************************************/

static MXML_STATUS mxml_sgs_append_char( MXML_SGS * sgs, char c )
{
   char * buf;

   sgs->buffer[ sgs->length++ ] = c;

   if( sgs->length >= sgs->allocated )
   {
      buf = ( char * ) MXML_REALLOCATOR( sgs->buffer, sgs->allocated + MXML_ALLOC_BLOCK );
      if( buf == NULL )
         return MXML_STATUS_ERROR;

      sgs->allocated += MXML_ALLOC_BLOCK;
      sgs->buffer     = buf;
   }

   return MXML_STATUS_OK;
}

static MXML_STATUS mxml_sgs_append_string_len( MXML_SGS * sgs, const char * s, HB_ISIZ slen )
{
   char * buf;

   if( slen > 0 )
   {
      if( sgs->length + slen >= sgs->allocated )
      {
         HB_ISIZ blklen = ( ( sgs->length + slen ) / MXML_ALLOC_BLOCK + 1 ) * MXML_ALLOC_BLOCK;
         buf = ( char * ) MXML_REALLOCATOR( sgs->buffer, blklen );

         if( buf == NULL )
            return MXML_STATUS_ERROR;

         sgs->allocated = blklen;
         sgs->buffer    = buf;
      }

      memcpy( sgs->buffer + sgs->length, s, slen + 1 );  /* include also the trailing space */
      sgs->length += slen;
   }

   return MXML_STATUS_OK;
}


/* Currently not used */
#if 0
static MXML_STATUS mxml_sgs_append_string( MXML_SGS * sgs, char * s )
{
   return mxml_sgs_append_string_len( sgs, s, strlen( s ) );
}
#endif

static char * mxml_sgs_extract( MXML_SGS * sgs )
{
   sgs->buffer[ sgs->length ] = 0;

   if( sgs->allocated > sgs->length + 1 )
      sgs->buffer = ( char * ) MXML_REALLOCATOR( sgs->buffer, sgs->length + 1 );

   return sgs->buffer;
}

/***********************************************************
   HBXML lib
   Error code routines
***********************************************************/

static const char * edesc[] =
{
   "Input/output error",
   "Not enough memory",
   "Character outside tags",
   "Invalid character as tag name",
   "Invalid character as attribute name",
   "Malformed attribute definition",
   "Invalid character",
   "Name of tag too long",
   "Name of attribute too long",
   "Value of attribute too long",
   "Unbalanced tag closeure",
   "Unbalanced entity opening",
   "Escape/entity '&;' found"
};

static const char * mxml_error_desc( MXML_ERROR_CODE code )
{
   int iCode = ( ( int ) code ) - 1;

   if( iCode < 0 || iCode > ( signed ) ( sizeof( edesc ) / sizeof( char * ) ) )
      return NULL;

   return edesc[ iCode ];
}


/***********************************************************
   HBXML lib
   xHarbour RTL & VM interface
***********************************************************/


/**
 * HB_XmlCreate( [xData] ) --> xmlDocument
 * xData can be a file handle from which an XML can be read,
 * a string containing an XML tree or NIL, in which case the
 * document is created empty.
 */

HB_FUNC( HBXML_DATAREAD )
{
   PHB_ITEM   pParam = hb_param( 2, HB_IT_ANY );
   PHB_ITEM   pDoc   = hb_param( 1, HB_IT_OBJECT );
   int        iStyle = hb_parni( 3 );
   PHB_ITEM   pRoot;
   MXML_REFIL refil;
   char       buffer[ 512 ], * buf;
   HB_SIZE    ulLen;

   if( pDoc == NULL || pParam == NULL ||
       ( ! HB_IS_STRING( pParam ) && ! HB_IS_NUMERIC( pParam ) ) )
   {
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   if( hb_itemGetWriteCL( pParam, &buf, &ulLen ) )
      mxml_refil_setup( &refil, NULL, buf, ulLen, ulLen );
   else /* can only be an integer, that is, a file handle */
   {
      mxml_refil_setup( &refil,
                        mxml_refill_from_handle_func,
                        buffer, 0, 512 );

      refil.u.vPtr = ( void * ) ( HB_PTRUINT ) hb_itemGetNInt( pParam );
   }

   /* Now we can get the root node */
   hb_objSendMsg( pDoc, "OROOT", 0 );
   pRoot = hb_itemNew( hb_param( -1, HB_IT_ANY ) );
   hb_retni( mxml_node_read( &refil, pRoot, pDoc, iStyle ) );
   hb_itemRelease( pRoot );
}


/**
 * hb_XMLErrorDesc( nErrorNumber ) --> cErrorDesc
 * Returns a descriptive string telling what the error number is meaning.
 */
HB_FUNC( HB_XMLERRORDESC )
{
   PHB_ITEM pNum = hb_param( 1, HB_IT_NUMERIC );

   if( pNum )
      hb_retc( mxml_error_desc( ( MXML_ERROR_CODE ) hb_itemGetNI( pNum ) ) );
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}



/**
 * HB_XmlToString( xmlDocument [, nStyle] ) --> cXml | NIL
 *
 * Writes an XML document to a string.
 */

HB_FUNC( HBXML_NODE_TO_STRING )
{
   PHB_ITEM    pNode  = hb_param( 1, HB_IT_OBJECT );
   PHB_ITEM    pStyle = hb_param( 2, HB_IT_NUMERIC );
   MXML_SGS *  sgs;
   MXML_OUTPUT out;
   int         iStyle;

   if( pNode == NULL )
   {
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   if( pStyle == NULL )
      iStyle = 0;
   else
      iStyle = hb_itemGetNI( pStyle );

   sgs = mxml_sgs_new();
   mxml_output_setup( &out, mxml_output_func_to_sgs, 0 );
   out.u.vPtr = ( void * ) sgs;

   if( mxml_node_write( &out, pNode, iStyle ) == MXML_STATUS_OK )
      hb_retclen( mxml_sgs_extract( sgs ), sgs->length );

   mxml_sgs_destroy( sgs );
}

/**
 * HB_XmlWrite( xmlDocument, nFileHandle, nStyle ) --> nStatus
 *
 * Writes an XML document to a file; returns the HB_XML status.
 */

HB_FUNC( HBXML_NODE_WRITE )
{
   PHB_ITEM    pNode   = hb_param( 1, HB_IT_OBJECT );
   PHB_ITEM    pHandle = hb_param( 2, HB_IT_NUMERIC );
   PHB_ITEM    pStyle  = hb_param( 3, HB_IT_NUMERIC );
   MXML_OUTPUT out;
   int         iStyle, iRet;

   if( pNode == NULL || pHandle == NULL )
   {
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   if( pStyle == NULL )
      iStyle = 0;
   else
      iStyle = hb_itemGetNI( pStyle );

   mxml_output_setup( &out, mxml_output_func_to_handle, 0 );
   out.u.hFile = ( HB_FHANDLE ) hb_itemGetNInt( pHandle );

   iRet = mxml_node_write( &out, pNode, iStyle );
   hb_retni( iRet );
}
