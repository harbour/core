/*
 * Harbour Project source code:
 * Base Object from which all object finally inherit
 *
 * Copyright 2000 J. Lefebvre <jfl@mafact.com> and RA. Cuylen <rac@mafact.com>
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

/* WARNING: Can not use the preprocessor, otherwise
            it will auto inherit from itself. */

#include "hboo.ch"
#include "error.ch"

FUNCTION HBObject()

   STATIC s_oClass
   LOCAL oClass

   IF s_oClass == NIL .AND. __clsLockDef( @s_oClass )

      BEGIN SEQUENCE

         oClass := HBClass():New( "HBObject",, @HBObject() )

#ifndef HB_CLP_STRICT
         oClass:AddInline( "ISDERIVEDFROM"   , {| Self, xPar1 | __objDerivedFrom( Self, xPar1 ) }, HB_OO_CLSTP_EXPORTED ) /* Xbase++ compatibility */
#endif
         /* Class(y) */
         oClass:AddInline( "ISKINDOF"        , {| Self, xPar1 | __objDerivedFrom( Self, xPar1 ) }, HB_OO_CLSTP_EXPORTED )

         oClass:AddMethod( "NEW"  , @HBObject_New()   , HB_OO_CLSTP_EXPORTED )
         oClass:AddMethod( "INIT" , @HBObject_Init()  , HB_OO_CLSTP_EXPORTED )

         oClass:AddMethod( "ERROR", @HBObject_Error() , HB_OO_CLSTP_EXPORTED )

         oClass:SetOnError( @HBObject_DftonError() )

         oClass:AddInline( "MSGNOTFOUND"     , {| Self, cMsg | ::Error( "Message not found", ::className(), cMsg, iif( hb_LeftEq( cMsg, "_" ), 1005, 1004 ) ) }, HB_OO_CLSTP_EXPORTED )

#if 0
         oClass:AddMultiData( , , HB_OO_CLSTP_EXPORTED, { "CLASS" }, .F. )

         oClass:AddInline( "ADDMETHOD" , {| Self, cMeth, pFunc, nScopeMeth         | __clsAddMsg( __classH( Self ), cMeth, pFunc, HB_OO_MSG_METHOD , NIL, iif( nScopeMeth == NIL, 1, nScopeMeth ) ) }, HB_OO_CLSTP_EXPORTED )
         oClass:AddInline( "ADDVAR"    , {| Self, cVAR, nScopeMeth, uiData, hClass | __clsAddMsg( hClass := __classH( Self ),     cVar , uidata := __cls_IncData( hClass ), HB_OO_MSG_ACCESS, NIL, iif( nScopeMeth == NIL, 1, nScopeMeth ) )  , ;
                                                                                     __clsAddMsg( hClass                    , "_"+cVar , uiData                           , HB_OO_MSG_ASSIGN, NIL, iif( nScopeMeth == NIL, 1, nScopeMeth ) ) }, HB_OO_CLSTP_EXPORTED )

         /* These ones exist within Class(y), so we will probably try to implement it */

         oClass:AddInline( "asString"       , {| Self | ::class:name + " object"   }, HB_OO_CLSTP_EXPORTED )
         oClass:AddInline( "asExpStr"       , {| Self |                            }, HB_OO_CLSTP_EXPORTED )
         oClass:AddInline( "basicSize"      , {| Self | Len( Self )                }, HB_OO_CLSTP_EXPORTED )
         oClass:AddInline( "become"         , {| Self |                            }, HB_OO_CLSTP_EXPORTED )
         oClass:AddInline( "isEqual"        , {| Self |                            }, HB_OO_CLSTP_EXPORTED )
         oClass:AddInline( "isScalar"       , {| Self |                            }, HB_OO_CLSTP_EXPORTED )
         oClass:AddInline( "copy"           , {| Self |                            }, HB_OO_CLSTP_EXPORTED )
         oClass:AddInline( "deepCopy"       , {| Self |                            }, HB_OO_CLSTP_EXPORTED )

         oClass:AddInline( "deferred"       , {| Self |                            }, HB_OO_CLSTP_EXPORTED )

         oClass:AddInline( "exec"           , {| Self |                            }, HB_OO_CLSTP_EXPORTED )
         oClass:AddInline( "error"          , {| Self |                            }, HB_OO_CLSTP_EXPORTED )
         oClass:AddInline( "hash"           , {| Self |                            }, HB_OO_CLSTP_EXPORTED )
         oClass:AddInline( "null"           , {| Self |                            }, HB_OO_CLSTP_EXPORTED )
         oClass:AddInline( "size"           , {| Self | Len( Self )                }, HB_OO_CLSTP_EXPORTED )

         /* No idea when those two could occur !!? */
         oClass:AddInline( "wrongClass"     , {| Self |                            }, HB_OO_CLSTP_EXPORTED )
         oClass:AddInline( "badMethod"      , {| Self |                            }, HB_OO_CLSTP_EXPORTED )

         /* this one exist within VO and seem to be Auto Called when object ran out of scope */
         oClass:AddInline( "Axit"           , {| Self |                            }, HB_OO_CLSTP_EXPORTED )
#endif

         oClass:Create()

      ALWAYS

         __clsUnlockDef( @s_oClass, oClass )

      END SEQUENCE

   ENDIF

#if 0
   oInstance := s_oClass:Instance()
   oInstance:class := s_oClass

   RETURN oInstance
#endif

   RETURN s_oClass:Instance()

STATIC FUNCTION HBObject_New( ... )

   QSelf():Init( ... )

   RETURN QSelf()

STATIC FUNCTION HBObject_Init()
   RETURN QSelf()

STATIC FUNCTION HBObject_Dftonerror( ... )
   RETURN QSelf():MsgNotFound( __GetMessage(), ... )

STATIC FUNCTION HBObject_Error( cDesc, cClass, cMsg, nCode )

   hb_default( @nCode, 1004 )

   RETURN __errRT_SBASE( iif( nCode == 1005, EG_NOVARMETHOD, EG_NOMETHOD ), nCode, cDesc, cClass + ":" + cMsg, 1, QSelf() )
