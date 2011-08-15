/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * CUI Forms Editor
 *
 * Copyright 2011 Pritpal Bedi <bedipritpal@hotmail.com>
 * http://harbour-project.org
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
 * along with this software; see the file COPYING.  If not, write to
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                       Harbour CUI Editor Source
 *
 *                             Pritpal Bedi
 *                               13Aug2011
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbcuied.ch"
#include "common.ch"
#include "hbgtinfo.ch"

//----------------------------------------------------------------------//

#define REP_MSG_WAIT_YES  .T.
#define REP_MSG_WAIT_NO   .F.

//----------------------------------------------------------------------//

FUNCTION dgn_Screen( cObject, cScreen )
   LOCAL rpt_:={}
   LOCAL bErr

   bErr := errorBlock( {|| Break() } )

   IF ! empty( rpt_:= rptDefine( cObject, rpt_, REP_FOR_MDL,/*struct_*/, cScreen, 1, REP_MSG_WAIT_YES ) )
      rptSave( cObject, /*cRpt*/, rpt_, 1, REP_FOR_MDL, REP_MSG_WAIT_YES )
   ENDIF

   errorBlock( bErr )

   RETURN NIL

//---------------------------------------------------------------------//

STATIC FUNCTION rptDefine( cObject,rpt_,nMode,struct_,cRpt,nWhere,lMsg )
   LOCAL scn_,a_,obj_

   a_  := rpt2ObjScn( cObject,rpt_,nMode,struct_,cRpt,nWhere,lMsg )
   obj_:= a_[ 1 ]
   scn_:= a_[ 2 ]

   a_  := Operate(obj_,scn_)

   obj_:= a_[ 1 ]
   scn_:= a_[ 2 ]
   rpt_:= objScn2rpt( obj_,scn_,nMode )

   RETURN rpt_

//----------------------------------------------------------------------//

FUNCTION rptSave( /* cObject,cRpt,rRpt_,nWhere,nMode,lMsgWait */ )
   RETURN .t.

//---------------------------------------------------------------------//

STATIC FUNCTION rptInit()
   RETURN { { '',0,'' } }

//---------------------------------------------------------------------//

FUNCTION rpt2ObjScn( cObject,rpt_,nMode,struct_,cRpt,nWhere,lMsg )
   LOCAL obj_,scn_:={}
   LOCAL n

   HB_SYMBOL_UNUSED( struct_ )

   rpt_:= iif( rpt_ == NIL .OR. empty( rpt_ ), rptInit(), rpt_ )
   obj_:= scrScn2obj(rpt_,0)

   DO CASE

   CASE nMode == REP_FOR_MDL
      scn_:= scrConfig( obj_,DGN_MODULE )
      IF (n := ascan( rpt_, {|e_| e_[2]==1 } ) ) > 0
         scn_[ SCN_PROPERTY ] := eval( COMPILE( rpt_[ n,3 ] ) )
      ENDIF
      #if 0
      IF (n := ascan(rpt_,{|e_| e_[2]== 51 })) > 0   //  Property
         scn_[ SCN_PROPERTY ] := prpStr2Mdl( rpt_[ n,3 ] )
      ENDIF
      #endif
      #if 0
      FOR i := 1 TO len( rpt_ )                        //  Fields
         IF VouchInRange( rpt_[i,2], 2001,3000 )
            aadd( scn_[ SCN_FIELDS ], prpStr2Fld( rpt_[ i,3 ] ) )
         ENDIF
      NEXT
      #endif

   ENDCASE

   scn_[SCN_FILE    ] := iif( empty( rpt_[ 1,1 ] ), 'Untitled', substr( rpt_[ 1,1 ], 13 ) )

   scn_[SCN_COBJECT ] := cObject   //  FOR threaded usage
   scn_[SCN_CRPT    ] := cRpt
   scn_[SCN_NWHERE  ] := nWhere
   scn_[SCN_NMODE   ] := nMode
   scn_[SCN_LMSG    ] := lMsg

   RETURN {obj_,scn_}

//----------------------------------------------------------------------//

FUNCTION objScn2Rpt( obj_, scn_, nMode )
   LOCAL rpt_:={}

   HB_SYMBOL_UNUSED( scn_ )

   DO CASE
   CASE nMode == REP_FOR_MDL
      aeval( obj_, {|e_| iif( e_[ OBJ_ROW ] == 0, '', aadd( rpt_, { '', 0, scrObj2str( e_ ) } ) ) } )
      #if 0
      IF ! empty( scn_[ SCN_PROPERTY ] )
         aadd(rpt_, { '', 51, prpMdl2Str( scn_[ SCN_PROPERTY ] ) } )
      ENDIF
      #endif
      #if 0
      IF !empty(scn_[SCN_FIELDS])
         FOR i := 1 TO len(scn_[SCN_FIELDS])
            aadd(rpt_,{'',scn_[SCN_FIELDS,i,1], prpFld2Str(scn_[SCN_FIELDS,i]) })
         NEXT
      ENDIF
      #endif
   ENDCASE

   RETURN rpt_

//----------------------------------------------------------------------//

