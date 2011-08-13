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

/*----------------------------------------------------------------------*/

#define FLD_UNIQUE                                1   //  N    4
                                                      
#define FLD_ALIAS                                 2   //  C    8
#define FLD_HEADING                               3   //  C   15
#define FLD_TYPE                                  4   //  C    1
#define FLD_LEN                                   5   //  N    3
#define FLD_DEC                                   6   //  N    3
#define FLD_ORD_NAT                               7   //  N    3
#define FLD_FLD_NO                                8   //  N    3
                                                      
#define FLD_INDEXED                               9   //  L    1
#define FLD_INDEX_NO                              10  //  N    3
                                                  
#define FLD_CONFIGURE                             11  //  L    1
#define FLD_CALCULATED                            12  //  L    1
#define FLD_TRIGGER                               13  //  L    1
                                                  
#define FLD_ORD_GET                               14  //  N    3
#define FLD_EDITABLE                              15  //  L    1
#define FLD_DATA                                  16  //  C   60
#define FLD_WHEN                                  17  //  C  100
#define FLD_VALID                                 18  //  C  100
                                                  
#define FLD_ORD_BRW                               19  //  N    3
#define FLD_BROWSE                                20  //  L    1
#define FLD_BRW_INIT                              21  //  L    1
#define FLD_BRW_VIS                               22  //  L    1
                                                  
#define FLD_HELP                                  23  //  C    8
#define FLD_PG_TOTAL                              24  //  L    1
                                                  
#define FLD_F_FLD                                 25  //  N    3
#define FLD_F_ALIAS                               26  //  C    8
#define FLD_F_LOOK_FLD                            27  //  N    3
#define FLD_F_RTN_FLD                             28  //  N    3
                                                  
#define FLD_ENCRYPTED                             29  //  L    1
#define FLD_ENK_BASE                              30  //  N    3
#define FLD_ENK_ALOG                              31  //  N    3
#define FLD_FLD_ATTR                              32  //  N    2
                                                      //     ---
                                                      //     352
#define FLD_INIT_VRBLS                            32

/*----------------------------------------------------------------------*/

#define FLD_LEN_UNIQUE                            4
                                                  
#define FLD_LEN_ALIAS                             8
#define FLD_LEN_HEADING                           15        
#define FLD_LEN_TYPE                              1
#define FLD_LEN_LEN                               3
#define FLD_LEN_DEC                               2
#define FLD_LEN_ORD_NAT                           3
#define FLD_LEN_FLD_NO                            3
                                                  
#define FLD_LEN_INDEXED                           1
#define FLD_LEN_INDEX_NO                          3
                                                  
#define FLD_LEN_CONFIGURE                         1
#define FLD_LEN_CALCULATED                        1
#define FLD_LEN_TRIGGER                           1
                                                  
#define FLD_LEN_ORD_GET                           3
#define FLD_LEN_EDITABLE                          1
#define FLD_LEN_DATA                              60 
#define FLD_LEN_WHEN                              100
#define FLD_LEN_VALID                             100
                                                  
#define FLD_LEN_ORD_BRW                           3
#define FLD_LEN_BROWSE                            1
#define FLD_LEN_BRW_INIT                          1
#define FLD_LEN_BRW_VIS                           1
                                                  
#define FLD_LEN_HELP                              8
#define FLD_LEN_PG_TOTAL                          1
                                                  
#define FLD_LEN_F_FLD                             3
#define FLD_LEN_F_ALIAS                           8
#define FLD_LEN_F_LOOK_FLD                        3
#define FLD_LEN_F_RTN_FLD                         3 
                                                  
#define FLD_LEN_ENCRYPTED                         1
#define FLD_LEN_ENK_BASE                          3
#define FLD_LEN_ENK_ALOG                          3
                                                  
#define FLD_LEN_FLD_ATTR                          2

/*----------------------------------------------------------------------*/

#define FLD_OS_UNIQUE                             1   //   4
                                                      
#define FLD_OS_ALIAS                              5   //   8
#define FLD_OS_HEADING                            13  //  15        
#define FLD_OS_TYPE                               28  //   1
#define FLD_OS_LEN                                29  //   3
#define FLD_OS_DEC                                32  //   2
#define FLD_OS_ORD_NAT                            34  //   3
#define FLD_OS_FLD_NO                             37  //   3
                                                  
#define FLD_OS_INDEXED                            40  //   1
#define FLD_OS_INDEX_NO                           41  //   3
                                                  
#define FLD_OS_CONFIGURE                          44  //   1
#define FLD_OS_CALCULATED                         45  //   1
#define FLD_OS_TRIGGER                            46  //   1
                                                  
#define FLD_OS_ORD_GET                            47  //   3
#define FLD_OS_EDITABLE                           50  //   1
#define FLD_OS_DATA                               51  //  60 
#define FLD_OS_WHEN                               111  //-----100
#define FLD_OS_VALID                              211  //-----100
                                                  
#define FLD_OS_ORD_BRW                            311  //    3
#define FLD_OS_BROWSE                             314  //    1
#define FLD_OS_BRW_INIT                           315  //    1
#define FLD_OS_BRW_VIS                            316  //    1
                                                  
#define FLD_OS_HELP                               317  //    8
#define FLD_OS_PG_TOTAL                           325  //    1
                                                  
#define FLD_OS_F_FLD                              326  //    3
#define FLD_OS_F_ALIAS                            329  //    8
#define FLD_OS_F_LOOK_FLD                         337  //    3
#define FLD_OS_F_RTN_FLD                          340  //    3 
                                                  
#define FLD_OS_ENCRYPTED                          343  //    1
#define FLD_OS_ENK_BASE                           344  //    3
#define FLD_OS_ENK_ALOG                           347  //    3
#define FLD_OS_FLD_ATTR                           350  //    2
                         

//----------------------------------------------------------------------//

#define PRM_TYPE                                  1   //  C   1
#define PRM_ALIAS                                 2   //  C   8
#define PRM_DESC                                  3   //  C  35
#define PRM_FILE_1                                4   //  C   8
#define PRM_FILE_2                                5   //  C   8
#define PRM_FILE_3                                6   //  C   8
#define PRM_PK                                    7   //  N   3
#define PRM_DK                                    8   //  N   3
#define PRM_PROTO                                 9   //  N   2
#define PRM_PATH                                  10  //  C  80
#define PRM_DELETE                                11  //  C  80
#define PRM_PREVALIDATE                           12  //  C  80
#define PRM_INITIALIZE                            13  //  C  80
#define PRM_ATTACHED                              14  //  L   1
#define PRM_MODE                                  15  //  C   1
#define PRM_SHORT                                 16  //  C   3
                                                  
#define PRM_INIT_VRBLS                            16

//----------------------------------------------------------------------//

#define PRM_LEN_TYPE                              1
#define PRM_LEN_ALIAS                             8  //  C   8
#define PRM_LEN_DESC                              35 //  C  35
#define PRM_LEN_FILE_1                            8  //  C   8
#define PRM_LEN_FILE_2                            8  //  C   8
#define PRM_LEN_FILE_3                            8  //  C   8
#define PRM_LEN_PK                                3  //  N   3
#define PRM_LEN_DK                                3  //  N   3
#define PRM_LEN_PROTO                             2  //  N   2
#define PRM_LEN_PATH                              80 //  C  80
#define PRM_LEN_DELETE                            80 //  C  80
#define PRM_LEN_PREVALIDATE                       80 //  C  80
#define PRM_LEN_INITIALIZE                        80 //  C  80
#define PRM_LEN_ATTACHED                          1  //  L   1
#define PRM_LEN_MODE                              1  //  C   1
#define PRM_LEN_SHORT                             3  //  C   3

//----------------------------------------------------------------------//

#define PRM_OS_TYPE                               1  //  C   1
#define PRM_OS_ALIAS                              2  //  C   8
#define PRM_OS_DESC                               10  //  C  35
#define PRM_OS_FILE_1                             45  //  C   8
#define PRM_OS_FILE_2                             53  //  C   8
#define PRM_OS_FILE_3                             61  //  C   8
#define PRM_OS_PK                                 69  //  N   3
#define PRM_OS_DK                                 72  //  N   3
#define PRM_OS_PROTO                              75  //  N   2
#define PRM_OS_PATH                               77  //  C  80
#define PRM_OS_DELETE                             157 //  C  80
#define PRM_OS_PREVALIDATE                        237 //  C  80
#define PRM_OS_INITIALIZE                         317 //  C  80
#define PRM_OS_ATTACHED                           397 //  L   1
#define PRM_OS_MODE                               398 //  C   1
#define PRM_OS_SHORT                              399 //  C   3

//----------------------------------------------------------------------//

#define FATR_CHR                                  1
#define FATR_DTE                                  2
#define FATR_NUM                                  3
#define FATR_LOG                                  4
#define FATR_PKS                                  5
#define FATR_PKM                                  6
#define FATR_DTF                                  7
#define FATR_FRN                                  8
#define FATR_INF                                  9
#define FATR_SUM                                  10
#define FATR_TRG                                  11
#define FATR_REF                                  12
#define FATR_OPN                                  13

//----------------------------------------------------------------------//

FUNCTION scrMdlFld( obj_, scn_, nObj )
   LOCAL v_, n, sel_
    
   sel_:= scrVvSelAble( scn_ )
   v_  := IF( nObj > 0, scrObj2Vv( obj_[ nObj ] ), scrVvBlank() )
   n   := v_[ VV_FIELD ]
   IF nObj == 0
      v_[ VV_FIELD   ] := 0
      v_[ VV_ID      ] := space( 10 )
      v_[ VV_F_PIC   ] := space( 15 )
      v_[ VV_COLOR   ] := 'N/W   '
   ENDIF   

   IF .T.
      v_[ VV_EQN        ] := 'fieldget('+ ltrim( str( n ) ) + ')'
      
      sel_[ VV_ALIGN    ] := .f.
      sel_[ VV_PRN_LEN  ] := .f.
//      sel_[ VV_ID       ] := .f.
      sel_[ VV_PRN_LEN  ] := .f.
      sel_[ VV_ALIGN    ] := .f.
      sel_[ VV_COLOR    ] := .f.
      sel_[ VV_POINT    ] := .f.
      sel_[ VV_COL_JUST ] := .f.
      sel_[ VV_PATTERN  ] := .f.
      sel_[ VV_F_TYPE   ] := .T.
      
      scrField( nObj, 1, obj_, scn_, v_, sel_, OBJ_CLR_FIELD )   
      IF nObj > 0
         scrOrdObj( obj_ )
      ENDIF   
   ENDIF   
   RETURN NIL

//----------------------------------------------------------------------//

STATIC FUNCTION scrFldUnique(obj_)
   LOCAL nUnq := 2001
   DO WHILE .t.
      IF ascan(obj_,{|e_| e_[OBJ_OBJ_UNIQUE] == nUnq }) == 0
         EXIT
      ENDIF
      nUnq++   
   ENDDO
   RETURN nUnq

//----------------------------------------------------------------------//
#IF 0
STATIC FUNCTION prpObj2Fld( obj_ )        //  Akin TO Blank

   LOCAL v_:= array( FLD_INIT_VRBLS )
   LOCAL idx_:={ .f.,.f.,.f.,.f.,.t.,.t.,.t.,.t.,.f.,.f.,.f.,.f.,.f. }
   LOCAL nAttr := obj_[ OBJ_MDL_F_TYPE ]
   
   v_[FLD_UNIQUE      ] := obj_[OBJ_OBJ_UNIQUE]
   v_[FLD_ALIAS       ] := space(8)
   v_[FLD_HEADING     ] := obj_[OBJ_ID]
   v_[FLD_TYPE        ] := obj_[OBJ_F_TYPE]
   v_[FLD_LEN         ] := obj_[OBJ_F_LEN]
   v_[FLD_DEC         ] := obj_[OBJ_F_DEC]
   v_[FLD_ORD_NAT     ] := 0
   v_[FLD_FLD_NO      ] := 0
   v_[FLD_INDEXED     ] := idx_[nAttr]
   v_[FLD_INDEX_NO    ] := 0
   v_[FLD_CONFIGURE   ] := VouchInArray(nAttr,{FATR_NUM,FATR_LOG,FATR_CHR,;
                              FATR_DTE,FATR_REF,FATR_TRG,FATR_INF})
   v_[FLD_CALCULATED  ] := VouchInArray(nAttr,{FATR_NUM,FATR_LOG,FATR_CHR,;
                              FATR_DTE,FATR_INF})
   v_[FLD_TRIGGER     ] := nAttr == FATR_TRG .OR. nAttr == FATR_REF
   v_[FLD_ORD_GET     ] := 0
   v_[FLD_EDITABLE    ] := !VouchInArray(nAttr,{FATR_PKS,FATR_PKM})
   v_[FLD_DATA        ] := space(FLD_LEN_DATA)
   v_[FLD_WHEN        ] := space(FLD_LEN_WHEN)
   v_[FLD_VALID       ] := space(FLD_LEN_VALID)
   v_[FLD_ORD_BRW     ] := 0
   v_[FLD_BROWSE      ] := nAttr <> FATR_TRG .OR. nAttr <> FATR_REF
   v_[FLD_BRW_INIT    ] := nAttr <> FATR_TRG .OR. nAttr <> FATR_REF
   v_[FLD_BRW_VIS     ] := nAttr <> FATR_TRG .OR. nAttr <> FATR_REF
   v_[FLD_HELP        ] := space(8)
   v_[FLD_PG_TOTAL    ] := obj_[OBJ_F_TYPE]=='N'
   v_[FLD_F_FLD       ] := 0
   v_[FLD_F_ALIAS     ] := space(8)
   v_[FLD_F_LOOK_FLD  ] := 0
   v_[FLD_F_RTN_FLD   ] := 0
   v_[FLD_ENCRYPTED   ] := .t.
   v_[FLD_ENK_BASE    ] := b_rand(254)
   v_[FLD_ENK_ALOG    ] := b_rand(19)
   v_[FLD_FLD_ATTR    ] := nAttr
   RETURN v_
#ENDIF
//----------------------------------------------------------------------//

FUNCTION prpFld2Str(v_)
   LOCAL s := ;
   str(v_[FLD_UNIQUE      ], FLD_LEN_UNIQUE )+ ;
   pad(v_[FLD_ALIAS       ], FLD_LEN_ALIAS )+ ;
   pad(v_[FLD_HEADING     ], FLD_LEN_HEADING )+ ;
   pad(v_[FLD_TYPE        ], FLD_LEN_TYPE )+ ;
   str(v_[FLD_LEN         ], FLD_LEN_LEN )+ ;
   str(v_[FLD_DEC         ], FLD_LEN_DEC )+ ;
   str(v_[FLD_ORD_NAT     ], FLD_LEN_ORD_NAT )+ ;
   str(v_[FLD_FLD_NO      ], FLD_LEN_FLD_NO )+ ;
    IF(v_[FLD_INDEXED     ], 'T', 'F' )+ ;
   str(v_[FLD_INDEX_NO    ], FLD_LEN_INDEX_NO )+ ;
    IF(v_[FLD_CONFIGURE   ], 'T', 'F' )+ ;
    IF(v_[FLD_CALCULATED  ], 'T', 'F' )+ ;
    IF(v_[FLD_TRIGGER     ], 'T', 'F' )+ ;
   str(v_[FLD_ORD_GET     ], FLD_LEN_ORD_GET )+ ;
    IF(v_[FLD_EDITABLE    ], 'T', 'F' )+ ;
   pad(v_[FLD_DATA        ], FLD_LEN_DATA )+ ;
   pad(v_[FLD_WHEN        ], FLD_LEN_WHEN )+ ;
   pad(v_[FLD_VALID       ], FLD_LEN_VALID )+ ;
   str(v_[FLD_ORD_BRW     ], FLD_LEN_ORD_BRW )+ ;
    IF(v_[FLD_BROWSE      ], 'T', 'F' )+ ;
    IF(v_[FLD_BRW_INIT    ], 'T', 'F' )+ ;
    IF(v_[FLD_BRW_VIS     ], 'T', 'F' )+ ;
   pad(v_[FLD_HELP        ], FLD_LEN_HELP )+ ;
    IF(v_[FLD_PG_TOTAL    ], 'T', 'F' )+ ;
   str(v_[FLD_F_FLD       ], FLD_LEN_F_FLD )+ ;
   pad(v_[FLD_F_ALIAS     ], FLD_LEN_F_ALIAS )+ ;
   str(v_[FLD_F_LOOK_FLD  ], FLD_LEN_F_LOOK_FLD )+ ;
   str(v_[FLD_F_RTN_FLD   ], FLD_LEN_F_RTN_FLD )+ ;
    IF(v_[FLD_ENCRYPTED   ], 'T', 'F' )+ ;
   str(v_[FLD_ENK_BASE    ], FLD_LEN_ENK_BASE )+ ;
   str(v_[FLD_ENK_ALOG    ], FLD_LEN_ENK_ALOG )+ ;
   str(v_[FLD_FLD_ATTR    ], FLD_LEN_FLD_ATTR )
   RETURN s

//----------------------------------------------------------------------//

FUNCTION prpStr2Fld(s)
   LOCAL v_:= array(FLD_INIT_VRBLS)
   
   v_[FLD_UNIQUE      ] := val(substr(s, FLD_OS_UNIQUE     , FLD_LEN_UNIQUE    ))
   v_[FLD_ALIAS       ] :=     substr(s, FLD_OS_ALIAS      , FLD_LEN_ALIAS     )
   v_[FLD_HEADING     ] :=     substr(s, FLD_OS_HEADING    , FLD_LEN_HEADING   )
   v_[FLD_TYPE        ] :=     substr(s, FLD_OS_TYPE       , FLD_LEN_TYPE      )
   v_[FLD_LEN         ] := val(substr(s, FLD_OS_LEN        , FLD_LEN_LEN       ))
   v_[FLD_DEC         ] := val(substr(s, FLD_OS_DEC        , FLD_LEN_DEC       ))
   v_[FLD_ORD_NAT     ] := val(substr(s, FLD_OS_ORD_NAT    , FLD_LEN_ORD_NAT   ))
   v_[FLD_FLD_NO      ] := val(substr(s, FLD_OS_FLD_NO     , FLD_LEN_FLD_NO    ))
   v_[FLD_INDEXED     ] :=     substr(s, FLD_OS_INDEXED    , FLD_LEN_INDEXED   )=='T'
   v_[FLD_INDEX_NO    ] := val(substr(s, FLD_OS_INDEX_NO   , FLD_LEN_INDEX_NO  ))
   v_[FLD_CONFIGURE   ] :=     substr(s, FLD_OS_CONFIGURE  , FLD_LEN_CONFIGURE )=='T'
   v_[FLD_CALCULATED  ] :=     substr(s, FLD_OS_CALCULATED , FLD_LEN_CALCULATED)=='T'
   v_[FLD_TRIGGER     ] :=     substr(s, FLD_OS_TRIGGER    , FLD_LEN_TRIGGER   )=='T'
   v_[FLD_ORD_GET     ] := val(substr(s, FLD_OS_ORD_GET    , FLD_LEN_ORD_GET   ))
   v_[FLD_EDITABLE    ] :=     substr(s, FLD_OS_EDITABLE   , FLD_LEN_EDITABLE  )=='T'
   v_[FLD_DATA        ] :=     substr(s, FLD_OS_DATA       , FLD_LEN_DATA      )
   v_[FLD_WHEN        ] :=     substr(s, FLD_OS_WHEN       , FLD_LEN_WHEN      )
   v_[FLD_VALID       ] :=     substr(s, FLD_OS_VALID      , FLD_LEN_VALID     )
   v_[FLD_ORD_BRW     ] := val(substr(s, FLD_OS_ORD_BRW    , FLD_LEN_ORD_BRW   ))
   v_[FLD_BROWSE      ] :=     substr(s, FLD_OS_BROWSE     , FLD_LEN_BROWSE    )=='T'
   v_[FLD_BRW_INIT    ] :=     substr(s, FLD_OS_BRW_INIT   , FLD_LEN_BRW_INIT  )=='T'
   v_[FLD_BRW_VIS     ] :=     substr(s, FLD_OS_BRW_VIS    , FLD_LEN_BRW_VIS   )=='T'
   v_[FLD_HELP        ] :=     substr(s, FLD_OS_HELP       , FLD_LEN_HELP      )
   v_[FLD_PG_TOTAL    ] :=     substr(s, FLD_OS_PG_TOTAL   , FLD_LEN_PG_TOTAL  )=='T'
   v_[FLD_F_FLD       ] := val(substr(s, FLD_OS_F_FLD      , FLD_LEN_F_FLD     ))
   v_[FLD_F_ALIAS     ] :=     substr(s, FLD_OS_F_ALIAS    , FLD_LEN_F_ALIAS   )
   v_[FLD_F_LOOK_FLD  ] := val(substr(s, FLD_OS_F_LOOK_FLD , FLD_LEN_F_LOOK_FLD))
   v_[FLD_F_RTN_FLD   ] := val(substr(s, FLD_OS_F_RTN_FLD  , FLD_LEN_F_RTN_FLD ))
   v_[FLD_ENCRYPTED   ] :=     substr(s, FLD_OS_ENCRYPTED  , FLD_LEN_ENCRYPTED )=='T'
   v_[FLD_ENK_BASE    ] := val(substr(s, FLD_OS_ENK_BASE   , FLD_LEN_ENK_BASE  ))
   v_[FLD_ENK_ALOG    ] := val(substr(s, FLD_OS_ENK_ALOG   , FLD_LEN_ENK_ALOG  ))
   v_[FLD_FLD_ATTR    ] := val(substr(s, FLD_OS_FLD_ATTR   , FLD_LEN_FLD_ATTR  ))
   
   RETURN v_

//----------------------------------------------------------------------//

FUNCTION prpMdlBlank()
   LOCAL v_:= array(PRM_INIT_VRBLS)
   
   v_[PRM_TYPE       ] := 'S'
   v_[PRM_ALIAS      ] := 'Z7_NEW  '
   v_[PRM_DESC       ] := 'The NEXT Module                   '
   v_[PRM_FILE_1     ] := 'Z7A00001'
   v_[PRM_FILE_2     ] := 'Z7B00002'
   v_[PRM_FILE_3     ] := 'Z7_NEW  '
   v_[PRM_PK         ] := 1
   v_[PRM_DK         ] := 2
   v_[PRM_PROTO      ] := 0
   v_[PRM_PATH       ] := space(PRM_LEN_PATH)
   v_[PRM_DELETE     ] := space(PRM_LEN_DELETE)
   v_[PRM_PREVALIDATE] := space(PRM_LEN_PREVALIDATE)
   v_[PRM_INITIALIZE ] := space(PRM_LEN_INITIALIZE)
   v_[PRM_ATTACHED   ] := .f. 
   v_[PRM_MODE       ] := 'V'
   v_[PRM_SHORT      ] := 'XXX'
   
   RETURN v_

//----------------------------------------------------------------------//

FUNCTION prpMdl2Str(v_)
   LOCAL s := ;
   pad(v_[PRM_TYPE       ], PRM_LEN_TYPE       ) +;
   pad(v_[PRM_ALIAS      ], PRM_LEN_ALIAS      ) +;
   pad(v_[PRM_DESC       ], PRM_LEN_DESC       ) +;
   pad(v_[PRM_FILE_1     ], PRM_LEN_FILE_1     ) +;
   pad(v_[PRM_FILE_2     ], PRM_LEN_FILE_2     ) +;
   pad(v_[PRM_FILE_3     ], PRM_LEN_FILE_3     ) +;
   str(v_[PRM_PK         ], PRM_LEN_PK         ) +;
   str(v_[PRM_DK         ], PRM_LEN_DK         ) +;
   str(v_[PRM_PROTO      ], PRM_LEN_PROTO      ) +;
   pad(v_[PRM_PATH       ], PRM_LEN_PATH       ) +;
   pad(v_[PRM_DELETE     ], PRM_LEN_DELETE     ) +;
   pad(v_[PRM_PREVALIDATE], PRM_LEN_PREVALIDATE) +;
   pad(v_[PRM_INITIALIZE ], PRM_LEN_INITIALIZE ) +;
    IF(v_[PRM_ATTACHED   ],'T','F')              +;
   pad(v_[PRM_MODE       ], PRM_LEN_MODE       ) +;
   pad(v_[PRM_SHORT      ], PRM_LEN_SHORT      )
   RETURN s

//----------------------------------------------------------------------//

FUNCTION prpStr2Mdl(s)
   LOCAL v_:= array(PRM_INIT_VRBLS)
   
   v_[PRM_TYPE       ] :=     substr(s, PRM_OS_TYPE       , PRM_LEN_TYPE       )
   v_[PRM_ALIAS      ] :=     substr(s, PRM_OS_ALIAS      , PRM_LEN_ALIAS      )
   v_[PRM_DESC       ] :=     substr(s, PRM_OS_DESC       , PRM_LEN_DESC       )
   v_[PRM_FILE_1     ] :=     substr(s, PRM_OS_FILE_1     , PRM_LEN_FILE_1     )
   v_[PRM_FILE_2     ] :=     substr(s, PRM_OS_FILE_2     , PRM_LEN_FILE_2     )
   v_[PRM_FILE_3     ] :=     substr(s, PRM_OS_FILE_3     , PRM_LEN_FILE_3     )
   v_[PRM_PK         ] := val(substr(s, PRM_OS_PK         , PRM_LEN_PK         ))
   v_[PRM_DK         ] := val(substr(s, PRM_OS_DK         , PRM_LEN_DK         ))
   v_[PRM_PROTO      ] := val(substr(s, PRM_OS_PROTO      , PRM_LEN_PROTO      ))
   v_[PRM_PATH       ] :=     substr(s, PRM_OS_PATH       , PRM_LEN_PATH       )
   v_[PRM_DELETE     ] :=     substr(s, PRM_OS_DELETE     , PRM_LEN_DELETE     )
   v_[PRM_PREVALIDATE] :=     substr(s, PRM_OS_PREVALIDATE, PRM_LEN_PREVALIDATE)
   v_[PRM_INITIALIZE ] :=     substr(s, PRM_OS_INITIALIZE , PRM_LEN_INITIALIZE )
   v_[PRM_ATTACHED   ] :=     substr(s, PRM_OS_ATTACHED   , PRM_LEN_ATTACHED   ) == 'T'
   v_[PRM_MODE       ] :=     substr(s, PRM_OS_MODE       , PRM_LEN_MODE       )
   v_[PRM_SHORT      ] :=     substr(s, PRM_OS_SHORT      , PRM_LEN_SHORT      )
   
   RETURN v_

//----------------------------------------------------------------------//

