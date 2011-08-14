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
#include "inkey.ch"

//----------------------------------------------------------------------//

FUNCTION scrProperty( obj_,scn_ )
   LOCAL prp_

   HB_SYMBOL_UNUSED( obj_ )

   DO CASE
   CASE scn_[ SCN_DESIGN ] == DGN_MODULE
      prp_:= iif( empty( scn_[SCN_PROPERTY ] ), prpMdlBlank(), scn_[ SCN_PROPERTY ] )
      //prp_:= prpModule( prp_ )  /* Fetch actual values */
      scn_[SCN_PROPERTY] := prp_

   ENDCASE

   RETURN NIL

//----------------------------------------------------------------------//
#IF 0
FUNCTION prpRStr2Ar(s,s1,s2)
   LOCAL prp_:= array(REP_INIT_VRBLS)

   IF empty(s)                    //  IF no property is defined, defaults
      RETURN  prpRepBlank()
   ENDIF

   prp_[ REP_DESC       ] :=      substr( s , REP_OS_DESC      , REP_LEN_DESC       )
   prp_[ REP_TYPE       ] := val( substr( s , REP_OS_TYPE      , REP_LEN_TYPE       ) )
   prp_[ REP_IDX_NO     ] := val( substr( s , REP_OS_IDX_NO    , REP_LEN_IDX_NO     ) )
   prp_[ REP_IDX_EXP    ] :=      substr( s , REP_OS_IDX_EXP   , REP_LEN_IDX_EXP    )
   prp_[ REP_PG_SIZE    ] :=      substr( s , REP_OS_PG_SIZE   , REP_LEN_PG_SIZE    )
   prp_[ REP_PG_LEN     ] := val( substr( s , REP_OS_PG_LEN    , REP_LEN_PG_LEN     ) )
   prp_[ REP_PG_WID     ] := val( substr( s , REP_OS_PG_WID    , REP_LEN_PG_WID     ) )
   prp_[ REP_LINES      ] := val( substr( s , REP_OS_LINES     , REP_LEN_LINES      ) )
   prp_[ REP_MG_TOP     ] := val( substr( s , REP_OS_MG_TOP    , REP_LEN_MG_TOP     ) )
   prp_[ REP_MG_LEFT    ] := val( substr( s , REP_OS_MG_LEFT   , REP_LEN_MG_LEFT    ) )
   prp_[ REP_MG_BOTTOM  ] := val( substr( s , REP_OS_MG_BOTTOM , REP_LEN_MG_BOTTOM  ) )
   prp_[ REP_EJ_BEFORE  ] :=      substr( s , REP_OS_EJ_BEFORE , REP_LEN_EJ_BEFORE  ) == 'T'
   prp_[ REP_EJ_AFTER   ] :=      substr( s , REP_OS_EJ_AFTER  , REP_LEN_EJ_AFTER   ) == 'T'
   prp_[ REP_PLAIN      ] := val( substr( s , REP_OS_PLAIN     , REP_LEN_PLAIN      ) )
   prp_[ REP_FTR_PAG    ] :=      substr( s , REP_OS_FTR_PAG   , REP_LEN_FTR_PAG    )
   prp_[ REP_SMR_NEXT   ] :=      substr( s , REP_OS_SMR_NEXT  , REP_LEN_SMR_NEXT   ) == 'T'
   prp_[ REP_HDR_PAGES  ] :=      substr( s , REP_OS_HDR_PAGES , REP_LEN_HDR_PAGES  )
   prp_[ REP_BNK_LINES  ] := val( substr( s , REP_OS_BNK_LINES , REP_LEN_BNK_LINES  ) )
   prp_[ REP_BNK_AFTER  ] := val( substr( s , REP_OS_BNK_AFTER , REP_LEN_BNK_AFTER  ) )
   prp_[ REP_DATA_BOX   ] :=      substr( s , REP_OS_DATA_BOX  , REP_LEN_DATA_BOX   ) == 'T'
   prp_[ REP_FILE       ] :=      substr( s , REP_OS_FILE      , REP_LEN_FILE       )
   prp_[ REP_COPIES     ] := val( substr( s , REP_OS_COPIES    , REP_LEN_COPIES     ) )
   prp_[ REP_TTL_PAGES  ] :=      substr( s , REP_OS_TTL_PAGES , REP_LEN_TTL_PAGES  )
   prp_[ REP_INIT_CPI   ] := val( substr( s , REP_OS_INIT_CPI  , REP_LEN_INIT_CPI   ) )
   prp_[ REP_PRN_QLY    ] :=      substr( s , REP_OS_PRN_QLY   , REP_LEN_PRN_QLY    )
   prp_[ REP_RECONFIRM  ] :=      substr( s , REP_OS_RECONFIRM , REP_LEN_RECONFIRM  ) == 'T'
   prp_[ REP_FTR_BOTTOM ] :=      substr( s , REP_OS_FTR_BOTTOM, REP_LEN_FTR_BOTTOM ) == 'T'
   prp_[ REP_HTML       ] :=      substr( s , REP_OS_HTML      , REP_LEN_HTML       ) == 'T'
   prp_[ REP_PRINTER    ] :=      substr( s , REP_OS_PRINTER   , REP_LEN_PRINTER    )
   prp_[ REP_ORIENT     ] :=      substr( s , REP_OS_ORIENT    , REP_LEN_ORIENT     )
   prp_[ REP_PREVIEW    ] :=      substr( s , REP_OS_PREVIEW   , REP_LEN_PREVIEW    ) == 'T'
   prp_[ REP_GRAY       ] :=      substr( s , REP_OS_GRAY      , REP_LEN_GRAY       ) == 'T'
   prp_[ REP_WINDLG     ] :=      substr( s , REP_OS_WINDLG    , REP_LEN_WINDLG     ) == 'T'
   prp_[ REP_DUPLEX     ] := val( substr( s , REP_OS_DUPLEX    , REP_LEN_DUPLEX     ) )
   prp_[ REP_COLS       ] := val( substr( s , REP_OS_COLS      , REP_LEN_COLS       ) )
   prp_[ REP_ROWS       ] := val( substr( s , REP_OS_ROWS      , REP_LEN_ROWS       ) )

   prp_[ REP_FOR        ] :=      substr( s1, REP_OS_FOR       , REP_LEN_FOR        )
   prp_[ REP_WHILE      ] :=      substr( s1, REP_OS_WHILE     , REP_LEN_WHILE      )
   prp_[ REP_FIRST      ] :=      substr( s1, REP_OS_FIRST     , REP_LEN_FIRST      )
   prp_[ REP_EXE_PRE    ] :=      substr( s2, REP_OS_EXE_PRE   , REP_LEN_EXE_PRE    )
   prp_[ REP_EXE_POST   ] :=      substr( s2, REP_OS_EXE_POST  , REP_LEN_EXE_POST   )

   prp_[ REP_ORIENT     ] := iif( empty( prp_[ REP_ORIENT ] ), 'P', prp_[ REP_ORIENT ] )

   RETURN prp_
#ENDIF
//----------------------------------------------------------------------//

FUNCTION prpRAr2Str(prp_,s,s1,s2)   //  Passed by reference

   s :=        pad( prp_[ REP_DESC       ]  , REP_LEN_DESC      ) +;
        pad( NTRIM( prp_[ REP_TYPE       ] ), REP_LEN_TYPE      ) +;
        pad( NTRIM( prp_[ REP_IDX_NO     ] ), REP_LEN_IDX_NO    ) +;
               pad( prp_[ REP_IDX_EXP    ]  , REP_LEN_IDX_EXP   ) +;
               pad( prp_[ REP_PG_SIZE    ]  , REP_LEN_PG_SIZE   ) +;
        pad( NTRIM( prp_[ REP_PG_LEN     ] ), REP_LEN_PG_LEN    ) +;
        pad( NTRIM( prp_[ REP_PG_WID     ] ), REP_LEN_PG_WID    ) +;
        pad( NTRIM( prp_[ REP_LINES      ] ), REP_LEN_LINES     ) +;
        pad( NTRIM( prp_[ REP_MG_TOP     ] ), REP_LEN_MG_TOP    ) +;
        pad( NTRIM( prp_[ REP_MG_LEFT    ] ), REP_LEN_MG_LEFT   ) +;
        pad( NTRIM( prp_[ REP_MG_BOTTOM  ] ), REP_LEN_MG_BOTTOM ) +;
               iif( prp_[ REP_EJ_BEFORE  ]  , 'T','F'           ) +;
               iif( prp_[ REP_EJ_AFTER   ]  , 'T','F'           ) +;
        pad( NTRIM( prp_[ REP_PLAIN      ] ), REP_LEN_PLAIN     ) +;
               pad( prp_[ REP_FTR_PAG    ]  , REP_LEN_FTR_PAG   ) +;
               iif( prp_[ REP_SMR_NEXT   ]  , 'T','F'           ) +;
               pad( prp_[ REP_HDR_PAGES  ]  , REP_LEN_HDR_PAGES ) +;
        pad( NTRIM( prp_[ REP_BNK_LINES  ] ), REP_LEN_BNK_LINES ) +;
        pad( NTRIM( prp_[ REP_BNK_AFTER  ] ), REP_LEN_BNK_AFTER ) +;
               iif( prp_[ REP_DATA_BOX   ]  ,'T','F'            ) +;
               pad( prp_[ REP_FILE       ]  , REP_LEN_FILE      ) +;
        pad( NTRIM( prp_[ REP_COPIES     ] ), REP_LEN_COPIES    ) +;
               pad( prp_[ REP_TTL_PAGES  ]  , REP_LEN_TTL_PAGES ) +;
        pad( NTRIM( prp_[ REP_INIT_CPI   ] ), REP_LEN_INIT_CPI  ) +;
               pad( prp_[ REP_PRN_QLY    ]  , REP_LEN_PRN_QLY   ) +;
               iif( prp_[ REP_RECONFIRM  ]  , 'T','F'           ) +;
               iif( prp_[ REP_FTR_BOTTOM ]  , 'T','F'           ) +;
               iif( prp_[ REP_HTML       ]  , 'T','F'           ) +;
               pad( prp_[ REP_PRINTER    ]  , REP_LEN_PRINTER   ) +;
               pad( prp_[ REP_ORIENT     ]  , REP_LEN_ORIENT    ) +;
               iif( prp_[ REP_PREVIEW    ]  , 'T','F'           ) +;
               iif( prp_[ REP_GRAY       ]  , 'T','F'           ) +;
               iif( prp_[ REP_WINDLG     ]  , 'T','F'           ) +;
        pad( NTRIM( prp_[ REP_DUPLEX     ] ), REP_LEN_DUPLEX    ) +;
        pad( NTRIM( prp_[ REP_COLS       ] ), REP_LEN_COLS      ) +;
        pad( NTRIM( prp_[ REP_ROWS       ] ), REP_LEN_ROWS      )

   s1 :=       pad( prp_[ REP_FOR        ] ,  REP_LEN_FOR       ) +;
               pad( prp_[ REP_WHILE      ] ,  REP_LEN_WHILE     ) +;
               pad( prp_[ REP_FIRST      ] ,  REP_LEN_FIRST     )

   s2 :=       pad( prp_[ REP_EXE_PRE    ] ,  REP_LEN_EXE_PRE   ) +;
               pad( prp_[ REP_EXE_POST   ] ,  REP_LEN_EXE_POST  )

   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION scrVvBlank()
   LOCAL v_:= array( VV_INIT_VRBLS )

   v_[ VV_ID         ]  := 'New            '
   v_[ VV_FIELD      ]  := 0
   v_[ VV_F_TYPE     ]  := 'C'
   v_[ VV_F_LEN      ]  := 25
   v_[ VV_F_DEC      ]  := 0
   v_[ VV_ATTRB      ]  := 'NONE    '
   v_[ VV_EQN        ]  := '               '
   v_[ VV_PRN_LEN    ]  := 25
   v_[ VV_F_PIC      ]  := '               '
   v_[ VV_PITCH      ]  := 10
   v_[ VV_FONT       ]  := 'COURIER '
   v_[ VV_BOLD       ]  := .f.
   v_[ VV_ITALIC     ]  := .f.
   v_[ VV_UNDERLN    ]  := .f.
   v_[ VV_S_SCRPT    ]  := .f.
   v_[ VV_U_SCRPT    ]  := .f.
   v_[ VV_HALF_H     ]  := .f.
   v_[ VV_ALIGN      ]  := 'C'
   v_[ VV_COLOR      ]  := 'W+/B   '
   v_[ VV_ZERO       ]  := .t.
   v_[ VV_REPEATED   ]  := 'NO    '
   v_[ VV_VERTICLE   ]  := .F.
   v_[ VV_WRAP_SEMI  ]  := .F.
   v_[ VV_FOR        ]  := space( 80 )
   v_[ VV_OBJ_UNIQUE ]  := 0
   v_[ VV_MDL_F_TYPE ]  := 0
   v_[ VV_POINT      ]  := 12
   v_[ VV_COL_JUST   ]  := 0
   v_[ VV_PATTERN    ]  := 'SOLID     '
   v_[ VV_BORDER     ]  := 0.50

   RETURN v_

//----------------------------------------------------------------------//

FUNCTION scrVvSelAble( /*scn_*/ )
   LOCAL sel_:= array( VV_INIT_VRBLS )
   LOCAL isPrint := .f.

   sel_[ VV_ID         ]  := .t.
   sel_[ VV_FIELD      ]  := .f.
   sel_[ VV_F_TYPE     ]  := .t.
   sel_[ VV_F_LEN      ]  := .t.
   sel_[ VV_F_DEC      ]  := .t.
   sel_[ VV_ATTRB      ]  := .f.
   sel_[ VV_EQN        ]  := .f.
   sel_[ VV_PRN_LEN    ]  := .T.
   sel_[ VV_F_PIC      ]  := .t.
   sel_[ VV_PITCH      ]  := isPrint
   sel_[ VV_FONT       ]  := isPrint
   sel_[ VV_BOLD       ]  := isPrint
   sel_[ VV_ITALIC     ]  := isPrint
   sel_[ VV_UNDERLN    ]  := isPrint
   sel_[ VV_S_SCRPT    ]  := .f.     //  isPrint
   sel_[ VV_U_SCRPT    ]  := .f.     //  isPrint
   sel_[ VV_HALF_H     ]  := isPrint
   sel_[ VV_ALIGN      ]  := .t.     //  isPrint
   sel_[ VV_COLOR      ]  := .t.     //  !isPrint
   sel_[ VV_ZERO       ]  := isPrint
   sel_[ VV_REPEATED   ]  := isPrint
   sel_[ VV_VERTICLE   ]  := isPrint
   sel_[ VV_WRAP_SEMI  ]  := isPrint
   sel_[ VV_FOR        ]  := isPrint
   sel_[ VV_OBJ_UNIQUE ]  := .f.
   sel_[ VV_MDL_F_TYPE ]  := .f.
   sel_[ VV_POINT      ]  := .T.
   sel_[ VV_COL_JUST   ]  := .t.
   sel_[ VV_PATTERN    ]  := .t.
   sel_[ VV_BORDER     ]  := .f.

   RETURN sel_

//----------------------------------------------------------------------//

FUNCTION scrVv2Obj( v_,o_ )

   o_[ OBJ_ID         ] := v_[ VV_ID         ]
   o_[ OBJ_FIELD      ] := v_[ VV_FIELD      ]
   o_[ OBJ_F_TYPE     ] := v_[ VV_F_TYPE     ]
   o_[ OBJ_F_LEN      ] := v_[ VV_F_LEN      ]
   o_[ OBJ_F_DEC      ] := v_[ VV_F_DEC      ]
   o_[ OBJ_ATTRB      ] := v_[ VV_ATTRB      ]
   o_[ OBJ_EQN        ] := v_[ VV_EQN        ]
   o_[ OBJ_PRN_LEN    ] := v_[ VV_PRN_LEN    ]
   o_[ OBJ_F_PIC      ] := v_[ VV_F_PIC      ]
   o_[ OBJ_PITCH      ] := v_[ VV_PITCH      ]
   o_[ OBJ_FONT       ] := v_[ VV_FONT       ]
   o_[ OBJ_BOLD       ] := v_[ VV_BOLD       ]
   o_[ OBJ_ITALIC     ] := v_[ VV_ITALIC     ]
   o_[ OBJ_UNDERLN    ] := v_[ VV_UNDERLN    ]
   o_[ OBJ_S_SCRPT    ] := v_[ VV_S_SCRPT    ]
   o_[ OBJ_U_SCRPT    ] := v_[ VV_U_SCRPT    ]
   o_[ OBJ_HALF_H     ] := v_[ VV_HALF_H     ]
   o_[ OBJ_ALIGN      ] := v_[ VV_ALIGN      ]
   o_[ OBJ_COLOR      ] := v_[ VV_COLOR      ]
   o_[ OBJ_ZERO       ] := v_[ VV_ZERO       ]
   o_[ OBJ_REPEATED   ] := v_[ VV_REPEATED   ]
   o_[ OBJ_VERTICLE   ] := v_[ VV_VERTICLE   ]
   o_[ OBJ_WRAP_SEMI  ] := v_[ VV_WRAP_SEMI  ]
   o_[ OBJ_FOR        ] := v_[ VV_FOR        ]
   o_[ OBJ_OBJ_UNIQUE ] := v_[ VV_OBJ_UNIQUE ]
   o_[ OBJ_MDL_F_TYPE ] := v_[ VV_MDL_F_TYPE ]
   o_[ OBJ_POINT      ] := v_[ VV_POINT      ]
   o_[ OBJ_COL_JUST   ] := v_[ VV_COL_JUST   ]
   o_[ OBJ_PATTERN    ] := v_[ VV_PATTERN    ]
   o_[ OBJ_BORDER     ] := v_[ VV_BORDER     ]

   RETURN o_

//----------------------------------------------------------------------//

FUNCTION scrObj2Vv( o_ )
   LOCAL v_:={}

   aadd( v_, pad( o_[OBJ_ID ],15 ) )
   aadd( v_, o_[ OBJ_FIELD      ] )
   aadd( v_, o_[ OBJ_F_TYPE     ] )
   aadd( v_, o_[ OBJ_F_LEN      ] )
   aadd( v_, o_[ OBJ_F_DEC      ] )
   aadd( v_, o_[ OBJ_ATTRB      ] )
   aadd( v_, o_[ OBJ_EQN        ] )
   aadd( v_, o_[ OBJ_PRN_LEN    ] )
   aadd( v_, o_[ OBJ_F_PIC      ] )
   aadd( v_, o_[ OBJ_PITCH      ] )
   aadd( v_, o_[ OBJ_FONT       ] )
   aadd( v_, o_[ OBJ_BOLD       ] )
   aadd( v_, o_[ OBJ_ITALIC     ] )
   aadd( v_, o_[ OBJ_UNDERLN    ] )
   aadd( v_, o_[ OBJ_S_SCRPT    ] )
   aadd( v_, o_[ OBJ_U_SCRPT    ] )
   aadd( v_, o_[ OBJ_HALF_H     ] )
   aadd( v_, o_[ OBJ_ALIGN      ] )
   aadd( v_, o_[ OBJ_COLOR      ] )
   aadd( v_, o_[ OBJ_ZERO       ] )
   aadd( v_, o_[ OBJ_REPEATED   ] )
   aadd( v_, o_[ OBJ_VERTICLE   ] )
   aadd( v_, o_[ OBJ_WRAP_SEMI  ] )
   aadd( v_, o_[ OBJ_FOR        ] )
   aadd( v_, o_[ OBJ_OBJ_UNIQUE ] )
   aadd( v_, o_[ OBJ_MDL_F_TYPE ] )
   aadd( v_, o_[ OBJ_POINT      ] )
   aadd( v_, o_[ OBJ_COL_JUST   ] )
   aadd( v_, o_[ OBJ_PATTERN    ] )
   aadd( v_, o_[ OBJ_BORDER     ] )

   RETURN v_

//----------------------------------------------------------------------//

FUNCTION scrStr2Obj( s )
   LOCAL obj_:= scrObjBlank()

   obj_[ OBJ_TYPE       ] := val( substr( s, OBJ_OS_TYPE,       OBJ_LEN_TYPE       ) )
   obj_[ OBJ_ROW        ] := val( substr( s, OBJ_OS_ROW,        OBJ_LEN_ROW        ) )
   obj_[ OBJ_COL        ] := val( substr( s, OBJ_OS_COL,        OBJ_LEN_COL        ) )
   obj_[ OBJ_TEXT       ] :=      substr( s, OBJ_OS_TEXT,       OBJ_LEN_TEXT       )
   obj_[ OBJ_COLOR      ] :=      substr( s, OBJ_OS_COLOR,      OBJ_LEN_COLOR      )
   obj_[ OBJ_TO_ROW     ] := val( substr( s, OBJ_OS_TO_ROW,     OBJ_LEN_TO_ROW     ) )
   obj_[ OBJ_TO_COL     ] := val( substr( s, OBJ_OS_TO_COL,     OBJ_LEN_TO_COL     ) )
   obj_[ OBJ_ID         ] :=      substr( s, OBJ_OS_ID,         OBJ_LEN_ID         )
   obj_[ OBJ_SECTION    ] := val( substr( s, OBJ_OS_SECTION,    OBJ_LEN_SECTION    ) )
   obj_[ OBJ_ALIAS      ] :=      substr( s, OBJ_OS_ALIAS,      OBJ_LEN_ALIAS      )
   obj_[ OBJ_FIELD      ] := val( substr( s, OBJ_OS_FIELD,      OBJ_LEN_FIELD      ) )
   obj_[ OBJ_EQN        ] :=      substr( s, OBJ_OS_EQN,        OBJ_LEN_EQN        )
   obj_[ OBJ_F_TYPE     ] :=      substr( s, OBJ_OS_F_TYPE,     OBJ_LEN_F_TYPE     )
   obj_[ OBJ_F_LEN      ] := val( substr( s, OBJ_OS_F_LEN,      OBJ_LEN_F_LEN      ) )
   obj_[ OBJ_F_DEC      ] := val( substr( s, OBJ_OS_F_DEC,      OBJ_LEN_F_DEC      ) )
   obj_[ OBJ_F_PIC      ] :=      substr( s, OBJ_OS_F_PIC,      OBJ_LEN_F_PIC      )
   obj_[ OBJ_ALIGN      ] :=      substr( s, OBJ_OS_ALIGN,      OBJ_LEN_ALIGN      )
   obj_[ OBJ_PITCH      ] := val( substr( s, OBJ_OS_PITCH,      OBJ_LEN_PITCH      ) )
   obj_[ OBJ_FONT       ] :=      substr( s, OBJ_OS_FONT,       OBJ_LEN_FONT       )
   obj_[ OBJ_BOLD       ] :=      substr( s, OBJ_OS_BOLD,       OBJ_LEN_BOLD       ) == 'T'
   obj_[ OBJ_ITALIC     ] :=      substr( s, OBJ_OS_ITALIC,     OBJ_LEN_ITALIC     ) == 'T'
   obj_[ OBJ_UNDERLN    ] :=      substr( s, OBJ_OS_UNDERLN,    OBJ_LEN_UNDERLN    ) == 'T'
   obj_[ OBJ_S_SCRPT    ] :=      substr( s, OBJ_OS_S_SCRPT,    OBJ_LEN_S_SCRPT    ) == 'T'
   obj_[ OBJ_U_SCRPT    ] :=      substr( s, OBJ_OS_U_SCRPT,    OBJ_LEN_U_SCRPT    ) == 'T'
   obj_[ OBJ_HALF_H     ] :=      substr( s, OBJ_OS_HALF_H,     OBJ_LEN_HALF_H     ) == 'T'
   obj_[ OBJ_PRN_LEN    ] := val( substr( s, OBJ_OS_PRN_LEN,    OBJ_LEN_PRN_LEN    ) )
   obj_[ OBJ_ZERO       ] :=      substr( s, OBJ_OS_ZERO  ,     OBJ_LEN_ZERO       ) == 'T'
   obj_[ OBJ_REPEATED   ] :=      substr( s, OBJ_OS_REPEATED,   OBJ_LEN_REPEATED   )
   obj_[ OBJ_VERTICLE   ] :=      substr( s, OBJ_OS_VERTICLE,   OBJ_LEN_VERTICLE   ) == 'T'
   obj_[ OBJ_WRAP_SEMI  ] :=      substr( s, OBJ_OS_WRAP_SEMI,  OBJ_LEN_WRAP_SEMI  ) == 'T'
   obj_[ OBJ_FOR        ] :=      substr( s, OBJ_OS_FOR,        OBJ_LEN_FOR        )
   obj_[ OBJ_SEC_ROW    ] := val( substr( s, OBJ_OS_SEC_ROW,    OBJ_LEN_SEC_ROW    ) )
   obj_[ OBJ_ATTRB      ] :=      substr( s, OBJ_OS_ATTRB,      OBJ_LEN_ATTRB      )
   obj_[ OBJ_VAL        ] :=      substr( s, OBJ_OS_VAL,        OBJ_LEN_VAL        )
   obj_[ OBJ_OBJ_UNIQUE ] := val( substr( s, OBJ_OS_OBJ_UNIQUE, OBJ_LEN_OBJ_UNIQUE ) )
   obj_[ OBJ_MDL_F_TYPE ] := val( substr( s, OBJ_OS_MDL_F_TYPE, OBJ_LEN_MDL_F_TYPE ) )
   obj_[ OBJ_POINT      ] := val( substr( s, OBJ_OS_POINT,      OBJ_LEN_POINT      ) )
   obj_[ OBJ_COL_JUST   ] := val( substr( s, OBJ_OS_COL_JUST,   OBJ_LEN_COL_JUST   ) )
   obj_[ OBJ_PATTERN    ] :=      substr( s, OBJ_OS_PATTERN,    OBJ_LEN_PATTERN    )
   obj_[ OBJ_BORDER     ] := val( substr( s, OBJ_OS_BORDER,     OBJ_LEN_BORDER     ) )

   obj_[OBJ_TEXT        ] := padc( alltrim( obj_[OBJ_TEXT] ),obj_[OBJ_F_LEN] )

   obj_[OBJ_EQN         ] := iif( obj_[ OBJ_TYPE ] == OBJ_O_TEXT, substr( obj_[ OBJ_EQN ], 1,;
                                      obj_[ OBJ_TO_COL ] - obj_[ OBJ_COL ] + 1 ), obj_[ OBJ_EQN ] )
   RETURN obj_

//----------------------------------------------------------------------//

FUNCTION scrObj2str( obj_ )
   LOCAL s := ;
   str( obj_[ OBJ_TYPE       ], OBJ_LEN_TYPE       ) + ;
   str( obj_[ OBJ_ROW        ], OBJ_LEN_ROW        ) + ;
   str( obj_[ OBJ_COL        ], OBJ_LEN_COL        ) + ;
   pad( alltrim( obj_[ OBJ_TEXT ] ), OBJ_LEN_TEXT  ) + ;
   pad( obj_[ OBJ_COLOR      ], OBJ_LEN_COLOR      ) + ;
   str( obj_[ OBJ_TO_ROW     ], OBJ_LEN_TO_ROW     ) + ;
   str( obj_[ OBJ_TO_COL     ], OBJ_LEN_TO_COL     ) + ;
   pad( obj_[ OBJ_ID         ], OBJ_LEN_ID         ) + ;
   str( obj_[ OBJ_SECTION    ], OBJ_LEN_SECTION    ) + ;
   pad( obj_[ OBJ_ALIAS      ], OBJ_LEN_ALIAS      ) + ;
   str( obj_[ OBJ_FIELD      ], OBJ_LEN_FIELD      ) + ;
   pad( obj_[ OBJ_EQN        ], OBJ_LEN_EQN        ) + ;
   pad( obj_[ OBJ_F_TYPE     ], OBJ_LEN_F_TYPE     ) + ;
   str( obj_[ OBJ_F_LEN      ], OBJ_LEN_F_LEN      ) + ;
   str( obj_[ OBJ_F_DEC      ], OBJ_LEN_F_DEC      ) + ;
   pad( obj_[ OBJ_F_PIC      ], OBJ_LEN_F_PIC      ) + ;
   pad( obj_[ OBJ_ALIGN      ], OBJ_LEN_ALIGN      ) + ;
   str( obj_[ OBJ_PITCH      ], OBJ_LEN_PITCH      ) + ;
   pad( obj_[ OBJ_FONT       ], OBJ_LEN_FONT       ) + ;
   iif( obj_[ OBJ_BOLD       ], 'T', 'F'           ) + ;
   iif( obj_[ OBJ_ITALIC     ], 'T', 'F'           ) + ;
   iif( obj_[ OBJ_UNDERLN    ], 'T', 'F'           ) + ;
   iif( obj_[ OBJ_S_SCRPT    ], 'T', 'F'           ) + ;
   iif( obj_[ OBJ_U_SCRPT    ], 'T', 'F'           ) + ;
   iif( obj_[ OBJ_HALF_H     ], 'T', 'F'           ) + ;
   str( obj_[ OBJ_PRN_LEN    ], OBJ_LEN_PRN_LEN    ) + ;
   iif( obj_[ OBJ_ZERO       ], 'T', 'F'           ) + ;
   pad( obj_[ OBJ_REPEATED   ], OBJ_LEN_REPEATED   ) + ;
   iif( obj_[ OBJ_VERTICLE   ], 'T', 'F'           ) + ;
   iif( obj_[ OBJ_WRAP_SEMI  ], 'T', 'F'           ) + ;
   pad( obj_[ OBJ_FOR        ], OBJ_LEN_FOR        ) + ;
   str( obj_[ OBJ_SEC_ROW    ], OBJ_LEN_SEC_ROW    ) + ;
   pad( obj_[ OBJ_ATTRB      ], OBJ_LEN_ATTRB      ) + ;
   pad( obj_[ OBJ_VAL        ], OBJ_LEN_VAL        ) + ;
   str( obj_[ OBJ_OBJ_UNIQUE ], OBJ_LEN_OBJ_UNIQUE ) + ;
   str( obj_[ OBJ_MDL_F_TYPE ], OBJ_LEN_MDL_F_TYPE ) + ;
   str( obj_[ OBJ_POINT      ], OBJ_LEN_POINT      ) + ;
   str( obj_[ OBJ_COL_JUST   ], OBJ_LEN_COL_JUST   ) + ;
   pad( obj_[ OBJ_PATTERN    ], OBJ_LEN_PATTERN    ) + ;
   str( obj_[ OBJ_BORDER     ], OBJ_LEN_BORDER, 2  )

   RETURN pad( s,490 )   //  10 FOR Random, 500 FOR attr

//----------------------------------------------------------------------//

FUNCTION scrObjBlank()
   LOCAL o_:= array( OBJ_INIT_VRBLS )

   o_[ OBJ_TYPE       ] := 0
   o_[ OBJ_ROW        ] := 0
   o_[ OBJ_COL        ] := 0
   o_[ OBJ_TEXT       ] := ''
   o_[ OBJ_COLOR      ] := 'W/B    '
   o_[ OBJ_TO_ROW     ] := 0
   o_[ OBJ_TO_COL     ] := 0
   o_[ OBJ_ID         ] := ''
   o_[ OBJ_SECTION    ] := 1
   o_[ OBJ_ALIAS      ] := ''
   o_[ OBJ_FIELD      ] := 0
   o_[ OBJ_EQN        ] := ''
   o_[ OBJ_F_TYPE     ] := ''
   o_[ OBJ_F_LEN      ] := 0
   o_[ OBJ_F_DEC      ] := 0
   o_[ OBJ_F_PIC      ] := ''
   o_[ OBJ_ALIGN      ] := 'L'
   o_[ OBJ_PITCH      ] := 10
   o_[ OBJ_FONT       ] := 'COURIER '
   o_[ OBJ_BOLD       ] := .F.
   o_[ OBJ_ITALIC     ] := .F.
   o_[ OBJ_UNDERLN    ] := .F.
   o_[ OBJ_S_SCRPT    ] := .F.
   o_[ OBJ_U_SCRPT    ] := .F.
   o_[ OBJ_PRN_LEN    ] := 0
   o_[ OBJ_HALF_H     ] := .F.
   o_[ OBJ_ZERO       ] := .T.
   o_[ OBJ_REPEATED   ] := 'NO    '
   o_[ OBJ_VERTICLE   ] := .F.
   o_[ OBJ_WRAP_SEMI  ] := .F.
   o_[ OBJ_FOR        ] := space( 80 )
   o_[ OBJ_SEC_ROW    ] := 0
   o_[ OBJ_ATTRB      ] := 'NONE    '
   o_[ OBJ_VAL        ] := ' '
   o_[ OBJ_OBJ_UNIQUE ] := 0
   o_[ OBJ_MDL_F_TYPE ] := 0
   o_[ OBJ_POINT      ] := 0
   o_[ OBJ_COL_JUST   ] := 0
   o_[ OBJ_PATTERN    ] := 'SOLID     '
   o_[ OBJ_BORDER     ] := 0.50

   RETURN o_

//----------------------------------------------------------------------//

FUNCTION scrScn2obj( scn_,nType )
   LOCAL obj_:={}

   DEFAULT nType TO 0
   aeval( scn_,{|e_| iif( e_[2] == nType, aadd( obj_,scrStr2Obj( e_[3] ) ), '' ) } )

   RETURN obj_

//----------------------------------------------------------------------//
#IF 0
FUNCTION scrClrFrBk( cClr )
   LOCAL nClrF, nClrB,cFr,cBk,n
   LOCAL pal_:={'W','N','N+','N++','BG','G','R','RB','GR',;
                        'B','GR+','W+','B+','R+','BG+','G+','RB+','X'}
   LOCAL mnu_:= ;
         {'White', 'Black', 'Grey','Pale Grey','Baige', 'Green', 'Red',;
          'Magenta', 'Brown', 'Blue', 'Yellow','Bright White',;
          'Bright Blue', 'Bright Red', 'Bright Baige',;
          'Bright Green', 'Bright Magenta','None'}

   cClr := iif( empty( cClr ),'W/B',alltrim( upper( cClr ) ) )
   IF( n := at( '/',cClr ) )==0
      RETURN pad( cClr,7 )
   ENDIF
   cFr := substr( cClr,1,n-1 )
   cBk := substr( cClr,n+1 )

   nClrF := ascan( pal_,{|e| e == cFr} )
   nClrB := ascan( pal_,{|e| e == cBk} )

   B_MSG 'ForeGround Color' ;
   CHOOSE mnu_ RESTORE SHADOW CENTER INTO nClrF INITIAL nClrF
   IF nClrF == 0
      RETURN space( 7 )
   ENDIF

   B_MSG 'BackGround Color' ;
   CHOOSE mnu_ RESTORE SHADOW CENTER INTO nClrB INITIAL nClrB
   IF nClrB == 0
      RETURN space( 7 )
   ENDIF
   RETURN pad( pal_[nClrF]+'/'+pal_[nClrB],7 )
#ENDIF
//----------------------------------------------------------------------//

FUNCTION scrAddBox( obj_, scn_, nObj, Sel )
   LOCAL nKey,o_,border,cClr, nnObj, cFile

   DEFAULT Sel TO 0

   nnObj     := nObj
   cFile     := ''
   border    := '        '+chr( 255 )
   cClr      := 'N/X   '
   sel       := 2

   IF nnObj == 0
      o_:= scrObjBlank()

      o_[ OBJ_TYPE       ] := OBJ_O_BOX
      o_[ OBJ_ROW        ] := scn_[ SCN_ROW_REP ]
      o_[ OBJ_COL        ] := scn_[ SCN_COL_REP ]
      o_[ OBJ_TO_ROW     ] := scn_[ SCN_ROW_REP ]
      o_[ OBJ_TO_COL     ] := scn_[ SCN_COL_REP ]
      o_[ OBJ_SECTION    ] := scrSecCur( scn_, scn_[ SCN_ROW_REP ] )
      o_[ OBJ_F_LEN      ] := 9
      o_[ OBJ_MDL_F_TYPE ] := sel + 60

      aadd( obj_, o_ )
      nObj := len( obj_ )
   ELSE
      sel := obj_[ nObj, OBJ_MDL_F_TYPE ] - 60
   ENDIF

   IF sel == 2
      border := 'ÚÄ¿³ÙÄÀ³'            //  can be DOUBLE_SINGLE, etc
      cClr     := "w/b"
      obj_[ nObj, OBJ_BORDER ] := 0.5
   ENDIF

   obj_[ nObj, OBJ_BOX_SHAPE ] := border
   obj_[ nObj, OBJ_COLOR     ] := cClr
   obj_[ nObj, OBJ_ID        ] := "Frame"
   obj_[ nObj, OBJ_EQN       ] := cFile
   obj_[ nObj, OBJ_PATTERN   ] := 'CLEAR     '

   IF nnObj == 0
      scrMsg( 'Draw Frame WITH <Arrow Keys>. Finish WITH <Enter>' )

      DO WHILE .t.
         nKey := inkey( 0 )
         DO CASE
         CASE nKey == K_RIGHT .AND. sel <> 5
            IF scrMovRgt( scn_ )
               obj_[ nObj,OBJ_TO_COL ]++
            ENDIF
         CASE nKey == K_LEFT  .AND. sel <> 5
            IF scrMovLft( scn_ )
               obj_[ nObj,OBJ_TO_COL ]--
            ENDIF
         CASE nKey == K_DOWN  .AND. sel <> 4
            IF scrMovDn( scn_ )
               obj_[ nObj,OBJ_TO_ROW ]++
            ENDIF
         CASE nKey == K_UP    .AND. sel <> 4
            IF scrMovUp( scn_ )
               obj_[ nObj,OBJ_TO_ROW ]--
            ENDIF
         CASE nKey == K_ENTER
            EXIT
         ENDCASE
         scrMove( obj_,scn_ )
         scrStatus( obj_,scn_ )
      ENDDO
   ENDIF

   scrOrdObj( obj_ )
   scrMsg()
   scn_[SCN_REFRESH] := OBJ_REFRESH_ALL
   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION scrAddFld( obj_,scn_,nObj )

   HB_SYMBOL_UNUSED( obj_ )
   HB_SYMBOL_UNUSED( nObj )

   DO CASE
   CASE scn_[SCN_DESIGN] == DGN_MODULE
      scrMdlFld( obj_, scn_, nObj )
   ENDCASE

   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION scrAddTxt( obj_,scn_,nKey,nMode )
   LOCAL txt_:={},n, lClub,i
   LOCAL n1,s1,s2,nTxt,nDel
   LOCAL nRepCol := scn_[SCN_COL_REP], nRepRow := scn_[SCN_ROW_REP]
   LOCAL lOrder := .f.
   //  nMode   1.Add   2.Del   3.BS

   //  Scan obj_ FOR Text Objects Related WITH Current Report Row
   aeval( obj_,{|e_| iif( e_[ OBJ_TYPE ] == OBJ_O_TEXT .AND. e_[ OBJ_ROW ] == nRepRow, aadd( txt_,e_ ),'' ) } )
   IF nMode == 1      //  New Character
      IF empty( txt_ ) .OR. ascan( txt_,{|e_| VouchInRange( nRepCol, e_[OBJ_COL],e_[OBJ_TO_COL] ) } ) == 0
         aadd( txt_, scrObjBlank() ) ; lOrder := .t.
         nTxt := len( txt_ )
         txt_[ nTxt, OBJ_TYPE    ]  := OBJ_O_TEXT
         txt_[ nTxt, OBJ_F_TYPE  ]  := 'C'
         txt_[ nTxt, OBJ_F_LEN   ]  := 1
         txt_[ nTxt, OBJ_ALIGN   ]  := 'L'
         txt_[ nTxt, OBJ_ROW     ]  := scn_[SCN_ROW_REP]
         txt_[ nTxt, OBJ_COL     ]  := scn_[SCN_COL_REP]
         txt_[ nTxt, OBJ_EQN     ]  := ''
         txt_[ nTxt, OBJ_ID      ]  := 'Text'
         txt_[ nTxt, OBJ_COLOR   ]  := 'N/W'
         txt_[ nTxt, OBJ_PITCH   ]  := 10
         txt_[ nTxt, OBJ_SECTION ]  := scrSecCur( scn_,scn_[SCN_ROW_REP] )
         txt_[ nTxt, OBJ_TO_ROW  ]  := scn_[SCN_ROW_REP]
         txt_[ nTxt, OBJ_TO_COL  ]  := scn_[SCN_COL_REP]
      ENDIF
   ENDIF

   nTxt := ascan( txt_,{|e_| VouchInRange( nRepCol, e_[ OBJ_COL ], e_[ OBJ_TO_COL ] ) } )

   IF     nMode == 1
      txt_[nTxt,OBJ_EQN] := substr( txt_[nTxt,OBJ_EQN],1,;
                        scn_[SCN_COL_REP]-txt_[nTxt,OBJ_COL] ) + ;
                                 chr( nKey ) + ;
           substr( txt_[nTxt,OBJ_EQN],scn_[SCN_COL_REP]-txt_[nTxt,OBJ_COL] + ;
                                            iif( ReadInsert(),1,2 ) )
      txt_[nTxt,OBJ_TO_COL] := txt_[nTxt,OBJ_COL]+len( txt_[nTxt,OBJ_EQN] )-1

   ELSEIF nMode == 2  .OR. nMode == 3 //  Delete
      IF readInsert()
         txt_[nTxt,OBJ_EQN] := substr( txt_[nTxt,OBJ_EQN],1,;
                        scn_[SCN_COL_REP]-txt_[nTxt,OBJ_COL] ) + ;
           substr( txt_[nTxt,OBJ_EQN],scn_[SCN_COL_REP]-txt_[nTxt,OBJ_COL] + 2 )
         txt_[nTxt,OBJ_TO_COL] := txt_[nTxt,OBJ_COL]+len( txt_[nTxt,OBJ_EQN] )-1
      ELSE             //  Divide it IN two objects
         s1   := substr( txt_[nTxt,OBJ_EQN],1,scn_[SCN_COL_REP]-txt_[nTxt,OBJ_COL] )
         s2   := substr( txt_[nTxt,OBJ_EQN],scn_[SCN_COL_REP]-txt_[nTxt,OBJ_COL] + 2 )
         nDel := 0
         IF len( s1 ) > 0
            txt_[nTxt,OBJ_EQN]     := s1
            txt_[nTxt,OBJ_TO_COL]  := txt_[nTxt,OBJ_COL]+len( s1 )-1
            txt_[nTxt,OBJ_PRN_LEN] := len( s1 )
         ELSE
            nDel := nTxt
         ENDIF

         IF len( s2 ) > 0
            IF nDel == 0
               aadd( txt_,aclone( txt_[nTxt] ) )  //  scrObjBlank() )
               lOrder := .t.
               n1                := len( txt_ )
            ELSE
               n1 := nDel
            ENDIF
            txt_[n1,OBJ_TYPE]    := OBJ_O_TEXT
            txt_[n1,OBJ_F_TYPE]  := 'C'
            txt_[n1,OBJ_F_LEN]   := len( s2 )
            txt_[n1,OBJ_PRN_LEN] := len( s2 )
            //  txt_[n1,OBJ_ALIGN]   := 'L'
            txt_[n1,OBJ_ROW]     := scn_[SCN_ROW_REP]
            txt_[n1,OBJ_COL]     := scn_[SCN_COL_REP]+1
            txt_[n1,OBJ_EQN]     := s2
            txt_[n1,OBJ_ID]      := 'Text'
            txt_[n1,OBJ_SECTION] := scrSecCur( scn_,scn_[SCN_ROW_REP] )
            txt_[n1,OBJ_TO_ROW]  := scn_[SCN_ROW_REP]
            txt_[n1,OBJ_TO_COL]  := txt_[n1,OBJ_COL]+len( s2 )-1
         ENDIF
         IF len( s1 )==0.AND.len( s2 )==0
            VouchAShrink( txt_,nTxt )
            IF empty( txt_ )
               aadd( txt_,scrObjBlank() )
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   IF !empty( txt_ )
      DO WHILE .t.
         IF( n := ascan( txt_,{|e_| e_[OBJ_TO_COL] < e_[OBJ_COL] } ) ) > 0
            VouchAShrink( txt_,n )
         ELSE
            EXIT
         ENDIF
      ENDDO
      IF empty( txt_ )
         aadd( txt_,scrObjBlank() )
      ENDIF
      //  CLUB DIFFERENT TEXT OBJECTS IF THESE ARE ADJACENT
      asort( txt_,,,{|e_,f_| e_[OBJ_COL]<f_[OBJ_COL] } )

      DO WHILE .t.
         lClub := .f.
//         n     := txt_[1,OBJ_TO_COL]
         FOR i := 2 TO len( txt_ )
            IF txt_[i,OBJ_COL] == txt_[i-1,OBJ_TO_COL]+1
               txt_[i-1,OBJ_EQN]    += txt_[i,OBJ_EQN]    //  Club both
               txt_[i-1,OBJ_TO_COL] := txt_[i-1,OBJ_COL] + len( txt_[i-1,OBJ_EQN] ) - 1
               txt_[i-1,OBJ_F_LEN]  := len( txt_[i-1,OBJ_EQN] )
               VouchAShrink( txt_,i )
//               lUpdate := .t.
               lClub   := .t.
            ENDIF
         NEXT
         IF !lClub
            EXIT
         ENDIF
      ENDDO
   ENDIF

   DO WHILE .t.
      IF( n := ascan( obj_,{|e_| e_[OBJ_TYPE] == OBJ_O_TEXT .AND. ;
                                e_[OBJ_ROW ] == scn_[SCN_ROW_REP] } ) ) > 0
         VouchAShrink( obj_,n )
         IF empty( obj_ )
            aadd( obj_, scrObjBlank() )
         ENDIF
      ELSE
         EXIT
      ENDIF
   ENDDO

   aeval( txt_,{|e_| iif( e_[OBJ_ROW]>0,aadd( obj_,e_ ),'' ) } )   //  Now attach txt_

   DO WHILE .t.
      IF( n := ascan( obj_,{|e_| e_[OBJ_TO_COL] < e_[OBJ_COL] } ) ) > 0
         VouchAShrink( obj_,n )
         IF empty( obj_ )
            aadd( obj_,scrObjBlank() )
         ENDIF
      ELSE
         EXIT
      ENDIF
   ENDDO

   IF lOrder
      //  scrOrdObj( obj_ )
   ENDIF
   IF     nMode == 1
      keyboard( chr( K_RIGHT ) )
   ENDIF

   scn_[SCN_REFRESH] := OBJ_REFRESH_LINE

   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION scrSecCur( scn_, nRepRow )         //  Numeric id of sector
   LOCAL n,i
   n := 0
   FOR i := 1 TO len( scn_[ SCN_SECTORS_ ] )
      n += scn_[ SCN_SECTORS_, i, SCT_ROWS ]
      IF nRepRow <= n
//         n1 := i
         EXIT
      ENDIF
   NEXT
   RETURN scn_[ SCN_SECTORS_, i, SCT_ORDER ]

//----------------------------------------------------------------------//

FUNCTION scrSecOrd( scn_, nRepRow )         //  Index of sector
   LOCAL n,n1,i
   n := 0
   FOR i := 1 TO len( scn_[ SCN_SECTORS_ ] )
      n += scn_[ SCN_SECTORS_, i, SCT_ROWS ]
      IF nRepRow <= n
         n1 := i
         EXIT
      ENDIF
   NEXT
   RETURN n1

//----------------------------------------------------------------------//
/* Sector wise row position */
FUNCTION scrSecRow( scn_,nRepRow )
   LOCAL n,n1,i
   n := 0   ;   n1 := 0
   FOR i := 1 TO len( scn_[ SCN_SECTORS_ ] )
      n += scn_[ SCN_SECTORS_, i, SCT_ROWS ]
      IF nRepRow <= n
         n1 := nRepRow -( n - scn_[ SCN_SECTORS_, i, SCT_ROWS ] )
         EXIT
      ENDIF
   NEXT
   RETURN n1

//----------------------------------------------------------------------//

FUNCTION scrMsg( msg )
   LOCAL row := row(), col := col()

   @ maxrow(),0 SAY padc( " ", maxcol()+1 ) COLOR "W+/W"
   IF empty( msg )
      msg := "F1 Help  F5 Edit  F6 Select  F7 Copy  F8 Paste  F9 Box  F10 Field"
   ENDIF
   msg := " " + msg + " "
   @ maxrow(),( maxcol()+1-len( msg ) )/2 SAY msg COLOR "W+/B"

   setPos( row,col )
   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION scrInkey( key_ )
   LOCAL nKey

   DO WHILE .t.
      nKey := inkey( 0 )
      IF ascan( key_,nKey )>0
         EXIT
      ENDIF
   ENDDO

   RETURN nKey

//----------------------------------------------------------------------//

FUNCTION scrConfig( obj_, nDgn, sct_ )
   LOCAL s, n, nRows, nCols
   LOCAL cor_:={ { 1, 0, maxrow()-2, maxcol(), 200, 400 } }
   LOCAL scn_:= array( SCN_NUM_VAR )

   scn_[ SCN_SECTORS_     ]   := {}
   scn_[ SCN_DESIGN       ]   := nDgn
   scn_[ SCN_FOR_ROWS     ]   := { { 0,'' } }

   nRows := scrSectors( obj_, scn_, nDgn, sct_ )

   nCols := 0
   aeval( obj_, {|e_| nCols := max( nCols, e_[ OBJ_TO_COL ] ) } )
   nCols := iif( nCols == 0, cor_[ nDgn,4 ], nCols )

   scn_[ SCN_TOP          ]   := cor_[ nDgn,1 ]
   scn_[ SCN_LEFT         ]   := cor_[ nDgn,2 ]
   scn_[ SCN_BOTTOM       ]   := cor_[ nDgn,3 ]
   scn_[ SCN_RIGHT        ]   := cor_[ nDgn,4 ]
   scn_[ SCN_MODE         ]   := 0
   scn_[ SCN_ROW_CUR      ]   := scn_[ SCN_TOP  ]      //  9
   scn_[ SCN_COL_CUR      ]   := scn_[ SCN_LEFT ]
   scn_[ SCN_ROW_REP      ]   := 1
   scn_[ SCN_COL_REP      ]   := 1
   scn_[ SCN_ROW_DIS      ]   := scn_[ SCN_TOP  ]-1
   scn_[ SCN_COL_DIS      ]   := scn_[ SCN_LEFT ]-1

   scn_[ SCN_ROW_MENU     ]   := 0
   scn_[ SCN_ROW_RULER    ]   := 0
   scn_[ SCN_STATUS_ROW   ]   := maxrow()-1
   scn_[ SCN_STATUS_COL   ]   := 0
   scn_[ SCN_STATUS_COL_TO]   := maxcol()

   scn_[ SCN_COL_MAX      ]   := iif( nDgn==DGN_SCREEN .OR. nDgn==DGN_MODULE, cor_[nDgn,6], nCols )
   scn_[ SCN_ROW_PREV     ]   := scn_[ SCN_TOP  ]
   scn_[ SCN_COL_PREV     ]   := scn_[ SCN_LEFT ]
   scn_[ SCN_REP_LINES    ]   := iif( nDgn==DGN_SCREEN .OR. nDgn==DGN_MODULE, cor_[ nDgn,5 ], nRows )

   scn_[ SCN_CLR_STATUS   ]   := "W+/BG"
   scn_[ SCN_CLR_TEXT     ]   := 'W+/B'
   scn_[ SCN_CLR_BOX      ]   := 'W/B'
   scn_[ SCN_CLR_FIELD    ]   := CLR_GET
   scn_[ SCN_CLR_HILITE   ]   := iif( wvt(),'N/GR*','GR+/BG' )
   scn_[ SCN_CLR_WINDOW   ]   := 'W+/BG'
   scn_[ SCN_CLR_RULER    ]   := "N/W"
   scn_[ SCN_CLR_MENU     ]   := "W+/B"
   scn_[ SCN_CLR_OVERALL  ]   := "N/W"
   scn_[ SCN_CLR_PREV     ]   := iif( wvt(), 'B/W' , 'B/W' )
   scn_[ SCN_CLR_SELECT   ]   := iif( wvt(), 'N/W*', 'GR+/N' )

   scn_[ SCN_OBJ_HILITE   ]   := 0      //  32
   scn_[ SCN_OBJ_SELECTED ]   := 0      //  33

   s := '.'
   FOR n := 1 TO 40
      s += '.......' + strtran( str( n,3 ), ' ', '.' )
   NEXT
   scn_[ SCN_RULER        ]   := s

   scn_[ SCN_DRAW_FILL    ]   := '±±±±±±±±±'
   scn_[ SCN_OBJ_ID_      ]   := { 'Bitmap','Line','Text','Field','Expression','BitMap' }
   scn_[ SCN_REFRESH      ]   := OBJ_REFRESH_ALL
   scn_[ SCN_OBJ_COPIED   ]   := 0
   scn_[ SCN_BOX_SHAPE    ]   := 'ÚÄ¿³ÙÄÀ³'
   scn_[ SCN_CHR_PREV     ]   := ''
   scn_[ SCN_DESIGN_ID    ]   := "Module"
   scn_[ SCN_FILE         ]   := "Untitled"
   scn_[ SCN_PROPERTY     ]   := {}
   scn_[ SCN_GRAPHICS     ]   := .f.
   scn_[ SCN_TEXT_BLOCK_  ]   := {}
   scn_[ SCN_FIELDS       ]   := {}
   scn_[ SCN_LASTKEY      ]   := 0

   RETURN scn_

//----------------------------------------------------------------------//

FUNCTION scrWvtConfig( obj_, scn_ )
   LOCAL nRows,nCols
   LOCAL cor_:={ { 1, 0, maxrow()-2, maxcol(), 200, 400 } }
   LOCAL nDgn := scn_[ SCN_DESIGN ]

   nRows := 0
   aeval( scn_[ SCN_SECTORS_ ], {|e_| nRows += e_[ SCT_ROWS ] } )

   nCols := 0
   aeval( obj_, {|e_| nCols := max( nCols,e_[ OBJ_TO_COL ] ) } )
   nCols := iif( nCols == 0, cor_[ nDgn,4 ], nCols )

   scn_[ SCN_TOP           ] := cor_[ nDgn,1 ]
   scn_[ SCN_LEFT          ] := cor_[ nDgn,2 ]
   scn_[ SCN_BOTTOM        ] := cor_[ nDgn,3 ]
   scn_[ SCN_RIGHT         ] := cor_[ nDgn,4 ]
   scn_[ SCN_STATUS_ROW    ] := maxrow()-1
   scn_[ SCN_STATUS_COL    ] := 0
   scn_[ SCN_STATUS_COL_TO ] := maxcol()
   scn_[ SCN_MODE          ] := 0
   scn_[ SCN_ROW_CUR       ] := scn_[ SCN_TOP  ]
   scn_[ SCN_COL_CUR       ] := scn_[ SCN_LEFT ]
   scn_[ SCN_ROW_REP       ] := 1
   scn_[ SCN_COL_REP       ] := 1
   scn_[ SCN_ROW_DIS       ] := scn_[ SCN_TOP  ]-1
   scn_[ SCN_COL_DIS       ] := scn_[ SCN_LEFT ]-1
   scn_[ SCN_ROW_RULER     ] := 0
   scn_[ SCN_ROW_MENU      ] := 0
   scn_[ SCN_COL_MAX       ] := cor_[ nDgn, 6  ]
   scn_[ SCN_ROW_PREV      ] := scn_[ SCN_TOP  ]
   scn_[ SCN_COL_PREV      ] := scn_[ SCN_LEFT ]
   scn_[ SCN_REP_LINES     ] := cor_[ nDgn, 5  ]

   RETURN NIL

//----------------------------------------------------------------------//

STATIC FUNCTION scrSectors( obj_, scn_, nDgn /*,sct_*/ )

   HB_SYMBOL_UNUSED( obj_ )

   IF nDgn == DGN_MODULE .OR. nDgn == DGN_SCREEN
      aadd( scn_[ SCN_SECTORS_ ], { 1,'Screen', 'R    ', 100, 'w+/bg', '', .f., .f. } )
   ENDIF

   RETURN 100

//----------------------------------------------------------------------//

STATIC FUNCTION scrAddPrp( scn_, sct_ )

   aadd( scn_[SCN_SECTORS_], ;
         { sct_[1], sct_[2], sct_[3], sct_[4], sct_[5], sct_[6], sct_[7], sct_[8] } )

   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION scrPicture( cType,nWidth,nDec,cPic,nScrWid )

   DO CASE
   CASE cType == 'N'
      cPic := '@Z ' + replicate( '9', nScrWid - iif( nDec > 0, nDec + 1, 0 ) ) + ;
                           iif( nDec > 0, '.' + replicate( "9", nDec ), "" )
   CASE cType == 'C'
      IF nScrWid <> nWidth
         cPic := '@' + iif( '!' $ cPic,'!','' ) + 'S' + NTRIM( nScrWid )
      ENDIF
   ENDCASE

   RETURN cPic

//----------------------------------------------------------------------//

