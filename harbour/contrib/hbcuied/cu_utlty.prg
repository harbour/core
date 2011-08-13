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
#include "hbgtinfo.ch"
#include "hbgtwvg.ch"
#include "common.ch"
#include "inkey.ch"
#include "achoice.ch"

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
   aeval( scn_,{|e_| IF( e_[2] == nType, aadd( obj_,scrStr2Obj( e_[3] ) ), '' ) } )
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

   cClr := IF( empty( cClr ),'W/B',alltrim( upper( cClr ) ) )
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

FUNCTION scrField( nObj,nFldExp,obj_,scn_,v_,sel_,cColor )
   LOCAL h_:={},w_:={},o_

   DEFAULT cColor TO scn_[ SCN_CLR_FIELD ]
   DEFAULT sel_   TO scrVvSelAble()

   IF nObj == 0
      o_:= scrObjBlank()
      DEFAULT v_ TO scrVvBlank()
   ELSE
      o_:= obj_[ nObj ]
      IF v_ == NIL .OR. empty( v_ )
         v_:= scrObj2Vv( o_ )
      ENDIF
   ENDIF

   v_[ VV_PRN_LEN ] := iif( v_[ VV_PRN_LEN ] == 0, v_[ VV_F_LEN ], v_[ VV_PRN_LEN ] )

   aadd( h_, '  Title                    ' )
   aadd( h_, '  Field                    ' )
   aadd( h_, '  Type                     ' )
   aadd( h_, '  Width                    ' )
   aadd( h_, '  Decimals                 ' )
   aadd( h_, '  Calculate                ' )
   aadd( h_, '  Expression               ' )
   aadd( h_, '  Printed Width            ' )
   aadd( h_, '  Picture                  ' )
   aadd( h_, '  Pitch                    ' )
   aadd( h_, '  Font                     ' )
   aadd( h_, '  Bold                     ' )
   aadd( h_, '  Italics                  ' )
   aadd( h_, '  UnderLine                ' )
   aadd( h_, '  SuperScript              ' )
   aadd( h_, '  SubScript                ' )
   aadd( h_, '  Half Height              ' )
   aadd( h_, '  Alignment                ' )
   aadd( h_, '  Color                    ' )
   aadd( h_, '  Zero as Blank            ' )
   aadd( h_, '  Supress Repeated Values  ' )
   aadd( h_, '  Verticle Stretch         ' )
   aadd( h_, '  Wrap Semi Colons         ' )
   aadd( h_, '  The FOR Condition        ' )
   aadd( h_, '  Unique Id                ' )
   aadd( h_, '  Field Type . Module      ' )
   aadd( h_, '  Point Size               ' )
   aadd( h_, '  Column FOR Justification ' )
   aadd( h_, '  Pattern TO fill a frame  ' )
   aadd( h_, '  Border Thickness         ' )

   aadd( w_, {| |.t.} )
   aadd( w_, {| |.t.} )
   aadd( w_, {| | VouchMenuM( 'MN_TYFLD' ) } )
   aadd( w_, {|v| v := oAchGet( 3 ),IF( v=='D',!oCPut( 8 ),IF( v=='L',!oCPut( 1 ),.t. ) ) } )
   aadd( w_, {|v| v := oAchGet( 3 ),IF( v<>'N',!oCPut( 0 ),.t. ) } )
   aadd( w_, {| | IF( oAchGet( 3 )=='N',VouchMenuM( 'MN_SUMAV' ),.f. ) } )
   aadd( w_, {| | !oCPut( scrExp( oGet(),oAchGet( 3 ) ) ) } )
   aadd( w_, {| |.t.} )   //  Printing Width
   aadd( w_, {| |.t.} )   //  Picture
   aadd( w_, {| | VouchMenuM( 'MN_CPI  ' ) } )
   aadd( w_, {| | VouchMenuM( 'MN_FONTS' ) } )   //  Font
   aadd( w_, {| | VouchYN( 'Bold Printing',oGet() ),.f. } )
   aadd( w_, {| | VouchYN( 'Italic Printing',oGet() ),.f. } )
   aadd( w_, {| | VouchYN( 'Underlined Printing',oGet() ),.f. } )
   aadd( w_, {| | VouchYN( 'SuperScript Printing',oGet() ),.f. } )
   aadd( w_, {| | VouchYN( 'SubScript Printing',oGet() ),.f. } )
   aadd( w_, {| | VouchYN( 'Half Height Printing',oGet() ),.f. } )
   aadd( w_, {| | VouchMenuM( 'MN_ALIGN' ),.f. } )
   aadd( w_, {| | .f. } )
   aadd( w_, {| | IF( oAchGet( 3 )=='N',!VouchYN( 'Zeros as Blank',oGet() ),.f. ) } )
   aadd( w_, {| | VouchMenuM( 'MN_SPRES' ) } )
   aadd( w_, {| | !VouchYN( 'Stretch Value Verically',oGet() ),.f. } )
   aadd( w_, {| | !VouchYN( 'Wrap Semicolons',oGet() ),.f. } )
   aadd( w_, {| | .f. } )
   aadd( w_, {| | .f. } )
   aadd( w_, {| | .f. } )
   aadd( w_, {| | .t. } )    //  Point Size
   aadd( w_, {| | .t. } )    //  Justify Column
   aadd( w_, {| | VouchMenuM( 'MN_PTTRN' ) } )
   aadd( w_, {| | .t. } )    //  Border Thickness

   B_GETS HEADERS h_ VALUES v_ ;
   TITLE 'Configure '+ IF( nFldExp==1,'Field',IF( nFldExp==2,'Expression',IF( nFldexp==3,'Text','OBJECT' ) ) ) ;
   INTO v_ WHEN w_ SELECTABLES sel_

   v_:= v_[1]
   v_[1] := alltrim( trim( v_[1] ) )
   IF empty( v_[1] )
      RETURN NIL
   ENDIF

   IF lastkey()<>K_ESC        //  Now Configure OBJECT
      v_[VV_PRN_LEN] := IF( v_[VV_PRN_LEN]=0,v_[VV_F_LEN],v_[VV_PRN_LEN] )

      o_:= scrVv2Obj( v_,o_ )

      o_[OBJ_TYPE]    := IF( nFldExp==1, OBJ_O_FIELD,;
                              IF( nFldExp==2, OBJ_O_EXP,;
                                 IF( nFldExp==3,OBJ_O_TEXT,;
                                    IF( nFldExp==4,OBJ_O_BOX,;
                                                 OBJ_O_FIELD ) ) ) )
      IF o_[ OBJ_TYPE   ] == OBJ_O_FIELD .OR. o_[OBJ_TYPE] == OBJ_O_EXP
         o_[ OBJ_ROW    ] := IF( nObj==0,scn_[SCN_ROW_REP],o_[OBJ_ROW] )
         o_[ OBJ_COL    ] := IF( nObj==0,scn_[SCN_COL_REP],O_[OBJ_COL] )
         o_[ OBJ_TEXT   ] := padc( alltrim( v_[VV_ID] ),v_[VV_F_LEN] )
         o_[ OBJ_COLOR  ] := IF( empty( o_[OBJ_COLOR] ),cColor,o_[OBJ_COLOR] )
         o_[ OBJ_TO_ROW ] := IF( nObj==0,scn_[SCN_ROW_REP],o_[OBJ_TO_ROW] )
         o_[ OBJ_TO_COL ] := IF( nObj==0,scn_[SCN_COL_REP],o_[OBJ_COL] ) + ;
                                                      v_[VV_F_LEN]-1
      ENDIF

      o_[OBJ_SECTION] := scrSecCur( scn_,scn_[SCN_ROW_REP] )

      IF nObj == 0
         aadd( obj_,o_ )
         nObj := len( obj_ )
      ELSE
         obj_[nObj] := o_
      ENDIF

      scn_[SCN_OBJ_SELECTED] := 0
      scn_[SCN_REFRESH]      := OBJ_REFRESH_LINE
      scn_[SCN_MODE]         := 0
   ENDIF

   RETURN nObj

//----------------------------------------------------------------------//

FUNCTION scrAddBox( obj_, scn_, nObj, Sel )
   LOCAL nKey,o_,border,cClr, nnObj, cFile

   DEFAULT Sel TO 0

   nnObj     := nObj
   cFile     := ''
   border    := '        '+chr( 255 )
   cClr      := 'N/X   '

   IF nnObj == 0
      sel := 2
      
      o_:= scrObjBlank()

      o_[ OBJ_TYPE       ] := OBJ_O_BOX
      o_[ OBJ_ROW        ] := scn_[ SCN_ROW_REP ]
      o_[ OBJ_COL        ] := scn_[ SCN_COL_REP ]
      o_[ OBJ_TO_ROW     ] := scn_[ SCN_ROW_REP ]
      o_[ OBJ_TO_COL     ] := scn_[ SCN_COL_REP ]
      o_[ OBJ_SECTION    ] := scrSecCur( scn_, scn_[ SCN_ROW_REP ] )
      o_[ OBJ_F_LEN      ] := 9
      o_[ OBJ_MDL_F_TYPE ]:= sel + 60

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

   IF nnObj == 0 .AND. sel <> 6
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

STATIC FUNCTION scrSetChild( /*scn_*/ )
   RETURN .f.
   
//----------------------------------------------------------------------//

FUNCTION scrAddExp( /*obj_,scn_,nObj*/ )
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
      IF empty( txt_ ) .OR. ascan( txt_,{|e_| inRange( nRepCol, e_[OBJ_COL],e_[OBJ_TO_COL] ) } ) == 0
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

   nTxt := ascan( txt_,{|e_| inRange( nRepCol, e_[ OBJ_COL ], e_[ OBJ_TO_COL ] ) } )

   IF     nMode == 1
      txt_[nTxt,OBJ_EQN] := substr( txt_[nTxt,OBJ_EQN],1,;
                        scn_[SCN_COL_REP]-txt_[nTxt,OBJ_COL] ) + ;
                                 chr( nKey ) + ;
           substr( txt_[nTxt,OBJ_EQN],scn_[SCN_COL_REP]-txt_[nTxt,OBJ_COL] + ;
                                            IF( ReadInsert(),1,2 ) )
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
            aShrink( txt_,nTxt )
            IF empty( txt_ )
               aadd( txt_,scrObjBlank() )
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   IF !empty( txt_ )
      DO WHILE .t.
         IF( n := ascan( txt_,{|e_| e_[OBJ_TO_COL] < e_[OBJ_COL] } ) ) > 0
            aShrink( txt_,n )
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
               aShrink( txt_,i )
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
         aShrink( obj_,n )
         IF empty( obj_ )
            aadd( obj_, scrObjBlank() )
         ENDIF
      ELSE
         EXIT
      ENDIF
   ENDDO

   aeval( txt_,{|e_| IF( e_[OBJ_ROW]>0,aadd( obj_,e_ ),'' ) } )   //  Now attach txt_

   DO WHILE .t.
      IF( n := ascan( obj_,{|e_| e_[OBJ_TO_COL] < e_[OBJ_COL] } ) ) > 0
         aShrink( obj_,n )
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

FUNCTION ScrGrid( grd_ )

   RETURN grd_

//----------------------------------------------------------------------//

FUNCTION scrSecCur( scn_,nRepRow )         //  Numeric id of sector
   LOCAL n,i
   n := 0
   FOR i := 1 TO len( scn_[SCN_SECTORS_] )
      n += scn_[SCN_SECTORS_,i,SCT_ROWS]
      IF nRepRow <= n
//         n1 := i
         EXIT
      ENDIF
   NEXT
   RETURN scn_[SCN_SECTORS_,i,SCT_ORDER]

//----------------------------------------------------------------------//

FUNCTION scrSecOrd( scn_,nRepRow )         //  Index of sector
   LOCAL n,n1,i
   n := 0
   FOR i := 1 TO len( scn_[SCN_SECTORS_] )
      n += scn_[SCN_SECTORS_,i,SCT_ROWS]
      IF nRepRow <= n
         n1 := i
         EXIT
      ENDIF
   NEXT
   RETURN n1

//----------------------------------------------------------------------//

FUNCTION scrSecRow( scn_,nRepRow )         //  Row position IN sector
   LOCAL n,n1,i
   n := 0   ;   n1 := 0
   FOR i := 1 TO len( scn_[SCN_SECTORS_] )
      n += scn_[SCN_SECTORS_,i,SCT_ROWS]
      IF nRepRow <= n
         n1 := nRepRow -( n - scn_[SCN_SECTORS_,i,SCT_ROWS] )
         EXIT
      ENDIF
   NEXT
   RETURN n1

//----------------------------------------------------------------------//

FUNCTION scrExp( eqn /*,cType*/ )
   RETURN eqn

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

FUNCTION scrEqnGen( /*obj_,scn_,nObj,cType,cMsg*/ )
   RETURN "" 

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
   nCols := IF( nCols == 0, cor_[ nDgn,4 ], nCols )

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

   scn_[ SCN_CLR_STATUS   ]   := "W+/BG" // 'W/B'
   scn_[ SCN_CLR_TEXT     ]   := 'W+/B'
   scn_[ SCN_CLR_BOX      ]   := 'W/B'
   scn_[ SCN_CLR_FIELD    ]   := CLR_GET
   scn_[ SCN_CLR_HILITE   ]   := iif( wvt(),'N/GR*','GR+/BG' )
   scn_[ SCN_CLR_WINDOW   ]   := 'W+/BG'
   scn_[ SCN_CLR_RULER    ]   := "N/W" //'W/B'
   scn_[ SCN_CLR_MENU     ]   := "W+/B" //'W+/W'
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
   nCols := IF( nCols == 0, cor_[ nDgn,4 ], nCols )

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

STATIC FUNCTION scrSectors( obj_,scn_,nDgn /*,sct_*/ )
   LOCAL nRows

   HB_SYMBOL_UNUSED( obj_ )
   
   IF nDgn == DGN_MODULE .OR. nDgn == DGN_SCREEN
      aadd( scn_[ SCN_SECTORS_ ], { 1,'Screen', 'R    ', 100, 'w+/bg', '', .f., .f. } )
      nRows := 100
   ENDIF
   
   RETURN nRows

//----------------------------------------------------------------------//

STATIC FUNCTION scrAddPrp( scn_,sct_ )
   aadd( scn_[SCN_SECTORS_], ;
         {sct_[1],sct_[2],sct_[3],sct_[4],sct_[5],sct_[6],sct_[7],sct_[8]} )
   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION scrPicture( cType,nWidth,nDec,cPic,nScrWid )

   DO CASE
   CASE cType == 'N'
      cPic := '@Z '+replicate( '9',nScrWid-IF( nDec>0,nDec+1,0 ) )+;
                           IF( nDec>0,'.'+replicate( '9',nDec ),'' )
   CASE cType == 'C'
      IF nScrWid <> nWidth
         cPic := '@'+IF( '!' $ cPic,'!','' )+'S'+NTRIM( nScrWid )
      ENDIF
   ENDCASE

   RETURN cPic

//----------------------------------------------------------------------//

FUNCTION inRange( v,r1,r2 )
   RETURN( v >= r1 .AND. v <= r2 )

/*----------------------------------------------------------------------*/

FUNCTION help( cToken )   

   DO CASE
   CASE cToken == "this"
      
   ENDCASE 
         
   RETURN NIL
   
/*----------------------------------------------------------------------*/
   
FUNCTION arview()
   RETURN NIL
   
/*----------------------------------------------------------------------*/

FUNCTION pad_max( a_,lNum,max )
   LOCAL i := 1
   DEFAULT lNum TO .f.
   IF max == NIL
      max := 0
      aeval( a_, {|x| max := max( max,len( x ) )} )
   ENDIF
   aeval( a_, {|x| a_[ i ] := iif( lNum, str( i,3 ) + '  ', '' ) + pad( x,max ), i++ } )
   RETURN a_

/*----------------------------------------------------------------------*/
   
FUNCTION VouchInArray( v,a_ )
   RETURN( ascan( a_,{|e| e = v } ) > 0 )

//----------------------------------------------------------------------//

FUNCTION aShrink( a_,n )
   IF n > 0
      adel( a_,n )
      asize( a_,len( a_ )-1 )
   ENDIF
   RETURN a_

//----------------------------------------------------------------------//

FUNCTION uiDebug( ... )
   LOCAL a_:= hb_aParams()
   LOCAL s := "", x
   
   FOR EACH x IN a_
      s += xtos( x ) + "   "
   NEXT    
   
   WAPI_OutputDebugString( s )
      
   RETURN NIL
   
/*----------------------------------------------------------------------*/
   
FUNCTION setGetAch( v_ )
   LOCAL lCrt
   THREAD STATIC sCrt := {}
   lCrt := sCrt
   IF hb_isArray( v_ )
      sCrt := v_
   ENDIF
   RETURN lCrt
   
/*----------------------------------------------------------------------*/

FUNCTION SetOCrt( oCrt )
   LOCAL lCrt
   THREAD STATIC sCrt
   lCrt := sCrt
   IF hb_isObject( oCrt )
      sCrt := oCrt
   ENDIF
   RETURN lCrt
   
/*----------------------------------------------------------------------*/

FUNCTION SetMTBrowser( lYes )
   LOCAL l_Yes
   THREAD STATIC s_Yes := .f.
   l_yes := s_Yes
   IF valtype( lYes ) == 'L'
      s_Yes := lYes
   ENDIF
   RETURN l_Yes
   
/*----------------------------------------------------------------------*/

FUNCTION SetMTModule( lYes )
   LOCAL l_Yes
   THREAD STATIC s_Yes := .f.
   l_yes := s_Yes
   IF valtype( lYes ) == 'L'
      s_Yes := lYes
   ENDIF
   RETURN l_Yes
   
/*----------------------------------------------------------------------*/

FUNCTION Vou_CreateOCrt( nT, nL, nB, nR, cTitle, xIcon, lModal, lRowCols, lHidden, ;
                                                  lCenter, nRow, nCol, lNoTitleBar )
   LOCAL oCrt, aPos

   DEFAULT cTitle      TO 'Info'
   DEFAULT xIcon       TO ""
   DEFAULT lModal      TO .T.
   DEFAULT lHidden     TO .F.
   DEFAULT lCenter     TO .F.
   DEFAULT lNoTitleBar TO .F.

   IF hb_isObject( SetOCrt() )
      SetOCrt():toFront()
   ENDIF

   aPos := iif( lCenter, {-1,-1}, iif( nRow == NIL, { nT, nL }, { nRow,nCol } ) )

   oCrt := WvgCrt():new( , , aPos, { nB - nT, nR - nL }, , .f. )
   oCrt:resizable := lRowCols
   oCrt:lModal := lModal
   IF lRowCols
      oCrt:resizeMode := HB_GTI_RESIZEMODE_ROWS
   ENDIF
   oCrt:CREATE()
   oCrt:cargo := GetCrtCargoSlots()
   oCrt:cargo[ 5 ] := oCrt:resizable

   SetCursor( 0 )

   IF hb_isNumeric( xIcon )
      hb_gtInfo( HB_GTI_ICONRES, xIcon )
   ELSE
      IF( '.ico' $ lower( xIcon ) )
         hb_gtInfo( HB_GTI_ICONFILE, xIcon )
      ELSE
         IF ! empty( xIcon )
            hb_gtInfo( HB_GTI_ICONRES, xIcon )
         ENDIF    
      ENDIF
   ENDIF

   hb_gtInfo( HB_GTI_WINTITLE, cTitle )

   SetColor( 'N/W' )
   CLS
   IF !( lHidden )
      oCrt:showWindow()
   ENDIF

   RETURN oCrt

/*----------------------------------------------------------------------*/

FUNCTION Vou_DestroyOCrt( oCrt, oOCrt )

   oCrt:destroy()
   SetOCrt( oOCrt )
   #IF 0
   IF hb_isObject( oOCrt )
      //oOCrt:setFocus()
   ENDIF
   #ENDIF
   RETURN NIL 

/*----------------------------------------------------------------------*/

FUNCTION VouchWndSave( t, l, b, r, lBmp )
   LOCAL wnd_,crs,a_

   crs := mSetCursor( .f. )
   DEFAULT t TO 0, ;
           l TO 0, ;
           b TO maxrow(), ;
           r TO maxcol()

   IF lBmp <> NIL
      a_:= Wvt_SaveScreen( t,l,b,r )
   ENDIF
   wnd_:= { t, l, b, r, saveScreen( t,l,b,r ), a_ }

   mSetCursor( crs )

   RETURN wnd_

//----------------------------------------------------------------------//

FUNCTION VouchWndRest( wnd_, lDoNotDestroyBMP )
   LOCAL crs, bError

   bError := errorblock( {|oErr| Break( oErr ) } )
   BEGIN SEQUENCE
      crs := mSetCursor( .f. )

      RestScreen( wnd_[1], wnd_[2], wnd_[3], wnd_[4], wnd_[5] )

      IF wnd_[ 6 ] <> NIL
         DEFAULT lDoNotDestroyBMP TO .f.
         
         Wvt_RestScreen( wnd_[ 1 ], wnd_[ 2 ], wnd_[ 3 ], wnd_[ 4 ], wnd_[ 6 ], lDoNotDestroyBMP )
      ENDIF
      mSetCursor( crs )
   END
   errorblock( bError )

   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION Wvt()
   RETURN .T.
   
/*----------------------------------------------------------------------*/
   
#define CGO_POS                                   1
#define CGO_ROW                                   2
#define CGO_LSEL                                  3
#define CGO_LNUM                                  4
#define CGO_CH_                                   5
#define CGO_SEL_                                  6
#define CGO_EXE_                                  7
#define CGO_SCROL                                 8
#define CGO_LENSCR                                9

FUNCTION VouchGetArray( h_,vv_, sel_, nTop, nLft, nBtm, nRgt, title, bWhen_, bValid_, pic_, hlp, ord_ )
   LOCAL i, nSel, maxL, mLen, oCrt, oOCrt
   LOCAL nLenVrb, clr1, s, cTyp
   LOCAL nLastKey,cgo_, aScrol_,nLenMnu,pmt_:={}
   LOCAL nMaxCol := max( maxcol(), 79 )
   LOCAL nMaxRow := max( maxrow(), 24 )

   HB_SYMBOL_UNUSED( hlp )
   HB_SYMBOL_UNUSED( ord_ )
   
   IF h_== NIL .OR. valtype( h_ )<>'A' .OR. vv_== NIL .OR. valtype( vv_ )<>'A'
      RETURN {vv_, 0}
   ENDIF

   IF bWhen_ == NIL
      bWhen_:= afill( array( len( vv_ ) ), {|| .t. } )
      FOR i := 1 TO len( vv_ )
         s := h_[i]
         IF valtype( vv_[i] ) == 'L'
            bWhen_[i] := {|| VouchYN( s,oGet() ),.f. }
         ENDIF
      NEXT
   ENDIF

   IF bValid_ == NIL
      bValid_:= afill( array( len( vv_ ) ),{|| .t. } )
   ENDIF

   IF pic_ == NIL
      pic_:= array( len( vv_ ) )
      FOR i := 1 TO len( vv_ )
         cTyp := valtype( vv_[i] )
         pic_[i] := IF( cTyp=="C","@ ",IF( cTyp=="N","@Z 99999999.999",IF( cTyp=="L","Y","@ " ) ) )
      NEXT
   ENDIF

   nLenVrb := 0
   aeval( vv_, {|e| cTyp := valtype( e ), nLenVrb := max( ;
                  IF( cTyp == 'C', len( e ), ;
                         IF( cTyp=='N', 15, IF( cTyp=='D',8,3 ) ) ), nLenVrb ) } )
   pmt_:={}
   aeval( h_,{|e,i| aadd( pmt_, e + " {"+xtos( vv_[i] )+ "}" ) } )

   //  decide maximum length of the largest prompt
   mLen := 0
   aeval( pmt_, {|x| mLen := max( mLen, len( x ) ) } )
   mLen := max( len( h_[1] )+2+nLenVrb, mLen )+2

   IF nTop == NIL
      nTop := int( ( nMaxrow - min( IF( wvt(),2,3 ) + len( h_ ), nMaxRow-3 ) ) / 2 )
   ENDIF
   IF nBtm == NIL
      nBtm := min( nTop + len( h_ ) + IF( wvt(),2,3 ), nMaxRow-3 )
   ENDIF
   nTop := max( nTop, 2 )

   IF nLft == NIL
      nLft := max( 4, int( ( nMaxCol - min( 2+mLen, nMaxCol-8 ) ) / 2 ) )
   ENDIF

   nLft := 2
   nRgt = min( nLft+2+mLen+2, nMaxCol-2 )

   IF title == NIL .OR. empty( title )
      title = "Untitled"
   ELSE
      title := alltrim( title )
   ENDIF
   title := padc( title, nRgt - nLft )
   title := { title, replicate( chr( 196 ), len( title ) + 2 ) }
   maxL  := len( h_[ 1 ] )
   sel_  := IF( sel_ == NIL,.t., sel_ )

   setcursor( 0 )

   B_CRT nTop,nLft,nBtm,nRgt MODAL TITLE alltrim( title[ 1 ] ) INTO oCrt
   oOCrt := SetOCrt( oCrt )

   nBtm -= nTop; nRgt -= nLft; nTop := 0; nLft := 0

   SetColor( 'b+/w*, w+/b*,,, n+/w*' )

   aScrol_ := ScrolBarNew( nTop+1, nRgt, nBtm, "B+/W" )
   DispBox( nTop, nLft, nBtm, nRgt, '         ', 'N/W' )

   Wvg_BoxRecessed( nTop+2,nLft+2,nBtm-1,nRgt-2 )
   Wvt_SetPen( 2, 0, VouchRgb( 225,225,225 ) )
   Wvg_Object( GOBJ_OBJTYPE_GRIDHORZ, {|| { nTop+3, nLft+2, nRgt-2, nBtm-( nTop+2 ) } } )

   nLenMnu := len( pmt_ )
   clr1    := setColor()

   ScrolBarDisplay( aScrol_ )
   ScrolBarUpdate( aScrol_, 1, nLenMnu, .t. )

   cgo_:= { 1, 0, .f., .f., pmt_, sel_,/*exe_*/, aScrol_, nLenMnu }

   SetGetAch( vv_ )           //  Put on stack FOR aChPut(), aChGet()

   DO WHILE .t.
      setColor( clr1 )

      pmt_:= {}
      aeval( h_, {|e,i| aadd( pmt_, e+" {"+xtos( vv_[i] )+"}" ) } )
      cgo_[ CGO_CH_ ] := pmt_

      clear typeahead

      nSel := VouchAChoice( nTop+IF( wvt(),2,3 ),nLft+1+IF( wvt(),1,0 ),nBtm-1,nRgt-1-IF( wvt(),1,0 ), ;
                       cgo_[CGO_CH_], cgo_[CGO_SEL_], "VouchFunc1", ;
                       cgo_[CGO_POS], cgo_[CGO_ROW],/* oWin */, @nLastKey, cgo_ )

      IF  nLastKey == K_ENTER
         vv_[ nSel ]  := VouchGetChoice( vv_[ nSel ], nTop + cgo_[ CGO_ROW ] + IF( wvt(),2,3 ), ;
                           nLft + maxL + 1, nRgt-IF( wvt(),2,1 ), bWhen_[ nSel ], ;
                           bValid_[ nSel ], pic_[ nSel ] )
      ELSEIF nLastKey == K_F10
         EXIT
      ELSEIF nLastKey == K_ESC
         EXIT
      ELSEIF nLastKey == K_CTRL_ENTER
         EXIT
      ELSEIF nLastKey == K_CTRL_END
         EXIT
      ENDIF
   ENDDO

   Vou_DestroyOCrt( oCrt,oOCrt )

   RETURN{ vv_, nSel }

//----------------------------------------------------------------------//

FUNCTION VouchFunc1( mode, nElem, nRow, nKey, cgo_ )
   LOCAL ret := AC_CONT

   IF nKey <> 0
      ScrolBarUpdate( cgo_[CGO_SCROL], nElem, cgo_[CGO_LENSCR], .t. )
   ENDIF

   cgo_[CGO_POS] := nElem
   cgo_[CGO_ROW] := nRow

   DO CASE
   CASE mode == AC_IDLE
   CASE mode == AC_HITTOP
      //NannyBoo
   CASE mode == AC_HITBOTTOM
      //Charge
   CASE mode == AC_NOITEM
      ret := AC_ABORT
   OTHERWISE
      DO CASE
      CASE nKey == K_CTRL_END
         ret := AC_SELECT
      CASE nKey == K_ENTER
         ret := AC_SELECT
      CASE nKey == K_CTRL_ENTER
         ret := AC_SELECT
      CASE nKey == K_F10
         ret := AC_SELECT
      CASE nKey == K_ESC
         ret := AC_ABORT
      CASE nKey > 31 .AND. nKey < 123
         cgo_[CGO_POS] := scan_ff( cgo_[CGO_POS], cgo_[CGO_CH_], chr( nKey ), 3 )
         RETURN AC_ABORT
      ENDCASE
   ENDCASE

   RETURN ret

//----------------------------------------------------------------------//

STATIC FUNCTION scan_ff( elem, a_, c /*, nFrom */ )
   LOCAL na, nlen

   c := lower( substr( c,1,1 ) )  
   nLen := len( c )
   IF( na := ascan( a_,{|e| lower( substr( ltrim( e ),1,nLen ) ) == c }, min( elem+1, len( a_ ) ) ) ) == 0
      na := ascan( a_,{|e| lower( substr( ltrim( e ),1,nlen ) ) == c },1,elem-1 )
   ENDIF

   RETURN iif( na == 0, elem, na )

//----------------------------------------------------------------------//

STATIC FUNCTION VouchGetChoice( vrb, row, col, e_col, whn, vld, pic )
   LOCAL scr, maxL, n_vrb, dec, r, c, r1, c1, crs, clr
   LOCAL type := valtype( vrb )
   LOCAL getlist := {}

   IF type == "N"
      n_vrb := str( vrb )
      maxL  := len( n_vrb )
      dec   := at( ".", n_vrb )
      IF pic == NIL
         IF dec > 0
            pic := replicate( "9", maxL -( maxL - dec ) - 1 ) + "." + ;
                                      replicate( "9", maxL - dec )
         ELSE
            pic := replicate( "9", maxL )
         ENDIF
      ENDIF
   ELSEIF type == "D"
      maxL := 8
      pic := ""
   ELSEIF type == "L"
      maxL := 1
      pic := "Y"
   ELSEIF type == "C"
      maxL := len( vrb )
      pic  := "@K"
      IF( maxL + col ) > e_col
         maxL := e_col -( col+1 )
         pic  := pic + "S" + ltrim( str( maxL ) )
      ENDIF
   ENDIF

   r  := row
   c  := col
   r1 := r
   c1 := e_col

   scr := VouchWndSave( r-1, c-1, r1, c1 )

   IF wvt()
      SetPaint( 'GET_ACH',3,{|| Wvt_DrawBoxGet( r, c+2, maxL ) } )
      clr := SetColor( 'W+/W*,N/GR*,,,W/GR*' )
      dispOutAt( r, c+1, space( c1-c ), 'W+/W*' )
   ELSE
      @ r, c clear TO r1, c1
   ENDIF

   crs := setcursor( 1 )
   @ r, c+1+IF( Wvt(),1,0 ) get vrb when whn() valid vld() picture pic
   atail( getlist ):cargo := { whn,vld }
   read
   setcursor( crs )
   SetColor( clr )

   IF wvt()
      DelPaint( 'GET_ACH',3 )
   ENDIF

   VouchWndRest( scr )
   RETURN vrb

//----------------------------------------------------------------------//

STATIC FUNCTION ScrolBarUpdate()
   RETURN NIL
   
/*----------------------------------------------------------------------*/
   
STATIC FUNCTION ScrolBarDisplay()
   RETURN NIL
   
/*----------------------------------------------------------------------*/
   
STATIC FUNCTION ScrolBarNew()
   RETURN NIL
   
/*----------------------------------------------------------------------*/

STATIC FUNCTION whn()
   RETURN eval( getActive():cargo[1] )

//----------------------------------------------------------------------//

STATIC FUNCTION vld()
   RETURN eval( getActive():cargo[2] )

//----------------------------------------------------------------------//

STATIC FUNCTION oAchGet( n )
   RETURN setGetAch()[n]

//----------------------------------------------------------------------//

STATIC FUNCTION oAchPut( n,v )
   setGetAch()[n] := v
   RETURN .t.

//----------------------------------------------------------------------//

STATIC FUNCTION oCPut( v )
   getactive():varPut( v )
   RETURN .t.

//----------------------------------------------------------------------//

STATIC FUNCTION oGet()
   RETURN getActive():varGet()

//----------------------------------------------------------------------//

FUNCTION VouchGetSome( msg, vrb, pass, pic, set_, wh, vl, nLastKey )
   LOCAL nMaxLen, nLenMsg, nLenVrb, r, oldCrt, oCrt, l
   LOCAL t       := maxrow()-7
   LOCAL b       := maxrow()-3
   LOCAL GetList := {}
   LOCAL dType   := valtype( vrb )

   DEFAULT msg  TO 'Please Enter Required Value'
   DEFAULT wh   TO {|| .t. }
   DEFAULT vl   TO {|| .t. }
   DEFAULT pass TO .f.
   DEFAULT pic  TO IF( dType == 'Y', 'Y', '@! ' )

   IF set_<> NIL
      msg += ' <F6 Select>'
   ENDIF

   nMaxLen := 90
   nLenMsg := len( msg )

   DO CASE
   CASE dType == 'D' ; nLenVrb := 8
   CASE dType == 'N' ; nLenVrb := 17   //  len( str( vrb ) )
   CASE dType == 'C' ; nLenVrb := len( vrb )
   CASE dType == 'L' ; nLenVrb := 1
   ENDCASE

   IF nLenMsg + nLenVrb > nMaxLen   //  Only when vrb type c will be asked
      nLenVrb := nMaxLen - nLenMsg - 7
      pic     := substr( pic,1,1 )+'S'+NTRIM( nLenVrb )+substr( pic,2 )
   ENDIF

   pic := IF( dType=='N', '@Z 999999999999.9999', IF( !pass, pic, strtran( pic,'!','' ) ) )
   l   := IF( maxcol() > nLenMsg+nLenVrb+6,( ( maxcol()+1 -( nLenMsg + nLenVrb + 7 ) ) / 2 ), 0 )
   r   := l + nLenMsg+nLenVrb + 6

   /* REQUEST New Window */
   B_CRT t,l,b,r MODAL TITLE '  Info Required!' ICON 'EXCLAIM' INTO oCrt
   oldCrt := SetOCrt( oCrt )

   b -= t
   r -= l
   t := 0
   l := 0

   IF !pass
      setcolor( 'GR+/B,N/GR*, , ,N/GR*' )
   ELSE
      setcolor( 'GR+/B,GR+/GR*, , ,N/GR*' )
   ENDIF

   Wvg_BoxRecessed( t+2, l+3+nlenMsg+1, t+2, l+3+nlenMsg+nLenVrb )
   DispBox( t, l, b, r, '         ', 'N/B' )
   oCrt:refresh()
   
   IF nLastKey <> NIL
      KeyBoard( chr( nLastKey ) )
   ENDIF

   @ t+2, l+3 SAY msg GET vrb PICTURE pic  WHEN eval( wh ) VALID eval( vl )
   setCursor( 1 )
   READ
   
   Vou_DestroyOCrt( oCrt, oldCrt )

   RETURN vrb

//----------------------------------------------------------------------//

FUNCTION GetCrtCargoSlots()
   RETURN { .f.,.f.,.f.,.f.,.f.,.f.,.f.,.f.,.f.,.f. }

/*----------------------------------------------------------------------*/

FUNCTION xtos( x )
   LOCAL type := valtype( x )
   DO CASE
   CASE type == 'C'
      RETURN alltrim( x )
   CASE type == 'D'
      RETURN dtoc( x )
   CASE type == 'L'
      RETURN iif( x, 'Y', 'N' )
   CASE type == 'N'
      RETURN ltrim( str( x ) )
   ENDCASE
   RETURN ""

//----------------------------------------------------------------------//

FUNCTION VouchRgb( nR, nG, nB )
   RETURN ( nR +( nG * 256 ) +( nB * 256 * 256 ) )
   
//---------------------------------------------------------------------//

FUNCTION VouchYN( msg, nInit )
   LOCAL g := getactive(), sel
   
   msg  := IF( msg==NIL,'',msg )
   nInit := IF( nInit==NIL,1,IF( valtype( nInit )=='N',nInit,IF( nInit,1,2 ) ) )
   
   B_MSG msg CHOOSE 'Yes','No ' TRIGGER {1,1} INITIAL nInit ;
   RESTORE SHADOW AT g:row - 3, g:col INTO sel
   
   IF g <> NIL
      g:varPut( IF( sel == 1, .t., .f. ) )
   ENDIF
   
   RETURN sel == 1

//----------------------------------------------------------------------//

FUNCTION VouchMenuMM( mnu_,nInit,msg,lExact,aSel )
   LOCAL n, i, t, m_:={}

   DEFAULT nInit  TO getActive():varGet()
   DEFAULT msg    TO 'Select an Option'
   DEFAULT lExact TO .f.
   DEFAULT aSel   TO {}

   aSel := asize( aSel, len( mnu_ ) )
   FOR i := 1 TO len( mnu_ )
      DEFAULT aSel[ i ] TO .t.
   NEXT

   aeval( mnu_,{|e_| aadd( m_,e_[ 1 ] ) } )

   IF( t := valtype( nInit ) == 'C' )
      //nInit := IF( lExact, nInit, trim( nInit ) )
   ENDIF

   n := max( 1, ascan( mnu_, {|e_| ;
      IF( t, IF( lExact, nInit, trim( nInit ) ) $ e_[ 2 ], nInit = e_[ 2 ] )  } ) )

   B_MSG msg CHOOSE m_ INITIAL n SELECTABLES aSel RESTORE SHADOW AT row()-3,col() WVT .T. INTO n
   n := max( 1,n )

   getActive():varPut( IF( t,pad( mnu_[n,2],len( nInit ) ),mnu_[n,2] ) )

   RETURN .f.

//----------------------------------------------------------------------//

FUNCTION VouchMenuM( id,nInit,msg )
   LOCAL n, m_:={},t, mnu_

   DEFAULT msg   TO 'Select'
   DEFAULT nInit TO getActive():varGet()

   mnu_:={}
   DO CASE
   CASE id == "MN_TYFLD"
      aadd( mnu_, { "Character", "C" } )
      aadd( mnu_, { "Numeric"  , "N" } )
      aadd( mnu_, { "Date"     , "D" } )
      aadd( mnu_, { "Logical"  , "L" } )
         
   CASE id == "MN_PGSZE"   
      aadd( mnu_, { "A4", "A4" } )
   ENDCASE    
      
   aeval( mnu_,{|e_| aadd( m_,e_[ 1 ] ) } )
   t := valtype( nInit ) == 'C'
   n := max( 1, ascan( mnu_, {|e_| iif( t, trim( nInit ) $ e_[ 2 ], nInit == e_[ 2 ] ) } ) )

   B_MSG msg CHOOSE m_ INITIAL n INTO n RESTORE SHADOW AT row()-3,col() WVT .T.
   n := max( 1,n )

   getActive():varPut( mnu_[n,2] )
   
   RETURN .f.   //  Note, because the FUNCTION is used IN when clause

//----------------------------------------------------------------------//

#define VOU_MAXROW  24

FUNCTION VouchMsgBox( r1, c1, r2, c2, width, depth, msg_, msgClr, ;
                      ch_, chClr, wait, restore, paste, shadow, trg_, sel, lSelect_, abr, ;
                      lSlctns, lLeftRight, center, tagged_,lNumeric,help,exe_,num_,;
                      lNoXpp, oWin, cIcon, lWvt, nAlign )

   LOCAL msgLen := 0, chLen := 0, maxLen, pmtWidth
   LOCAL boxWide, boxDeep, tBoxDeep
   LOCAL cr1, i, oGet, oVal, gap, mCrs,n,nLastKey
   LOCAL nLenScrol, nMsg, nOff, aScrolbar
   LOCAL oldPnt_, oCrt, oldCrt
   LOCAL nSlctns_:={}, dd_:={}, cgo_:={}
   LOCAL xRet := NIL
   
   HB_SYMBOL_UNUSED( trg_ )
   HB_SYMBOL_UNUSED( help )
   
   DEFAULT lNoXpp     TO .f.
   DEFAULT nAlign     TO 2  

   DEFAULT r1         TO row()
   DEFAULT c1         TO col()
   DEFAULT msg_       TO {}
   DEFAULT ch_        TO {}
   DEFAULT lSelect_   TO {}
   DEFAULT msgClr     TO 1 //C_NORMAL
   DEFAULT chClr      TO 3 //C_ENHANCED
   DEFAULT restore    TO .f.
   DEFAULT paste      TO .f.
   DEFAULT shadow     TO .f.
   DEFAULT abr        TO .f.
   DEFAULT lSlctns    TO .f.
   DEFAULT lLeftRight TO .f.
   DEFAULT center     TO .f.
   DEFAULT tagged_    TO {}
   DEFAULT lNumeric   TO .f.
   DEFAULT num_       TO {}

   // grab get OBJECT
   oGet := iif( paste, getactive(), oGet )

   // NIL is used TO indicate no MESSAGE is desired, just choices
   IF( len( msg_ ) > 0 ) .AND. ( valtype( msg_[1] ) == "A" )
      msg_ := aclone( msg_[ 1 ] )
   ENDIF

   IF( len( msg_ ) > 0 ) .AND. ( msg_[1] == NIL )
      msg_ := {}
   ENDIF

   IF( len( ch_ ) > 0 ) .AND. ( valtype( ch_[1] ) = "A" )
      ch_ := aclone( ch_[ 1 ] )
   ENDIF

   IF len( msg_ ) == 0 .AND. len( ch_ ) == 0
      RETURN .f.
   ENDIF

   IF lSlctns
      IF lNumeric
         IF empty( num_ )
            FOR i := 1 TO len( ch_ )
               ch_[i] := '    '+ch_[i]
            NEXT
         ELSE
            FOR i := 1 TO len( ch_ )
               IF( n := ascan( num_,i ) )==0
                  ch_[ i ] := '    '+ch_[i]
               ELSE
                  ch_[ i ] := pad( NTRIM( n ), 4 ) + ch_[ i ]
               ENDIF
            NEXT
         ENDIF
      ELSE
         FOR i := 1 TO len( ch_ )
            ch_[ i ] := iif( empty( tagged_ ),'  ', ;
                     iif( tagged_[ i ], chr( 251 ) +' ','  ' ) ) + ch_[ i ]
         NEXT
      ENDIF
   ENDIF

   aeval( msg_, {|s| msgLen := max( msgLen, len( s ) ) } )
   aeval( ch_,  {|s| chLen  := max( chLen,  len( s ) ) } )
   maxlen := max( msgLen, chLen )
   aeval( ch_, {|s,i| s:=s, ch_[i] := pad( ch_[i], maxLen ) } )

   IF empty( lSelect_ )
      lSelect_:= {}
      aeval( ch_,  {|s| aadd( lSelect_, IF( empty( s ), .f., .t. ) ) } )
   ELSE
      aeval( ch_, {|s,i| lSelect_[i] := IF( empty( s ),.f.,lSelect_[i] ) } )
   ENDIF
   IF ascan( lSelect_, {|e| e } ) == 0
      IF len( ch_ ) > 0
         RETURN 0
      ENDIF
   ENDIF

   nMsg := Len( msg_ )
   nOff := IF( nMsg == 1, 0,  1 )

   boxDeep  := IF( len( msg_ )=0,0,len( msg_ )+1 ) + ;
               IF( len( ch_ )=0,0,len( ch_ )+1 )

   IF nMsg == 1
      boxDeep--
   ENDIF

   tBoxDeep := boxDeep
   boxWide  := max( msgLen, chLen ) + 3

   DEFAULT r2 TO r1 + iif( depth = NIL, boxDeep, depth )
   DEFAULT c2 TO c1 + iif( width = NIL, boxWide, width )

   IF center
      r1 := int( ( maxrow() - tBoxDeep ) / 2 )
      r1 := IF( r1 < 0, 0, r1 )
      r2 := r1 + tBoxDeep
      IF r2 > VOU_MAXROW 
         r2 := VOU_MAXROW
      ENDIF
      c1 := int( ( maxcol() - boxWide ) / 2 )
      c1 := IF( c1 < 0, 3, c1 )
      c2 := c1 + boxWide
   ELSE
      IF r2 <= r1
         r2 := maxrow()
      ENDIF
      IF c2 <= c1
         c2 := maxcol()
      ENDIF

      // IF coordinates are off the screen, confine them
      IF r2 > VOU_MAXROW
         gap := r2 - VOU_MAXROW 
         r2  := r2 - gap
         r1  := r1 - gap
      ENDIF

      IF c2 > maxcol() - 4
         gap :=( c2 - maxcol() + 4 )
         c2  := c2 - gap
         c1  := c1 - gap
      ENDIF
   ENDIF

   lWvt := .t.

   IF sel == NIL .OR. sel < 1 .OR. sel > len( ch_ )
      sel := 1
   ENDIF

   mCrs   := mSetCursor( .f. )
   oldPnt_:= WvtSetPaint( {} )

   r2 -= IF( nMsg == 1, 1, 0 )

   //                          CREATE New Window
   B_CRT r1,c1,r2,c2 MODAL TITLE IF( len( msg_ ) == 1, msg_[ 1 ], 'Selections' ) ICON cIcon INTO oCrt
   oldCrt := SetOCrt( oCrt )

   // Adjust coordinates
   r2 -= r1 
   c2 -= c1
   r1 := 0
   c1 := 0

   IF len( ch_ ) > 0
      DispBox( r1 + nOff + nMsg + IF( nMsg <= 1, 0, 1 ), c1+2, r2-1, c2-2, '         ', 'W/W*' )
      Wvg_Outline( r1 + nOff + nMsg + IF( nMsg <= 1, 0, 1 ), c1+2, r2-1, c2-2 )
   ENDIF

   FOR i = 1 TO min( len( msg_ ), r2 -( r1 + 1 ) )
      IF len( msg_ ) > 1
         IF     nAlign == 2
            dispOutAt( r1 + i, c1+2, padc( msg_[ i ], c2 -( c1+3 ) ), 'N/W' )
         ELSEIF nAlign == 0
            dispOutAt( r1 + i, c1+2, pad( msg_[ i ], c2 -( c1+3 ) ), 'N/W' )
         ELSEIF nAlign == 1
            dispOutAt( r1 + i, c1+2, padr( msg_[ i ], c2 -( c1+3 ) ), 'N/W' )
         ENDIF
      ENDIF
   NEXT i
   mSetCursor( mCrs )

   IF len( ch_ ) > 0
      IF nMsg > 1
         mCrs := mSetCursor( .f. )
         mSetCursor( mCrs )
         cr1 := r1 + nMsg + 2

         IF nMsg == 1
            cr1--
         ENDIF
      ELSE
         cr1 := r1 + 1
      ENDIF

      SetColor( 'N/W*,W+/B*,,,W/W*' )
      aScrolBar := ScrolBarNew( cr1 - 1, c2, r2, iif( wvt(),'B+/W', "b+/w" ) )

      nLenScrol := len( ch_ )
      pmtWidth  := c2-c1-3
      aeval( ch_, {|e,i| ch_[ i ] := pad( e,pmtWidth ) } )

      IF .t.
         ScrolBarUpdate( aScrolBar, sel, nLenScrol, .t. )

         cgo_:= { sel, 0, lSlctns, lNumeric, ch_, lSelect_, exe_, aScrolbar, nLenScrol }

         DO WHILE .t.
            sel := VouchAChoice( cr1, c1 + 2, r2 - 1, c1 +( c2 - c1 ) - 2, ;
                                 cgo_[CGO_CH_], cgo_[CGO_SEL_], "VouchFunc2", ;
                                 cgo_[CGO_POS], cgo_[CGO_ROW], oWin, ;
                                 @nLastKey, @cgo_ )
            IF ! lSlctns
               EXIT
            ELSE
               IF VouchInArray( nLastKey, { K_ESC, K_CTRL_ENTER, K_ALT_F7 } )
                  EXIT
               ENDIF
            ENDIF
         ENDDO
      ENDIF         
      
   ELSEIF valtype( wait ) = "N"
      sel := inkey( wait )
   ENDIF

   Vou_DestroyOCrt( oCrt, oldCrt )

   WvtSetPaint( oldPnt_ )

   IF paste
      IF valtype( oGet:varGet() ) == "C"
         oVal := oGet:varGet()
         oGet:varPut( pad( ch_[ IF( sel = 0,1,sel ) ], len( oVal ) ) )
         oGet:display()
      ENDIF
   ENDIF

   IF lSlctns
      IF !lNumeric
         FOR i = 1 TO len( cgo_[CGO_CH_] )
            IF substr( cgo_[CGO_CH_,i], 1, 1 ) == chr( 251 ) /*CHECKMARK*/
               aadd( nSlctns_,i )
            ENDIF
         NEXT
      ELSE
         FOR i := 1 TO len( cgo_[CGO_CH_] )
            IF val( left( cgo_[CGO_CH_,i],4 ) )>0
               aadd( dd_,{val( left( cgo_[CGO_CH_,i],4 ) ),i} )
            ENDIF
         NEXT
         IF !empty( dd_ )
            asort( dd_,,,{|e_,f_| e_[1]<f_[1] } )
            aeval( dd_,{|e_| aadd( nSlctns_,e_[2] ) } )
         ENDIF
      ENDIF
   ENDIF

   IF hb_isLogical( xRet ) .AND. xRet
      KEYBOARD Chr( K_RETURN )
   ENDIF

   RETURN iif( lSlctns, nSlctns_, sel )

//----------------------------------------------------------------------//

FUNCTION VouchFunc2( nMode, nElem, nRel, nKey, cgo_ )
   LOCAL n, i, nn, s

   IF nKey <> 0 .AND. nKey <> K_MOVING
      ScrolBarUpdate( cgo_[ CGO_SCROL ], nElem, cgo_[ CGO_LENSCR ], .t. )
      IF cgo_[ CGO_EXE_ ] <> NIL
         eval( cgo_[ CGO_EXE_,nElem ] )
      ENDIF
   ENDIF

   cgo_[CGO_POS] := nElem
   cgo_[CGO_ROW] := nRel

   DO CASE
   CASE nKey == K_F1
      //  help()
      RETURN AC_CONT
   CASE nmode = AC_IDLE
      RETURN AC_CONT
   CASE nmode = AC_HITTOP
      KEYBOARD CHR( K_CTRL_PGDN )
      RETURN AC_CONT
   CASE nmode = AC_HITBOTTOM
      KEYBOARD CHR( K_CTRL_PGUP )
      RETURN AC_CONT
   CASE nmode = AC_EXCEPT
      DO CASE
      CASE nKey == K_F1
         //  help()
         RETURN AC_CONT
      CASE nKey = K_ESC
         RETURN AC_ABORT
      CASE nKey == K_F9      // TAG ALL
         IF cgo_[CGO_LSEL]
            IF cgo_[CGO_LNUM]
               FOR i := 1 TO len( cgo_[CGO_CH_] )
                  IF cgo_[CGO_SEL_]
                     cgo_[CGO_CH_,i] := chr( 251 ) + substr( cgo_[ CGO_CH_,i ], 2 )
                  ENDIF
               NEXT
               RETURN AC_ABORT
            ELSE
               RETURN AC_CONT
            ENDIF
         ELSE
            RETURN AC_CONT
         ENDIF
      CASE nKey == K_F10      // UnTAG ALL
         IF cgo_[CGO_LSEL]
            FOR i := 1 TO len( cgo_[CGO_CH_] )
               cgo_[CGO_CH_,i] := " "+substr( cgo_[CGO_CH_,i],2 )
            NEXT
            RETURN AC_ABORT
         ELSE
            RETURN AC_CONT
         ENDIF

      CASE nKey = K_ENTER
         IF cgo_[CGO_LSEL]
            IF !cgo_[CGO_LNUM]
               cgo_[CGO_CH_,cgo_[CGO_POS]] := IF( substr( cgo_[CGO_CH_,cgo_[CGO_POS]],1,1 )==CHECKMARK, ;
                             " ",CHECKMARK )+substr( cgo_[CGO_CH_,cgo_[CGO_POS]],2 )
               cgo_[CGO_POS] := min( cgo_[CGO_POS]+1,len( cgo_[CGO_CH_] ) )
               RETURN AC_ABORT
            ELSE
               IF( n:=val( substr( cgo_[CGO_CH_,cgo_[CGO_POS]],1,4 ) ) )>0
                  cgo_[CGO_CH_,cgo_[CGO_POS]] := "    "+substr( cgo_[CGO_CH_,cgo_[CGO_POS]],5 )
                  cgo_[CGO_POS] := min( cgo_[CGO_POS]+1,len( cgo_[CGO_CH_] ) )
                  FOR i := 1 TO len( cgo_[CGO_CH_] )
                     IF( nn := val( left( cgo_[CGO_CH_,i],4 ) ) )>0
                        IF nn > n
                           nn := nn - 1
                           s := IF( nn > 0,pad( NTRIM( nn ),4 ),"    " )
                           cgo_[CGO_CH_,i] := s + substr( cgo_[CGO_CH_,i],5 )
                        ENDIF
                     ENDIF
                  NEXT
               ELSE
                  nn := 0
                  n  := 0
                  aeval( cgo_[CGO_CH_], {|e| n := val( left( e,4 ) ), nn := IF( n>nn,n,nn ) } )
                  cgo_[CGO_CH_,cgo_[CGO_POS]] := pad( NTRIM( nn+1 ),4 ) + substr( cgo_[CGO_CH_,cgo_[CGO_POS]],5 )
                  cgo_[CGO_POS] := min( cgo_[CGO_POS]+1, len( cgo_[CGO_CH_] ) )
               ENDIF
               RETURN AC_ABORT
            ENDIF
         ELSE
            RETURN AC_SELECT
         ENDIF

      CASE nKey = K_CTRL_ENTER
         RETURN AC_SELECT
      CASE nKey = HB_K_RESIZE
         RETURN AC_CONT
      OTHERWISE
         IF cgo_[CGO_LSEL]
            cgo_[CGO_POS] := scan_f( cgo_[CGO_POS], cgo_[CGO_CH_], nKey, iif( !cgo_[CGO_LNUM],3,5 ) )
            RETURN AC_ABORT
         ELSE
            RETURN AC_GOTO
         ENDIF
      ENDCASE
   CASE nmode = AC_NOITEM
      RETURN AC_ABORT
   OTHERWISE
      RETURN AC_GOTO
   ENDCASE

   RETURN AC_CONT

//----------------------------------------------------------------------//

STATIC FUNCTION scan_f( elem, a_, key, nFrom )
   LOCAL n := elem, na, c

   c := lower( chr( key ) )
   na := ascan( a_, {|e| lower( substr( e, nFrom, 1 ) ) == c }, min( elem + 1, len( a_ ) ) )
   IF na == 0
      na := ascan( a_,{|e| lower( substr( e, nFrom, 1 ) ) == c },1,elem-1 )
   ENDIF
   IF na <> 0
      n := na
   ENDIF
   RETURN n

//----------------------------------------------------------------------//

