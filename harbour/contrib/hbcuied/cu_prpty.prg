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

