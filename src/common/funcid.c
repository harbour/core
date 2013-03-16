/*
 * Harbour Project source code:
 *    get function identifier
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbcomp.h"

typedef struct
{
   const char * szFuncName;
   int          iMinLen;
   int          flags;
   HB_FUNC_ID   funcID;
} _HB_FUNCID;

/* NOTE: THIS TABLE MUST BE SORTED ALPHABETICALLY
 */
static _HB_FUNCID s_funcId[] =
{
   { "AADD",                    0, HB_FN_RESERVED, HB_F_AADD                 },
   { "ABS",                     0, HB_FN_RESERVED, HB_F_ABS                  },
   { "ASC",                     0, HB_FN_RESERVED, HB_F_ASC                  },
   { "AT",                      0, HB_FN_RESERVED, HB_F_AT                   },
   { "BOF",                     0, HB_FN_RESERVED, HB_F_BOF                  },
   { "BREAK",                   4, HB_FN_RESERVED, HB_F_BREAK                },
   { "CDOW",                    0, HB_FN_RESERVED, HB_F_CDOW                 },
   { "CHR",                     0, HB_FN_RESERVED, HB_F_CHR                  },
   { "CMONTH",                  4, HB_FN_RESERVED, HB_F_CMONTH               },
   { "COL",                     0, HB_FN_RESERVED, HB_F_COL                  },
   { "CTOD",                    0, HB_FN_RESERVED, HB_F_CTOD                 },
   { "DATE",                    0, HB_FN_RESERVED, HB_F_DATE                 },
   { "DAY",                     0, HB_FN_RESERVED, HB_F_DAY                  },
   { "DELETED",                 4, HB_FN_RESERVED, HB_F_DELETED              },
   { "DEVPOS",                  4, HB_FN_RESERVED, HB_F_DEVPOS               },
   { "DOW",                     0, HB_FN_RESERVED, HB_F_DOW                  },
   { "DTOC",                    0, HB_FN_RESERVED, HB_F_DTOC                 },
   { "DTOS",                    0, HB_FN_RESERVED, HB_F_DTOS                 },
   { "EMPTY",                   4, HB_FN_RESERVED, HB_F_EMPTY                },
   { "EOF",                     0, HB_FN_RESERVED, HB_F_EOF                  },
   { "EVAL",                    0, HB_FN_UDF,      HB_F_EVAL                 },
   { "EXP",                     0, HB_FN_RESERVED, HB_F_EXP                  },
   { "FCOUNT",                  4, HB_FN_RESERVED, HB_F_FCOUNT               },
   { "FIELDNAME",               4, HB_FN_RESERVED, HB_F_FIELDNAME            },
   { "FILE",                    0, HB_FN_UDF,      HB_F_FILE                 },
   { "FLOCK",                   4, HB_FN_RESERVED, HB_F_FLOCK                },
   { "FOUND",                   4, HB_FN_RESERVED, HB_F_FOUND                },
   { "HB_ARRAYTOPARAMS",        0, HB_FN_UDF,      HB_F_ARRAYTOPARAMS        },
   { "HB_BCHAR",                0, HB_FN_UDF,      HB_F_BCHAR                },
   { "HB_BCODE",                0, HB_FN_UDF,      HB_F_BCODE                },
   { "HB_BITAND",               0, HB_FN_UDF,      HB_F_BITAND               },
   { "HB_BITNOT",               0, HB_FN_UDF,      HB_F_BITNOT               },
   { "HB_BITOR",                0, HB_FN_UDF,      HB_F_BITOR                },
   { "HB_BITRESET",             0, HB_FN_UDF,      HB_F_BITRESET             },
   { "HB_BITSET",               0, HB_FN_UDF,      HB_F_BITSET               },
   { "HB_BITSHIFT",             0, HB_FN_UDF,      HB_F_BITSHIFT             },
   { "HB_BITTEST",              0, HB_FN_UDF,      HB_F_BITTEST              },
   { "HB_BITXOR",               0, HB_FN_UDF,      HB_F_BITXOR               },
   { "HB_I18N_GETTEXT",         0, HB_FN_UDF,      HB_F_I18N_GETTEXT         },
   { "HB_I18N_GETTEXT_NOOP",    0, HB_FN_UDF,      HB_F_I18N_GETTEXT_NOOP    },
   { "HB_I18N_GETTEXT_STRICT",  0, HB_FN_UDF,      HB_F_I18N_GETTEXT_STRICT  },
   { "HB_I18N_NGETTEXT",        0, HB_FN_UDF,      HB_F_I18N_NGETTEXT        },
   { "HB_I18N_NGETTEXT_NOOP",   0, HB_FN_UDF,      HB_F_I18N_NGETTEXT_NOOP   },
   { "HB_I18N_NGETTEXT_STRICT", 0, HB_FN_UDF,      HB_F_I18N_NGETTEXT_STRICT },
   { "HB_STOD",                 0, HB_FN_UDF,      HB_F_STOD                 },
   { "HB_STOT",                 0, HB_FN_UDF,      HB_F_STOT                 },
   { "INKEY",                   4, HB_FN_RESERVED, HB_F_INKEY                },
   { "INT",                     0, HB_FN_RESERVED, HB_F_INT                  },
   { "LASTREC",                 4, HB_FN_RESERVED, HB_F_LASTREC              },
   { "LEFT",                    0, HB_FN_RESERVED, HB_F_LEFT                 },
   { "LEN",                     0, HB_FN_RESERVED, HB_F_LEN                  },
   { "LOCK",                    0, HB_FN_RESERVED, HB_F_LOCK                 },
   { "LOG",                     0, HB_FN_RESERVED, HB_F_LOG                  },
   { "LOWER",                   4, HB_FN_RESERVED, HB_F_LOWER                },
   { "LTRIM",                   4, HB_FN_RESERVED, HB_F_LTRIM                },
   { "MAX",                     0, HB_FN_RESERVED, HB_F_MAX                  },
   { "MIN",                     0, HB_FN_RESERVED, HB_F_MIN                  },
   { "MONTH",                   4, HB_FN_RESERVED, HB_F_MONTH                },
   { "PCOL",                    0, HB_FN_RESERVED, HB_F_PCOL                 },
   { "PCOUNT",                  4, HB_FN_RESERVED, HB_F_PCOUNT               },
   { "PROW",                    0, HB_FN_RESERVED, HB_F_PROW                 },
   { "QSELF",                   4, HB_FN_RESERVED, HB_F_QSELF                },
   { "RECCOUNT",                4, HB_FN_RESERVED, HB_F_RECCOUNT             },
   { "RECNO",                   4, HB_FN_RESERVED, HB_F_RECNO                },
   { "REPLICATE",               4, HB_FN_RESERVED, HB_F_REPLICATE            },
   { "RLOCK",                   4, HB_FN_RESERVED, HB_F_RLOCK                },
   { "ROUND",                   4, HB_FN_RESERVED, HB_F_ROUND                },
   { "ROW",                     0, HB_FN_RESERVED, HB_F_ROW                  },
   { "RTRIM",                   4, HB_FN_RESERVED, HB_F_RTRIM                },
   { "SECONDS",                 4, HB_FN_RESERVED, HB_F_SECONDS              },
   { "SELECT",                  4, HB_FN_RESERVED, HB_F_SELECT               },
   { "SETPOS",                  4, HB_FN_RESERVED, HB_F_SETPOS               },
   { "SETPOSBS",                4, HB_FN_RESERVED, HB_F_SETPOSBS             },
   { "SPACE",                   4, HB_FN_RESERVED, HB_F_SPACE                },
   { "SQRT",                    0, HB_FN_RESERVED, HB_F_SQRT                 },
   { "STOD",                    0, HB_FN_UDF,      HB_F_STOD                 },
   { "STR",                     0, HB_FN_RESERVED, HB_F_STR                  },
   { "SUBSTR",                  4, HB_FN_RESERVED, HB_F_SUBSTR               },
   { "TIME",                    0, HB_FN_RESERVED, HB_F_TIME                 },
   { "TRANSFORM",               4, HB_FN_RESERVED, HB_F_TRANSFORM            },
   { "TRIM",                    0, HB_FN_RESERVED, HB_F_TRIM                 },
   { "TYPE",                    0, HB_FN_RESERVED, HB_F_TYPE                 },
   { "UPPER",                   4, HB_FN_RESERVED, HB_F_UPPER                },
   { "VAL",                     0, HB_FN_RESERVED, HB_F_VAL                  },
   { "VALTYPE",                 4, HB_FN_RESERVED, HB_F_VALTYPE              },
   { "WORD",                    0, HB_FN_RESERVED, HB_F_WORD                 },
   { "YEAR",                    0, HB_FN_RESERVED, HB_F_YEAR                 },
   { "_GET_",                   0, HB_FN_UDF,      HB_F__GET_                }
};

const char * hb_compGetFuncID( const char * szFuncName, HB_FUNC_ID * pFunID, int * piFlags )
{
   unsigned int uiFirst = 0, uiLast = HB_SIZEOFARRAY( s_funcId ) - 1, uiMiddle;
   int i;

   do
   {
      uiMiddle = ( uiFirst + uiLast ) >> 1;
      i = strcmp( szFuncName, s_funcId[ uiMiddle ].szFuncName );
      if( i <= 0 )
         uiLast = uiMiddle;
      else
         uiFirst = uiMiddle + 1;
   }
   while( uiFirst < uiLast );

   if( uiFirst != uiMiddle )
      i = strcmp( szFuncName, s_funcId[ uiFirst ].szFuncName );

   if( i < 0 && s_funcId[ uiFirst ].iMinLen )
   {
      int iLen = ( int ) strlen( szFuncName );

      if( iLen >= s_funcId[ uiFirst ].iMinLen )
         i = strncmp( szFuncName, s_funcId[ uiFirst ].szFuncName, iLen );
   }

   if( i == 0 )
   {
      *piFlags = s_funcId[ uiFirst ].flags;
      *pFunID = s_funcId[ uiFirst ].funcID;
      return s_funcId[ uiFirst ].szFuncName;
   }

   *piFlags = HB_FN_UDF;
   *pFunID = HB_F_UDF;

   /* hack for HB_I18N_GETTEXT_[NOOP_|STRICT_]* functions */
   if( strncmp( szFuncName, "HB_I18N_", 8 ) == 0 )
   {
      const char * szName = szFuncName + 8;
      i = *szName == 'N' ? 1 : 0;
      szName += i;
      if( strncmp( szName, "GETTEXT_", 8 ) == 0 )
      {
         szName += 8;
         if( strncmp( szName, "STRICT_", 7 ) == 0 )
            *pFunID = i ? HB_F_I18N_NGETTEXT_STRICT : HB_F_I18N_GETTEXT_STRICT;
         else if( strncmp( szName, "NOOP_", 5 ) == 0 )
            *pFunID = i ? HB_F_I18N_NGETTEXT_NOOP : HB_F_I18N_GETTEXT_NOOP;
         else
            *pFunID = i ? HB_F_I18N_NGETTEXT :  HB_F_I18N_GETTEXT;
      }
   }

   return szFuncName;
}
