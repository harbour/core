/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    get function identifier
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://www.harbour-project.org
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

#include "hbcomp.h"

typedef struct
{
   const char *   szFuncName;
   int            iMinLen;
   HB_FUNC_ID     funcID;
} _HB_FUNCID;

/* NOTE: THIS TABLE MUST BE SORTED ALPHABETICALLY
 */
static _HB_FUNCID s_funcId[] =
{
   { "ASC",                      0, HB_F_ASC },
   { "AT",                       0, HB_F_AT },
   { "CHR",                      0, HB_F_CHR },
   { "CTOD",                     0, HB_F_CTOD },
   { "DTOS",                     0, HB_F_DTOS },
   { "EMPTY",                    4, HB_F_EMPTY },
   { "EVAL",                     0, HB_F_EVAL },
   { "HB_ARRAYTOPARAMS",         0, HB_F_ARRAYTOPARAMS },
   { "HB_BITAND",                0, HB_F_BITAND },
   { "HB_BITNOT",                0, HB_F_BITNOT },
   { "HB_BITOR",                 0, HB_F_BITOR },
   { "HB_BITRESET",              0, HB_F_BITRESET },
   { "HB_BITSET",                0, HB_F_BITSET },
   { "HB_BITSHIFT",              0, HB_F_BITSHIFT },
   { "HB_BITTEST",               0, HB_F_BITTEST },
   { "HB_BITXOR",                0, HB_F_BITXOR },
   { "HB_I18N_GETTEXT",          0, HB_F_I18N_GETTEXT },
   { "HB_I18N_GETTEXT_NOOP",     0, HB_F_I18N_GETTEXT_NOOP },
   { "HB_I18N_GETTEXT_STRICT",   0, HB_F_I18N_GETTEXT_STRICT },
   { "HB_I18N_NGETTEXT",         0, HB_F_I18N_NGETTEXT },
   { "HB_I18N_NGETTEXT_NOOP",    0, HB_F_I18N_NGETTEXT_NOOP },
   { "HB_I18N_NGETTEXT_STRICT",  0, HB_F_I18N_NGETTEXT_STRICT },
   { "HB_STOD",                  0, HB_F_STOD },
   { "HB_STOT",                  0, HB_F_STOT },
   { "INT",                      0, HB_F_INT },
   { "LEN",                      0, HB_F_LEN },
   { "MAX",                      0, HB_F_MAX },
   { "MIN",                      0, HB_F_MIN },
   { "STOD",                     0, HB_F_STOD },
   { "UPPER",                    4, HB_F_UPPER },
   { "_GET_",                    0, HB_F__GET_ }
};

HB_FUNC_ID hb_compGetFuncID( const char * szFuncName )
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

   if( i == 0 )
      return s_funcId[ uiFirst ].funcID;

   if( i > 0 && uiFirst )
      uiFirst--;

   if( s_funcId[ uiFirst ].iMinLen )
   {
      int iLen = ( int ) strlen( szFuncName );

      if( iLen >= s_funcId[ uiFirst ].iMinLen &&
          strncmp( szFuncName, s_funcId[ uiMiddle ].szFuncName, iLen ) == 0 )
         return s_funcId[ uiFirst ].funcID;
   }

   /* hack for HB_I18N_GETTEXT_[NOOP_|STRICT_]* functions */
   if( strncmp( szFuncName, "HB_I18N_", 8 ) == 0 )
   {
      szFuncName += 8;
      i = *szFuncName == 'N' ? 1 : 0;
      szFuncName += i;
      if( strncmp( szFuncName, "GETTEXT_", 8 ) == 0 )
      {
         if( strncmp( szFuncName, "STRICT_", 7 ) == 0 )
            return i == 0 ? HB_F_I18N_GETTEXT_STRICT : HB_F_I18N_NGETTEXT_STRICT;
         else if( strncmp( szFuncName, "NOOP_", 5 ) == 0 )
            return i == 0 ? HB_F_I18N_GETTEXT_NOOP : HB_F_I18N_NGETTEXT_NOOP;
         else
            return i == 0 ? HB_F_I18N_GETTEXT :  HB_F_I18N_NGETTEXT;
      }
   }

   return HB_F_UDF;
}
