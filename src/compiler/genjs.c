/*
 * Compiler JS source generation
 *
 * Copyright 2020 Antonino Perricone <antonino.perricone@yahoo.it>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 * (or visit their website at https://www.gnu.org/licenses/).
 *
 */

#include "hbcomp.h"
#include "hbdate.h"
#include "hbassert.h"

void hb_compGenJavascript( HB_COMP_DECL, PHB_FNAME pFileName )       /* generates the JS language output */
{
   char szFileName[ HB_PATH_MAX ];
   HB_SIZE nSize;
   HB_BYTE * pHrbBody;
   FILE * yyc;

   if( ! pFileName->szExtension )
      pFileName->szExtension = ".js";
   hb_fsFNameMerge( szFileName, pFileName );

   yyc = hb_fopen( szFileName, "wt" );
   if( ! yyc )
   {
      hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );
      return;
   }

   if( ! HB_COMP_PARAM->fQuiet )
   {
      char buffer[ 80 + HB_PATH_MAX - 1 ];
      hb_snprintf( buffer, sizeof( buffer ),
                   "Generating Javascript output to \'%s\'... ", szFileName );
      hb_compOutStd( HB_COMP_PARAM, buffer );
   }

   fprintf( yyc, "// TODO" );


   fclose( yyc );

   if( ! HB_COMP_PARAM->fQuiet )
      hb_compOutStd( HB_COMP_PARAM, "Done.\n" );
}
