/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Test VistaSoftware's Apollo database driver. See http://www.VistaSoftware.com
 *
 * Copyright 2001 Patrick Mast <email@PatrickMast.com>
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

/* ************************************ */
/* Data type identifiers for sx_Replace */
/* ************************************ */
#define R_INTEGER       1
#define R_LONG          2
#define R_DOUBLE        8
#define R_JULIAN       32
#define R_LOGICAL     128
#define R_CHAR       1024
#define R_DATESTR    1056
#define R_MEMO       3072
#define R_BITMAP     4096
#define R_BLOBFILE   8192
#define R_BLOBPTR    8193
#define R_GENERAL    8195

#define SDENTX          1   // CA-Clipper compatible DBF-NTX driver
#define SDEFOX          2   // FoxPro compatible DBF-IDX/CDX driver
#define SDENSX          3   // Vista DBF-NSX driver

#define READWRITE       0
#define READONLY        1
#define EXCLUSIVE       2

Function Main()
LOCAL nAlias,f

   ? "Apollo version " + sx_Version()

   nAlias:=sx_Use("TEST.DBF","test2",EXCLUSIVE,SDENSX)
   sx_Zap()
   IF Valtype(nAlias)="N" .AND. nAlias # 0
      ? "OK opening 'TEST.DBF'"

      ? "Adding 1000 records..."
      FOR f=1 to 1000
         sx_AppendBlank()
         sx_Replace("FIRST", R_CHAR, "Patrick " + Str( f ) )
         sx_Replace("LAST" , R_CHAR, LTrim( Str( f ) ) + " Mast" )
         sx_Replace("NOTES", R_MEMO, "This is record " + LTrim( Str( f ) ) )
         sx_Commit()
      NEXT

      ? "Creating Index..."
      sx_IndexTag(,"LAST","LAST+FIRST",0)
      ? "Created a HiPer-Six index. See 'TEST.NSX'"

      sx_GoTop()
      WHILE !sx_Eof()
         @ 17,2 SAY "RecNo: "+ LTrim( Str( sx_RecNo() ) )
         @ 18,2 SAY sx_GetString( "LAST" )
         sx_Skip(1)
      ENDDO
      sx_Close()

   ELSE
      ? "ERROR Opening 'TEST.DBF'"
   ENDIF

return