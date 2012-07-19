/*
 * $Id: ttable.prg 9576 2012-07-17 16:41:57Z andijahja $
 */

/*
 * SixAPI Project source code:
 *
 * Copyright 2010 Andi Jahja <xharbour@telkom.net.id>
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
 */

#include "hbclass.ch"

#define SX_READWRITE  1
#define SX_READONLY  2
#define SX_EXCLUSIVE  3

   // SX_DBINFO( CALIAS ) =>   ARRAY
#define SXINFO_AREA  1  // INTEGER
#define SXINFO_FILENAME  2  // STRING
#define SXINFO_ALIAS  3  // STRING
#define SXINFO_SHARED  4  // LOGICAL
#define SXINFO_READONLY  5  // LOGICAL
#define SXINFO_RDETYPE  6  // INTEGER
#define SXINFO_MODE  7  // INTEGER
#define SXINFO_RDD  8  // STRING
#define SXINFO_COMMITLEVEL 9  // INTEGER
#define SXINFO_RECSIZE  10 // INTEGER
#define SXINFO_FIELDCOUNT 11 // INTEGER
#define SXINFO_FIELDINFO 12 // ARRAY

CLASS TApollo

   VAR cDBFFile
   VAR cIndexFile
   VAR cAlias
   VAR nWorkArea
   VAR nOpenMode INIT SX_READWRITE  // { "READWRITE","READONLY","EXCLUSIVE" };
   VAR nCommitLevel INIT 1  // 1 or 2
   VAR cRDD                 // "SDENTX","SDEFOX","SDENSX","SDENSX_DBT"
   VAR aFieldName INIT {}

   METHOD New( cDBFFile, nOpenMode )
   METHOD Open( nOpenMode )
   METHOD CLOSE()
   METHOD RecCount()
   METHOD LastRec()
   METHOD RecNo()
   METHOD COMMIT()
   METHOD dbGoTop()
   METHOD dbGoto( nRecNo )
   METHOD dbGoBottom()
   METHOD dbSkip( nSkip )
   METHOD FCount()
   METHOD FieldName( iFieldNum )
   METHOD FieldGet( cFieldName )
   METHOD BOF()
   METHOD EOF()
   METHOD REPLACE( cpFieldName, xData )
   METHOD dbSeek( cSeek )
   METHOD Found()
   METHOD DBLocate( cpExpression, iDirection, bContinue )

ENDCLASS

METHOD New( cDBFFile, nOpenMode ) CLASS TApollo

   ::cDBFFile  := cDBFFile
   ::nOpenMode := nOpenMode

   RETURN Self

METHOD Open( nOpenMode ) CLASS TApollo

   LOCAL i, j, cField
   LOCAL hClass := self:ClassH

   IF ValType( nOpenMode ) == "N"
      ::nOpenMode := nOpenMode
   ENDIF

   ::nWorkArea := sx_Use( ;
      ::cDBFFile, ;
      ::cAlias, ;
      ::nOpenMode, ;
      ::cRDD, ;
      ::nCommitLevel )

   j := sx_FieldCount( ::cAlias )

   FOR i := 1 TO j
      cField := sx_FieldName( i, ::cAlias )
      AAdd( ::aFieldName, cField )
      __clsAddMsg( hClass,       cField, __blockGet( cField ), HB_OO_MSG_INLINE )
      __clsAddMsg( hClass, "_" + cField, __blockPut( cField ), HB_OO_MSG_INLINE )
   NEXT

   RETURN Self

METHOD RecCount() CLASS TApollo

   RETURN sx_RecCount( ::cAlias )

METHOD LastRec() CLASS TApollo

   RETURN sx_RecCount( ::cAlias )

METHOD CLOSE() CLASS TApollo

   RETURN sx_Close( ::cAlias )

METHOD RecNo() CLASS TApollo

   RETURN sx_RecNo( ::cAlias )

METHOD dbGoto( nRecNo ) CLASS TApollo

   RETURN sx_Go( nRecNo, ::cAlias )

METHOD dbGoTop() CLASS TApollo

   RETURN sx_GoTop( ::cAlias )

METHOD dbGoBottom() CLASS TApollo

   RETURN sx_GoBottom( ::cAlias )

METHOD FCount() CLASS TApollo

   RETURN sx_FieldCount( ::cAlias )

METHOD FieldName( iFieldNum ) CLASS TApollo

   RETURN sx_FieldName( iFieldNum, ::cAlias )

METHOD FieldGet( cFieldName ) CLASS TApollo

   RETURN sx_FieldGet( cFieldName, ::cAlias )

METHOD BOF() CLASS TApollo

   RETURN sx_Bof( ::cAlias )

METHOD EOF() CLASS TApollo

   RETURN sx_Eof( ::cAlias )

METHOD COMMIT() CLASS TApollo

   RETURN sx_Commit( ::cAlias )

METHOD dbSkip( nSkip ) CLASS TApollo

   IF nSkip == NIL
      nSkip := 1
   ENDIF

   RETURN sx_Skip( nSkip, ::cAlias )

METHOD REPLACE( cpFieldName, xData ) CLASS TAPOLLO

   RETURN sx_Replace( cpFieldName, xData, ::cAlias )

METHOD dbSeek( cSeek ) CLASS TAPOLLO

   RETURN sx_Seek( cSeek, ::cAlias )

METHOD Found() CLASS TAPOLLO

   RETURN sx_Found( ::cAlias )

METHOD DBLocate( cpExpression, iDirection, bContinue ) CLASS TAPOLLO

   RETURN sx_Locate( cpExpression, iDirection, bContinue, ::cAlias )

STATIC FUNCTION __blockGet( cField )

   RETURN {| self | sx_FieldGet( cField, ::cAlias ) }

STATIC FUNCTION __blockPut( cField )

   RETURN {| self, xval | iif( xval == nil, , sx_Replace( cField, xval, ::cAlias ) ) }
