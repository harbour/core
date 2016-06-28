/*
 * dbModifyStructure( <cFile> ) -> lSuccess
 *
 * Copyright 2009 Ron Pinkas <Ron.Pinkas at xHarbour.com>
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "dbinfo.ch"
#include "dbstruct.ch"
#include "error.ch"

#ifndef EG_RENAME
#define EG_RENAME       26
#endif

FUNCTION dbModifyStructure( cFile )

   LOCAL lRet
   LOCAL cDir
   LOCAL cExt
   LOCAL cTable
   LOCAL cBakFile
   LOCAL cStructureFile
   LOCAL cNewFile
   LOCAL oErr
   LOCAL nPresetArea := Select()
   LOCAL nSourceArea
   LOCAL cDateTime

   BEGIN SEQUENCE WITH __BreakBlock()

      // Open exclusively, get name info, and create the structure db.
      USE ( cFile ) ALIAS ModifySource EXCLUSIVE NEW
      nSourceArea := Select()

      cFile := dbInfo( DBI_FULLPATH )
      cExt  := dbInfo( DBI_TABLEEXT )
      cDateTime := DToS( Date() ) + "_" + StrTran( Left( Time(), 5 ), ":", "_" )

      hb_FNameSplit( cFile, @cDir, @cTable )

      /* TOFIX: long filenames, not MS-DOS compatible */
      cBakFile       := cDir + cTable + "_bak_" + cDateTime + cExt
      cStructureFile := cDir + cTable + "_str_" + cDateTime + cExt
      cNewFile       := cDir + cTable + "_new_" + cDateTime + cExt

      COPY STRUCTURE EXTENDED TO ( cStructureFile )

      // Let user modify the structure.
      USE ( cStructureFile ) ALIAS NewStructure EXCLUSIVE NEW

      Browse( 0, 0, Min( 20, MaxRow() - 1 ), Min( 50, MaxCol() - 30 ) )

      hb_dbPack()
      dbCloseArea()

      CREATE ( cNewFile ) FROM ( cStructureFile ) ALIAS NEW_MODIFIED NEW

      // Import data into the new file, and close it
      lRet := dbMerge( nSourceArea )
      dbCloseArea()

      ( nSourceArea )->( dbCloseArea() )

      dbSelectArea( nPresetArea )

      // Rename original as backup, and new file as the new original.
      IF lRet
         IF ! hb_dbRename( cFile, cBakFile )
            BREAK
         ENDIF
         IF ! hb_dbRename( cNewFile, cFile )
            // If we cannot then try to restore backup as original
            IF ! hb_dbRename( cBakFile, cFile )
               // Oops - must advise the user!
               oErr := ErrorNew()
               oErr:severity     := ES_ERROR
               oErr:genCode      := EG_RENAME
               oErr:subSystem    := "DBCMD"
               oErr:canDefault   := .F.
               oErr:canRetry     := .F.
               oErr:canSubtitute := .F.
               oErr:operation    := cFile
               oErr:subCode      := 1101
               oErr:args         := { cNewFile, cBakFile }

               BREAK oErr
            ENDIF
         ENDIF
      ENDIF

   RECOVER USING oErr
      IF oErr:className() == "ERROR"
         IF oErr:genCode == EG_RENAME
            // This kind of error must be reported
            lRet := Throw( oErr )
         ELSE
            lRet := .F.
         ENDIF
      ELSE
         lRet := .F.
      ENDIF
   END SEQUENCE

   IF cBakFile != NIL
      hb_dbDrop( cBakFile )
   ENDIF
   IF cStructureFile != NIL
      hb_dbDrop( cStructureFile )
   ENDIF
   IF cNewFile != NIL
      hb_dbDrop( cNewFile )
   ENDIF

   dbSelectArea( nPresetArea )

   RETURN lRet

FUNCTION dbImport( xSource )
   RETURN dbMerge( xSource )

FUNCTION dbMerge( xSource, lAppend )

   LOCAL nArea, nSource, nRecNo
   LOCAL aFields
   LOCAL cField, xField
   LOCAL nSourcePos, aTranslate := {}, aTranslation

   LOCAL cTargetType

   hb_default( @lAppend, .F. )

   // Safety
   IF LastRec() > 0 .AND. ! lAppend
      RETURN .F.
   ENDIF

   // Validate args
   DO CASE
   CASE HB_ISSTRING( xSource )
      nArea := Select()

      USE ( xSource ) ALIAS MergeSource EXCLUSIVE NEW
      nSource := Select()

      dbSelectArea( nArea )
   CASE HB_ISNUMERIC( xSource )
      nSource := xSource
   OTHERWISE
      RETURN .F.
   ENDCASE

   // Temp working record
   IF LastRec() == 0
      dbAppend()
   ENDIF

   // Create translation plan
   aFields := Array( FCount() )
   AFields( aFields )

   FOR EACH cField IN aFields
      nSourcePos := ( nSource )->( FieldPos( cField ) )

      IF nSourcePos > 0
         BEGIN SEQUENCE WITH __BreakBlock()
            // Save
            xField := FieldGet( cField:__enumIndex() )

            // Test type compatability
            FieldPut( cField:__enumIndex(), ( nSource )->( FieldGet( nSourcePos ) ) )

            // Restore
            FieldPut( cField:__enumIndex(), xField )

            // Ok to process
            AAdd( aTranslate, { cField:__enumIndex(), nSourcePos, {| xSource | xSource } } )
         RECOVER
            cTargetType := ValType( FieldGet( cField:__enumIndex() ) )

            BEGIN SEQUENCE WITH __BreakBlock()
               // Test type compatability
               FieldPut( cField:__enumIndex(), ValToType( ( nSource )->( FieldGet( nSourcePos ) ), cTargetType ) )

               // Restore
               FieldPut( cField:__enumIndex(), xField )

               // Ok to process
               AAdd( aTranslate, { cField:__enumIndex(), nSourcePos, {| xSource | ValToType( xSource, cTargetType ) } } )
            RECOVER
               // TraceLog( oErr:Description, oErr:Operation )
            END SEQUENCE
         END SEQUENCE
      ENDIF
   NEXT

   // Reset
   IF LastRec() == 1 .AND. ! lAppend
      dbDelete()
      hb_dbZap()
   ENDIF

   // Process
   nRecNo := ( nSource )->( RecNo() )
   ( nSource )->( dbGoTop() )

   DO WHILE ! ( nSource )->( Eof() )
      dbAppend()

      FOR EACH aTranslation IN aTranslate
         FieldPut( aTranslation[ 1 ], Eval( aTranslation[ 3 ], ( nSource )->( FieldGet( aTranslation[ 2 ] ) ) ) )
      NEXT

      ( nSource )->( dbSkip() )
   ENDDO

   ( nSource )->( dbGoto( nRecNo ) )

   // Reset
   IF ! Empty( nArea )
      ( nSource )->( dbCloseArea() )
      dbSelectArea( nArea )
   ENDIF

   RETURN .T.
