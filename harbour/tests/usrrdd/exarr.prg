/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    ARRAY RDD example
 *
 * Copyright 2009 Francesco Saverio Giudice <info / at / fsgiudice / dot / com>
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

REQUEST ARRAYRDD

#define USE_DBCREATE_EXTENSIONS

PROCEDURE Main()
   LOCAL aStruct

   SET DATE ANSI
   SET CENTURY ON
   SET DELETED OFF
   CLS

   ? "Create a new dbf in memory using dbCreate() command"
   aStruct := { { "NAME"     , "C", 40, 0 } ,;
                { "ADDRESS"  , "C", 40, 0 } ,;
                { "BIRTHDAY" , "D",  8, 0 } ,;
                { "AGE"      , "N",  3, 0 } }

#ifndef USE_DBCREATE_EXTENSIONS
   ? "Create it"
   dbCreate( "arrtest.dbf", aStruct, "ARRAYRDD" )
   wait
   ? "Open it"
   USE arrtest.dbf VIA "ARRAYRDD"
   wait
#else
   ? "Create it and leave opened"
   dbCreate( "arrtest.dbf", aStruct, "ARRAYRDD", .T., "arrtest" )
   wait
#endif

   ? "Show structure"
   ? hb_ValToExp( dbStruct() )
   WAIT

   ? "ALIAS", ALIAS(), "RECNO", RECNO(), ;
     "BOF", BOF(), "EOF", EOF(), "LASTREC", LASTREC()
   ? RECNO(), '"' + FIELD->NAME + '"'
   DBGOBOTTOM()
   ? RECNO(), '"' + FIELD->NAME + '"'
   DBGOTOP()
   ? RECNO(), '"' + FIELD->NAME + '"'
   WAIT

   ? "Adding some data"
   dbAppend()
   field->name     := "Giudice Francesco Saverio"
   field->address  := "Main Street 10"
   field->birthday := SToD( "19670103" )
   field->age      := 39

   ? RECNO(), '"' + FIELD->NAME + '"'

   dbAppend()
   field->name     := "Mouse Mickey"
   field->address  := "Main Street 20"
   field->birthday := SToD( "19400101" )
   field->age      := 66

   WHILE !EOF()
      ? RECNO(), '"' + FIELD->NAME + '"'
      IF RECNO() == 20
         INKEY( 0 )
      ENDIF
      DBSKIP()
   ENDDO
   ? "ALIAS", ALIAS(), "RECNO", RECNO(), ;
     "BOF", BOF(), "EOF", EOF(), "LASTREC", LASTREC()
   WAIT
   DBGOBOTTOM()
   ? "ALIAS", ALIAS(), "RECNO", RECNO(), ;
     "BOF", BOF(), "EOF", EOF(), "LASTREC", LASTREC()
   WAIT
   WHILE !BOF()
      ? RECNO(), '[' + FIELD->NAME + ']'
      IF RECNO() == LASTREC() - 20
         INKEY( 0 )
      ENDIF
      DBSKIP( -1 )
   ENDDO
   ? "ALIAS", ALIAS(), "RECNO", RECNO(), ;
     "BOF", BOF(), "EOF", EOF(), "LASTREC", LASTREC()
   WAIT

   ? "Show it - Please don't press any key except movement keys and ESC"
   ? "          to exit from browse(), otherwise you will get an error"
   ? "          due to missing index support"
   WAIT
   BROWSE()

RETURN
