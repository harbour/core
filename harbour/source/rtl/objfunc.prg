/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Dynamic Object management and misc. Object related functions
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 *    __objGetMsgList
 *
 * Copyright 1999 Chen Kedem <niki@actcom.co.il>
 *    Documentation for all functions
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "common.ch"
#include "error.ch"
#include "hboo.ch"

/*  $DOC$
 *  $FUNCNAME$
 *      __objHasData()
 *  $CATEGORY$
 *      Object manipulation
 *  $ONELINER$
 *      Determine whether a symbol exist in object as DATA
 *  $SYNTAX$
 *      __objHasData( <oObject>, <cSymbol> ) --> lExist
 *  $ARGUMENTS$
 *      <oObject> is an object to scan.
 *
 *      <cSymbol> is the name of the symbol to look for.
 *  $RETURNS$
 *      __objHasData() return .T. if the given <cSymbol> exist as DATA
 *      (instance variable) in object <oObject), .F. if it does not exist.
 *  $DESCRIPTION$
 *      __objHasData() is a low level class support function that let you
 *      find out if a symbol is an instance variable in a given object.
 *  $EXAMPLES$
 *      oB := TBrowseNew( 0, 0, 24, 79 )
 *      ? __objHasData( oB, "nLeft" )      // this should return .T.
 *      ? __objHasData( oB, "lBugFree" )   // hopefully this should be .F.
 *      ? __objHasData( oB, "Left" )       // .F. since this is a METHOD
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      __objHasData() is an Harbour extension.
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      __ObjGetMethodLis  __objGetMsgList()  __objHasMethod()
 *  $END$
 */

function __objHasData( oObject, cSymbol )

   if !ISOBJECT( oObject ) .or. ;
      !ISCHARACTER( cSymbol )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

return __objHasMsg( oObject, cSymbol ) .and. ;
       __objHasMsg( oObject, "_" + cSymbol )

/*  $DOC$
 *  $FUNCNAME$
 *      __objHasMethod()
 *  $CATEGORY$
 *      Object manipulation
 *  $ONELINER$
 *      Determine whether a symbol exist in object as METHOD
 *  $SYNTAX$
 *      __objHasMethod( <oObject>, <cSymbol> ) --> lExist
 *  $ARGUMENTS$
 *      <oObject> is an object to scan.
 *
 *      <cSymbol> is the name of the symbol to look for.
 *  $RETURNS$
 *      __objHasMethod() return .T. if the given <cSymbol> exist as METHOD
 *      (class function) in object <oObject), .F. if it does not exist.
 *  $DESCRIPTION$
 *      __objHasMethod() is a low level class support function that let you
 *      find out if a symbol is a class function in a given object.
 *  $EXAMPLES$
 *      oB := TBrowseNew( 0, 0, 24, 79 )
 *      ? __objHasMethod( oB, "nLeft" )      // .F. since this is a DATA
 *      ? __objHasMethod( oB, "FixBugs" )    // hopefully this should be .F.
 *      ? __objHasMethod( oB, "Left" )       // this should return .T.
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      __objHasMethod() is an Harbour extension.
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      __ObjGetMethodLis  __objGetMsgList()  __objHasData()
 *  $END$
 */

function __objHasMethod( oObject, cSymbol )

   if !ISOBJECT( oObject ) .or. ;
      !ISCHARACTER( cSymbol )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

return __objHasMsg( oObject, cSymbol ) .and. ;
       !__objHasMsg( oObject, "_" + cSymbol )

/*  $DOC$
 *  $FUNCNAME$
 *      __objGetMsgList()
 *  $CATEGORY$
 *      Object manipulation
 *  $ONELINER$
 *      Return names of all DATA or METHOD for a given object
 *  $SYNTAX$
 *      __objGetMsgList( <oObject>, [<lData>] ) --> aNames
 *  $ARGUMENTS$
 *      <oObject> is an object to scan.
 *
 *      <lData> is an optional logical value that specifies the information
 *      to return. A value of .T. instruct the function to return list of
 *      all DATA names, .F. return list of all METHOD names. Default value
 *      is .T.
 *  $RETURNS$
 *      __objGetMsgList() return an array of character stings with all DATA
 *      names or all METHOD names for a given object. __objGetMsgList()
 *      would return an empty array {} if the given object does not contain
 *      the requested information.
 *  $DESCRIPTION$
 *      __objGetMsgList() is a low level class support function that let you
 *      find all instance variable or class functions names for a given
 *      object.
 *  $EXAMPLES$
 *      // show information about TBrowse class
 *      oB := TBrowseNew( 0, 0, 24, 79 )
 *      aData   := __objGetMsgList( oB, .T. )
 *      aMethod := __objGetMsgList( oB, .F. )
 *      FOR i = 1 to len ( aData )
 *          ? "DATA name:", aData[ i ]
 *      NEXT
 *      FOR i = 1 to len ( aMethod )
 *          ? "METHOD name:", aMethod[ i ]
 *      NEXT
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      __objGetMsgList() is an Harbour extension.
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      __ObjGetMethodLis  __ObjGetValueList  __objHasData()  __objHasMethod()
 *  $END$
 */

function __objGetMsgList( oObject, lDataMethod )

   local aInfo
   local aData
   local n
   local nLen
   local lFoundDM                               // Found DATA ?

   if !ISOBJECT( oObject )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

   IF !ISLOGICAL( lDataMethod )
        lDataMethod := .T.
   ENDIF

   aInfo  := aSort( oObject:ClassSel() )
   aData  := {}
   n      := 1
   nLen   := Len( aInfo )

   do while n <= nLen .and. Substr( aInfo[ n ], 1, 1 ) != "_"

      /* If in range and no set function found yet ( set functions */
      /* begin with a leading underscore ).                        */

      lFoundDM := !Empty( aScan( aInfo, "_" + aInfo[ n ], n + 1 ) )

      /* Find position of matching set function in array with all symbols */

      if lFoundDM == lDataMethod                // If found -> DATA
                                                //     else    METHOD
         aAdd( aData, aInfo[ n ] )
      endif
      n++
   enddo

return aData

/*  $DOC$
 *  $FUNCNAME$
 *      __objGetMethodList()
 *  $CATEGORY$
 *      Object manipulation
 *  $ONELINER$
 *      Return names of all METHOD for a given object
 *  $SYNTAX$
 *      __objGetMethodList( <oObject> ) --> aMethodNames
 *  $ARGUMENTS$
 *      <oObject> is an object to scan.
 *  $RETURNS$
 *      __objGetMethodList() return an array of character stings with all
 *      METHOD names for a given object. __objGetMethodList() would return
 *      an empty array {} if the given object does not contain any METHOD.
 *  $DESCRIPTION$
 *      __objGetMethodList() is a low level class support function that let
 *      you find all class functions names for a given object.
 *      It is equivalent to __objGetMsgList( oObject, .F. ).
 *  $EXAMPLES$
 *      // show information about TBrowse class
 *      oB := TBrowseNew( 0, 0, 24, 79 )
 *      aMethod := __objGetMethodList( oB )
 *      FOR i = 1 to len ( aMethod )
 *          ? "METHOD name:", aMethod[ i ]
 *      NEXT
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      __objGetMethodList() is an Harbour extension.
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      __objGetMsgList()  __ObjGetValueList  __objHasData()    __objHasMethod()
 *  $END$
 */

function __objGetMethodList( oObject )

   if !ISOBJECT( oObject )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

return __objGetMsgList( oObject, .F. )

/*  $DOC$
 *  $FUNCNAME$
 *      __objGetValueList()
 *  $CATEGORY$
 *      Object manipulation
 *  $ONELINER$
 *      Return an array of DATA names and values for a given object
 *  $SYNTAX$
 *      __objGetValueList( <oObject>, [<aExcept>] ) --> aData
 *  $ARGUMENTS$
 *      <oObject> is an object to scan.
 *
 *      <aExcept> is an optional array with DATA names you want to exclude
 *      from the scan.
 *  $RETURNS$
 *      __objGetValueList() return a 2D array that contain pairs of a DATA
 *      symbol name and the value of DATA. __objGetValueList() would return
 *      an empty array {} if the given object does not contain the requested
 *      information.
 *  $DESCRIPTION$
 *      __objGetValueList() is a low level class support function that
 *      return an array with DATA names and value, each array element is a
 *      pair of: aData[ i, HB_OO_DATA_SYMBOL ] contain the symbol name
 *               aData[ i, HB_OO_DATA_VALUE  ] contain the value of DATA
 *  $EXAMPLES$
 *      // show information about TBrowse class
 *      oB := TBrowseNew( 0, 0, 24, 79 )
 *      aData := __objGetValueList( oB )
 *      FOR i = 1 to len ( aData )
 *          ? "DATA name:", aData[ i, HB_OO_DATA_SYMBOL ], ;
 *            "    value=", aData[ i, HB_OO_DATA_VALUE  ]
 *      NEXT
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      __objGetValueList() is an Harbour extension.
 *  $PLATFORMS$
 *  $FILES$
 *      Header file is hboo.ch
 *  $SEEALSO$
 *      __ObjGetMethodLis  __objGetMsgList()  __objHasData()    __objHasMethod()  __ObjSetValueList
 *  $END$
 */

function __objGetValueList( oObject, aExcept )

   local aDataSymbol
   local nLen
   local aData
   local cSymbol
   local n

   if !ISOBJECT( oObject )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

   IF !ISARRAY( aExcept )
        aExcept := {}
   ENDIF

   aDataSymbol := __objGetMsgList( oObject )
   nLen        := Len( aDataSymbol )
   aData       := {}

   for n := 1 to nLen
      cSymbol := aDataSymbol[ n ]
      if Empty( aScan( aExcept, cSymbol ) )
         aAdd( aData, { cSymbol, __objSendMsg( oObject, cSymbol ) } )
      endif
   next n

return aData

/*  $DOC$
 *  $FUNCNAME$
 *      __ObjSetValueList()
 *  $CATEGORY$
 *      Object manipulation
 *  $ONELINER$
 *      Set object with an array of DATA names and values
 *  $SYNTAX$
 *      __ObjSetValueList( <oObject>, <aData> ) --> oObject
 *  $ARGUMENTS$
 *      <oObject> is an object to set.
 *
 *      <aData> is a 2D array with a pair of instance variables and values
 *      for setting those variable.
 *  $RETURNS$
 *      __ObjSetValueList() return a reference to <oObject>.
 *  $DESCRIPTION$
 *      __ObjSetValueList() is a low level class support function that let
 *      you set a group of instance variables with values. each array
 *      element in <aData> is a pair of:
 *      aData[ i, HB_OO_DATA_SYMBOL ] which contain the variable name to set
 *      aData[ i, HB_OO_DATA_VALUE  ] contain the new variable value.
 *  $EXAMPLES$
 *      // set some TBrowse instance variable
 *      oB := TBrowse():New()
 *      aData := array( 4, 2 )
 *      aData[ 1, HB_OO_DATA_SYMBOL ] = "nTop"
 *      aData[ 1, HB_OO_DATA_VALUE  ] = 1
 *      aData[ 2, HB_OO_DATA_SYMBOL ] = "nLeft"
 *      aData[ 2, HB_OO_DATA_VALUE  ] = 10
 *      aData[ 3, HB_OO_DATA_SYMBOL ] = "nBottom"
 *      aData[ 3, HB_OO_DATA_VALUE  ] = 20
 *      aData[ 4, HB_OO_DATA_SYMBOL ] = "nRight"
 *      aData[ 4, HB_OO_DATA_VALUE  ] = 70
 *      __ObjSetValueList( oB, aData )
 *      ? oB:nTop      // 1
 *      ? oB:nLeft     // 10
 *      ? oB:nBottom   // 20
 *      ? oB:nRight    // 70
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      __ObjSetValueList() is an Harbour extension.
 *  $PLATFORMS$
 *  $FILES$
 *      Header file is hboo.ch
 *  $SEEALSO$
 *      __ObjGetValueList
 *  $END$
 */

function __ObjSetValueList( oObject, aData )

   if !ISOBJECT( oObject )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

   aEval( aData,;
        {| aItem | __objSendMsg( oObject, "_" + aItem[ HB_OO_DATA_SYMBOL ], aItem[ HB_OO_DATA_VALUE ] ) } )

return oObject

/*  $DOC$
 *  $FUNCNAME$
 *      __objAddMethod()
 *  $CATEGORY$
 *      Object manipulation
 *  $ONELINER$
 *      Add a METHOD to an already existing class
 *  $SYNTAX$
 *      __objAddMethod( <oObject>, <cMethodName>, <nFuncPtr> ) --> oObject
 *  $ARGUMENTS$
 *      <oObject> is the object to work on.
 *
 *      <cMethodName> is the symbol name of the new METHOD to add.
 *
 *      <nFuncPtr> is a pointer to a function to associate with the method.
 *  $RETURNS$
 *      __objAddMethod() return a reference to <oObject>.
 *  $DESCRIPTION$
 *      __objAddMethod() is a low level class support function that add a
 *      new METHOD to an object. If a symbol with the name <cMethodName>
 *      already exist in <oObject> a run time error will occur.
 *
 *      Note that <nFuncPtr> is a special pointer to a function that was
 *      created using the @ operator, see example below.
 *  $EXAMPLES$
 *      // create a new THappy class and add a Smile method
 *      oHappy := TClass():New( "THappy" )
 *      __objAddMethod( oHappy, "Smile", @MySmile() )
 *      ? oHappy:Smile( 1 )       // :)
 *      ? oHappy:Smile( 2 )       // ;)
 *      ? oHappy:Smile( 3 )       // *SMILE*
 *
 *      STATIC FUNCTION MySmile( nType )
 *      LOCAL cSmile
 *      DO CASE
 *         CASE nType == 1
 *              cSmile := ":)"
 *         CASE nType == 2
 *              cSmile := ";)"
 *         CASE nType == 3
 *              cSmile := "*SMILE*"
 *      ENDCASE
 *      RETURN cSmile
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      __objAddMethod() is an Harbour extension.
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      __objAddInline()  __objAddData()  __objDelMethod()   __ObjGetMethodLis  __objGetMsgList()  __objHasMethod()   __objModMethod()
 *  $END$
 */

function __objAddMethod( oObject, cSymbol, nFuncPtr )

   if !ISOBJECT( oObject ) .or. ;
      !ISCHARACTER( cSymbol ) .or. ;
      !ISNUMBER( nFuncPtr )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

   if !__objHasMsg( oObject, cSymbol )
      __clsAddMsg( oObject:ClassH, cSymbol, nFuncPtr, HB_OO_MSG_METHOD )
   else
      __errRT_BASE(EG_ARG, 3103, "Already existing symbol in class", ProcName( 0 ) )
   endif

return oObject

/*  $DOC$
 *  $FUNCNAME$
 *      __objAddInline()
 *  $CATEGORY$
 *      Object manipulation
 *  $ONELINER$
 *      Add an INLINE to an already existing class
 *  $SYNTAX$
 *      __objAddInline( <oObject>, <cInlineName>, <bInline> ) --> oObject
 *  $ARGUMENTS$
 *      <oObject> is the object to work on.
 *
 *      <cInlineName> is the symbol name of the new INLINE to add.
 *
 *      <bInline> is a code block to associate with the INLINE method.
 *  $RETURNS$
 *      __objAddInline() return a reference to <oObject>.
 *  $DESCRIPTION$
 *      __objAddInline() is a low level class support function that add a
 *      new INLINE method to an object. If a symbol with the name
 *      <cInlineName> already exist in <oObject> a run time error will
 *      occur.
 *  $EXAMPLES$
 *      // create a new THappy class and add a Smile INLINE method
 *      oHappy  := TClass():New( "THappy" )
 *      bInline := { | nType | { ":)", ";)", "*SMILE*" }[ nType ] }
 *      __objAddInline( oHappy, "Smile", bInline )
 *      ? oHappy:Smile( 1 )       // :)
 *      ? oHappy:Smile( 2 )       // ;)
 *      ? oHappy:Smile( 3 )       // *SMILE*
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      __objAddInline() is an Harbour extension.
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      __objAddData()  __objAddMethod()  __objDelInline()  __ObjGetMethodLis  __objGetMsgList()  __objHasMethod()   __objModInline()
 *  $END$
 */

function __objAddInline( oObject, cSymbol, bInline )

   if !ISOBJECT( oObject ) .or. ;
      !ISCHARACTER( cSymbol )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

   if !__objHasMsg( oObject, cSymbol )
      __clsAddMsg( oObject:ClassH, cSymbol, bInline, HB_OO_MSG_INLINE )
   else
      __errRT_BASE(EG_ARG, 3103, "Already existing symbol in class", ProcName( 0 ) )
   endif

return oObject

/*  $DOC$
 *  $FUNCNAME$
 *      __objAddData()
 *  $CATEGORY$
 *      Object manipulation
 *  $ONELINER$
 *      Add a DATA to an already existing class
 *  $SYNTAX$
 *      __objAddData( <oObject>, <cDataName> ) --> oObject
 *  $ARGUMENTS$
 *      <oObject> is the object to work on.
 *
 *      <cDataName> is the symbol name of the new DATA to add.
 *  $RETURNS$
 *      __objAddData() return a reference to <oObject>.
 *  $DESCRIPTION$
 *      __objAddData() is a low level class support function that add a new
 *      DATA to an object. If a symbol with the name <cDataName> already
 *      exist in <oObject> a run time error will occur.
 *  $EXAMPLES$
 *      // create a new THappy class and add a lHappy DATA
 *      oHappy  := TClass():New( "THappy" )
 *      __objAddData( oHappy, "lHappy" )
 *      oHappy:lHappy := .T.
 *      IF oHappy:lHappy
 *         ? "Happy, Happy, Joy, Joy !!!"
 *      ELSE
 *         ? ":(..."
 *      ENDIF
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      __objAddData() is an Harbour extension.
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      __objAddInline()  __objAddMethod()  __objDelData()  __objGetMsgList()  __ObjGetValueList  __objHasData()     __ObjSetValueList
 *  $END$
 */

function __objAddData( oObject, cSymbol )

   local nSeq

   if !ISOBJECT( oObject ) .or. ;
      !ISCHARACTER( cSymbol )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

   if !__objHasMsg( oObject, cSymbol ) .and. ;
      !__objHasMsg( oObject, "_" + cSymbol )

      nSeq := __cls_IncData( oObject:ClassH )         // Allocate new Seq#
      __clsAddMsg( oObject:ClassH, cSymbol,       nSeq, HB_OO_MSG_DATA )
      __clsAddMsg( oObject:ClassH, "_" + cSymbol, nSeq, HB_OO_MSG_DATA )
   else
      __errRT_BASE(EG_ARG, 3103, "Already existing symbol in class", ProcName( 0 ) )
   endif

return oObject

/*  $DOC$
 *  $FUNCNAME$
 *      __objModMethod()
 *  $CATEGORY$
 *      Object manipulation
 *  $ONELINER$
 *      Modify (replace) a METHOD in an already existing class
 *  $SYNTAX$
 *      __objModMethod( <oObject>, <cMethodName>, <nFuncPtr> ) --> oObject
 *  $ARGUMENTS$
 *      <oObject> is the object to work on.
 *
 *      <cMethodName> is the symbol name of the METHOD to modify.
 *
 *      <nFuncPtr> is a pointer to a new function to associate with the
 *      method.
 *  $RETURNS$
 *      __objModMethod() return a reference to <oObject>.
 *  $DESCRIPTION$
 *      __objModMethod() is a low level class support function that modify
 *      a METHOD in an object and replace it with a new function. If a
 *      symbol with the name <cMethodName> does not exist in <oObject> a run
 *      time error will occur. __objModMethod() is used in inheritance
 *      mechanism.
 *
 *      Note that <nFuncPtr> is a special pointer to a function that was
 *      created using the @ operator, see example below.
 *  $EXAMPLES$
 *      // create a new THappy class and add a Smile method
 *      oHappy := TClass():New( "THappy" )
 *      __objAddMethod( oHappy, "Smile", @MySmile() )
 *      ? oHappy:Smile( 1 )       // :)
 *      ? oHappy:Smile( 2 )       // ;)
 *      // replace Smile method with a new function
 *      __objAddMethod( oHappy, "Smile", @YourSmile() )
 *      ? oHappy:Smile( 1 )       // *SMILE*
 *      ? oHappy:Smile( 2 )       // *WINK*
 *
 *      STATIC FUNCTION MySmile( nType )
 *      LOCAL cSmile
 *      DO CASE
 *         CASE nType == 1
 *              cSmile := ":)"
 *         CASE nType == 2
 *              cSmile := ";)"
 *      ENDCASE
 *      RETURN cSmile
 *
 *      STATIC FUNCTION YourSmile( nType )
 *      LOCAL cSmile
 *      DO CASE
 *         CASE nType == 1
 *              cSmile := "*SMILE*"
 *         CASE nType == 2
 *              cSmile := "*WINK*"
 *      ENDCASE
 *      RETURN cSmile
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      __objModMethod() is an Harbour extension.
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      __objAddMethod()  __objDelMethod()  __ObjGetMethodLis  __objGetMsgList()  __objHasMethod()
 *  $END$
 */

function __objModMethod( oObject, cSymbol, nFuncPtr )

   if !ISOBJECT( oObject ) .or. ;
      !ISCHARACTER( cSymbol ) .or. ;
      !ISNUMBER( nFuncPtr )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

   if __objHasMethod( oObject, cSymbol )
      __clsModMsg( oObject:ClassH, cSymbol, nFuncPtr )
   else
      __errRT_BASE(EG_ARG, 3102, "Not existing symbol in class", ProcName( 0 ) )
   endif

return oObject

/*  $DOC$
 *  $FUNCNAME$
 *      __objModInline()
 *  $CATEGORY$
 *      Object manipulation
 *  $ONELINER$
 *      Modify (replace) an INLINE method in an already
 *      existing class
 *  $SYNTAX$
 *      __objModInline( <oObject>, <cInlineName>, <bInline> ) --> oObject
 *  $ARGUMENTS$
 *      <oObject> is the object to work on.
 *
 *      <cInlineName> is the symbol name of the INLINE method to modify.
 *
 *      <bInline> is a new code block to associate with the INLINE method.
 *  $RETURNS$
 *      __objModInline() return a reference to <oObject>.
 *  $DESCRIPTION$
 *      __objModInline() is a low level class support function that modify
 *      an INLINE method in an object and replace it with a new code block.
 *      If a symbol with the name <cInlineName> does not exist in <oObject>
 *      a run time error will occur. __objModInline() is used in inheritance
 *      mechanism.
 *  $EXAMPLES$
 *      // create a new THappy class and add a Smile INLINE method
 *      oHappy  := TClass():New( "THappy" )
 *      bMyInline   := { | nType | { ":)", ";)" }[ nType ] }
 *      bYourInline := { | nType | { "*SMILE*", "*WINK*" }[ nType ] }
 *      __objAddInline( oHappy, "Smile", bMyInline )
 *      ? oHappy:Smile( 1 )       // :)
 *      ? oHappy:Smile( 2 )       // ;)
 *      // replace Smile inline method with a new code block
 *      __objModInline( oHappy, "Smile", bYourInline )
 *      ? oHappy:Smile( 1 )       // *SMILE*
 *      ? oHappy:Smile( 2 )       // *WINK*
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      __objModInline() is an Harbour extension.
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      __objAddInline()  __objDelInline()  __ObjGetMethodLis  __objGetMsgList()  __objHasMethod()
 *  $END$
 */

function __objModInline( oObject, cSymbol, bInline )

   if !ISOBJECT( oObject ) .or. ;
      !ISCHARACTER( cSymbol ) .or. ;
      !ISBLOCK( bInline )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

   if __objHasMethod( oObject, cSymbol )
      __clsModMsg( oObject:ClassH, cSymbol, bInline )
   else
      __errRT_BASE(EG_ARG, 3102, "Not existing symbol in class", ProcName( 0 ) )
   endif

return oObject

/*  $DOC$
 *  $FUNCNAME$
 *      __objDelMethod() | __objDelInline()
 *  $CATEGORY$
 *      Object manipulation
 *  $ONELINER$
 *      Delete a METHOD or INLINE method from class
 *  $SYNTAX$
 *      __objDelMethod( <oObject>, <cSymbol> ) --> oObject
 *
 *      or
 *
 *      __objDelInline( <oObject>, <cSymbol> ) --> oObject
 *  $ARGUMENTS$
 *      <oObject> is the object to work on.
 *
 *      <cSymbol> is the symbol name of METHOD or INLINE method to be
 *      deleted (removed) from the object.
 *  $RETURNS$
 *      __objDelMethod() return a reference to <oObject>.
 *  $DESCRIPTION$
 *      __objDelMethod() is a low level class support function that delete
 *      (remove) a METHOD or an INLINE method from an object. If a symbol
 *      with the name <cSymbol> does not exist in <oObject> a run time error
 *      will occur.
 *
 *      __objDelInline() is exactly the same as __objDelMethod().
 *  $EXAMPLES$
 *      // create a new THappy class and add a Smile method
 *      oHappy := TClass():New( "THappy" )
 *      __objAddMethod( oHappy, "Smile", @MySmile() )
 *      ? __objHasMethod( oHappy, "Smile" )    // .T.
 *      // remove Smile method
 *      __objDelMethod( oHappy, "Smile" )
 *      ? __objHasMethod( oHappy, "Smile" )    // .F.
 *
 *      STATIC FUNCTION MySmile( nType )
 *      LOCAL cSmile
 *      DO CASE
 *         CASE nType == 1
 *              cSmile := ":)"
 *         CASE nType == 2
 *              cSmile := ";)"
 *      ENDCASE
 *      RETURN cSmile
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      __objDelMethod() is an Harbour extension.
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      __objAddInline()  __objAddMethod()  __ObjGetMethodLis  __objGetMsgList()  __objHasMethod()  __objModInline() __objModMethod()
 *  $END$
 */

function __objDelMethod( oObject, cSymbol )

   if !ISOBJECT( oObject ) .or. ;
      !ISCHARACTER( cSymbol )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

   if __objHasMethod( oObject, cSymbol )
      __clsDelMsg( oObject:ClassH, cSymbol )
   else
      __errRT_BASE(EG_ARG, 3102, "Not existing symbol in class", ProcName( 0 ) )
   endif

return oObject
/*  $DOC$
 *  $FUNCNAME$
 *      __objDelInline()
 *  $CATEGORY$
 *      Object manipulation
 *  $ONELINER$
 *      Delete a METHOD or INLINE method from class
 *  $SYNTAX$
 *      __objDelMethod( <oObject>, <cSymbol> ) --> oObject
 *
 *      or
 *
 *      __objDelInline( <oObject>, <cSymbol> ) --> oObject
 *  $ARGUMENTS$
 *      <oObject> is the object to work on.
 *
 *      <cSymbol> is the symbol name of METHOD or INLINE method to be
 *      deleted (removed) from the object.
 *  $RETURNS$
 *      __objDelMethod() return a reference to <oObject>.
 *  $DESCRIPTION$
 *      __objDelMethod() is a low level class support function that delete
 *      (remove) a METHOD or an INLINE method from an object. If a symbol
 *      with the name <cSymbol> does not exist in <oObject> a run time error
 *      will occur.
 *
 *      __objDelInline() is exactly the same as __objDelMethod().
 *  $EXAMPLES$
 *      // create a new THappy class and add a Smile method
 *      oHappy := TClass():New( "THappy" )
 *      __objAddMethod( oHappy, "Smile", @MySmile() )
 *      ? __objHasMethod( oHappy, "Smile" )    // .T.
 *      // remove Smile method
 *      __objDelMethod( oHappy, "Smile" )
 *      ? __objHasMethod( oHappy, "Smile" )    // .F.
 *
 *      STATIC FUNCTION MySmile( nType )
 *      LOCAL cSmile
 *      DO CASE
 *         CASE nType == 1
 *              cSmile := ":)"
 *         CASE nType == 2
 *              cSmile := ";)"
 *      ENDCASE
 *      RETURN cSmile
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      __objDelMethod() is an Harbour extension.
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      __objAddInline()  __objAddMethod()  __ObjGetMethodLis  __objGetMsgList()  __objHasMethod()  __objModInline() __objModMethod()
 *  $END$
 */

function __objDelInline( oObject, cSymbol )
return __objDelMethod( oObject, cSymbol )              // Same story

/*  $DOC$
 *  $FUNCNAME$
 *      __objDelData()
 *  $CATEGORY$
 *      Object manipulation
 *  $ONELINER$
 *      Delete a DATA (instance variable) from class
 *  $SYNTAX$
 *      __objDelMethod( <oObject>, <cDataName> ) --> oObject
 *  $ARGUMENTS$
 *      <oObject> is the object to work on.
 *
 *      <cDataName> is the symbol name of DATA to be deleted (removed) from
 *      the object.
 *  $RETURNS$
 *      __objDelData() return a reference to <oObject>.
 *  $DESCRIPTION$
 *      __objDelData() is a low level class support function that delete
 *      (remove) a DATA from an object. If a symbol with the name
 *      <cDataName> does not exist in <oObject> a run time error will occur.
 *  $EXAMPLES$
 *      // create a new THappy class and add a lHappy DATA
 *      oHappy  := TClass():New( "THappy" )
 *      __objAddData( oHappy, "lHappy" )
 *      ? __objHasData( oHappy, "lHappy" )    // .T.
 *      // remove lHappy DATA
 *      __objDelData( oHappy, "lHappy" )
 *      ? __objHasData( oHappy, "lHappy" )    // .F.
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      __objDelData() is an Harbour extension.
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      __objAddData()  __objGetMsgList()  __ObjGetValueList 
 *      __objHasData()  __ObjSetValueList
 *  $END$
 */

function __objDelData( oObject, cSymbol )

   if !ISOBJECT( oObject ) .or. ;
      !ISCHARACTER( cSymbol )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

   if __objHasData( oObject, cSymbol )
      __clsDelMsg( oObject:ClassH, cSymbol )
      __clsDelMsg( oObject:ClassH, "_" + cSymbol )
      __cls_DecData( oObject:ClassH )         // Decrease wData
   else
      __errRT_BASE(EG_ARG, 3102, "Not existing symbol in class", ProcName( 0 ) )
   endif

return oObject

