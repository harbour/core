/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for Class commands
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
 * Copyright 2000 Brian Hays <bhays@abacuslaw.com>
 *    Documentation for the commands
 *
 * See doc/license.txt for licensing terms.
 *
 */

#ifndef HB_CLASS_CH_
#define HB_CLASS_CH_

/*  $DOC$
 *  $COMMANDNAME$
 *      CLASS
 *  $CATEGORY$
 *      Command
 *  $ONELINER$
 *      Define a Class for Object Oriented Programming
 *  $SYNTAX$
 *      CLASS <ClassName> [ <FROM, INHERIT> <SuperClass> ]
 *  $ARGUMENTS$
 *      <ClassName>  Name of the class to define. By tradition, Harbour
 *                   classes start with "T" to avoid collisions with user-
 *                   created classes.
 *      <SuperClass> The Parent class to use for inheritance
 *  $DESCRIPTION$
 *      CLASS creates a class from which you can create objects.
 *      Each Class is defined in a separate .PRG file dedicated to that
 *      purpose. You cannot create more than one class in a .PRG.
 *      After the CLASS command begins the definition, the DATA
 *      elements (also known as instance variables) and METHODS of the
 *      class are named.
 *
 *      Classes can inherit from a single <SuperClass>, but the chain of
 *      inheritance can extend to many levels.
 *
 *      A program uses a Class by calling the Class Constructor, the
 *      New() method, to create an object. That object is usually assigned
 *      to a variable, which is used to access the DATA elements and
 *      methods.
 *  $EXAMPLES$
 *      CLASS TBColumn
 *
 *         DATA Block      // Code block to retrieve data for the column
 *         DATA Cargo      // User-definable variable
 *         DATA ColorBlock // Code block that determines color of data items
 *         DATA ColSep     // Column separator character
 *         DATA DefColor   // Array of numeric indexes into the color table
 *         DATA Footing    // Column footing
 *         DATA FootSep    // Footing separator character
 *         DATA Heading    // Column heading
 *         DATA HeadSep    // Heading separator character
 *         DATA Width      // Column display width
 *         DATA ColPos     // Temporary column position on screen
 *
 *         METHOD New()    // Constructor
 *
 *      ENDCLASS
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      CLASS is a Harbour extension.
 *  $PLATFORMS$
 *      All
 *  $SEEALSO$
 *      TClass(),Object Oriented Programming,DATA,METHOD
 *  $END$
 */
#xcommand CLASS <ClassName> [ <frm: FROM, INHERIT> <SuperClass> ] => ;
   function <ClassName>() ;;
      static oClass ;;
      if oClass == nil ;;
         oClass = TClass():New( <(ClassName)> [,<(SuperClass)>] ) ;;
     #define _CLASS_NAME_ <ClassName> ;;
     #translate CLSMETH <ClassName> <MethodName>() => @<ClassName>_<MethodName>() ;
     [ ; #translate Super : => ::<SuperClass>: ] ;
     [ ; extern <SuperClass> ]

/*  $DOC$
 *  $COMMANDNAME$
 *      DATA
 *  $CATEGORY$
 *      Command
 *  $ONELINER$
 *      Define a DATA instance variable for the objects of a class
 *  $SYNTAX$
 *      DATA <DataName1> [,<DataNameN>] [ AS <type> ] [ INIT <uValue> ]
 *  $ARGUMENTS$
 *      <DataName1>  Name of the DATA
 *      <type>       Optional data type specification from the following:
 *                      Character, Numeric, Date, Logical, Codeblock, Nil
 *      <uValue>     Optional initial value when creating a new object
 *  $DESCRIPTION$
 *      DATA elements can also be thought of as the "properties" of an
 *      object. They can be of any data type, including codeblock.
 *      Once an object has been created, the DATA elements are referenced
 *      with the colon (:) as in  MyObject:Heading := "Last name".
 *      Usually a class also defines methods to manipulate the DATA.
 *
 *      You can use the "AS <type>" clause to enforce that the DATA is
 *      maintained as a certain type. Otherwise it will take on the type of
 *      whatever value is first assigned to it.
 *
 *      Use the "INIT <uValue>" clause to initialize that DATA to <uValue>
 *      whenever a new object is created.
 *  $EXAMPLES$
 *      CLASS TBColumn
 *
 *         DATA Block      // Code block to retrieve data for the column
 *         DATA Cargo      // User-definable variable
 *         DATA ColorBlock // Code block that determines color of data items
 *         DATA ColSep     // Column separator character
 *         DATA DefColor   // Array of numeric indexes into the color table
 *         DATA Footing    // Column footing
 *         DATA FootSep    // Footing separator character
 *         DATA Heading    // Column heading
 *         DATA HeadSep    // Heading separator character
 *         DATA Width      // Column display width
 *         DATA ColPos     // Temporary column position on screen
 *
 *         METHOD New()    // Constructor
 *
 *      ENDCLASS
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      DATA is a Harbour extension.
 *  $PLATFORMS$
 *      All
 *  $SEEALSO$
 *      Object Oriented Programming,CLASS,METHOD,CLASSDATA
 *  $END$
 */
#xcommand DATA <DataName1> [,<DataNameN>] [ AS <type> ] [ INIT <uValue> ] => ;
   [ oClass:SetType( <(type)> ) ; ][ oClass:SetInit( <uValue> ) ; ] ;
     oClass:AddData( <(DataName1)> ) ;
   [; oClass:AddData( <(DataNameN)> ) ] ;
   [; oClass:SetInit(,<uValue>) ] [ ; oClass:SetType(,<(type)>) ]
   // Note the use of commas ',' on the above two rules to avoid their call
   // if there are no AS ... or INIT clauses specified. As we just use
   // those methods first parameter, the second one supplied acts as a dummy one

/*  $DOC$
 *  $COMMANDNAME$
 *      CLASSDATA
 *  $CATEGORY$
 *      Command
 *  $ONELINER$
 *      Define a CLASSDATA variable for a class (NOT for an Object!)
 *  $SYNTAX$
 *      CLASSDATA <DataName1> [,<DataNameN>] [ AS <type> ] [ INIT <uValue> ]
 *  $ARGUMENTS$
 *      <DataName1>  Name of the DATA
 *      <type>       Optional data type specification from the following:
 *                      Character, Numeric, Date, Logical, Codeblock, Nil
 *      <uValue>     Optional initial value at program startup
 *  $DESCRIPTION$
 *      CLASSDATA variables can also be thought of as the "properties" of an
 *      entire class. Each CLASSDATA exists only once, no matter how many
 *      objects are created. A common usage is for a counter that is
 *      incremented whenever an object is created and decremented when one
 *      is destroyed, thus monitoring the number of objects in existance
 *      for this class.
 *
 *      You can use the "AS <type>" clause to enforce that the CLASSDATA is
 *      maintained as a certain type. Otherwise it will take on the type of
 *      whatever value is first assigned to it.
 *
 *      Use the "INIT <uValue>" clause to initialize that DATA to <uValue>
 *      whenever the class is first used.
 *  $EXAMPLES$
 *      CLASS TWindow
 *         DATA   hWnd, nOldProc
 *         CLASSDATA lRegistered AS LOGICAL
 *      ENDCLASS
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      CLASSDATA is a Harbour extension.
 *  $PLATFORMS$
 *      All
 *  $SEEALSO$
 *      Object Oriented Programming,CLASS,METHOD,DATA
 *  $END$
 */
#xcommand CLASSDATA <DataName1> [,<DataNameN>] [ AS <type> ] [ INIT <uValue> ] => ;
   [ oClass:SetType( <(type)> ) ; ][ oClass:SetInit( <uValue> ) ; ] ;
     oClass:AddClassData( <(DataName1)> ) ;
   [; oClass:AddClassData( <(DataNameN)> ) ] ;
   [; oClass:SetInit(,<uValue>) ] [ ; oClass:SetType(,<(type)>) ]
   // Note the use of commas ',' on the above two rules to avoid their call
   // if there are no AS ... or INIT clauses specified. As we just use
   // those methods first parameter, the second one supplied acts as a dummy one

/*  $DOC$
 *  $COMMANDNAME$
 *      METHOD
 *  $CATEGORY$
 *      Command
 *  $ONELINER$
 *      Declare a METHOD for a class in the class header
 *  $SYNTAX$
 *      METHOD <MethodName>( [<params,...>] ) [ CONSTRUCTOR ]
 *      METHOD <MethodName>( [<params,...>] ) INLINE <Code,...>
 *      METHOD <MethodName>( [<params,...>] ) BLOCK  <CodeBlock>
 *      METHOD <MethodName>( [<params,...>] ) EXTERN <FuncName>([<args,...>])
 *      METHOD <MethodName>( [<params,...>] ) SETGET
 *      METHOD <MethodName>( [<params,...>] ) VIRTUAL
 *      METHOD <MethodName>( [<param>] )      OPERATOR <op>
 *      METHOD <MethodName>( [<params,...>] ) CLASS <ClassName>
 *  $ARGUMENTS$
 *      <MethodName>  Name of the method to define
 *      <params,...>  Optional parameter list
 *  $DESCRIPTION$
 *      Methods are "class functions" which do the work of the class.
 *      All methods must be defined in the class header between the
 *      CLASS and ENDCLASS commands.  If the body of a method is not fully
 *      defined here, the full body is written below the ENDCLASS command
 *      using this syntax:
 *
 *         METHOD <MethodName>( [<params,...>] ) CLASS <ClassName>
 *
 *      Methods can reference the current object with the keyword "Self:" or
 *      its shorthand version "::".
 *
 *      CLAUSES:
 *
 *      CONSTRUCTOR  Defines a special method Class Constructor method,
 *                   used to create objects.  This is usually the
 *                   New() method. Constructors always return the new
 *                   object.
 *
 *      INLINE       Fast and easy to code, INLINE lets you define the
 *                   code for the method immediately within the definition
 *                   of the Class. Any methods not declared INLINE or BLOCK
 *                   must be fully defined after the ENDCLASS command.
 *                   The <Code,...> following INLINE receives a parameter
 *                   of Self. If you need to receive more parameters, use
 *                   the BLOCK clause instead.
 *
 *      BLOCK        Use this clause when you want to declare fast 'inline'
 *                   methods that need parameters. The first parameter to
 *                   <CodeBlock> must be Self, as in:
 *
 *          METHOD <MethodName> BLOCK {|Self,<arg1>,<arg2>, ...,<argN>|...}
 *
 *      EXTERN       If an external function does what the method needs,
 *                   use this clause to make an optimized call to that
 *                   function directly.
 *
 *      SETGET       For calculated Data. The name of the method can be
 *                   manipulated like a Data element to Set or Get a value.
 *
 *      VIRTUAL      Methods that do nothing. Useful for Base classes where
 *                   the child class will define the method's behavior, or
 *                   when you are first creating and testing a Class.
 *
 *      OPERATOR     Operator Overloading for classes.
 *                   See example TestOp.prg for details.
 *
 *      CLASS <ClassName>
 *                   Use this syntax only for defining a full method after
 *                   the ENDCLASS command.
 *  $EXAMPLES$
 *      CLASS TWindow
 *         DATA   hWnd, nOldProc
 *         METHOD New( ) CONSTRUCTOR
 *         METHOD Capture() INLINE  SetCapture( ::hWnd )
 *         METHOD End() BLOCK  { | Self, lEnd | If( lEnd := ::lValid(),;
 *                                 ::PostMsg( WM_CLOSE ),), lEnd }
 *         METHOD EraseBkGnd( hDC )
 *         METHOD cTitle( cNewTitle ) SETGET
 *         METHOD Close() VIRTUAL
 *      ENDCLASS
 *
 *      METHOD New( ) CLASS TWindow
 *         local nVar, cStr
 *         ... <code> ...
 *         ... <code> ...
 *      RETURN Self
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      METHOD is a Harbour extension.
 *  $PLATFORMS$
 *      All
 *  $SEEALSO$
 *      TClass(),Object Oriented Programming,DATA,CLASS,TestOp.prg
 *  $END$
 */

#xcommand METHOD <MethodName>( [<params,...>] ) [ CONSTRUCTOR ] => ;
   oClass:AddMethod( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>() )

#xcommand METHOD <MethodName>( [<params,...>] ) BLOCK <CodeBlock> => ;
   oClass:AddInline( <(MethodName)>, <CodeBlock> )

#xcommand METHOD <MethodName>( [<params,...>] ) EXTERN <FuncName>( [<params,...>] ) => ;
   oClass:AddMethod( <(MethodName)>, @<FuncName>() )

#xcommand METHOD <MethodName>( [<params,...>] ) INLINE <Code,...> => ;
   oClass:AddInline( <(MethodName)>, {|Self [,<params>] | <Code> } )

#xcommand METHOD <MethodName>( [<params,...>] ) VIRTUAL => ;
   oClass:AddVirtual( <(MethodName)> )

#xcommand METHOD <MethodName>( [<params,...>] ) SETGET => ;
   oClass:AddMethod( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>() ) ;;
   oClass:AddMethod( "_" + <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>() )

#xcommand METHOD <MethodName>( [<param>] ) OPERATOR <op> => ;
   oClass:AddMethod( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>() ) ;;
   oClass:AddInline( <(op)>, {|Self [,<param>] | ::<MethodName>( [<param>] ) } )

/*  $DOC$
 *  $COMMANDNAME$
 *      MESSAGE
 *  $CATEGORY$
 *      Command
 *  $ONELINER$
 *      Route a method call to another Method
 *  $SYNTAX$
 *      MESSAGE <MessageName>   METHOD <MethodName>( [<params,...>] )
 *      MESSAGE <MessageName>() METHOD <MethodName>( [<params,...>] )
 *  $ARGUMENTS$
 *      <MessageName>  The pseudo-method name to define
 *      <MethodName>   The method to create and call when <MessageName>
 *                     is invoked.
 *      <params,...>   Optional parameter list for the method
 *  $DESCRIPTION$
 *      The MESSAGE command is a seldom-used feature that lets you re-route
 *      a call to a method with a different name. This can be necessary if
 *      a method name conflicts with a public function that needs to be
 *      called from within the class methods.
 *
 *      For example, your app may have a public function called BeginPaint()
 *      that is used in painting windows. It would also be natural to have a
 *      Window class method called :BeginPaint() that the application can
 *      call. But within the class method you would not be able to call the
 *      public function because internally methods are based on static
 *      functions (which hide public functions of the same name).
 *
 *      The MESSAGE command lets you create the true method with a different
 *      name (::xBeginPaint()), yet still allow the ::BeginPaint() syntax
 *      to call ::xBeginPaint().  This is then free to call the public
 *      function BeginPaint().
 *  $EXAMPLES$
 *      CLASS TWindow
 *         DATA   hWnd, nOldProc
 *         METHOD New( ) CONSTRUCTOR
 *         MESSAGE BeginPaint METHOD xBeginPaint()
 *      ENDCLASS
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      MESSAGE is a Harbour extension.
 *  $PLATFORMS$
 *      All
 *  $SEEALSO$
 *      METHOD,DATA,CLASS,Object Oriented Programming
 *  $END$
 */

#xcommand MESSAGE <MessageName> METHOD <MethodName>( [<params,...>] ) => ;
   oClass:AddMethod( <(MessageName)>, CLSMETH _CLASS_NAME_ <MethodName>() )

#xcommand MESSAGE <MessageName>() METHOD <MethodName>( [<params,...>] ) => ;
   oClass:AddMethod( <(MessageName)>, CLSMETH _CLASS_NAME_ <MethodName>() )

/*  $DOC$
 *  $COMMANDNAME$
 *      ERROR HANDLER
 *  $CATEGORY$
 *      Command
 *  $ONELINER$
 *      Designate a method as an error handler for the class
 *  $SYNTAX$
 *      ERROR HANDLER <MethodName>( [<params,...>] )
 *  $ARGUMENTS$
 *      <MethodName>  Name of the method to define
 *      <params,...>  Optional parameter list
 *  $DESCRIPTION$
 *      ERROR HANDLER names the method that should handle errors for the
 *      class being defined.
 *  $EXAMPLES$
 *      CLASS TWindow
 *         ERROR HANDLER  MyErrHandler()
 *      ENDCLASS
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      ERROR HANDLER is a Harbour extension.
 *  $PLATFORMS$
 *      All
 *  $SEEALSO$
 *      Object Oriented Programming,ON ERROR,CLASS,METHOD,DATA
 *  $END$
 */
#xcommand ERROR HANDLER <MethodName>( [<params,...>] ) => ;
   oClass:SetOnError( CLSMETH _CLASS_NAME_ <MethodName>() )

/*  $DOC$
 *  $COMMANDNAME$
 *      ON ERROR
 *  $CATEGORY$
 *      Command
 *  $ONELINER$
 *      Designate a method as an error handler for the class
 *  $SYNTAX$
 *      ON ERROR <MethodName>( [<params,...>] )
 *  $ARGUMENTS$
 *      <MethodName>  Name of the method to define
 *      <params,...>  Optional parameter list
 *  $DESCRIPTION$
 *      ON ERROR is a synonym for ERROR HANDLER.
 *      It names the method that should handle errors for the
 *      class being defined.
 *  $EXAMPLES$
 *      CLASS TWindow
 *         ON ERROR  MyErrHandler()
 *      ENDCLASS
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      ON ERROR is a Harbour extension.
 *  $PLATFORMS$
 *      All
 *  $SEEALSO$
 *      Object Oriented Programming,ERROR HANDLER,CLASS,METHOD,DATA
 *  $END$
 */
#xcommand ON ERROR <MethodName>( [<params,...>] ) => ;
   oClass:SetOnError( CLSMETH _CLASS_NAME_ <MethodName>() )

/*  $DOC$
 *  $COMMANDNAME$
 *      ENDCLASS
 *  $CATEGORY$
 *      Command
 *  $ONELINER$
 *      End the declaration of a class.
 *  $SYNTAX$
 *      ENDCLASS
 *  $DESCRIPTION$
 *      ENDCLASS marks the end of a class declaration.
 *      It is usually followed by the class methods that are not INLINE.
 *  $EXAMPLES$
 *      CLASS TWindow
 *         DATA   hWnd, nOldProc
 *      ENDCLASS
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      ON ERROR is a Harbour extension.
 *  $PLATFORMS$
 *      All
 *  $SEEALSO$
 *      Object Oriented Programming,CLASS,METHOD,DATA
 *  $END$
 */
#xcommand ENDCLASS => oClass:Create() ;;
                      endif ;;
                      return oClass:Instance()

#xcommand METHOD <MethodName>( [<params,...>] ) CLASS <ClassName> => ;
          static function <ClassName>_<MethodName>( [<params>] ) ;;
          local Self := QSelf()

#endif /* HB_CLASS_CH_ */
