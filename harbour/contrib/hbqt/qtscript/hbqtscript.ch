/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009-2012 Pritpal Bedi <bedipritpal@hotmail.com>
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

#ifndef _HBQTSCRIPT_CH
   #define _HBQTSCRIPT_CH

#define QScriptClass_Callable                     0          // Instances of this class can be called as functions.
#define QScriptClass_HasInstance                  1          // Instances of this class implement [[HasInstance]].
                                                             //
#define QScriptClass_HandlesReadAccess            0x01       // The  #define QScriptClass handles read access to this property.
#define QScriptClass_HandlesWriteAccess           0x02       // The  #define QScriptClass handles write access to this property.

#define QScriptContext_ReferenceError             1          // A reference error.
#define QScriptContext_SyntaxError                2          // A syntax error.
#define QScriptContext_TypeError                  3          // A type error.
#define QScriptContext_RangeError                 4          // A range error.
#define QScriptContext_URIError                   5          // A URI error.
#define QScriptContext_UnknownError               0          // An unknown error.
                                                             //
#define QScriptContext_NormalState                0          // The context is in a normal state.
#define QScriptContext_ExceptionState             1          // The context is in an exceptional state.
                                                             //
#define QScriptContextInfo_ScriptFunction         0          // The function is a Qt Script function, i.e. it was defined through a call to  #define QScriptEngine_evaluate().
#define QScriptContextInfo_QtFunction             1          // The function is a Qt function (a signal, slot or method).
#define QScriptContextInfo_QtPropertyFunction     2          // The function is a Qt property getter or setter.
#define QScriptContextInfo_NativeFunction         3          // The function is a built-in Qt Script function, or it was defined through a call to  #define QScriptEngine_newFunction().
                                                             //
#define QScriptEngine_ExcludeChildObjects         0x0001     // The script object will not expose child objects as properties.
#define QScriptEngine_ExcludeSuperClassMethods    0x0002     // The script object will not expose signals and slots inherited from the superclass.
#define QScriptEngine_ExcludeSuperClassProperties 0x0004     // The script object will not expose properties inherited from the superclass.
#define QScriptEngine_ExcludeSuperClassContents   0x0006     // Shorthand form for ExcludeSuperClassMethods | ExcludeSuperClassProperties
#define QScriptEngine_ExcludeDeleteLate           0x0010     // The script object will not expose the QObject_deleteLater() slot.
#define QScriptEngine_ExcludeSlots                0x0020     // The script object will not expose the QObject's slots.
#define QScriptEngine_AutoCreateDynamicProperties 0x0100     // Properties that don't already exist in the QObject will be created as dynamic properties of that object, rather than as properties of the script object.
#define QScriptEngine_PreferExistingWrapperObject 0x0200     // If a wrapper object with the requested configuration already exists, return that object.
#define QScriptEngine_SkipMethodsInEnumeration    0x0008     // Don't include methods (signals and slots) when enumerating the object's properties.

#define QScriptEngine_QtOwnership                 0          // The standard Qt ownership rules apply, i.e. the associated object will never be explicitly deleted by the script engine. This is the default. (QObject ownership is explained in Object Trees & Ownership.)
#define QScriptEngine_ScriptOwnership             1          // The value is owned by the script environment. The associated data will be deleted when appropriate (i.e. after the garbage collector has discovered that there are no more live references to the value).
#define QScriptEngine_AutoOwnership               2          // If the associated object has a parent, the Qt ownership rules apply (QtOwnership); otherwise, the object is owned by the script environment (ScriptOwnership).
                                                             //
#define QScriptSyntaxCheckResult_Error            0          // The program contains a syntax error.
#define QScriptSyntaxCheckResult_Intermediate     1          // The program is incomplete.
#define QScriptSyntaxCheckResult_Valid            2          // The program is a syntactically correct Qt Script program.

#define QScriptValue_ReadOnly                     0x00000001 // The property is read-only. Attempts by Qt Script code to write to the property will be ignored.
#define QScriptValue_Undeletable                  0x00000002 // Attempts by Qt Script code to delete the property will be ignored.
#define QScriptValue_SkipInEnumeration            0x00000004 // The property is not to be enumerated by a for-in enumeration.
#define QScriptValue_PropertyGetter               0x00000008 // The property is defined by a function which will be called to get the property value.
#define QScriptValue_PropertySetter               0x00000010 // The property is defined by a function which will be called to set the property value.

#define QScriptValue_KeepExistingFlags            0x00000800 // This value is used to indicate to setProperty() that the property's flags should be left unchanged. If the property doesn't exist, the default flags (0) will be used.

#define QScriptValue_ResolveLocal                 0x00       // Only check the object's own properties.
#define QScriptValue_ResolvePrototype             0x01       // Check the object's own properties first, then search the prototype chain. This is the default.

#define QScriptValue_UndefinedValue               1          // An undefined value.
#define QScriptValue_NullValue                    0          // A null value.

#endif

