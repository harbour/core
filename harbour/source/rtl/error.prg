/*
 * $Id$

   Copyright(C) 1999 by Antonio Linares.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to:

   The Free Software Foundation, Inc.,
   675 Mass Ave, Cambridge, MA 02139, USA.

   You can contact me at: alinares@fivetech.com
 */

// Class error. We are keeping Clipper compatibility here, instead of using
// TError():New() style and also avoiding hungarian notation.

//----------------------------------------------------------------------------//

function ErrorNew()

   static oClass

   if oClass == nil
      oClass = TClass():New( "ERROR" )

      oClass:AddData( "args"         , 0 )
      oClass:AddData( "CanDefault"   , .F. )
      oClass:AddData( "CanRetry"     , .F. )
      oClass:AddData( "CanSubstitute", .F. )
      oClass:AddData( "Cargo" )
      oClass:AddData( "description"  , '' )
      oClass:AddData( "filename"     , '' )
      oClass:AddData( "GenCode"      , 0 )
      oClass:AddData( "Operation"    , '' )
      oClass:AddData( "OsCode"       , 0 )
      oClass:AddData( "Severity"     , 0 )
      oClass:AddData( "SubCode"      , 0 )
      oClass:AddData( "SubSystem"    , '' )
      oClass:AddData( "Tries"        , 0 )

      oClass:Create()
   endif

return oClass:Instance()

//----------------------------------------------------------------------------//
