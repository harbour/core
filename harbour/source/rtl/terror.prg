/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Error Class
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

/* Error Class. We are keeping Clipper compatibility here, instead of using
   TError():New() style and also avoiding hungarian notation. */

FUNCTION ErrorNew()

   STATIC s_oClass

   IF s_oClass == NIL
      s_oClass := TClass():New( "ERROR" )

      s_oClass:AddData( "Args"         , 0 )
      s_oClass:AddData( "CanDefault"   , .F. )
      s_oClass:AddData( "CanRetry"     , .F. )
      s_oClass:AddData( "CanSubstitute", .F. )
      s_oClass:AddData( "Cargo" )
      s_oClass:AddData( "Description"  , "" )
      s_oClass:AddData( "FileName"     , "" )
      s_oClass:AddData( "GenCode"      , 0 )
      s_oClass:AddData( "Operation"    , "" )
      s_oClass:AddData( "OsCode"       , 0 )
      s_oClass:AddData( "Severity"     , 0 )
      s_oClass:AddData( "SubCode"      , 0 )
      s_oClass:AddData( "SubSystem"    , "" )
      s_oClass:AddData( "Tries"        , 0 )

      s_oClass:Create()

   ENDIF

   RETURN s_oClass:Instance()

