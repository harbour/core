/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Testing the operators-overloading feature
 *
 * Copyright 2000 Antonio Linares <alinares@fivetech.com>
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

#include "hbclass.ch"

// Right now you can overload operators with Harbour
// as C++ does!!! Not all operators are available. ASAP we will provide all
// of them.


//----------------------------------------------------------------------------//

function Main()

   local oCar := TCar():New( "red", 2 )
   local oPetrol

   oCar = oCar + oPetrol

return nil

//----------------------------------------------------------------------------//

CLASS TCar

   DATA cColor
   DATA nDoors
   DATA oGas

   METHOD New( cColor, nDoors ) CONSTRUCTOR

   METHOD Sum( oObject ) OPERATOR '+'

ENDCLASS

//----------------------------------------------------------------------------//

METHOD New( cColor, nDoors ) CLASS TCar

   if cColor == nil
      cColor = "White"
   endif
   if nDoors == nil
      nDoors = 4
   endif

   ::cColor = cColor
   ::nDoors = nDoors

return Self

//----------------------------------------------------------------------------//

METHOD Sum( oObject ) CLASS TCar

   Alert( "+ has a special meaning and " + ;
          "functionality for TCar Class objects!!!" )

return nil

//----------------------------------------------------------------------------//