/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 Financial functions
 *     - PV
 *     - FV
 *     - PAYMENT
 *     - PERIODS
 *     - RATE
 * Copyright 2001  Alejandro de Garate  <alex_degarate@hotmail.com>
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

PROCEDURE Main()

   ctinit()

   SET DECIMAL TO 3
   CLS
   ?? "Testing Financial functions...."
   ?
   ? "Calculate how loan summs if you make deposits for $175.00 for 24 months,"
   ? "if the annual rate of the Bank for this mortage is 9.5% fixed"
   ? "PV( 175, 0.095 / 12, 24 ) = 3811.433   // CT3"
   ? Space( 21 ), PV( 175, 0.095 / 12, 24 ), "  <-- CT for Harbour"
   ?
   ?

   ? "Calculate the amount in your account after 3 years, if you make deposits"
   ? "for $150.00 per month, and the annual rate of the Bank for this is 6%"
   ? "Capital = FV( 150, 0.06 / 12, 36 ) = 5900.416    // CT3"
   ? Space( 30 ), FV( 150, 0.06 / 12, 36 ), "  <-- CT for Harbour"
   ?
   ?

   ? "Calculate the monthly payment for a loan of $2000.00 at an annual rate"
   ? "of 10%, within 24 month "
   ? "Payment( 2000.00, 0.10 / 12, 24 ) =  92.290    // CT3"
   ? Space( 28 ), Payment( 2000.00, 0.10 / 12, 24 ), "  <-- CT for Harbour"
   ?
   ? "  PRESS ANY KEY"

   Inkey( 0 )
   ? "Continue Testing Financial functions...."
   ?
   ? "Calculate how many month do you need to cancel a loan of $4000.00 at"
   ? "an annual rate of 9.5% with payments of $200.00 max"
   ? "Periods( 4000.00, 200.00, 0.095 / 12 ) =  21.859    // CT3"
   ? Space( 33 ), Periods( 4000.00, 200.00, 0.095 / 12 ), "  <-- CT for Harbour"
   ?
   ?

   ? "Calculate which is the effective anual rate of your Bank, for a loan"
   ? "of $2500.00 if you pay $86.67 per month for 3 years"
   ? "Rate( 2500.00, 86.67, 36 ) * 12 = 0.1501  // CT3"
   ? Space( 24 ), Rate( 2500.00, 86.67, 36 ) * 12.0, "  <-- CT for Harbour"
   ?
   ? "  PRESS ANY KEY"
   Inkey( 0 )

   ctexit()

   RETURN
