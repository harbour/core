/*
 * $Id$
 */

#include <extend.h>

HARBOUR VERSION()
{
   _retc( "Harbour alpha version" );
}

HARBOUR GETENV()
{
   if( _pcount() == 1 )
   {
      char *szName = _parc(1);
      long lName = _parclen(1);

      while( lName && szName[lName - 1] == '=' )
      {
         /* strip the '=' or else it will clear the variable! */
         szName[lName - 1] = 0;
         lName--;
      }
      if( lName )
      {
         char *Value = getenv(szName);
         _retc(Value? Value: "");
      }
      else
         _retc("");
   }
   else
      _retc("");
}

