/*
 * Harbour Project source code:
 * Harbour OS X Cocoa wrappers
 *
 * Copyright 2014 Viktor Szakats (vszakats.net/harbour)
 * www - http://www.harbour-project.org
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

#include "hbapi.h"
#include "hbapistr.h"

#import <Foundation/Foundation.h>
#import <SystemConfiguration/SystemConfiguration.h>

HB_FUNC( __OSX_PROXYGET )
{
   const char * protoreq = hb_parcx( 1 );

   CFStringRef     keyEnabled, keyHost, keyPort;
   CFDictionaryRef proxyDict = SCDynamicStoreCopyProxies( NULL );
   CFNumberRef     aNum;

   if( hb_stricmp( protoreq, "https" ) == 0 )
   {
      keyEnabled = kSCPropNetProxiesHTTPSEnable;
      keyHost    = kSCPropNetProxiesHTTPSProxy;
      keyPort    = kSCPropNetProxiesHTTPSPort;
   }
   else if( hb_stricmp( protoreq, "ftp" ) == 0 )
   {
      keyEnabled = kSCPropNetProxiesFTPEnable;
      keyHost    = kSCPropNetProxiesFTPProxy;
      keyPort    = kSCPropNetProxiesFTPPort;
   }
   else if( hb_stricmp( protoreq, "gopher" ) == 0 )
   {
      keyEnabled = kSCPropNetProxiesGopherEnable;
      keyHost    = kSCPropNetProxiesGopherProxy;
      keyPort    = kSCPropNetProxiesGopherPort;
   }
   else
   {
      keyEnabled = kSCPropNetProxiesHTTPEnable;
      keyHost    = kSCPropNetProxiesHTTPProxy;
      keyPort    = kSCPropNetProxiesHTTPPort;
   }

   hb_retc_null();
   hb_stor( 2 );

   aNum = CFDictionaryGetValue( proxyDict, keyEnabled );
   if( aNum )
   {
      HB_I32 iValue;

      CFNumberGetValue( aNum, kCFNumberSInt32Type, &iValue );

      if( iValue )
      {
         CFStringRef hostString;

         hostString = CFDictionaryGetValue( proxyDict, keyHost );

         if( hostString )
         {
            NSString * str = ( __bridge NSString * ) hostString;
            hb_retstr_utf8( [ str UTF8String ] );

            aNum = CFDictionaryGetValue( proxyDict, keyPort );
            if( aNum )
            {
               CFNumberGetValue( aNum, kCFNumberSInt32Type, &iValue );
               hb_stornl( iValue, 2 );
            }
         }
      }
   }
}
