 ; Copyright(C) 1999 by Antonio Linares.
 ;
 ; This program is free software; you can redistribute it and/or modify
 ; it under the terms of the GNU General Public License as published
 ; by the Free Software Foundation; either version 2 of the License, or
 ; (at your option) any later version.
 ;
 ; This program is distributed in the hope that it will be useful, but
 ; WITHOUT ANY WARRANTY; without even the implied warranty of
 ; MERCHANTABILITY or FITNESS FOR A PARTICULAR
 ; PURPOSE.  See the GNU General Public License for more details.
 ;
 ; You should have received a copy of the GNU General Public
 ; License along with this program; if not, write to:
 ;
 ; The Free Software Foundation, Inc.,
 ; 675 Mass Ave, Cambridge, MA 02139, USA.
 ;
 ; You can contact me at: alinares@fivetech.com
 ;

.386

HB_STARTSYMBOLS segment dword use32 public 'DATA'
HB_STARTSYMBOLS ends

HB_SYMBOLS segment dword use32 public 'DATA'
HB_SYMBOLS ends

HB_ENDSYMBOLS segment dword use32 public 'DATA'
HB_ENDSYMBOLS ends

DGROUP  group HB_STARTSYMBOLS, HB_SYMBOLS, HB_ENDSYMBOLS

    public  _hb_firstsymbol, _hb_lastsymbol

HB_STARTSYMBOLS segment
  _hb_firstsymbol label byte
HB_STARTSYMBOLS ends

HB_ENDSYMBOLS segment
  _hb_lastsymbol label byte
HB_ENDSYMBOLS ends

    end
