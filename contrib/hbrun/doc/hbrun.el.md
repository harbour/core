Harbour Shell / Script Runner 3\.2\.0dev \(r2013\-03\-28 03:24\)  
Copyright \(c\) 2007\-2013, Viktor SzakÃ¡ts  
Copyright \(c\) 2003\-2007, PrzemysÅ‚aw Czerpak  
<http://harbour\-project\.org/>  

Syntax:  
  
  hbrun &lt;file\[\.hb|\.prg|\.hrb|\.dbf\]&gt;|&lt;option&gt; \[&lt;Ï€  
±Ï  
±  
¼  
µÏ„Ï  
¿Ï‚\[  
¹\]&gt;\]  
  
  
   
µÏ  
¹  
³Ï  
±Ï†  
®:  


    
¤  
¿ hbrun   
¼Ï€  
¿Ï  
µ  
¯   
½  
±   
µ  
ºÏ„  
µ  
»  
µ  
¯ Ïƒ  
µ  
½  
¬Ï  
¹  
± Harbour  \(Ï€  
·  
³  
±  
¯  
±   
® Ï€Ï  
¿  
º  
±Ï„  
±Ïƒ  
º  
µÏ…  
±Ïƒ  
¼  
­  
½  
±\),   
º  
±  
¹ Ï€Ï  
¿ÏƒÏ†  
­Ï  
µ  
¹   
µÏ€  
¯Ïƒ  
·Ï‚   
­  
½  
±   
´  
¹  
±  
»  
¿  
³  
¹  
ºÏŒ Ï€  
µÏ  
¹  
²  
¬  
»  
»  
¿  
½\.
  
  
Ÿ  
¹ Ï€  
±Ï  
±  
º  
¬Ï„Ï‰   
µÏ€  
¹  
»  
¿  
³  
­Ï‚   
µ  
¯  
½  
±  
¹   
´  
¹  
±  
¸  
­Ïƒ  
¹  
¼  
µÏ‚ ÏƒÏ„  
·   
³Ï  
±  
¼  
¼  
®\-  
µ  
½Ï„  
¿  
»Ï  
½:  


 - **\-\-hb:debug**   
•  
½  
µÏ  
³  
¿Ï€  
¿  
¯  
·Ïƒ  
·   
±Ï€  
¿ÏƒÏ†  
±  
»  
¼  
¬Ï„Ï‰Ïƒ  
·Ï‚ script


 - **\-help**   
· Ï€  
±Ï  
¿ÏÏƒ  
±   
²  
¿  
®  
¸  
µ  
¹  
±
 - **\-viewhelp**   
µ  
ºÏ„  
µÏ„  
±  
¼  
­  
½  
·   
²  
¿  
®  
¸  
µ  
¹  
± Ïƒ  
µ Ï€Ï  
¿  
²  
¿  
»  
®   
º  
µ  
¹  
¼  
­  
½  
¿Ï…
 - **\-longhelp** long help
 - **\-longhelpmd**   
µ  
ºÏ„  
µÏ„  
±  
¼  
­  
½  
·   
²  
¿  
®  
¸  
µ  
¹  
± Ïƒ  
µ   
¼  
¿ÏÏ†  
® [Markdown](http://daringfireball.net/projects/markdown/)
  
  
‘ÏÏ‡  
µ  
¯  
±:  


 - **\*\.hb**   
£  
µ  
½  
¬Ï  
¹  
¿ Harbour
 - **\*\.hrb**   
œ  
µÏ„  
±Ï†  
­ÏÏƒ  
¹  
¼  
¿   
´Ï…  
±  
´  
¹  
ºÏŒ Harbour \(  
³  
½Ï‰ÏƒÏ„ÏŒ   
º  
±  
¹ Ï‰Ï‚ Ï€Ï  
¿  
º  
±Ï„  
±Ïƒ  
º  
µÏ…  
±Ïƒ  
¼  
­  
½  
¿ Ïƒ  
µ  
½  
¬Ï  
¹  
¿\)
 - **hbstart\.hb** startup Harbour script for interactive Harbour shell\. It gets executed automatically on shell startup, if present\. Possible locations \(in order of precedence\) \[\*\]: \.\\, %APPDATA%\\\.harbour, &lt;hbrun   
º  
±Ï„  
¬  
»  
¿  
³  
¿Ï‚&gt;
 - **shell plugins** \.hb and \.hrb plugins   
³  
¹  
± Ï„  
¿   
´  
¹  
±  
»  
¿  
³  
¹  
ºÏŒ   
º  
­  
»Ï…Ï†  
¿Ï‚ Ï„  
¿Ï… Harbour\.   
 Ï  
­Ï€  
µ  
¹   
½  
±   
²Ï  
¯Ïƒ  
º  
¿  
½Ï„  
±  
¹   
¼  
­Ïƒ  
± ÏƒÏ„  
¿ \[\*\]: %APPDATA%\\\.harbour\\
 - **\.hb\_history**   
±Ï€  
¿  
¸  
·  
º  
µÏ  
µ  
¹   
¹ÏƒÏ„  
¿Ï  
¹  
ºÏŒ   
µ  
½Ï„  
¿  
»Ï  
½   
³  
¹  
± Ï„  
¿   
´  
¹  
±  
»  
¿  
³  
¹  
ºÏŒ shell Harbour\.   
œÏ€  
¿Ï  
µ  
¯Ï„  
µ   
½  
±   
±Ï€  
µ  
½  
µÏ  
³  
¿Ï€  
¿  
¹  
®Ïƒ  
µÏ„  
µ Ï„  
¿   
¹ÏƒÏ„  
¿Ï  
¹  
ºÏŒ   
º  
¬  
½  
¿  
½Ï„  
±Ï‚ Ï„  
·  
½ Ï€ÏÏÏ„  
·   
³Ï  
±  
¼  
¼  
® 'no' \(Ï‡Ï‰Ï  
¯Ï‚ Ï„  
±   
µ  
¹Ïƒ  
±  
³Ï‰  
³  
¹  
º  
¬   
º  
±  
¹   
¼  
µ   
½  
µ  
±   
³Ï  
±  
¼  
¼  
®\)\.   
’Ï  
¯Ïƒ  
º  
µÏ„  
±  
¹ ÏƒÏ„  
¿ \[\*\]: %APPDATA%\\\.harbour\\
 - **hb\_extension**   
»  
¯ÏƒÏ„  
±   
º  
±Ï„  
±  
»  
®  
¾  
µÏ‰  
½ Ï€Ï  
¿Ï‚ Ï†ÏŒÏÏ„Ï‰Ïƒ  
· ÏƒÏ„  
¿   
´  
¹  
±  
»  
¿  
³  
¹  
ºÏŒ   
º  
­  
»Ï…Ï†  
¿Ï‚ Ï„  
¿Ï… Harbour\.   
œ  
¯  
±   
º  
±Ï„  
¬  
»  
·  
¾  
·   
±  
½  
±   
³Ï  
±  
¼  
¼  
®, Ï„  
¿ Ï„  
¼  
®  
¼  
± Ï„  
·Ï‚   
³Ï  
±  
¼  
¼  
®Ï‚   
¼  
µÏ„  
¬   
±Ï€ÏŒ   
­  
½  
± Ï‡  
±Ï  
±  
ºÏ„  
®Ï  
± '\#'   
±  
³  
½  
¿  
µ  
¯Ï„  
±  
¹\.   
•  
½  
±  
»  
»  
±  
ºÏ„  
¹  
º  
¬   
¿  
½  
¿  
¼  
±\-  
±ÏÏ‡  
µ  
¯  
¿Ï… ÏƒÏ„  
¿ Ms\-DOS:   
¤  
¿ hb\_ext\.ini\.   
’Ï  
¯Ïƒ  
º  
µÏ„  
±  
¹   
¼  
µÏƒ  
± ÏƒÏ„  
¿ \[\*\]: %APPDATA%\\\.harbour\\


  
 Ï  
¿  
º  
±  
¸  
¿Ï  
¹Ïƒ  
¼  
­  
½  
µÏ‚ ÏƒÏ„  
±  
¸  
µÏ  
­Ï‚ Ïƒ  
µ Ï€  
·  
³  
±  
¯  
±   
±ÏÏ‡  
µ  
¯  
±:


 - **\_\_HBSCRIPT\_\_HBSHELL** when a Harbour source file is run as a shell script
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, etc\.\.\.
  
  
œ  
µÏ„  
±  
²  
»  
·Ï„  
­Ï‚ Ï€  
µÏ  
¹  
²  
¬  
»  
»  
¿  
½Ï„  
¿Ï‚:  


 - **HB\_EXTENSION**   
»  
¯ÏƒÏ„  
±   
º  
±Ï„  
±  
»  
®  
¾  
µÏ‰  
½,   
´  
¹  
±Ï‡Ï‰Ï  
¹Ïƒ  
¼  
­  
½Ï‰  
½   
¼  
µ   
º  
µ  
½ÏŒ   
´  
¹  
¬ÏƒÏ„  
·  
¼  
±, Ï€Ï  
¿Ï‚ Ï†ÏŒÏÏ„Ï‰Ïƒ  
· ÏƒÏ„  
¿   
´  
¹  
±  
»  
¿  
³  
¹  
ºÏŒ   
º  
­  
»Ï…Ï†  
¿Ï‚ Ï„  
¿Ï… Harbour 
  
Shell API   
´  
¹  
±  
¸  
­Ïƒ  
¹  
¼  
¿ Ïƒ  
µ Ïƒ  
µ  
½  
¬Ï  
¹  
± Harbour:  


 - **hbshell\_gtSelect\( \[&lt;cGT&gt;\] \) \-&gt; NIL**  
Switch GT\. Default \[\*\]: 'gtwin'
 - **hbshell\_Clipper\(\) \-&gt; NIL**  
Enable Clipper compatibility \(non\-Unicode\) mode\.
 - **hbshell\_include\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
  
¦ÏŒÏÏ„Ï‰Ïƒ  
· Harbour header\.
 - **hbshell\_uninclude\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
  
‘Ï€  
¿Ï†ÏŒÏÏ„Ï‰Ïƒ  
· Harbour header\.
 - **hbshell\_include\_list\(\) \-&gt; NIL**  
  
•  
¼Ï†  
¬  
½  
¹Ïƒ  
·   
»  
¯ÏƒÏ„  
±Ï‚ Ï„Ï‰  
½ Ï†  
¿ÏÏ„Ï‰  
¼  
­  
½Ï‰  
½ Harbour header\.
 - **hbshell\_ext\_load\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
  
¦ÏŒÏÏ„Ï‰Ïƒ  
· Ï€  
±  
º  
­Ï„  
¿Ï…\.   
   
±ÏÏŒ  
¼  
¿  
¹  
¿   
¼  
µ Ï„  
·   
½Ï„  
¹Ï  
µ  
ºÏ„  
¯  
²  
± \#request PP\.
 - **hbshell\_ext\_unload\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
  
‘Ï€  
¿Ï†ÏŒÏÏ„Ï‰Ïƒ  
· Ï€  
±  
º  
­Ï„  
¿Ï…
 - **hbshell\_ext\_get\_list\(\) \-&gt; &lt;aPackages&gt;**  
List of loaded packages\.
 - **hbshell\_DirBase\(\) \-&gt; &lt;cBaseDir&gt;**  
hb\_DirBase\(\) not mapped to script\.
 - **hbshell\_ProgName\(\) \-&gt; &lt;cPath&gt;**  
hb\_ProgName\(\) not mapped to script\.
  
Notes:  


  - \.hb, \.prg, \.hrb   
® \.dbf   
±ÏÏ‡  
µ  
¯  
¿   
´  
¿Ïƒ  
¼  
­  
½  
¿ Ï‰Ï‚ Ï€ÏÏÏ„  
· Ï€  
±Ï  
±  
¼  
­Ï„Ï  
¿Ï‚   
¸  
±   
µ  
ºÏ„  
µ  
»  
µÏƒÏ„  
µ  
¯ Ïƒ  
±  
½ Ïƒ  
µ  
½  
¬Ï  
¹  
¿ Harbour\.   
‘  
½ Ï„  
¿ ÏŒ  
½  
¿  
¼  
±\-  
±ÏÏ‡  
µ  
¯  
¿Ï…   
´  
µ  
½ Ï€  
µÏ  
¹  
­Ï‡  
µ  
¹   
¿Ï  
¯Ïƒ  
¼  
±Ï„  
±   
¼  
¿  
½  
¿Ï€  
±Ï„  
¹Ï  
½,   
¸  
±   
±  
½  
±  
¶  
·Ï„  
·  
¸  
µ  
¯ ÏƒÏ„  
¿ Ï„Ï  
­Ï‡  
¿  
½Ï„  
±   
º  
±Ï„  
¬  
»  
¿  
³  
¿   
µÏ  
³  
±Ïƒ  
¯  
±Ï‚   
º  
±  
¹ ÏƒÏ„  
¿  PATH\.   
‘  
½   
´  
µ  
½   
´ÏŒ  
¸  
·  
º  
µ   
º  
±Ï„  
¬  
»  
·  
¾  
·,   
¸  
±   
±  
½  
±  
¶  
·Ï„  
·  
¸  
¿Ï  
½ \.hb and \.hrb   
º  
±Ï„  
±  
»  
®  
¾  
µ  
¹Ï‚,   
¼  
µ   
±Ï…Ï„  
® Ï„  
· Ïƒ  
µ  
¹Ï  
¬\.   
‘ÏÏ‡  
µ  
¯  
¿ \.dbf   
¸  
±   
±  
½  
¿  
¹Ï‡Ï„  
µ  
¯   
±Ï…Ï„ÏŒ  
¼  
±Ï„  
± Ïƒ  
µ   
º  
±Ï„  
¬ÏƒÏ„  
±Ïƒ  
· shared   
º  
±  
¹   
¸  
±   
¾  
µ  
º  
¹  
½  
®Ïƒ  
µ  
¹ Ï„  
¿   
´  
¹  
±  
»  
¿  
³  
¹  
ºÏŒ shell Ï„  
¿Ï… Harbour\.   
œ  
· standard   
º  
±Ï„  
±  
»  
®  
¾  
µ  
¹Ï‚   
¸  
±   
±  
½  
¹Ï‡  
½  
µÏ…Ï„  
¿Ï  
½   
±Ï…Ï„  
¿  
¼  
¬Ï„Ï‰Ï‚   
³  
¹  
± Ï€  
·  
³  
±  
¯  
¿Ï…Ï‚   
º  
±  
¹ Ï€Ï  
¿  
º  
±Ï„  
±Ïƒ  
µ  
ºÏ…  
±Ïƒ  
¼  
­  
½  
¿Ï…Ï‚ Ï„Ï…Ï€  
¿Ï…Ï‚\.   
£  
·  
¼  
µ  
¹ÏÏƒÏ„  
µ,   
³  
¹  
± Ïƒ  
µ  
½  
¬Ï  
¹  
± Harbour,   
·   
ºÏ‰  
´  
¹  
º  
¿Ïƒ  
µ  
»  
¯  
´  
±   
¿Ï  
¯  
¶  
µÏ„  
±  
¹   
±Ï€  
¿ Ï€Ï  
¿  
µÏ€  
¹  
»  
¿  
³  
® Ïƒ  
µ  UTF\-8\.   
¤  
¿   
µ  
¾'  
¿Ï  
¹Ïƒ  
¼  
¿Ï   
²  
±Ïƒ  
¹  
ºÏŒ   
±ÏÏ‡  
µ  
¯  
¿ header 'hb\.ch'  ÏƒÏ…  
¼Ï€  
µÏ  
¹  
»  
±  
¼  
²  
¬  
½  
µÏ„  
±  
¹   
±Ï…Ï„ÏŒ  
¼  
±Ï„  
±,   
´  
·  
»\. \#included\.   
 Ï  
¿  
µÏ€  
¹  
»  
µ  
³  
¼  
­  
½  
·   
¼  
¿ÏÏ†  
®   
·  
¼  
µÏ/  
½  
¯  
±Ï‚   
µ  
¯  
½  
±  
¹   
· Ï€ÏÏŒÏ„Ï…Ï€  
·   
º  
±Ï„  
±  ISO   
¼  
¿ÏÏ†  
®:   
µ  
µ  
µ  
µ\-  
¼  
¼\-  
·  
·\.   
 Ï  
¿  
µÏ€  
¹  
»  
µ  
³  
¼  
­  
½  
¿ GT   
µ  
¯  
½  
±  
¹ Ï„  
¿ 'gtcgi',   
µ  
ºÏ„ÏŒÏ‚   
±  
½   
±  
½  
¹Ï‡  
½  
µÏ…Ï„  
¿Ï  
½ CUI   
º  
»  
®Ïƒ  
µ  
¹Ï‚ Ï€  
»  
®Ï  
¿Ï…Ï‚   
¿  
¸ÏŒ  
½  
·Ï‚,   
¿Ï€ÏŒÏ„  
µ   
µÏ€  
¹  
»  
­  
³  
µÏ„  
±  
¹   
±Ï…Ï„  
¿  
¼  
¬Ï„Ï‰Ï‚ 'gtwin' \[\*\] \(  
µ  
ºÏ„ÏŒÏ‚   
³  
¹  
± Ï„  
¹Ï‚ INIT PROCEDUREs\)\.
  -   
œÏ€  
¿Ï  
µ  
¯   
½  
± Ï‡Ï  
·Ïƒ  
¹  
¼  
¿Ï€  
¿  
¹  
·  
¸  
µ  
¯   
¿ ÏƒÏ…  
½  
´Ï…  
±Ïƒ  
¼ÏŒÏ‚ Ï€  
»  
®  
ºÏ„ÏÏ‰  
½ &lt;Alt\+V&gt; ÏƒÏ„  
¿   
´  
¹  
±  
´Ï  
±ÏƒÏ„  
¹  
ºÏŒ shell Ï„  
¿Ï… Harbour   
³  
¹  
±   
µÏ€  
¹  
ºÏŒ  
»  
·Ïƒ  
·   
±Ï€ÏŒ Ï„  
¿ Ï€ÏÏŒÏ‡  
µ  
¹Ï  
¿\.
  -   
¤  
¹  
¼  
­Ï‚   
¼  
µ   
±ÏƒÏ„  
µÏ  
¯Ïƒ  
º  
¿ \[\*\]   
¼Ï€  
¿Ï  
µ  
¯   
½  
±   
µ  
¾  
±ÏÏ„Ï  
½Ï„  
±  
¹   
±Ï€ÏŒ Ï„  
·  
½ Ï€  
»  
±Ï„Ï†ÏŒÏ  
¼  
± Ï…Ï€  
¿  
´  
¿Ï‡  
®Ï‚   
®/  
º  
±  
¹ Ï„  
·   
´  
¹  
±  
¼ÏŒÏÏ†Ï‰Ïƒ  
·\.   
— Ï€  
±Ï  
¿ÏÏƒ  
±   
²  
¿  
®  
¸  
µ  
¹  
±   
´  
·  
¼  
¹  
¿Ï…Ï  
³  
®  
¸  
·  
º  
µ ÏƒÏ„  
·  
½ 'win' Ï€  
»  
±Ï„Ï†ÏŒÏ  
¼  
± Ï…Ï€  
¿  
´  
¿Ï‡  
®Ï‚\.
  
  
‘  
´  
µ  
¹  
±:  


  This program is free software; you can redistribute it and/or modify  
it under the terms of the GNU General Public License as published by  
the Free Software Foundation; either version 2 of the License, or  
\(at your option\) any later version\.  
  
This program is distributed in the hope that it will be useful,  
but WITHOUT ANY WARRANTY; without even the implied warranty of  
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE\.  See the  
GNU General Public License for more details\.  
  
You should have received a copy of the GNU General Public License  
along with this program; if not, write to the Free Software  
Foundation, Inc\., 675 Mass Ave, Cambridge, MA 02139, USA \(or visit  
their web site at http://www\.gnu\.org/\)\.  
  
License extensions:  
  \- This source code must be kept and distributed as part  
    of the Harbour package and/or the placement of the tool sources  
    and files must reflect that it is part of Harbour Project\.  
  \- Copyright information must always be presented by  
    projects including this tool or help text\.  
  \- Modified versions of the tool must clearly state this  
    fact on the copyright screen\.  
  \- Source code modifications shall always be made available  
    along with binaries\.  
  \- Help text and documentation is licensed under  
    Creative Commons Attribution\-ShareAlike 3\.0:  
    http://creativecommons\.org/licenses/by\-sa/3\.0/  

  
  
£Ï…  
³  
³Ï  
±Ï†  
­  
±Ï‚:  


 - Viktor SzakÃ¡ts \(harbour syenar\.net\) 
