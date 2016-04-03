Harbour Shell / Script Runner 3\.4\.0dev \(9dea61d\) \(2016\-03\-09 22:28\)  
Copyright &copy; 2007\-2016, Viktor Szakáts  
Copyright &copy; 2003\-2007, Przemysław Czerpak  
<https://github\.com/vszakats/harbour\-core/>  
Magyar \(hu\) fordítás: Copyright &copy; 2009\-2016, Szakáts Viktor  

Használat:  
  
  hbrun &lt;fájl\[\.hb|\.prg|\.hrb|\.dbf\]&gt;|&lt;kapcsoló&gt; \[&lt;parameter\[s\]&gt;\]  
  
Description:  


  hbrun is able to run Harbour scripts \(both source and precompiled\), and it also features an interactive shell prompt\.
  
Options below are available on command\-line:  


 - **\-\-hb:debug** enable script debugging


 - **\-help** ez a súgó
 - **\-viewhelp** full help in text viewer
 - **\-fullhelp** full help
 - **\-fullhelpmd** full help in [Markdown](https://daringfireball.net/projects/markdown/) format
  
Fájlok:  


 - **\*\.hb** Harbour script
 - **\*\.hrb** Harbour portable binary \(aka precompiled Harbour script\)
 - **hbstart\.hb** startup Harbour script for interactive Harbour shell\. It gets executed automatically on shell startup, if present\. Possible locations \(in order of precedence\) \[\*\]: \./, $HOME/\.harbour, /etc/harbour, &lt;hbrun mappa&gt;/\.\./etc/harbour, &lt;hbrun mappa&gt;/\.\./etc, &lt;hbrun mappa&gt;
 - **shell plugins** \.hb and \.hrb plugins for interactive Harbour shell\. They may reside in \[\*\]: $HOME/\.harbour/
 - **\.hb\_history** stores command history for interactive Harbour shell\. You can disable history by making the first line 'no' \(without quotes and with newline\)\. Resides in \[\*\]: $HOME/\.harbour/
 - **hb\_extension** list of extensions to load in interactive Harbour shell\. One extension per line, part of line beyond a '\#' character is ignored\. Alternate filename on MS\-DOS: hb\_ext\.ini\. Resides in \[\*\]: $HOME/\.harbour/


Predefined constants in sources \(do not define them manually\):


 - **\_\_HBSCRIPT\_\_HBSHELL** when a Harbour source file is run as a shell script
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, etc\.
  
Environment variables:  


 - **HB\_EXTENSION** space separated list of extensions to load in interactive Harbour shell
  
Shell API available in Harbour scripts:  


 - **hbshell\_gtSelect\( \[&lt;cGT&gt;\] \) \-&gt; NIL**  
Switch GT\. Default \[\*\]: 'gttrm'
 - **hbshell\_Clipper\(\) \-&gt; NIL**  
Enable Cl\*pper compatibility \(non\-Unicode\) mode\.
 - **hbshell\_include\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Load Harbour header\.
 - **hbshell\_uninclude\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Unload Harbour header\.
 - **hbshell\_include\_list\(\) \-&gt; NIL**  
Display list of loaded Harbour header\.
 - **hbshell\_ext\_load\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Load package\. Similar to \#request PP directive\.
 - **hbshell\_ext\_unload\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Unload package\.
 - **hbshell\_ext\_get\_list\(\) \-&gt; &lt;aPackages&gt;**  
List of loaded packages\.
 - **hbshell\_DirBase\(\) \-&gt; &lt;cBaseDir&gt;**  
hb\_DirBase\(\) not mapped to script\.
 - **hbshell\_ProgName\(\) \-&gt; &lt;cPath&gt;**  
hb\_ProgName\(\) not mapped to script\.
 - **hbshell\_ScriptName\(\) \-&gt; &lt;cPath&gt;**  
Name of the script executing\.
  
Megjegyzések:  


  - \.hb, \.prg, \.hrb or \.dbf file passed as first parameter will be run as Harbour script\. If the filename contains no path components, it will be searched in current working directory and in PATH\. If not extension is given, \.hb and \.hrb extensions are searched, in that order\. \.dbf file will be opened automatically in shared mode and interactive Harbour shell launched\. Non\-standard extensions will be auto\-detected for source and precompiled script types\. Note, for Harbour scripts, the codepage is set to UTF\-8 by default\. The default core header 'hb\.ch' is automatically \#included at the interactive shell prompt\. The default date format is the ISO standard: yyyy\-mm\-dd\. SET EXACT is set to ON\. Set\( \_SET\_EOL \) is set to OFF\. The default GT is 'gtcgi', unless full\-screen CUI calls are detected, when 'gttrm' \[\*\] is automatically selected \(except for INIT PROCEDUREs\)\.
  - You can use key &lt;Ctrl\+V&gt; in interactive Harbour shell to paste text from the clipboard\.
  - Values marked with \[\*\] may be host platform and/or configuration dependent\. This help was generated on 'darwin' host platform\.
  
Licenc \(angolul\):  


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
their website at https://www\.gnu\.org/\)\.  
  
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
    Creative Commons Attribution\-ShareAlike 4\.0 International:  
    https://creativecommons\.org/licenses/by\-sa/4\.0/  

  
Szerző:  


 - Viktor Szakáts \(vszakats\.net/harbour\) 
