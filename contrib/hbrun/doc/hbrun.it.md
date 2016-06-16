Harbour Shell / Script Runner 3\.4\.0dev \(9dea61d\) \(2016\-03\-09 22:28\)  
Copyright &copy; 2007\-2016, Viktor Szakáts  
Copyright &copy; 2003\-2007, Przemysław Czerpak  
<https://github\.com/vszakats/harbour\-core/>  
Traduzione \(it\): \(inserisci qui il tuo nome\)  

Sintassi:  
  
  hbrun &lt;file\[\.hb|\.prg|\.hrb|\.dbf\]&gt;|&lt;option&gt; \[&lt;parametro\[i\]&gt;\]  
  
Descrizione:  


  hbrun is able to run Harbour scripts \(both source and precompiled\), and it also features an interactive shell prompt\.
  
Le opzioni riportate di seguito sono disponibili da riga di comando:  


 - **\-\-hb:debug** abilita il debug dello script


 - **\-help** questo aiuto
 - **\-viewhelp** full help in text viewer
 - **\-fullhelp** full help
 - **\-fullhelpmd** full help in [Markdown](https://daringfireball.net/projects/markdown/) format
  
Files:  


 - **\*\.hb** Script di Harbour
 - **\*\.hrb** Harbour binario portabile \(conosciuto anche come script Harbour precompilato\)
 - **hbstart\.hb** startup Harbour script for interactive Harbour shell\. It gets executed automatically on shell startup, if present\. Possible locations \(in order of precedence\) \[\*\]: \./, $HOME/\.harbour, /etc/harbour, &lt;cartella hbrun&gt;/\.\./etc/harbour, &lt;cartella hbrun&gt;/\.\./etc, &lt;cartella hbrun&gt;
 - **shell plugins** plugins \.hb e \.hrb per la shell interattiva di Harbour\. Possono essere situati in \[\*\]: $HOME/\.harbour/
 - **\.hb\_history** memorizza la cronologia dei comandi per la shell interattiva di Harbour\. E' possibile disattivare la cronologia creando la prima linea 'no' \(senza virgolette e ritorno a capo\)\. E' contenuta in \[\*\]: $HOME/\.harbour/
 - **hb\_extension** list of extensions to load in interactive Harbour shell\. One extension per line, part of line beyond a '\#' character is ignored\. Alternate filename on MS\-DOS: hb\_ext\.ini\. Resides in \[\*\]: $HOME/\.harbour/


Predefined constants in sources \(do not define them manually\):


 - **\_\_HBSCRIPT\_\_HBSHELL** quando un file sorgente Harbour è eseguito come uno script di shell
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, etc\.
  
Variabili d'ambiente:  


 - **HB\_EXTENSION** lista di estensioni separate da spazio da caricare nella shell interattiva di Harbour
  
Shell API disponibile negli Harbour scripts:  


 - **hbshell\_gtSelect\( \[&lt;cGT&gt;\] \) \-&gt; NIL**  
Cambia GT\. Default \[\*\]: 'gttrm'
 - **hbshell\_Clipper\(\) \-&gt; NIL**  
Enable Cl\*pper compatibility \(non\-Unicode\) mode\.
 - **hbshell\_include\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Carica l'intestazione Harbour\.
 - **hbshell\_uninclude\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Scarica l'intestazione Harbour\.
 - **hbshell\_include\_list\(\) \-&gt; NIL**  
Visualizza l'elenco di intestazione di Harbour caricato\.
 - **hbshell\_ext\_load\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Carica un pacchetto\. Simile alla direttiva PP \#request\.
 - **hbshell\_ext\_unload\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Scaricare pacchetto\.
 - **hbshell\_ext\_get\_list\(\) \-&gt; &lt;aPackages&gt;**  
Lista dei pacchetti caricati
 - **hbshell\_DirBase\(\) \-&gt; &lt;cBaseDir&gt;**  
hb\_DirBase\(\) not mapped to script\.
 - **hbshell\_ProgName\(\) \-&gt; &lt;cPath&gt;**  
hb\_ProgName\(\) not mapped to script\.
 - **hbshell\_ScriptName\(\) \-&gt; &lt;cPath&gt;**  
Name of the script executing\.
  
Note:  


  - \.hb, \.prg, \.hrb or \.dbf file passed as first parameter will be run as Harbour script\. If the filename contains no path components, it will be searched in current working directory and in PATH\. If not extension is given, \.hb and \.hrb extensions are searched, in that order\. \.dbf file will be opened automatically in shared mode and interactive Harbour shell launched\. Non\-standard extensions will be auto\-detected for source and precompiled script types\. Note, for Harbour scripts, the codepage is set to UTF\-8 by default\. The default core header 'hb\.ch' is automatically \#included at the interactive shell prompt\. The default date format is the ISO standard: yyyy\-mm\-dd\. SET EXACT is set to ON\. Set\( \_SET\_EOL \) is set to OFF\. The default GT is 'gtcgi', unless full\-screen CUI calls are detected, when 'gttrm' \[\*\] is automatically selected \(except for INIT PROCEDUREs\)\.
  - You can use key &lt;Ctrl\+V&gt; in interactive Harbour shell to paste text from the clipboard\.
  - Values marked with \[\*\] may be host platform and/or configuration dependent\. This help was generated on 'darwin' host platform\.
  
Licenza:  


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

  
Autore:  


 - Viktor Szakáts \(vszakats\.net/harbour\) 
