Harbour Shell / Script Runner 3\.4\.0dev \(r2014\-01\-01 13:00\)  
Copyright &copy; 2007\-2014, Viktor Szakáts  
Copyright &copy; 2003\-2007, Przemysław Czerpak  
<http://harbour\-project\.org/>  
Μετάφραση \(el\): Pete D\. \(pete\_westg@yahoo\.gr\)  

Σύνταξη:  
  
  hbrun &lt;αρχείο\[\.hb|\.prg|\.hrb|\.dbf\]&gt;|&lt;επιλογή&gt; \[&lt;παραμετρος\[ι\]&gt;\]  
  
Περιγραφή:  


  Το hbrun μπορεί να εκτελεί σενάρια Harbour \(πηγαία ή προκατασκευασμένα\), ενώ προσφέρει επίσης ένα διαδραστικό περιβάλλον εργασίας\.
  
Οι παρακάτω επιλογές είναι διαθέσιμες στη γραμμή\-εντολών:  


 - **\-\-hb:debug** Ενεργοποίηση αποσφαλμάτωσης script


 - **\-help** η παρούσα βοήθεια
 - **\-viewhelp** εκτεταμένη βοήθεια σε προβολή κειμένου
 - **\-longhelp** εκτεταμένη βοήθεια
 - **\-longhelpmd** εκτεταμένη βοήθεια σε μορφή [Markdown](http://daringfireball.net/projects/markdown/)
  
Αρχεία:  


 - **\*\.hb** Σενάριο Harbour
 - **\*\.hrb** Μεταφέρσιμο δυαδικό Harbour \(γνωστό και ως προκατασκευασμένο σενάριο\)
 - **hbstart\.hb** εναρκτήριο σενάριο Harbour για το διαδραστικό κέλυφος Harbour\. Εκτελείται αυτόματα κατά την έναρξη του κελύφους, αν υπάρχει\. Ενδεχόμενες τοποθεσίες \(με σειρά προτεραιότητας\) \[\*\]: \.\\, %APPDATA%\\\.harbour, &lt;hbrun κατάλογος&gt;
 - **shell plugins** \.hb και \.hrb plugins για το διαδραστικό κέλυφος του Harbour\. Πρέπει να βρίσκονται μέσα στο \[\*\]: %APPDATA%\\\.harbour\\
 - **\.hb\_history** αποθηκεύει ιστορικό εντολών για το διαδραστικό κέλυφος του Harbour\. Μπορείτε να απενεργοποιήσετε το ιστορικό κάνοντας την πρώτη γραμμή 'no' \(χωρίς τα εισαγωγικά και με νεα γραμμή\)\. Βρίσκεται στο \[\*\]: %APPDATA%\\\.harbour\\
 - **hb\_extension** λίστα καταλήξεων προς φόρτωση στο διαδραστικό κέλυφος του Harbour\. Μία κατάληξη ανα γραμμή, το τμήμα της γραμμής μετά από ένα χαρακτήρα '\#' αγνοείται\. Εναλλακτικά ονομα\-αρχείου στο Ms\-DOS: Το hb\_ext\.ini\. Βρίσκεται μεσα στο \[\*\]: %APPDATA%\\\.harbour\\


Predefined constants in sources \(do not define them manually\):


 - **\_\_HBSCRIPT\_\_HBSHELL** όταν ένα πηγαίο αρχείο Harbour εκτελείται ως σενάριο κελύφους
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, etc\.
  
Μεταβλητές περιβάλλοντος:  


 - **HB\_EXTENSION** λίστα καταλήξεων, διαχωρισμένων με κενό διάστημα, προς φόρτωση στο διαδραστικό κέλυφος του Harbour
  
Shell API διαθέσιμο σε σενάρια Harbour:  


 - **hbshell\_gtSelect\( \[&lt;cGT&gt;\] \) \-&gt; NIL**  
Αλλαγή GT\. Προεπιλογή \[\*\]: 'gtwin'
 - **hbshell\_Clipper\(\) \-&gt; NIL**  
Ενεργοποίηση κατάστασης συμβατότητας Clipper \(όχι\-Unicode\)\.
 - **hbshell\_include\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Φόρτωση Harbour header\.
 - **hbshell\_uninclude\( &lt;cHeader&gt; \) \-&gt; &lt;lSuccess&gt;**  
Αποφόρτωση Harbour header\.
 - **hbshell\_include\_list\(\) \-&gt; NIL**  
Εμφάνιση λίστας των φορτωμένων Harbour header\.
 - **hbshell\_ext\_load\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Φόρτωση πακέτου\. Παρόμοιο με τη ντιρεκτίβα \#request PP\.
 - **hbshell\_ext\_unload\( &lt;cPackageName&gt; \) \-&gt; &lt;lSuccess&gt;**  
Αποφόρτωση πακέτου
 - **hbshell\_ext\_get\_list\(\) \-&gt; &lt;aPackages&gt;**  
Λίστα φορτωμένων πακέτων
 - **hbshell\_DirBase\(\) \-&gt; &lt;cBaseDir&gt;**  
Το hb\_DirBase\(\) δεν χαρτογραφήθηκε σε σενάριο\.
 - **hbshell\_ProgName\(\) \-&gt; &lt;cPath&gt;**  
Το hb\_ProgName\(\) δεν χαρτογραφήθηκε σε σενάριο\.
 - **hbshell\_ScriptName\(\) \-&gt; &lt;cPath&gt;**  
Name of the script executing\.
  
Σημειώσεις:  


  - \.hb, \.prg, \.hrb ή \.dbf file passed as first parameter will be run as Harbour script\. If the filename contains no path components, it will be searched in current working directory and in PATH\. If not extension is given, \.hb and \.hrb extensions are searched, in that order\. \.dbf file will be opened automatically in shared mode and interactive Harbour shell launched\. Non\-standard extensions will be autodetected for source and precompiled script types\. Note, for Harbour scripts, the codepage is set to UTF\-8 by default\. The default core header 'hb\.ch' is automatically \#included at the interactive shell prompt\. The default date format is the ISO standard: yyyy\-mm\-dd\. The default GT is 'gtcgi', unless full\-screen CUI calls are detected, when 'gtwin' \[\*\] is automatically selected \(except for INIT PROCEDUREs\)\.
  - Μπορείτε να χρησιμοποιείτε το συνδυασμό πλήκτρων &lt;Alt\+V&gt; στο διαδραστικό κέλυφος του Harbour για επικόλληση από το πρόχειρο\.
  - Τιμές με αστερίσκο \[\*\] μπορεί να εξαρτώνται από την πλατφόρμα υποδοχής ή/και τη διαμόρφωση\. Η παρούσα βοήθεια δημιουργήθηκε στην 'win' πλατφόρμα υποδοχής\.
  
Αδεια:  


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
their web site at https://www\.gnu\.org/\)\.  
  
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

  
Συγγραφέας:  


 - Viktor Szakáts \(vszakats\.net/harbour\) 
