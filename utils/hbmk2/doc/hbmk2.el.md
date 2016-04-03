Harbour Make \(hbmk2\) 3\.4\.0dev \(9dea61d\) \(2016\-03\-09 22:28\)  
Copyright &copy; 1999\-2016, Viktor Szakáts  
<https://github\.com/vszakats/harbour\-core/>  
Μετάφραση \(el\): Pete D\. \(pete\_westg@yahoo\.gr\)  

Σύνταξη:  
  
  hbmk2 \[options\] \[&lt;script\[s\]&gt;\] &lt;src\[s\]\[\.prg|\.hbc|\.c|\.obj|\.o|\.rc|\.res|\.def|\.po|\.pot|\.hbl|@\.clp|\.d|\.ch\]&gt;  
  
Περιγραφή:  


  hbmk2 is an integrated and portable build tool, making it possible to create various types of executable binaries \(executable, dynamic library, static library, Harbour portable binary\) out of multiple types of source files \(C, C\+\+, Objective\-C, Harbour, gettext translations, Windows resources\)\. 'Integrated' means that a single hbmk2 project file can control all or most aspects of the build process\. 'Portable' means that a single hbmk2 project file can control the build on all supported OS platforms and across all supported C compilers\. It also aims to cover the majority of build tasks via short and simple project files \(options\)\. hbmk2 supports pure \-non\-Harbour\- C/C\+\+/Objective\-C projects as well\. In order to achieve above goals, hbmk2 will auto\-detect Harbour, C compiler and other required tools, then configure and call them appropriately\. hbmk2 allows to extend the types of supported source files via plugins\.  
Besides building executables, hbmk2 is able to run Harbour scripts \(both source and precompiled\) directly, and it also features an interactive shell prompt\.
  
Επιλογές:  


 - **\-o&lt;outname&gt;** όνομα αρχείου εξόδου
 - **\-l&lt;libname&gt;** διασύνδεση της &lt;libname&gt; βιβλιοθήκης\. Το &lt;libname&gt; πρέπει να είναι χωρίς μονοπάτι, κατάληξη και πρόθεμα 'lib' \(εκτός αν είναι μέρος του ονόματος\)\. Μη προσθέτετε τις βασικές βιβλιοθήκες Harbour, αυτές προστίθενται αυτόματα εφόσον χρειάζονται\. Αν το &lt;libname&gt; αρχίζει με ένα χαρακτήρα '\-', η βιβλιοθήκη θα αφαιρεθεί από τη λίστα βιβλιοθηκών τη στιγμή της διασύνδεσης\.
 - **\-L&lt;libpath&gt;** πρόσθετο μονοπάτι για αναζήτηση βιβλιοθηκών
 - **\-i&lt;p&gt;|\-incpath=&lt;p&gt;** πρόσθετο μονοπάτι για αναζήτηση headers
 - **\-static|\-shared** διασύνδεση με static/shared βιβλιοθήκες
 - **\-gt&lt;name&gt;** διασύνδεση με τον GT&lt;name&gt; GT οδηγό, μπορεί να επαναληφθεί για να διασυνδεθούν περισσότεροι GTs οδηγοί\. Ο πρώτος από αυτούς θα είναι προεπιλεγμένος κατα τον χρόνο\-εκτέλεσης
 - **\-inc\[\-\]** ενεργοποίηση/απενεργοποίηση incremental μεθόδου κατασκευής \(προεπιλογή: απενεργοποίηση\)
 - **\-hbexe** δημιουργία εκτελέσιμου \(προεπιλογή\)
 - **\-hblib** δημιουργία στατικής βιβλιοθήκης
 - **\-hbdyn** δημιουργία δυναμικής βιβλιοθήκης \(χωρίς συνδεδεμένη Harbour VM\)
 - **\-hbdynvm** δημιουργία δυναμικής βιβλιοθήκης \(με συνδεδεμένη Harbour VM\)
 - **\-strip\[\-\]** strip \(or don't\) debugging \(and other extra\) information from target binary\. They are included by default by certain C compilers, f\.e\.: gcc\*, clang, mingw\*, djgpp\.


 - **\-mt|\-st** διασύνδεση με την πολυ/μονο\-νηματική εικονική μηχανή Harbour VM
 - **\-gui|\-std|\-cli** δημιουργία GUI/console/command\-line εκτελέσιμου
 - **\-main=&lt;mainfunc&gt;** υπερκάλυψη του ονόματος της εναρκτήριας συνάρτησης
 - **\-request=&lt;func&gt;** εξαναγκασμός συνάρτησης/λειτουργίας να διασυνδεθεί
 - **\-fullstatic** διασύνδεση με όλες τις static βιβλιοθήκες
 - **\-pic\[\-\]** δημιουργία position independent αντικειμενικού κώδικα \(πάντα ενεροπιημένο στα μοντέλα \-hbdyn/\-hbdynvm\)
 - **\-\[full|fix\]shared** δημιούργησε κοινόχρηστα Harbour binaries χωρίς/με απόλυτη αναφορά καταλόγου στη βιβλιοθήκη Harbour \(προεπιλογή: ''fullshared'' οταν το Harbour είναι εγκατεστημένο σε περιοχή συστήματος, αλλοιώς 'fixshared'\) \(επιλογή fix/full μόνο σε \*nix\)
 - **\-nulrdd\[\-\]** διασύνδεση με nulrdd
 - **\-debug\[\-\]** προσθήκη/παράλειψη πληροφοριών αποσφλαμάτωσης μεταγλωττιστή C\. Για αποσφαλμάτωση σε επίπεδο Harbour, χρησιμοποιείστε την συνηθισμένη επιλογή \-b του Harbour
 - **\-optim\[\-\]** εναλλαγή βελτιστοποιήσεων μεταγλωττιστή C \(προεπιλογή: on\)
 - **\-cpp\[\-\]** εξαναγκασμός C\+\+/C μοντέλου
 - **\-cpp=&lt;value&gt;** κατάσταση λειτουργίας C\+\+ \. Επιτρεπτές τιμές είναι: def, yes, no
 - **\-c=&lt;value&gt;** select C standard\. Allowed values are: iso90, iso99, iso11, gnu90, gnu99, gnu11
 - **\-cpp=&lt;value&gt;** select C\+\+ mode or standard\. Allowed values are: def, yes, no, iso98, iso11, iso14, gnu98, gnu11, gnu14
 - **\-map\[\-\]** δημιουργία \(ή όχι\) ενός map αρχείου
 - **\-implib\[\-\]** create \(or not\) an import library \(in \-hbdyn/\-hbexe mode\)\. The name will have a suffix added\.
 - **\-implib=&lt;output&gt;** δημιουργία ονόματος βιβλιοθήκης εισαγωγής \(in \-hbdyn/\-hbexe mode\) στο &lt;output&gt; \(Προεπιλογή: ίδιο με το output\)
 - **\-ln=&lt;link&gt;** δημιουργία συμβολικού δεσμού που δείχνει στο &lt;output&gt; \(το &lt;link&gt; θεωρείται σχετικό με το &lt;output&gt;\)
 - **\-trace\[\-\]** εμφάνιση εντολών που εκτελούνται
 - **\-beep\[\-\]** ενεργοποίηση \(ή απενεργοποίηση\) ενός μπιπ στην επιτυχή ολοκληρωση, διπλού μπιπ στην αποτυχία
 - **\-ignore\[\-\]** αγνόηση λαθών όταν εκτελούνται εργαλεία του μεταγλωττιστή \(προεπιλογή: off\)
 - **\-hbcppmm\[\-\]** αντικατάσταση των πρότυπων C\+\+ συναρτήσεων διαχείρησης μνήμης με εκείνες του Harbour
 - **\-winuni\[\-\]** select between UNICODE \(WIDE\) and ANSI Windows API usage for C/C\+\+ input files \(default: ANSI\) \(Windows only\. For WinCE it is always set to UNICODE\)
 - **\-nohblib\[\-\]** μη χρήση στατικών βιβλιοθηκών πυρήνα Harbour κατά τη διασύνδεση
 - **\-nodefgt\[\-\]** μη σύνδεση προεπιλεγμένων GTs \(ισχύει σε \-static μορφή κατασκευής\)
 - **\-nolibgrouping\[\-\]** απενεργοποίηση ομαδοποίησης βιβλιοθηκών σε gcc μεταγλωττιστές
 - **\-nomiscsyslib\[\-\]** μη προσθέτετε επιπλέον λιστα βιβλιοθηκών συστήματος στην προεπιλεγμένη λιστα βιβλιοθηκών
 - **\-traceonly** εμφάνιση των προς εκτέλεση εντολών, αλλά χωρίς να εκτελεστούν
 - **\-warn=&lt;level&gt;** ορισμός επίπεδου ειδοποιήσεων του μεταγλωττιστή C   
το &lt;lev&gt; μπορεί να είναι: max, yes, low, no, def \(προεπιλογή: yes\)
 - **\-harden\[\-\]** ενεργοποίηση προστατευτικών επιλογών στο μεταγλωττιστή/συνδέτη C \(προεπιλογή: ενεργοποιημένο σε Windows, απενεργοποιημένο σε άλλα συστήματα\)
 - **\-vcsts\[\-\]** set timestamp of output file\(s\) to the last repository commit \(Supported with: Git\)
 - **\-compr=&lt;level&gt;** συμπίεση εκτελέσιμου/δυναμικής βιβλιοθήκης \(απαιτεί το εργαλείο UPX\)  
το &lt;level&gt; μπορεί να είναι: yes, no, min, high, max
 - **\-run\[\-\]** εκτέλεση/ή όχι, του εκτελέσιμου που θα δημιουργηθεί
 - **\-vcshead=&lt;file&gt;** generate \.ch header file with local repository information\. Git, SVN, Mercurial, Bazaar, Fossil, CVS and Monotone are currently supported\. Generated header will define preprocessor constant \_HBMK\_VCS\_TYPE\_ with the name of detected VCS and \_HBMK\_VCS\_ID\_ with the unique ID of local repository\. VCS specific information is added as \_HBMK\_VCS\_&lt;TYPE&gt;\_\*\_ constants, where supported\. If no VCS system is detected, a sequential number will be rolled automatically on each build\.
 - **\-bldhead=&lt;file&gt;** generate \.ch header file with build information, like build sequence number and timestamp\. Generated header will define preprocessor constants \_HBMK\_BUILD\_ID\_ and \_HBMK\_BUILD\_ID\_NUM\_ with sequence number \(incremented on each build\), \_HBMK\_BUILD\_DATE\_, \_HBMK\_BUILD\_TIME\_, \_HBMK\_BUILD\_TIMESTAMP\_ with the date/time of build and \_HBMK\_BUILD\_RANDSTR\_32\_ with a random string of 32 bytes in hexadecimal format
 - **\-haltrev\[\-\]** do not increase revision numbers in \-bldhead= \(\_HBMK\_BUILD\_ID\_\) and \-vcshead= \(\_HBMK\_VCS\_ID\_\) options \(default: do increase\)
 - **\-icon=&lt;file&gt;** ορίζει το &lt;file&gt; ως εικονίδιο της εφαρμογής\. Το &lt;file&gt; πρέπει να είναι σε μορφή υποστηριζόμενη από τη πλατφόρμα\-στόχο \(δεν υποστηρίζεται από μερικές πλατφόρμες/μεταγλωττιστές\)\. Σε Windows, υλοποιείται με τη δημιουργία και διασύνδεση ενός resource αρχείου\.
 - **\-manifest=&lt;file&gt;** ενσωμάτωση του μανιφέστου &lt;file&gt; στο εκτελέσιμο/δυναμική βιβλ\. \(μονο σε Windows\)
 - **\-sign=&lt;key&gt;** sign executable with &lt;key&gt; \(Windows and Darwin only\)\. On Windows signtool\.exe is used \(part of MS Windows SDK\) or posign\.exe \(part of Pelles C 7\), in that order, both auto\-detected\.
 - **\-signpw=&lt;pw&gt;** χρήση του &lt;pw&gt; ως συνθηματικού κατά την σήμανση του εκτελέσιμου \(μόνο σε Windows και Darwin\)
 - **\-signts=&lt;\[std:\]url&gt;** use &lt;url&gt; as trusted timestamp server\. Optional &lt;std&gt; might specify the standard as 'rfc3161' or 'authenticode' \(without quotes\)\. The default is 'rfc3161'\. Empty value resets it to the default: http://timestamp\.globalsign\.com/scripts/timstamp\.dll
 - **\-instfile=&lt;g:file&gt;** προσθήκη &lt;file&gt; iστη λίστα αρχείων προς αντιγραφή στο μονοπάτι που ορίστηκε από την επιλογή \-instpath\. &lt;g&gt; είναι μια προαιρετική ομάδα αντιγραφής \(πεζά/κεφαλαία ευαίσθητο\), πρέπει να έχει μήκος τουλάχιστον δύο χαρακτήρων\. Σε περίπτωση μη καθορισμού του &lt;file&gt;, η λίστα αρχείων σε αυτή την ομάδα θα εκκενωθεί\.
 - **\-instpath=&lt;g:path&gt;** αντιγραφή των αρχείου\(ων\) στο &lt;path&gt;\. Αν το &lt;path&gt; είναι κατάλογος, πρέπει να τελειώνει με τον διαχωριστή μονοπατιού· σε αυτή την περίπτωση τα αρχεία που καθορίστηκαν με την επιλογή \-instfile θα αντιγραφούν επίσης· μπορεί να οριστεί πολλαπλές φορές\. &lt;g&gt; είναι μια προαιρετική ομάδα αντιγραφής, πρέπει να έχει μήκος τουλάχιστον δύο χαρακτήρων\. Το κατασκευασμένο έργο θα αντιγραφεί αυτόματα σε προεπιλεγμένη \(άδεια\) ομάδα αντιγραφής\. Υπάρχουν οι ακόλουθες ενσωματωμένες &lt;g&gt; ομάδες: 'depimplib' για τις βιβλιοθήκες εισαγωγής και 'depimplibsrc' για πηγαία \(\.dll\) αρχεία βιβλιοθηκών εισαγωγής, αμφότερες ανήκουσες σε εξαρτήσεις\.
 - **\-instforce\[\-\]** αντιγραφή του κατασκευασμένου αρχείου\(ων\) του έργου στο κατάλογο εγκατάστασης έστω κι αν είναι ενήμερο /up to date/
 - **\-depimplib\[\-\]** ενεργοποίηση \(ή απενεργοποίηση\) δημιουργίας βιβλιοθήκης εισαγωγής για πηγαίες βιβλιοθήκες εισαγωγής που ορίστηκαν με τις επιλογές \-depimplibs= \(προεπιλογή: yes\)
 - **\-stop\[=&lt;text&gt;\]** Τερματισμός χωρίς να κάνει οτιδήποτε και εμφάνιση του κειμένου &lt;text&gt; άν καθορίστηκε
 - **\-echo=&lt;text&gt;** αντήχηση \-echo\- κειμένου στην οθόνη
 - **\-skip** skip processing the rest of the project file \(filters not supported\)
 - **\-pause** υποχρεωση αναμονής για πάτημα πλήκτρου κατά την έξοδο σε περίπτωση αποτυχιας \(με εναλλακτικά GTs μόνο\)
 - **\-exitstr** εμφάνιση λαθών σε ανθρώπινη μορφή ανάγνωσης
 - **\-info** εμφάνιση των πληροφοριακών μηνυμάτων
 - **\-quiet\[\-\]** απόκρυψη όλων των μηνυμάτων οθόνης


 - **\-bldf\[\-\]** κληρονόμηση όλων/όχι \(προεπιλογή\) των σημαιών απο την κατασκευή Harbour
 - **\-bldf=\[p\]\[c\]\[l\]** κληρονόμηση των σημαιών \.prg/\.c/linker \(ή καμμία\) απο την κατασκευή Harbour
 - **\-F&lt;framework&gt;** διασύνδεση με το framework &lt;framework&gt; \(μόνο σε Darwin\)
 - **\-prgflag=&lt;f&gt;** απλό πέρασμα flag στο μεταλωττιστή Harbour
 - **\-cflag=&lt;f&gt;** πέρασμα μιας σημαίας στο μεταλωττιστή C
 - **\-resflag=&lt;f&gt;** πέρασμα μιας σημαίας στο μεταλωττιστή resource \(μόνο σε Windows\)
 - **\-ldflag=&lt;f&gt;** απλό πέρασμα σημαίας \-flag\- στο συνδέτη \-linker\- \(εκτελέσιμο\)
 - **\-dflag=&lt;f&gt;** πέρασμα μιας σημαίας στον συνδέτη \(dynamic library\)
 - **\-aflag=&lt;f&gt;** πέρασμα μιας σημαίας στον συνδέτη \(static library\)
 - **\-iflag=&lt;f&gt;** πέρασμα μιάς σημαίας στην εντολή δημιουργίας βιβλιοθήκης εισαγωγής
 - **\-signflag=&lt;f&gt;** περασμα απλής σημαίας στην εντολή σήμανσης κώδικα
 - **\-runflag=&lt;f&gt;** πέρασμα μίας σημαίας \-flag\- στο εκτελέσιμο εξόδου όταν έχει γίνει χρήση της επιλογής \-run
 - **\-cflag\+=&lt;f&gt;** πέρασμα μιάς σημαίας \-flag\- στον μεταγλωττιστή C υπερκαλύπτοντας αντίστοιχες σημαίες C που προστέθηκαν απο το ίδιο το hbmk2 itself\. Χρησιμοποιείστε το με προσοχή\.
 - **\-ldflag\+=&lt;f&gt;** πέρασμα, χωρίς έλεγχο, μιας επιλογής στο συνδέτη/linker \(εκτελέσιμο\) μετά τη λίστα βιβλιοθηκών\. Χρησιμοποιήστε το με προσοχή\!
 - **\-dflag\+=&lt;f&gt;** πέρασμα, χωρίς έλεγχο, μιάς επιλογής στο συνδέτη/linker \(δυναμική βιβλιοθήκη\) μετά τη λίστα βιβλιοθηκών\. Χρησιμοποιήστε το με προσοχή\!
 - **\-3rd=&lt;f&gt;** επιλογές/σημαίες εφεδρικές για 3rd party εργαλεία, πάντοτε αγνοούνται από το ίδιο το hbmk2
 - **\-env:&lt;e&gt;\[&lt;o&gt;\[&lt;v&gt;\]\]** τροποποίηση τοπικού περιβάλλοντος\. &lt;e&gt; είναι το όνομα μεταβλητής περιβάλλοντος για αλλαγή\. &lt;o&gt; μπορεί να είναι '=' για ορισμό/επικάλυψη, '\-' για διαγραφή, '\+' για προσθήκη στο τέλος της υπάρχουσας τιμής, '\#' για εισαγωγή στην αρχή της υπάρχουσας τιμής\. &lt;v&gt; είναι η τιμή που θα οριστεί/προστεθεί/εισαχθεί\.
 - **\-jobs=&lt;n&gt;** start n compilation threads \(multiprocess platforms only\) \(default: number of processors available or 1 if not detectable/applicable; on this system: 2\)
 - **\-head=&lt;m&gt;** έλεγχος της ανάλυσης του πηγαίου header \(σε αυξητικό \-incremental\- ρυθμό κατασκευής\)  
&lt;m&gt; μπορεί να είναι: native \(χρήση του μεταγλωττιστή για εξαγωγή των εξαρτήσεων\), full \(προεπιλογή, χρήση απλού αναλυτή κειμένου σε όλο το αρχείο\), dep, off
 - **\-rebuild** ανακατασκευή \(σε incremental ρυθμό\)
 - **\-rebuildall** ανακατασκευή μαζί με τα υπο\-έργα \(με αυξητική \-incremental\- μέθοδο κατασκευής\)
 - **\-clean** καθαρισμός \(σε incremental ρυθμό\)
 - **\-workdir=&lt;dir&gt;** κατάλογος εργασίας  
\(προεπιλογή: \.hbmk/&lt;platform&gt;/&lt;compiler&gt; \[\*\] σε incremental μέθοδο κατασκευής, αλλιώς ο temp κατάλογος του Λ\.Σ\.\)


 - **\-hbcontainer** εικονική κατασκευή έργου· δεν δημιουργεί τίποτα\. Χρήσιμο για την δημιουργία ενός \.hbp με μοναδικό σκοπό την μνημόνευση των υπο\-έργων
 - **\-hbimplib** Δημιουργία βιβλιοθήκης εισαγωγής \(μόνο σε Windows\)


 - **\-hbl\[=&lt;output&gt;\]** έξοδος \.hbl αρχειοονόματος\. %\{hb\_lng\} macro είναι δεκτή στο αρχειοόναμα
 - **\-lng=&lt;languages&gt;** λίστα των γλωσσών προς αντικατάσταση σε %\{hb\_lng\} macros σε \.pot/\.po filenames and output \.hbl/\.po filenames\. Λίστα διαχωριζόμενη με κόμμα:  
\-lng=en,hu\-HU,de
 - **\-po=&lt;output&gt;** δημιουργία/ενημέρωση αρχείου \.po από το πηγαίο\. Συγχώνευσή του με προηγούμενο αρχείο \.po ίδιου ονόματος
 - **\-minipo\[\-\]** do \(not\) add source file reference to \.po \(default: add them\)
 - **\-rebuildpo** αναδημιουργία \.po αρχείου, απομακρύνοντας έτσι όλες τις παρωχημένες καταχωρίσεις


 - **\-hbx=&lt;n\[\.hbx&gt;\]&gt;** δημιουργία Harbour header \(σε μορφή \.hbx\) με όλα τα εξωτερικά symbols\. Κενή παράμετρος θα απενεργοποιήσει την δημιουργία\. Η προκαθορισμένη κατάληξη είναι \.hbx\. Αν οριστεί, &lt;n&gt; θα προστεθεί αυτόματα στη λίστα των Harbour αρχείων εισόδου και θα 'κτισθεί' εντός του έργου\. Ως εκ τούτου, το τμήμα 'όνομα' του &lt;n&gt; δεν πρέπει να είναι ίδιο με κανένα άλλο όνομα αρχείου εισόδου που ενυπάρχει στο έργο\.
 - **\-hbx\[\-\]** update \(or don't\) \.hbx file specified in \-hbx= option \(default: update\)
 - **\-autohbc=&lt;\.ch:\.hbc&gt;** &lt;\.ch&gt; είναι ένα όνομα αρχείου header\. &lt;\.hbc&gt; είναι ένα \.hbc όνομα αρχείου που θα συμπεριληφθεί αυτομάτως σε περίπτωση που το header βρίσκεται σε κάποιο απο τα μεταγλωττισμένα πηγαία αρχεία\. \(ΠΕΙΡΑΜΑΤΙΚΟ\)


 - **\-depurlbase=&lt;d:u&gt;** &lt;d&gt; είναι το όνομα της εξάρτησης\. &lt;u&gt; είναι το URL του έργου\. Μπορεί να οριστεί πολλαπλές φορές\.
 - **\-deppkgname=&lt;d:n&gt;** &lt;d&gt; είναι το όνομα της εξάρτησης\. &lt;n&gt; είναι το όνομα του πακέτου εξάρτησης\. Μπορεί να οριστεί πολλαπλές φορές\.
 - **\-depkeyhead=&lt;d:h&gt;** &lt;d&gt; είναι το όνομα της εξάρτησης\. &lt;h&gt; είναι το header\-κλειδί \(\.h\) της εξάρτησης πακέτου\. Μπορούν να οριστούν πολλαπλά εναλλακτικά headers \.
 - **\-depoptional=&lt;d:f&gt;** &lt;d&gt; είναι το όνομα της εξάρτησης\. το &lt;f&gt; μπορεί να είναι 'yes' ή 'no', καθορίζει το άν η εξάρτηση είναι προαιρετική\. Προεπιλογή: no
 - **\-depcontrol=&lt;d:v&gt;** &lt;d&gt; είναι το όνομα της εξάρτησης\. &lt;v&gt; είναι η τιμή που ελέγχει πως γίνεται η ανίχνευση\. Αποδεκτές τιμές: no, yes, force, nolocal, local\. Προεπιλογή: το περιεχόμενο της μεταβλητής περιβάλλοντος HBMK\_WITH\_&lt;d&gt;
 - **\-depincroot=&lt;d:r&gt;** &lt;d&gt; είναι το όνομα της εξάρτησης\. Ορίστε το &lt;r&gt; ως ριζικό κατάλογο για μονοπάτια που ορίζονται στις \-depincpath επιλογές\.
 - **\-depincpath=&lt;d:i&gt;** &lt;d&gt; είναι το όνομα της εξάρτησης\. Προσθέστε &lt;i&gt; στη λίστα μονοπατιών ανίχνευσης header
 - **\-depincpathlocal=&lt;d:i&gt;** &lt;d&gt; είναι το όνομα της εξάρτησης\. Προσθέστε &lt;i&gt; στη λιστα μονοπατιών ανίχνευσης header, όπου &lt;i&gt; δείχνει σε ένα κατάλογο τοπικό προς το έργο και περιέχει μια ενσωματωμένη \(aka\. 'locally hosted'\) εξάρτηση\.
 - **\-depimplibs=&lt;d:dll\[:lib\]&gt;** &lt;d&gt; is the name of the dependency\. Add &lt;dll&gt; to the import library source list\. Optionally override the name of the generated implib to become &lt;lib&gt;\.
 - **\-depimplibd=&lt;d:lib&gt;** &lt;d&gt; είναι το όνομα της εξάρτησης\. Ορίστε το όνομα της δημιουργούμενης βιβλιοθήκης εισαγωγής σε &lt;lib&gt;
 - **\-depfinish=&lt;d&gt;** &lt;d&gt; είναι το όνομα της εξάρτησης\. Κλείνει τον ορισμό της εξάρτησης και επιτελεί την πραγματική ανίχνευση της εξάρτησης, ορίζοντας ανάλογα όλες τις προκαθορισμένες macro μεταβλητές φίλτρου και τις κατασκευαστικές επιλογές\. Προαιρετικό, αν παραληφθεί, η ανίχνευση θα λάβει χώρα μετά την επεξεργασία όλων των επιλογών\.


 - **\-plugin=&lt;filename&gt;** προσθήκη plugin\. το &lt;filename&gt; μπορεί να είναι: \.hb, \.prg, \.hrb
 - **\-pi=&lt;filename&gt;** πέρασμα του αρχείου εισόδου στα plugins
 - **\-pflag=&lt;f&gt;** απλό πέρασμα σημαίας \-flag\- στα plugins
  
Οι παρακάτω επιλογές είναι διαθέσιμες στη γραμμή\-εντολών:  


 - **\-target=&lt;script&gt;** καθορίζει ένα νέο έργο προς κατασκευή\. το &lt;script&gt; μπορεί να είναι \.prg \(ή καμμία κατάληξη\) ή \.hbp αρχείο\. Σημειώστε ότι τα αρχεία \.hbp θεωρούνται αυτόματα ως ξεχωριστές κατασκευές\.


 - **\-hbrun** εκτέλεση του κατασκευασμένου έργου
 - **\-hbraw** διακοπή μετά το τρέξιμο του μεταγλωττιστή
 - **\-hbcmp|\-clipper** τερματισμός μετά την δημιουργία των object αρχείων  
δημιουργείστε δεσμό/αντιγράψτε το hbmk2 σε hbcmp/clipper για να επιτύχετε το αυτό αποτέλεσμα
 - **\-hbcc** αποδοχή σημαιών C χωρίς έλεγχο  
δημιουργείστε σύνδεσμο/αντιγράψτε το hbmk2 σε hbcc για να επιτύχετε όμοια λειτουργικότητα
 - **\-hblnk** αποδοχή ανεπεξέργαστων flags συνδέτη
 - **\-autohbm\[\-\]** ενεργοποίηση \(ή απενεργοποίηση\) επεξεργασίας του hbmk\.hbm στο τρέχοντα κατάλογο \(προεπιλογή: yes\)
 - **\-hb10** ενεργοποίηση συμβατότητας Harbour 1\.0\.x
 - **\-hb20** ενεργοποίηση συμβατότητας Harbour 2\.0\.x
 - **\-hb30** ενργοποίηση συμβατότητας Harbour 3\.0\.x
 - **\-hb32** enable Harbour 3\.2\.0dev compatibility mode
 - **\-xhb** ενεργοποίηση xhb mode
 - **\-hbc** ενεργοποίηση καθαρού C mode
 - **\-blinker** emulate Cl\*pper compatible linker behavior  
create link/copy hbmk2 to rtlink/blinker/exospace for the same effect
 - **\-exospace** see above
 - **\-rtlink** see above


 - **\-hbreg\[=global\]** καταχώριση συσχέτισης σεναρίου Harbour \(\.hb\) με το hbmk2 \(μόνο σε Windows \-registry\-\)
 - **\-hbunreg\[=global\]** κατάργηση καταχώρισης συσχέτισης σεναρίου Harbour \(\.hb\) με το hbmk2 \(μόνο σε Windows \-registry\-\)


 - **\-find &lt;text&gt;** list all known Harbour functions that contain &lt;text&gt; in their name, along with their package \(case insensitive, accepts multiple values, can contain wildcard characters\)
 - **\-doc &lt;text&gt;** show documentation for function\[s\]/command\[s\] in &lt;text&gt;
 - **\-docjson &lt;text&gt;** output documentation in JSON format for function\[s\]/command\[s\] in &lt;text&gt;
 - **\-fixcase &lt;file\[s\]&gt;** fix casing of Harbour function names to their 'official' format\. Core functions and functions belonging to all active contribs/addons with an \.hbx file will be processed\.
 - **\-sanitize &lt;file\[s\]&gt;** convert filenames to lowercase, EOLs to platform native and remove EOF character, if present\.


 - **\-hbmake=&lt;file&gt;** μετατροπή έργου hbmake &lt;file&gt; σε αρχείο \.hbp
 - **\-xbp=&lt;file&gt;** μετατροπή ενός \.xbp \(xbuild\) &lt;file&gt; έργου σε \.hbp αρχείο
 - **\-xhp=&lt;file&gt;** μετατροπή έργου \.xhp \(xMate\) &lt;file&gt; σε αρχείο \.hbp


 - **\-\-hbdirbin** εξάγει τον κατάλογο /path/ των εκτελέσιμων Harbour στη stdout
 - **\-\-hbdirdyn** εξάγει τον κατάλογο /path/ των δυναμικών βιβλιοθηκών Harbour στη stdout
 - **\-\-hbdirlib** εξάγει τον κατάλογο /path/ των στατικών βιβλιοθηκών Harbour στη stdout
 - **\-\-hbdirinc** εξάγει τον κατάλογο /path/ των αρχείων Harbour στη stdout
 - **\-\-hbinfo\[=nested\]** Εξαγωγή πληροφορίες κατασκευής του Harbour στην stdout\. Το αποτέλεσμα είναι σε μορφή JSON\. Τα περιλαμβανόμενα μονοπάτια πάντοτε περιέχουν κάθετες εμπρός παύλες\. Κάθε JSON τμήμα ακολουθείται από ένα 0x0A χαρακτήρα\.


 - **\-plat=&lt;platform&gt;** επικάλυψη προεπιλεγμένης πλατφόρμας\-έργου \(προεπιλογή: αυτομάτως\)
 - **\-cpu=&lt;cpu&gt;** υπερισχύει έναντι της προεπιλεγμένης CPU\-στόχου \(προεπιλογή: αυτομάτως\) \(ΠΕΙΡΑΜΑΤΙΚΟ\)
 - **\-comp=&lt;compiler&gt;** override C compiler auto\-detection  
Special value:  
 \- bld: use original build settings \(default on \*nix\)
 - **\-build=&lt;name&gt;** ορισμός ονόματος κατασκευής
 - **\-lang=&lt;lang&gt;** υπερισχύει έναντι της προεπιλεγμένης γλώσσας\. Το &lt;lang&gt; είναι ένας κατά ISO κωδικός γλώσσας\.
 - **\-width=&lt;n&gt;** ορισμός του πλάτους εξόδου σε &lt;n&gt; χαρακτήρες \(0=απεριόριστο\)\.
 - **\-shl** εμφάνιση επιπέδου του υπο\-έργου στις γραμμές\-εξόδου
 - **\-viewhelp** full help in text viewer
 - **\-fullhelp** full help
 - **\-fullhelpmd** full help in [Markdown](https://daringfireball.net/projects/markdown/) format
 - **\-harbourhelp** Βοηθεια μεταγλωττιστή Harbour \(όλες οι επιλογές μεταγλωττιστή Harbour γίνονται δεκτές ως έχουν υπό hbmk2\)
 - **\-credits** Διαπιστευτήρια του μεταγλωτιστή Harbour
 - **\-build** Πληροφορία κατασκευής μεταγλωτιστή Harbour
 - **\-version** εμφάνιση κεφαλίδας έκδοσης μόνο
  
Οι παρακάτω επιλογές είναι εσωτερικές/developer \(η συμβατότητα δεν είναι εγγυημένη\)  


 - **\-debugtime** μέτρηση χρόνου κατασκευής
 - **\-debuginc** εμφάνιση εσωτερικών στοιχείων της incremental κατασκευής
 - **\-debugstub** εμφάνιση περιεχομένων όλων των εσωτερικά δημιουργημένων πηγαίων αρχείων
 - **\-debugi18n** εμφάνιση εσωτερικών στοιχείων της δημιουργίας αρχείου μετάφρασης
 - **\-debugdepd** εμφάνιση εσωτερικών στοιχείων της ανίχνευσης εξάρτησης
 - **\-debugpars** εμφάνιση όλων των παραμέτρων εισόδου με τη σειρά που επεξεργάζονται
 - **\-debugrte** παράγει ένα λάθος χρόνου\-εκτέλεσης /run\-time error/


Μορείτε να συνδέσετε/αντιγράψετε/μετονομάσετε το hbmk2 στα ακόλουθα ονόματα για να αλλάξετε την προεπιλεγμένη λειτουργία του:


 - **hbrun\*|\*hbrun** κατάσταση λειτουργίας εκτέλεσης σεναρίων / διαδραστικό κέλυφος
 - **hbrund|hbrun\*d** κατάσταση λειτουργίας εκτέλεσης σεναρίων / διαδραστικό κέλυφος με λειτουργία αποσφαλμάτωσης
 - **harbour** μοντέλο \-hbraw \(προσομοίωση \- raw \- του μεταγλωττιστή Harbour\)
 - **clipper** mode \-hbcmp \(emulate Cl\*pper compiler\)
 - **rtlink** mode \-rtlink \(emulate Cl\*pper linker\)
 - **exospace** mode \-rtlink \(emulate Cl\*pper linker\)
 - **blinker** mode \-rtlink \(emulate Cl\*pper linker\)
 - **\*10** επιλογή \-hb10
 - **\*20** επιλογή \-hb20
 - **\*30** επιλογή \-hb30
 - **\*32** επιλογή \-hb32
 - **x\*** επιλογή \-xhb
 - **hbcmp\*|\*hbcmp** κατάσταση λειτουργίας \-hbcmp \(προσομοίωση μεταγλώτισης Harbour που δημιουργεί δυαδικό object\)
 - **hbcc\*|\*hbcc** μοντέλο \-hbcc \(προσομοίωση μεταγλωττιστή C\)
 - **hblnk\*|\*hblnk** μοντέλο \-hblnk \(προσομοίωση C συνδέτη\)
 - **hbexe\*|\*hbexe** μέθοδος \-hbexe
 - **hblib\*|\*hblib** μέθοδος \-hblib
 - **hbdyn\*|\*hbdyn** μέθοδος \-hbdyn
  
Αρχεία:  


 - **\*\.hbp** αρχείο έργου\. Μπορεί να περιέχει οποιοδήποτε αριθμό επιλογών γραμμής\-εντολών, οι οποίες αναμένεται να αποφέρουν ένα αποτέλεσμα, δηλ\. δημιουργία μιας εφαρμογής\. Γραμμές που αρχίζουν με το χαρακτήρα '\#' αγνοούνται, κατα τ'άλλα, η δημιουργία ξεχωριστών γραμμών είναι προαιρετική και οι επιλογές μπαίνουν διαχωρισμένες με κενό διάστημα, ακριβώς όπως στη γραμμή\-εντολών\. Επιλογή που περιέχει κενό διάστημα πρέπει να εγκλειστεί σε "εισαγωγικα"\. Κάθε αναφορά αρχείου \.hbp θα εκτελεστεί σαν υπο\-έργο\.
 - **\*\.hbm** συλλογή επιλογών\. Μπορεί να χρησιμοποιηθεί για συγκέντρωση κοινών επιλογών μέσα σε ένα αρχείο το οποίο θα συμπεριληφθεί μέσα σε αρχεία έργου\. Χρησιμοποιεί μορφή ίδια με τα \.hbp αρχεία\.
 - **\*\.hbc** συλλογή από επιλογές που συνοδεύουν συστατικά στοιχεία \(γνωστά και ως 'βιβλιοθήκες', γνωστά και ως πακέτα\)\. γίνεται χρήση διαφορετικής σύνταξης απο αυτήν της γραμμής\-εντολών και των αρχείων \.hbp/\.hbm\. Γραμμές που αρχίζουν με το χαρακτήρα '\#' αγνοούνται, κάθε ντιρεκτίβα πρέπει να τοποθετείται σε ξεχωριστή γραμμή\.
 - **\*\.ch** αν περαστεί κατευθείαν σαν πηγαίο αρχείο, θα χρησιμοποιηθεί σαν επιπλέον πρότυπο header
 - **hbmk\.hbc** standard αρχείο \.hbc που δέχεται αυτόματη επεξεργασία, αν είναι παρόν\. Πιθανή\(ές\) τοποθεσίες \(με σειρά προτεραιότητας\) \[\*\]: $HOME/\.harbour, /etc/harbour, &lt;hbmk2 κατάλογος&gt;/\.\./etc/harbour, &lt;hbmk2 κατάλογος&gt;/\.\./etc, &lt;hbmk2 κατάλογος&gt;
 - **hbmk\.hbm** προαιρετικό \.hbm αρχείο μέσα στο τρέχοντα κατάλογο εργασίας, που δέχεται επεξεργασία αυτομάτως, πρίν από τυχόν άλλες επιλογές
 - **$hb\_pkg\_dynlib\.hbm** special \.hbm file built\-in inside hbmk2\. It manages the details of creating a dynamic library \(in the style of Harbour contribs\)\.
 - **$hb\_pkg\_install\.hbm** special \.hbm file built\-in inside hbmk2\. It manages the details of installing build targets and related package files to standard locations \(in the style of Harbour contribs\)\.


 - **\*\.hb** Σενάριο Harbour
 - **\*\.hrb** Μεταφέρσιμο δυαδικό Harbour \(γνωστό και ως προκατασκευασμένο σενάριο\)
 - **hbstart\.hb** εναρκτήριο σενάριο Harbour για το διαδραστικό κέλυφος Harbour\. Εκτελείται αυτόματα κατά την έναρξη του κελύφους, αν υπάρχει\. Ενδεχόμενες τοποθεσίες \(με σειρά προτεραιότητας\) \[\*\]: \./, $HOME/\.harbour, /etc/harbour, &lt;hbmk2 κατάλογος&gt;/\.\./etc/harbour, &lt;hbmk2 κατάλογος&gt;/\.\./etc, &lt;hbmk2 κατάλογος&gt;
 - **shell plugins** \.hb και \.hrb plugins για το διαδραστικό κέλυφος του Harbour\. Πρέπει να βρίσκονται μέσα στο \[\*\]: $HOME/\.harbour/
 - **\.hb\_history** αποθηκεύει ιστορικό εντολών για το διαδραστικό κέλυφος του Harbour\. Μπορείτε να απενεργοποιήσετε το ιστορικό κάνοντας την πρώτη γραμμή 'no' \(χωρίς τα εισαγωγικά και με νεα γραμμή\)\. Βρίσκεται στο \[\*\]: $HOME/\.harbour/
 - **hb\_extension** λίστα καταλήξεων προς φόρτωση στο διαδραστικό κέλυφος του Harbour\. Μία κατάληξη ανα γραμμή, το τμήμα της γραμμής μετά από ένα χαρακτήρα '\#' αγνοείται\. Εναλλακτικά ονομα\-αρχείου στο Ms\-DOS: Το hb\_ext\.ini\. Βρίσκεται μεσα στο \[\*\]: $HOME/\.harbour/
  
Μεταβλητές Macro:  


 - **$\{hb\_root\}** κατάλογος του hbmk2
 - **$\{hb\_dir\}** κατάλογος του χρησιμοποιούμενου ονόματος αρχείου
 - **$\{hb\_dirname\}** ανώτατος κατάλογος του αρχείου που χρησιμοποιείται
 - **$\{hb\_name\}** όνομα του αρχείου που χρησιοποιείται \(χωρίς κατάλογο και κατάληξη\)
 - **$\{hb\_self\}** πλήρες όνομα του αρχείου που χρησιμοποιείται
 - **$\{hb\_curdir\}** τρέχων καταλόγος εργασίας
 - **$\{hb\_tempdir\}** Κατάλογος Λ\.Σ\. για προσωρινα αρχεία
 - **$\{hb\_targetname\}** όνομα του έργου \(χωρίς κατάλογο και κατάληξη\)\. Επιστρέφει \.adhoc\. αν δεν υπαρχει αρχείο έργου\.
 - **$\{hb\_targettype\}** ο τύπος του έργου \(hbexe, hblib, hbdyn, hbdynvm, hbimplib, hbppo, hbhrb, hbcontainer\)
 - **$\{hb\_plat\}** επιλεγμένη πλατφόρμα
 - **$\{hb\_comp\}** επιλεγμένος μεταγλωττιστής C
 - **$\{hb\_comp\_ver\}** Εκδοση μεταγλωττιστή C
 - **$\{hb\_build\}** όνομα κατασκευής
 - **$\{hb\_cpu\}** επιλεγμένη CPU
 - **$\{hb\_work\}** προεπιλεγμένο όνομα βασικού καταλόγου εργασίας
 - **$\{hb\_workdynsub\}** προεπιλεγμένος υποκατάλογος εργασίας για τις κατασκευαζόμενες δυναμικές βιβλιοθήκες
 - **$\{hb\_dynprefix\}** πρόθεμα δυναμικής βιβλιοθήκης
 - **$\{hb\_dynsuffix\}** επίθεμα δυναμικής βιβλιοθήκης
 - **$\{hb\_dynext\}** κατάληξη δυναμικής βιβλιοθήκης
 - **$\{hb\_ver\}** Η έκδοση Harbour σε δεκαεξαδική μορφή τριπλού χαρακτήρα\. Π\.χ\.: 030400
 - **$\{hb\_verstr\}** η έκδοση Harbour σε ανθρώπινα αναγνώσιμη μορφή &lt;major&gt;\.&lt;minor&gt;\.&lt;release&gt;&lt;status&gt;\. Π\.χ\.: 3\.4\.0dev
 - **$\{hb\_major\}** Μείζων αριθμός έκδοσης Harbour
 - **$\{hb\_minor\}** Δευτερεύων αριθμός έκδοσης
 - **$\{hb\_release\}** Αριθμός έκδοσης Harbour \-release\-
 - **$\{hb\_status\}** Κατάσταση έκδοσης Harbour
 - **$\{hb\_ver\_id\}** Harbour version ID
 - **$\{hb\_revision\}** Αναθεώρηση Harbour
 - **$\{hb\_host\_plat\}** Πλατφόρμα υποδοχής
 - **$\{hb\_host\_plat\_unix\}** επιστρέφει '1' αν η πλατφόρμα υποδοχής Harbour είναι \*nix συμβατή
 - **$\{hb\_bin\}** Κατάλογος εκτελέσιμων αρχείων Harbour
 - **$\{hb\_lib\}** Κατάλογος στατικών βιβλιοθηκών Harbour
 - **$\{hb\_lib3rd\}** Κατάλογος 3rd party βιβλιοθηκών Harbour
 - **$\{hb\_dyn\}** Κατάλογος δυναμικών βιβλιοθηκών Harbour
 - **$\{hb\_inc\}** Κατάλογος header Harbour
 - **$\{hb\_addons\}** Κατάλογος βάσης add\-ons Harbour
 - **$\{hb\_first\}** το όνομα του πηγαίου αρχείου που περιέχει την συνάρτηση εκκίνησης \(χωρίς κατάλογο και κατάληξη\)
 - **$\{hb\_outputdir\}** ο κατάλογος εξόδου
 - **$\{hb\_outputname\}** το όνομα εξόδου \(χωρίς κατάληξη\)
 - **$\{hb\_level\}** επίπεδο αναδρομικότητας υπο\-έργου
 - **$\{&lt;depname&gt;\}** επιστρέφει τον επικεφαλής κατάλογο της εξάρτησης &lt;depname&gt;, ή '1' αν δεν ανιχνευτεί
 - **$\{&lt;envvar&gt;\}** επιστρέφει την τιμή της μεταβλητής\-περιβάλλοντος &lt;envvar&gt;
  
Φίλτρα \(μπορείτε να τα συνδυάσετε και/ή να τα αναιρέσετε\):  


 - **\{&lt;platform&gt;\}** η πλατφόρμα\-στόχος\. Οπου &lt;platform&gt; μπορεί να είναι κάθε τιμή αποδεκτή από την επιλογή \-plat= \.
 - **\{&lt;compiler&gt;\}** ο C μεταγλωττιστής\-στόχος\. Οπου &lt;compiler&gt; μπορεί να είναι κάθε τιμή αποδεκτή απο την επιλογή \-comp\.
 - **\{&lt;cpu&gt;\}** τύπος CPU εκτέλεσης του έργου\. Οπου &lt;cpu&gt; μπορεί να είναι κάτι από: x86, x86\_64, ia64, arm, mips, sh
 - **\{&lt;targettype&gt;\}** τύπος κατασκευής έργου\. Οπου &lt;targettype&gt; είναι μια από τις τιμές που επιστρέφονται από την macro μεταβλητή $\{hb\_targettype\}\.
 - **\{&lt;package\-manager&gt;\}** package manager\. Where &lt;package\-manager&gt; can be any of: deb, rpm, portage, homebrew, rudix, macports, fink, pkg, cygwin
 - **\{mt\}** το κατασκευαζόμενο έργο είναι πολυ\-νηματικό /multi\-threaded/ \(δες επιλογή \-mt\)
 - **\{st\}** το κατασκευαζόμενο έργο είναι μονό\-νηματικό /single\-threaded/ \(δες επιλογή \-st\)
 - **\{gui\}** στόχος Γραφικού Περιβάλλοντος Χρήστη GUI \(δες επιλογή \-gui\)
 - **\{std\}** στόχος κονσόλας \(δες επιλογή \-console\)
 - **\{debug\}** Ενεργοποιημένη αποσφαλμάτωση επιπέδου C \(δες επιλογή \-debug\)
 - **\{nodebug\}** Απενεργοποίηση αποσφαλμάτωσης επιπέδου C \(δες \-debug\- επιλογή\)
 - **\{shared\}** κατασκευή shared \(δες \-shared και τις σχετικές επιλογές\)
 - **\{static\}** κατασκευή static \(δες \-static και τις σχετικές επιλογές\)
 - **\{lngcpp\}** εξαναγκασμένη C\+\+ κατάσταση λειτουργίας \(δες \-cpp επιλογή\)
 - **\{lngc\}** εξαναγκασμένη C κατάσταση λειτουργίας \(δες \-cpp επιλογή\)
 - **\{winuni\}** κατάσταση λειτουργίας Windows UNICODE \(WIDE\) \(δες επιλογή \-winuni\)
 - **\{winansi\}** κατάσταση λειτουργίας Windows ANSI \(δες επιλογή \-winuni\-\)
 - **\{unix\}** η παλτφόρμα\-στόχος είναι \*nix συμβατή \(bsd, hpux, sunos, beos, qnx, android, vxworks, symbian, linux, darwin, cygwin, minix, aix\)
 - **\{allwin\}** η πλατφόρμα\-προορισμού είναι Windows συμβατή \(win, wce\)
 - **\{allgcc\}** ο χρησιμοποιούμενος μεταγλωττιστής C ανήκει στην οικογένεια gcc \(gcc, mingw, mingw64, mingwarm, djgpp, gccomf, clang, open64, pcc\)
 - **\{allmingw\}** ο χρησιμοποιούμενος μεταγλωττιστής C είναι mingw\* \(mingw, mingw64, mingwarm\)
 - **\{allmsvc\}** ο μεταγλωττιστής C που θα χησιμοποιηθεί είναι msvc\* \(msvc, msvc64, msvcia64, msvcarm\)
 - **\{allbcc\}** ο χρησιμοποιούμενος μεταγλωττιστής C είναι bcc\* \(bcc, bcc64\)
 - **\{allpocc\}** ο στοχευμένος μεταγλωττιστής C είναι pocc\* \(pocc, pocc64, poccarm\)
 - **\{allicc\}** ο στοχευμένος μεταγλωττιστής C είναι icc\* \(icc, iccia64\)
 - **\{hb10\}** Κατάσταση συμβατότητας Harbour 1\.0\.x \(δες επιλογή \-hb10\)
 - **\{hb20\}** Κατάσταση συμβατότητας Harbour 2\.0\.x \(δες επιλογή \-hb20\)
 - **\{hb30\}** Κατάσταση συμβατότητας Harbour 3\.0\.x \(δες επιλογή \-hb30\)
 - **\{hb32\}** Harbour 3\.2\.0dev compatibility mode \(see \-hb32 option\)
 - **\{xhb\}** xhb μοντέλο \(δες επιλογή \-xhb\)
 - **\{hb\_ispath='&lt;file|dir&gt;'\}** το φίλτρο θα περάσει αν το όνομα &lt;file&gt; ή &lt;dir&gt; υπάρχει στο δίσκο\.
 - **\{MACRO\}** το φίλτρο θα περάσει αν η τιμή $\{MACRO\} δεν είναι άδεια και όχι ίση με '0' ή 'no' \(πεζά/κεφαλαία: αδιάφορο\)
 - **\{MACRO='&lt;value&gt;'\}** το φίλτρο θα περάσει αν η τιμή $\{MACRO\} ισούται με την &lt;value&gt; \(ευαίσθητο σε πεζά/κεφαλαία\)
 - **\{MACRO&gt;'&lt;value&gt;'\}** το φίλτρο θα περάσει αν η τιμή $\{MACRO\} είναι μεγαλίτερη από την &lt;value&gt; \(πεζά/κεφαλαία: αδιάφορο\)
 - **\{MACRO&lt;'&lt;value&gt;'\}** το φίλτρο θα περάσει αν η τιμή $\{MACRO\} είναι μικρότερη από την &lt;value&gt; \(πεζά/κεφαλαία: αδιάφορο\)


Predefined constants in sources \(do not define them manually\):


 - **\_\_HBSCRIPT\_\_HBMK\_PLUGIN** όταν ένα σενάριο \.hb είναι μεταγλωττισμένο ως plugin του hbmk2
 - **\_\_HBEXTREQ\_\_** όταν ενα \.hbx πηγαίο αρχείο είναι παρόν σε ένα έργο \(διαθέσιμο στα πηγαία αρχεία Harbour\)
 - **HBMK\_HAS\_&lt;hbcname&gt;** όταν το πακέτο &lt;hbcname&gt;\.hbc είναι συνδεδεμένο στο έργο κατασκευής\. Η τιμή είναι η version= τιμή απο το αρχείο \.hbc, μετασχηματισμένη σε δεκαδικό αριθμό, ο οποίος είναι '1', αν δεν έχει οριστεί\. \(διαθέσιμο σε πηγαία αρχεία Harbour\)
 - **HBMK\_HAS\_&lt;depname&gt;** όταν ανιχνεύτηκε η εξάρτηση &lt;depname&gt; \(διαθέσιμο σε πηγαίους κώδικες C\)


 - **\_\_HBSCRIPT\_\_HBSHELL** όταν ένα πηγαίο αρχείο Harbour εκτελείται ως σενάριο κελύφους
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, etc\.


Predefined constants in build files \(they are available after '\-depfinish=&lt;depname&gt;' / 'depfinish=&lt;depname&gt;'\) \(do not define them manually\):


 - **HBMK\_HAS\_&lt;depname&gt;** όταν &lt;depname&gt; εξάρτηση ανιχνεύτηκε
 - **HBMK\_DIR\_&lt;depname&gt;** επιστρέφει τον επικεφαλής κατάλογο όπου η &lt;depname&gt; ανιχνεύτηκε, ή κενό αν δεν ανιχνεύτηκε\.
 - **HBMK\_HAS\_&lt;depname&gt;\_LOCAL** οταν η εξάρτηση &lt;depname&gt; ανιχνεύτηκε σε μια θέση που ορίστηκε με την επιλογή \-depincpathlocal=
  
Μεταβλητές περιβάλλοντος:  


 - **HBMK\_OPTIONS** δέχεται οποιεσδήποτε επιλογές σαν να είχαν αυτές περαστεί στην αρχή της γραμμής\-εντολών
 - **HB\_PLATFORM** δέχεται τις ίδιες τιμές όπως και η επιλογή \-plat=
 - **HB\_COMPILER** δέχεται τις ίδιες τιμές όπως και η επιλογή \-comp=
 - **HB\_CPU** δέχεται τις ίδιες τιμές όπως και η επιλογή \-cpu=
 - **HB\_BUILD\_NAME** δέχεται τις ίδιες τιμές όπως και η επιλογή \-build=
 - **HB\_LANG** δέχεται τις ίδιες τιμές όπως και η επιλογή \-lang=
 - **HB\_USER\_LIBS** δέχεται τις ίδιες τιμές \(χωρισμένες με κενό\) όπως και η επιλογή \-l
 - **HB\_USER\_LIBPATHS** δέχεται τις ίδιες τιμές \(χωρισμένες με κενό\) όπως και η επιλογή \-L
 - **HB\_USER\_PRGFLAGS** επιλογές που θα περαστούν στον μεταγλωττιστή Harbour \(πρίν από τυχόν επιλογές γραμμής\-εντολών\)
 - **HB\_USER\_CFLAGS** επιλογές που θα περαστούν στον μεταγλωττιστή C \(πρίν από τυχόν επιλογές γραμμής\-εντολών\)
 - **HB\_USER\_RESFLAGS** επιλογές που θα περαστούν στον resource compiler \(πρίν από επιλογές γραμμής\-εντολών\) \(μόνο για Windows\)
 - **HB\_USER\_LDFLAGS** επιλογής που θα περαστούν στον συνδέτη \(εκτελέσιμου\) \(πρίν τις επιλογές της γραμμής\-εντολών\)
 - **HB\_USER\_DFLAGS** επιλογές που θα περαστούν στον συνδέτη \-linker\- \(dynamic library\) \(πρίν από επιλογές γραμμής\-εντολών\)
 - **HB\_USER\_AFLAGS** επιλογές που θα περαστούν στον συνδέτη \-linker\- \(static library\) \(πρίν τις επιλογές της γραμμής\-εντολών\)
 - **HB\_CCPATH** επικάλυψη του καταλόγου εκτελέσιμων του μεταγλωττιστή C \(μόνο σε gcc οικογένειες μεταγλωττιστών\)
 - **HB\_CCPREFIX** επικάλυψη του προθέματος εκτελέσιμου του μεταγλωττιστή C \(μόνο σε gcc οικογένειες μεταγλωττιστών\)
 - **HB\_CCSUFFIX** επικάλυψη του επιθέματος εκτελέσιμου του μεταγλωττιστή C \(μόνο σε gcc οικογένειες μεταγλωττιστών\)
 - **HB\_INSTALL\_PREFIX** επικάλυψη του βασικού καταλόγου εγκατάστασης του Harbour
 - **HB\_INSTALL\_ADDONS** επικάλυψη του βασικού καταλόγου add\-ons του Harbour


 - **HB\_EXTENSION** λίστα καταλήξεων, διαχωρισμένων με κενό διάστημα, προς φόρτωση στο διαδραστικό κέλυφος του Harbour
  
\.hbc ντιρεκτίβες \(πρέπει να αναγράφονται σε ξεχωριστές γραμμές\):  


 - **echo=&lt;msg&gt;** εμφάνιση &lt;msg&gt;
 - **skip=\[&lt;msg&gt;\]** παράλειψη επεξεργασίας του υπόλοιπου \.hbc αρχείου\. Εμφάνιση &lt;msg&gt;, αν οριστεί\.
 - **stop=\[&lt;msg&gt;\]** διακοπή της κατασκευής\. Εμφάνιση του &lt;msg&gt;, αν οριστεί\.
 - **sources=** προσθήκη λίστας αρχείων ως αρχεία εισόδου, διαχωρισμένων με κενό
 - **headers=** προσθήκη διαχωρισμένης με κενό λίστας από headers μορφής \.ch ως πρότυπο header
 - **libs=** προσθήκη λίστας βιβλιοθηκών διαχωρισμένων με κενό \(δες περισσότερα στην \-l επιλογή\)
 - **frameworks=** προσθήκη λίστας frameworks διαχωρισμένων με κενό \(μόνο σε Darwin\)
 - **requests=** προσθήκη λίστας συμβόλων /ονοματα συναρτήσεων κ\.λ\.π/ διαχωρισμένα με κενό, που θα διασυνδεθούν υποχρεωτικά στο κατασκευαζόμενο έργο
 - **syslibs=** προσθήκη λίστας βιβλιοθηκών χωρισμένων με κενό διάστημα ως βιβλιοθήκες συστήματος \(πρίν από άλλες βιβλιοθήκες\)
 - **hbcs=** ενσωμάτψση λίστας \.hbc αρχείων διαχωρισμένων με κενό διάστημα\. Επιτρέπονται ονόματα χωρίς την κατάληξη\. Οι αναφορές αυτές επεξεργάζονται επι τόπου\.
 - **autohbcs=** λίστα τιμών διαχωρισμένων με κενό διάστημα, όπως στην επιλογή \-autohbc=
 - **libpaths=** λίστα μονοπατιών, διαχωρισμένων με κενό διάστημα, για πρόσθετες βιβλιοθήκες
 - **incpaths=** προσθήκη διαχωρισμένης με κενό λίστας πρόσθετων μονοπατιών header \(για αμφότερα Harbour και C\)
 - **instfiles=** λίστα τιμών διαχωρισμένων με κενό διάστημα, όπως στην επιλογή \-instfile=
 - **instpaths=** λίστα τιμών διαχωρισμένων με κενό διάστημα, όπως στην επιλογή \-instpath=
 - **prgflags=** λίστα τιμών διαχωρισμένων με κενό διάστημα, όπως στην επιλογή \-prgflag=
 - **cflags=** λίστα τιμών διαχωρισμένων με κενό διάστημα, όπως στην επιλογή \-cflag=
 - **resflags=** λίστα τιμών διαχωρισμένων με κενό διάστημα, όπως στην επιλογή \-resflag=
 - **ldflags=** λίστα τιμών διαχωρισμένων με κενό διάστημα, όπως στην επιλογή \-ldflag=
 - **ldflags\+=** λίστα τιμών διαχωρισμένων με κενό διάστημα, όπως στην επιλογή \-ldflag\+=
 - **dflags=** λίστα τιμών διαχωρισμένων με κενό διάστημα, όπως στην επιλογή \-dflag=
 - **dflags\+=** λίστα τιμών διαχωρισμένων με κενό διάστημα, όπως στην επιλογή \-dflag\+=
 - **pflags=** λίστα τιμών διαχωρισμένων με κενό διάστημα, όπως στην επιλογή \-pflag=
 - **psources=** λίστα τιμών διαχωρισμένων με κενό διάστημα, όπως στην επιλογή \-pi=
 - **gui=&lt;bool&gt;** 'yes' = \-gui, 'no' = \-std επιλογή
 - **mt=&lt;bool&gt;** 'yes' = \-mt, 'no' = \-st επιλογή
 - **pic=&lt;bool&gt;** 'yes' = \-pic, 'no' = \-pic\- επιλογή
 - **shared=&lt;bool&gt;** 'yes' = \-shared, 'no' = \-static επιλογή
 - **shareddef=&lt;bool&gt;** παρόμοιο με shared=, αλλά λειτουργεί μόνο αν το μοντέλο shared/static δεν έχει οριστεί πιο πρίν
 - **fullstatic=&lt;bool&gt;** 'yes' = \-fullstatic, 'no' = \-static επιλογή
 - **debug=&lt;bool&gt;** 'yes' = \-debug, 'no' = \-debug\- επιλογή
 - **optim=** 'yes' = \-optim, 'no' = \-optim\- επιλογή
 - **nulrdd=&lt;bool&gt;** 'yes' = \-nulrdd, 'no' = \-nulrdd\- επιλογή
 - **nodefgt=&lt;bool&gt;** 'yes' = \-nodefgt, 'no' = \-nodefgt\- επιλογή
 - **map=&lt;bool&gt;** 'yes' = \-map, 'no' = \-map\- επιλογή
 - **hbcppmm=&lt;bool&gt;** 'yes' = \-hbcpmm, 'no' = \-hbcpmm\- επιλογή
 - **implib=&lt;bool&gt;** 'yes' = \-implib, 'no' = \-implib\- επιλογή
 - **winuni=&lt;bool&gt;** 'yes' = \-winuni, 'no' = \-winuni\- επιλογή
 - **strip=&lt;bool&gt;** 'yes' = \-strip, 'no' = \-strip\- επιλογή
 - **run=&lt;bool&gt;** 'yes' = \-run, 'no' = \-run\- επιλογή
 - **inc=&lt;bool&gt;** 'yes' = \-inc, 'no' = \-inc\- επιλογή
 - **harden=&lt;bool&gt;** 'yes' = \-harden, 'no' = \-harden\- επιλογή
 - **cpp=** όμοιο με την επιλογή \-cpp=
 - **warn=** όμοιο με την επιλογή \-warn=
 - **compr=** όμοιο με την επιλογή \-compr=
 - **head=** όμοιο με την επιλογή \-head=
 - **plugins=** λίστα plugins του hbmk2 διαχωρισμένων με κενό διάστημα
 - **gt=&lt;name&gt;** όμοιο με την επιλογή \-gt&lt;name&gt;
 - **gtdef=&lt;name&gt;** ορισμός του πρεπιλεγμένου GT που θα χρησιμοποιηθεί
 - **env=** όμοιο με την επιλογή \-env:
 - **depurlbase=** όμοιο με την επιλογή \-depurlbase=
 - **deppkgname=** όμοιο με την επιλογή \-deppkgname=
 - **depkeyhead=** όμοιο με την επιλογή \-depkeyhead=
 - **depoptional=** όμοιο με την επιλογή \-depoptional=
 - **depcontrol=** όμοιο με την επιλογή \-depcontrol=
 - **depincroot=** όμοιο με την επιλογή \-depincroot=
 - **depincpath=** όμοιο με την επιλογή \-depincpath=
 - **depincpathlocal=** όμοιο με την επιλογή \-depincpathlocal=
 - **depimplibs=** όμοιο με την επιλογή \-depimplibs=
 - **depimplibd=** όμοιο με την επιλογή \-depimplibd=
 - **depfinish=** όμοιο με την επιλογή \-depfinish=
 - **signts=** όμοιο με την επιλογή \-signts=
 - **name=** όνομα πακέτου
 - **description=** περιγραφή πακέτου
 - **version=&lt;x\.y\.z&gt;** ο αριθμός έκδοσης του πακέτου, όπου x,y,z &gt;= 0 &lt;= 255\. Τίθεται σε 0\.0\.1, αν δεν οριστεί\.
 - **keywords=** λίστα keywords διαχωρισμένων με κενό διάστημα
 - **licences=** λίστα αδειών διαχωρισμένων με κενό διάστημα
 - **repository=** λίστα αναφορών πηγαίων αποθηκών \-repository\-, διαχωρισμένων με κενό διάστημα


Plugin API:  
\(η 'hbmk' είναι η μεταβλητή περιβάλλοντος που λαμβάνεται από τη συνάρτηση εισόδου του plugin\)


 - **hbmk\_Register\_Input\_File\_Extension\( hbmk, &lt;cExt&gt; \) \-&gt; NIL**  
Register input file extension to be passed to plugin \(by default all unrecognized file extensions are passed to Harbour compiler\)\.
 - **hbmk\_AddInput\_PRG\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Προσθήκη ενός Harbour αρχείου εισόδου στο έργο\.
 - **hbmk\_AddInput\_C\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Προσθήκη ενός C αρχείου εισόδου στο έργο\.
 - **hbmk\_AddInput\_CPP\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Προσθήκη ενός C\+\+ αρχείου εισόδου στο έργο\.
 - **hbmk\_AddInput\_RC\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Προσθήκη ενός Windows resource αρχείου στο έργο\.
 - **hbmk\_AddInput\_OBJ\( hbmk, &lt;cFileName&gt; \) \-&gt; NIL**  
Προσθήκη ενός δυαδικού object αρχείου στο έργο\.
 - **hbmk\_AddInput\_INSTFILE\( hbmk, &lt;cFileName&gt;, \[&lt;cGroup&gt;\] \) \-&gt; NIL**  
Προσθήκη αρχείου προς εγκατάσταση, με ένα προαιρετικό \-instpath= όνομα ομάδας\.
 - **hbmk\_AddOption\_PRG\( hbmk, &lt;cOption&gt; \) \-&gt; NIL**  
Add a Harbour compiler option\.
 - **hbmk\_AddOption\_C\( hbmk, &lt;cOption&gt; \) \-&gt; NIL**  
Add a C compiler option\.
 - **hbmk\_OutStd\( hbmk, &lt;cText&gt; \) \-&gt; NIL**  
Εξοδος κειμένου στο stdout\.
 - **hbmk\_OutErr\( hbmk, &lt;cText&gt; \) \-&gt; NIL**  
Εξοδος κειμένου στο
 - **hbmk\_OutStdRaw\( hbmk, &hellip; \) \-&gt; NIL**  
Εξοδος κειμένου στο stdout χωρίς καμμία μορφοποίηση\.
 - **hbmk\_OutErrRaw\( hbmk, &hellip; \) \-&gt; NIL**  
Εξοδος κειμένου στο stderr χωρίς καμμία μορφοποίηση\.
 - **hbmk\_Macro\( hbmk, &lt;cMacro&gt; \) \-&gt; &lt;cResult&gt;**  
Αξιολόγηση της macro έκφρασης hbmk2 \.
 - **hbmk\_FNameEscape\( hbmk, &lt;cFileName&gt; \) \-&gt; &lt;cFileName&gt;**  
Οριοθετημενο με Escape ή εισαγωγικά όνομα αρχείου που θα χρησιμοποιηθεί ως παραμέτρος εξωτερικής εντολή\.
 - **hbmk\_PathSepToTarget\( hbmk, &lt;cFileName&gt; \) \-&gt; &lt;cFileName&gt;**  
Μετατροπή ονόματος αρχείου στη μορφή που απαιτείται για την πλατφόρμα προορισμού/μεταγλωττιστή C\.
 - **hbmk\_PathSepToForward\( &lt;cPath&gt; \) \-&gt; &lt;cPath&gt;**  
Μετατροπή του ονόματος αρχείου ωστε να έχει, ως διαχωριστικό καταλόγου, την εμπρός κάθετη παύλα / \.
 - **hbmk\_PathFromWorkdirToCWD\( hbmk \) \-&gt; &lt;cRelativePath&gt;**  
Επιστροφή σχετικού μονοπατιού της τιμής του \-workdir= από το τρέχοντα κατάλογο εργασίας\.
 - **hbmk\_FindInPath\( &lt;cFileName&gt;, \[&lt;xPath&gt;\], \[&lt;aExtDef&gt;\] \) \-&gt; &lt;cFNFound&gt; | NIL**  
Ευρεση αρχείου στο &lt;xPath&gt; \(είναι δεκτά, πίνακας ή συμβολοσειρά διαχωρισμένη με διαχωριστές καταλόγου\) που περιέχει λίστα από &lt;aExtDef&gt; εναλλακτικές καταλήξεις \(defaults to executable binaries\)\. Επιστρέφει όνομα\-αρχείου αν βρεθεί και NIL αν όχι\.
 - **hbmk\_FNameDirExtSet\( &lt;cFileName&gt;, \[&lt;cDirNew&gt;\], \[&lt;cExtNew&gt;\] \) \-&gt; &lt;cFileName&gt;**  
Αλλαγή του καταλόγου και της κατάληξης του αρχείου
 - **hbmk\_FuncNameEncode\( &lt;cFuncName&gt; \) \-&gt; &lt;cFuncNameEncoded&gt;**  
Κωδικοποίηση ονόματος συνάρτησης σύμφωνα με τους κανόνες Harbour περί μορφοποίησης ονόματων των HB\_FUNC\(\) συναρτήσεων σε κώδικα C \.
 - **hbmk\_StrStripQuote\( &lt;cString&gt; \) \-&gt; &lt;cString&gt;**  
Αφαίρεση των διπλών εισαγωγικών εγκλεισμού από μία συμβολοσειρά\.
 - **hbmk\_ArrayToList\( &lt;aList&gt;, \[&lt;cSeparator&gt;\] \) \-&gt; &lt;cList&gt;**  
Μετατροπή ενός πίνακα συμβολοσειρών σε συμβολοσειρά\. Προεπιλεγμένος διαχωριστής είναι ένα κενό διάστημα\.


Μεταβλητές Plugin:  
\(στοιχεία hash συναφή με 'hbmk', ευαίσθητα σε πεζά\-κεφαλαία, μόνο\-για ανάγνωση, εκτός κι αν σημειώνεται κάτι άλλο\)


 - **"apiver"** η έκδοση ΑΡΙ σαν ακέραιος αριθμός
 - **"cSTATE"** δήλωση επίκλισης\. Μπορεί να είναι: 'init', 'pre\_all', 'pre\_prg', 'pre\_res', 'pre\_c', 'pre\_link', 'pre\_lib', 'pre\_cleanup', 'post\_build', 'post\_all'
 - **"params"** πίνακας παραμέτρων που περνιώνται στα plugins μέσω των επιλογών \-pflag=/pi= ή που έχουν μία κατάληξη καταχωρημένη μέσω της συνάρτησης hbmk\_Register\_Input\_File\_Extension\(\)
 - **"vars"** Κατακερματισμός \-hash\- των ιδιαίτερων μεταβλητών plugin\. Εγγράψιμο, τοπικά σε κάθε plugin
 - **"cPLAT"** \-plat τιμή
 - **"cCOMP"** \-comp τιμή
 - **"nCOMPVer"** detected compiler version in &lt;MMmm&gt; format
 - **"cCPU"** \-cpu τιμή
 - **"cBUILD"** \-build= τιμή
 - **"cOUTPUTNAME"** \-o τιμή
 - **"cTARGETNAME"** δες $\{hb\_targetname\} macro
 - **"cTARGETTYPE"** δες $\{hb\_targettype\} macro
 - **"lREBUILD"** \-rebuild κατάσταση επιλογής
 - **"lCLEAN"** \-clean κατάσταση επιλογής
 - **"lDEBUG"** \-debug κατάσταση επιλογής
 - **"lMAP"** \-map κατάσταση επιλογής
 - **"lSTRIP"** \-strip κατάσταση επιλογής
 - **"lDONTEXEC"** \-traceonly κατάσταση επιλογής
 - **"lIGNOREERROR"** \-ignore κατάσταση επιλογής
 - **"lTRACE"** \-trace κατάσταση επιλογής
 - **"lQUIET"** \-q κατάσταση επιλογής
 - **"lINFO"** \-info κατάσταση επιλογής
 - **"lBEEP"** \-beep κατάσταση επιλογής
 - **"lRUN"** \-run κατάσταση επιλογής
 - **"lINC"** \-inc κατάσταση επιλογής
 - **"cCCPATH"** δες HB\_CCPATH μεταβλητή περιβάλοντος
 - **"cCCPREFIX"** δες HB\_CCPREFIX μεταβλητή περιβάλοντος
 - **"cCCSUFFIX"** δες HB\_CCSUFFIX μεταβλητή περιβάλοντος
 - **"cCCEXT"** δες HB\_CCEXT μεταβλητή περιβάλοντος
 - **"cWorkDir"** \-workdir= τιμή
 - **"nExitCode"** Τρέχων κωδικός εξόδου
  
Shell API διαθέσιμο σε σενάρια Harbour:  


 - **hbshell\_gtSelect\( \[&lt;cGT&gt;\] \) \-&gt; NIL**  
Αλλαγή GT\. Προεπιλογή \[\*\]: 'gttrm'
 - **hbshell\_Clipper\(\) \-&gt; NIL**  
Enable Cl\*pper compatibility \(non\-Unicode\) mode\.
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


Παραδείγματα για να ξεκινήσετε τη χρήση του hbmk2:


 - **Για εκτέλεση το διαδραστικού κελύφους \('dot' prompt\)**  
$ hbmk2 \.
 - **Για να εκτελέσετε ένα σενάριο Harbour**  
$ hbmk2 myscript\.hb \[&lt;παραμετρος\[ι\]&gt;\]


Παραδείγματα δημιουργίας &amp; εκετέλεσης μεταφέρσιμου δυαδικού αρχείου Harbour \(γνωστό και ως προκατασκευασμένο σενάριο\):


 - **Για δημιουργία**  
$ hbmk2 \-gh myscript\.hb
 - **Για να εκτελέσετε το αποτέλεσμα των παραπάνω**  
$ hbmk2 myscript\.hrb \[&lt;παραμετρος\[ι\]&gt;\]


Παραδείγματα δημιουργίας μιας εφαρμογής Harbour:


 - **Για την κατασκευή ενός μοναδικού**  
$ hbmk2 hello\.prg
 - **Για να δημιουργήσετε μία εφαρμογή που περιλαμβάνει πολλαπλά \.prg αρχεία με αυξητικό ρυθμό \-incremetnal\-**  
$ hbmk2 mymain\.prg myfuncs\.prg \-inc
 - **Για να δημιουργήσετε μία εφαρμογή με χρήση ενός αρχείου Εργου**  
$ hbmk2 myapp\.hbp
 - **Για να δημιουργήσετε μία εφαρμογή χρησιμοποιώντας σταδιακά\-αυξητική \-incremental\- μέθοδο**  
$ hbmk2 myapp\.hbp \-inc
 - **Για κατασκευή μιάς εφαρμογής που χρησιμοποιεί πακέτο contrib ή πακέτο 3rd party \(πρόσθετο\) το οποίο συμπεριλαμβάνει ένα \.hbc αρχείο**  
$ hbmk2 myapp\.prg hbct\.hbc
 - **Για να δημιουργήσετε μία εφαρμογή που χρησιμοποιεί μία raw βιβλιοθήκη**  
$ hbmk2 myapp\.prg \-lmylib \-L&lt;path\_to\_mylib&gt;
 - **Για να δημιουργήσετε μία εφαρμογή που χρησιμοποιεί ένα Windows resource**  
$ hbmk2 mymain\.prg myres\.rc
 - **Για να δημιουργήσετε μία εφαρμογή με αναφορές σε δυναμικές βιβλιοθήκες Harbour**  
$ hbmk2 \-shared myapp\.prg
 - **Για δημιουργία εφαρμογής από όλα τα \.prg και \.c αρχεία που υπάρχουν μεσα σε ένα 'πηγαίο' υποκατάλογο**  
$ hbmk2 \-omyapp src/\*\.prg src/\*\.c


Παραδείγματα δημιουργίας μιας στατικής βιβλιοθήκης:


 - **Για κατασκευή της βιβλιοθήκης 'mylib' από πηγαία αρχεία**  
$ hbmk2 \-hblib mylibsrc\.prg \-omylib
 - **Για κατασκευή της βιβλιοθήκης 'mylib' από πηγαίο κώδικα με επαυξητικό "incremental" τρόπο**  
$ hbmk2 \-hblib mylibsrc\.prg \-omylib \-inc
  
Κωδικοί εξόδου \("errorlevels"\):  


 - **0** κανένα λάθος
 - **1** unrecognized platform
 - **2** unrecognized compiler
 - **3** αποτυχία ανίχνευσης Harbour
 - **5** αποτυχίας δημιουργίας stub
 - **6** αποτυχία κατά το στάδιο της μεταγλώττισης
 - **7** αποτυχία στη τελική συναρμολόγηση \(linker or library manager\)
 - **8** δεν υποστηρίζεται
 - **9** αποτυχία δημιουργίας καταλόγου εργασίας
 - **19** βοήθεια
 - **10** εξάρτηση απούσα ή απενεργοποιημένη
 - **20** αρχικοποίηση plugin
 - **30** πολύ βαθειά εμφώλευση
 - **50** ζητήθηκε σταμάτημα
 - **&lt;other&gt;** όταν γίνει χρήση της επιλογής \-run, κωδικός εξόδου θα είναι αυτός που θα επιστραφεί από το δημιουργημένο εκτελέσιμο\.
  
Σημειώσεις:  


  - Το &lt;script&gt; μπορεί να είναι:  
&lt;@script&gt; ή &lt;script\.hbm&gt;: επιλογές γραμμής\-εντολών σε αρχείο  
&lt;script\.hbp&gt;: επιλογές γραμμής εντολών σε αρχείο, σηματοδοτεί επίσης ενα νέο έργο αν οριστεί στην γραμμή\-εντολών  
&lt;script\.hbc&gt;: αρχείο πακέτου διαμόρφωσης
  - Πηγαίο όνομα\-αρχείου χωρίς κατάληξη θα φορτώσει το \.hbp αρχείο, αν υπάρχει τέτοιο \.hbp αρχείο στον τρέχοντα κατάλογο\. Αν όχι, θα γίνει χρήση \.prg κατάληξης\.
  - Πολλαπλές \-l, \-L, \-i και &lt;script&gt; παράμετροι είναι δεκτές\.
  - Οι συνηθισμένες επιλογές του μεταγλωττιστή Harbour γίνονται επίσης δεκτές ως έχουν\.  
\(μπορείτε να τις δείτε με την επιλογή \-harbourhelp\)
  - hbmk\.hbc option file in hbmk2 directory is always processed if it exists\. On \*nix platforms ~/harbour, /etc/\.harbour, &lt;base&gt;/etc/\.harbour, &lt;base&gt;/etc are checked \(in that order\) before the hbmk2 directory\.
  - Το hbmk\.hbm σενάριο κατασκευής στον τρέχοντα κατάλογο δέχεται πάντοτε επεξεργασία, αν υπάρχει\.
  - Η χρήση πλάγιας εμπρός κάθετης παύλας συνιστάται στις τιμές των επιλογών ως διαχωριστής μονοπατιού, αλλά η πίσω πλάγια κάθετη παύλα είναι εξίσου αποδεκτή\.
  - Φίλτρα επιτρέπονται σε κάθε γραμμή \.hbc καθώς και περισσότερες επιλογές\.  
Τα φίλτρα μπορούν να συνδυαστούν με χρήση τελεστών '&amp;' \(and\), '|' \(or\), να αναιρεθούν με τον τελεστή '\!' και να ομαδοποιηθούν με αγγύλες\. Π\.χ\.: \{win\}, \{gcc\}, \{linux|darwin\}, \{win&amp;\!pocc\}, \{\(win|linux\)&amp;\!watcom\}, \{unix&amp;mt&amp;gui\}, \-cflag=\{win\}\-DMYDEF, \-stop\{dos\}, \-stop\{\!allwin\}
  - Οι περισσότερες \.hbc γραμμές \(libs=, hbcs=, prgflags=, cflags=, ldflags=, libpaths=, instfiles=, instpaths=, echo=\) και οι αντίστοιχες παράμετροι γραμμής\-εντολής αποδέχονται μεταβλητές macro\. Η libpaths= επίσης αποδέχεται %\{hb\_name\} που μεταφράζεται στο όνομα του υπο διερεύνηση \.hbc αρχείου
  - Επιλογές που δέχονται μεταβλητές macro υποστηρίζουν επίσης υποκατάσταση εντολών\. Περικλείστε την εντολή μεσα σε \`\`, και, αν η εντολή περιέχει κενό διάστημα, περικλείστε τη επίσης σε διπλά εισαγωγικά\. Η standard έξοδος τη εντολής θα χρησιμοποιηθεί ως τιμή\. Π\.χ\. "\-cflag=\`wx\-config \-\-cflags\`", ή ldflags=\{unix&amp;gcc\}"\`wx\-config \-\-libs\`"\.
  - Οταν ορίζονται πολλαπλές επιλογές καθορισμού του τύπου του κατασκευαζόμενου έργου \(\-hblib, \-hbdyn, etc\.\), σημαντική θα είναι η πρώτη απ' αυτές, οι υπόλοιπες θα αγνοηθούν σιωπηρά\.
  - Βιβλιοθήκες και object αρχεία δημιουργημένα με/για τον CA\-Cl\*pper δεν θα λειτουργήσουν με κανέναν υποστηριζόμενο μεταγλωττιστή/πλατφόρμα\.
  - Η υποστήριξη προεπιλογών και χαρακτηριστικών μπορεί να διαφέρει ανά πλατφόρμα/μεταγλωττιστή\.
  - Δεν χρειάζεστε το GNU Make ή κάποιο άλλο make μεταγλωττιστή C και το MSYS \(σε Windows\) για να εκτελέσετε το hbmk2\.
  - '\.' \(dot\) passed as first parameter will enter the interactive Harbour shell\.


  - \.hb, \.hrb ή \.dbf file passed as first parameter will be run as Harbour script\. If the filename contains no path components, it will be searched in current working directory and in PATH\. If not extension is given, \.hb and \.hrb extensions are searched, in that order\. \.dbf file will be opened automatically in shared mode and interactive Harbour shell launched\. Non\-standard extensions will be auto\-detected for source and precompiled script types\. Note, for Harbour scripts, the codepage is set to UTF\-8 by default\. The default core header 'hb\.ch' is automatically \#included at the interactive shell prompt\. The default date format is the ISO standard: yyyy\-mm\-dd\. SET EXACT is set to ON\. Set\( \_SET\_EOL \) is set to OFF\. The default GT is 'gtcgi', unless full\-screen CUI calls are detected, when 'gttrm' \[\*\] is automatically selected \(except for INIT PROCEDUREs\)\.
  - You can use key &lt;Ctrl\+V&gt; in interactive Harbour shell to paste text from the clipboard\.
  - Τιμές με αστερίσκο \[\*\] μπορεί να εξαρτώνται από την πλατφόρμα υποδοχής ή/και τη διαμόρφωση\. Η παρούσα βοήθεια δημιουργήθηκε στην 'darwin' πλατφόρμα υποδοχής\.


Υποστηριζόμενες τιμές &lt;compiler&gt; γιά κάθε υποστηριζόμενη &lt;platform&gt; τιμή:


 - **linux** gcc, clang, icc, watcom, sunpro, open64
 - **darwin** gcc, clang, icc
 - **win** mingw, msvc, clang, bcc, bcc64, watcom, icc, pocc, xcc, mingw64, msvc64, msvcia64, iccia64, pocc64
 - **wce** mingwarm, mingw, msvcarm, poccarm
 - **os2** gcc, gccomf, watcom
 - **dos** djgpp, watcom
 - **bsd** gcc, clang, pcc
 - **hpux** gcc
 - **beos** gcc
 - **qnx** gcc
 - **android** gcc, gccarm
 - **vxworks** gcc, diab
 - **symbian** gcc
 - **cygwin** gcc
 - **minix** clang, gcc
 - **aix** gcc
 - **sunos** gcc, sunpro
  
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

  
Συγγραφέας:  


 - Viktor Szakáts \(vszakats\.net/harbour\) 
