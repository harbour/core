Name:                  libcups (printing) [*nix, free, open-source]
URL:                   http://cups.org/
Environment variable:  HB_WITH_CUPS= (on *nix only)
Install (debian):      sudo apt-get install libcups2-dev
Install (rpm):         libcups2-devel
mpkg_rpm.sh option:    --with cups


hbcups provides access to the CUPS printing library.

Function Reference
=====================================

cupsGetDefault() --> cPrinterName
    Returns the CUPS name of the default printer on the default server,
    or an empty string if no printer is configured as default printer.

cupsGetDests() --> aPrinterNames
    Returns a list of all CUPS printers on the default server.

cupsPrintFile( cPrinterName, cFileName, cTitle, [ aOptions | hOptions ] )
        --> nJobNumber
    Prints the named file on the named printer (on the default server), using
    the given title.  Options may be passed as an array of strings of the form
    "option=value" or as a hash of options and values (again expressed as
    character strings).  Returns the job number, or 0 on error.

Refer to the CUPS documentation for more details of these operations, including
a list of valid printing options.
