
@Echo OFF
ECHO Assembling input files
ECHO Creating CONV.TXT
COPY hdf\BINTODEC.rtf HarDoc.hdf  > NUL
TYPE hdf\DECTOBIN.rtf >> HarDoc.hdf 
TYPE hdf\DECTOHEX.rtf >> HarDoc.hdf 
TYPE hdf\DECTOOCT.rtf >> HarDoc.hdf 
TYPE hdf\HEXATODE.rtf >> HarDoc.hdf 
TYPE hdf\ISBIN.rtf >> HarDoc.hdf 
TYPE hdf\ISDEC.rtf >> HarDoc.hdf 
TYPE hdf\ISHEXA.rtf >> HarDoc.hdf 
TYPE hdf\ISOCTAL.rtf >> HarDoc.hdf 
TYPE hdf\OCTALTOD.rtf >> HarDoc.hdf 
TYPE hdf\CD.rtf >> HarDoc.hdf 
TYPE hdf\MD.rtf >> HarDoc.hdf 
TYPE hdf\RD.rtf >> HarDoc.hdf 
TYPE hdf\TFILEREA.rtf >> HarDoc.hdf 
TYPE hdf\GT_ASCII.rtf >> HarDoc.hdf 
TYPE hdf\GT_ASCPO.rtf >> HarDoc.hdf 
TYPE hdf\GT_ATDIF.rtf >> HarDoc.hdf 
TYPE hdf\GT_CHARE.rtf >> HarDoc.hdf 
TYPE hdf\GT_CHARM.rtf >> HarDoc.hdf 
TYPE hdf\GT_CHARO.rtf >> HarDoc.hdf 
TYPE hdf\GT_CHRCO.rtf >> HarDoc.hdf 
TYPE hdf\GT_CHRFI.rtf >> HarDoc.hdf 
TYPE hdf\GT_CHRTO.rtf >> HarDoc.hdf 
TYPE hdf\GT_STRCO.rtf >> HarDoc.hdf 
TYPE hdf\GT_STRCS.rtf >> HarDoc.hdf 
TYPE hdf\GT_STRDI.rtf >> HarDoc.hdf 
TYPE hdf\GT_STREX.rtf >> HarDoc.hdf 
TYPE hdf\GT_STRLE.rtf >> HarDoc.hdf 
TYPE hdf\GT_STRPB.rtf >> HarDoc.hdf 
TYPE hdf\GT_STRRI.rtf >> HarDoc.hdf 
TYPE hdf\STRFORMA.rtf >> HarDoc.hdf 
REM Compile the sources
Echo Compiling the sources
Helpc /W31   hardoc.hdf
REM  Link the files
Echo Linking library
hc31 hardoc.hpj
 