/*
program for testing hbgt.lib
*/

function main()

qout('gt_ascpos("Harbour",1) => ' + ltrim(str(gt_ascpos("Harbour",1))) )
qout('gt_atdiff("This Is Harbour","This Is Clipper") => ' + ltrim(str(gt_atdiff("This Is Harbour","This Is Clipper"))) )
qout('gt_chareven("The_Power_Of_Harbour") => ' + gt_chareven("The_Power_Of_Harbour") )
qout('gt_charodd("The_Power_Of_Harbour") => ' + gt_charodd("The_Power_Of_Harbour") )
qout('gt_chrcount("s","she sells shells by the sea shore") => ' + ltrim(str(gt_chrcount("s","she sells shells by the sea shore"))))
qout('gt_chrtotal("sl","she sells shells by the sea shore") => ' + ltrim(str(gt_chrtotal("sl","she sells shells by the sea shore"))))
qout('gt_charmix("CLIPPER","harbour") => ' + gt_charmix("CLIPPER","harbour") )
qout('gt_asciisum("harbour") => ' + ltrim(str(gt_asciisum("harbour"))) )
qout('gt_chrfirst("Ho",  "the power of Harbour") => ' + ltrim(str(gt_chrfirst("Ho", "the power of Harbour"))) )
qout('gt_strcount("the", "the cat sat on the mat") => ' + ltrim(str(gt_strcount("the", "the cat sat on the mat"))))
qout('gt_strcspn("this is a test", "as ") => ' + ltrim(str(gt_strcspn("this is a test", "as ")   )) )
qout('gt_strcspn("this is a test", "elnjpq") => ' + ltrim(str(gt_strcspn("this is a test", "elnjpq"))) )
qout('gt_strDiff("the cat", "the rat") => ' + gt_strDiff("the cat", "the rat") )
qout('gt_strexpand("HARBOUR", 2,"-") => ' + gt_strexpand("HARBOUR", 2,"-"))
qout('gt_strleft("this is a test", "hsit ") => ' + ltrim(str(gt_strleft("this is a test", "hsit ")) ))
qout('gt_strpbrk("this is a test", "sa ")   => ' + gt_strpbrk("this is a test", "sa ")  )
qout('gt_strright("this is a test", "teas ") => ' + ltrim(str(gt_strright("this is a test", "teas "))) )

return nil
