@echo off

:: DBFCDX
clipper rddmktst /m/n/w/es2/d_TEST_CMPDIDX_
rtlink fi rddmktst lib dbfcdx
del *.obj
rddmktst cdxcl52.prg dbfcdx
clipper cdxcl52 /m/n/w/es2
rtlink fi cdxcl52 lib dbfcdx

:: DBFNTX
clipper rddmktst /m/n/w/es2
rtlink fi rddmktst lib dbfcdx
del *.obj
rddmktst ntxcl52.prg dbfntx
clipper ntxcl52 /m/n/w/es2
rtlink fi ntxcl52 lib dbfcdx

:: DBFCDX for ADS RDD
clipper rddmktst /m/n/w/es2/d_TEST_ADS_ /d_TEST_CMPDIDX_
rtlink fi rddmktst lib dbfcdx
del *.obj
rddmktst adscl52.prg dbfcdx
clipper adscl52 /m/n/w/es2
rtlink fi adscl52 lib dbfcdx
