@rem
@rem $Id$
@rem

@echo off

rem DBFCDX
clipper rddmktst /m/n/w/es2/d_TEST_DESCEND_ /d_TEST_UNIQUE_ /d_TEST_SCOPE_ /d_TEST_CMPDIDX_
if errorlevel 1 goto ERROR
exospace fi rddmktst lib _dbfcdx, dbfcdx
if errorlevel 1 goto ERROR
del *.obj
rddmktst cdxcl53.prg dbfcdx
clipper cdxcl53 /m/n/w/es2
if errorlevel 1 goto ERROR
exospace fi cdxcl53 lib _dbfcdx, dbfcdx
if errorlevel 1 goto ERROR

rem DBFNTX
clipper rddmktst /m/n/w/es2
if errorlevel 1 goto ERROR
exospace fi rddmktst lib _dbfcdx, dbfcdx
if errorlevel 1 goto ERROR
del *.obj
rddmktst ntxcl53.prg dbfntx
clipper ntxcl53 /m/n/w/es2
if errorlevel 1 goto ERROR
exospace fi ntxcl53 lib _dbfcdx, dbfcdx
if errorlevel 1 goto ERROR

rem DBFCDX for ADS RDD
clipper rddmktst /m/n/w/es2/d_TEST_ADS_ /d_TEST_SCOPE_ /d_TEST_CMPDIDX_
if errorlevel 1 goto ERROR
exospace fi rddmktst lib _dbfcdx, dbfcdx
if errorlevel 1 goto ERROR
del *.obj
rddmktst adscl53.prg dbfcdx
clipper adscl53 /m/n/w/es2
if errorlevel 1 goto ERROR
exospace fi adscl53 lib _dbfcdx, dbfcdx
if errorlevel 1 goto ERROR

:ERROR
if exist *.obj del *.obj
