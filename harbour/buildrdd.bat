@echo off
bin\harbour source\rdd\rddsys /n /osource\rdd /iinclude
bin\harbour source\rdd\dbfntx0 /n /osource\rdd /iinclude
make -frdd.b32
