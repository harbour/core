@echo off
make -fhbodbc.b32
implib ..\..\libs\b32\odbc32.lib odbc32.def
