@echo off
make -fhbodbc.b32
implib .\libs\b32\odbc32.lib source\odbc\odbc32.def
