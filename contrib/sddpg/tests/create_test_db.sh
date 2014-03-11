#!/bin/bash

echo enter password: test_password
sudo -u postgres  createuser test -P
echo create test_db, owner user: test
sudo -u postgres createdb test_db -O test

