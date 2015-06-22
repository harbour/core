Harbour Library Project Template
================================

* Build static lib:

   `hbmk2 hbtpl.hbp`

* Build dynamic lib:

   `hbmk2 -hbdyn hbtpl.hbp`

* Build and run sample and test code:

   ```
   cd tests
   hbmk2 sample
   ./sample
   hbmk2 test hbtest.hbc
   ./test
   ```

* Run sample and test code as scripts:

   ```
   cd tests
   hbrun sample.prg
   hbrun test.prg
   ```

* Use lib from command prompt ("dot prompt"):

   ```
   cd tests
   hbrun
   ```

   Type on the Harbour "dot prompt":

   ```
   ? HBTPL_MYCONSTANT
   ? hbtpl_MyPublicFunction()
   ```

[vszakats]
