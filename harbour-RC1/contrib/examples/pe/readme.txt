/*
 * $Id$
*/
Known bugs:
----------
1) It requires files separated with CR/LF pairs
2) NextWord() doesn't work correctly
3) If text contains color escape codes then deleting or inserting
 of characters doesn't work correctly in the line that contains it

To fix:
------
1) All TAB characters are replaced with spaces at startup - if edited file is
  very large and contains many TABs then it can take a vary long time - TAB
  characters should be left unchanged and interpreted during editing
2) It reformats whole text at startup - again for a very long text it can
  take too much time
3) Text buffer shold be reallocated dynamically
4) Remove static table of created editors
