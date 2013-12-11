* Created `tester.py`, which compares the spelling differences between two files
* Created a test misspelled document (several, in fact, of which only `holbrook-very-short-wrong` was of manageable size)
* Tried a new corpus (Norvig), which had better performance
* Tested 24 different combinations of `*alpha*` and `*edit-distance*` values (see `BATCH-PROCESS`), finding `3`/`.0001` to be the best.
* Final version performs within a percent of LibreOffice (`holbrook-very-short-lowriter`), and improved by approximately 2% from the original version
