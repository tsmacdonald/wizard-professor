# Wizard Professor
A groovy little spell-checker written in Common Lisp. (Wizard professors, of course, having to check the spells of their wizard students.)

## Usage

* Set up Quicklisp appropriately.
* Clone this repository to somewhere Quicklisp can find it.
* `(ql:quickload "wizard-professor")`
* `(in-package #:wizard-professor)`
* `(defparameter *frequencies* (train-trigram-model "corpus/brown-corpus-clean"))`
* `(correct-file *frequencies* "file you would like to correct" "optional output file")`

## Design Overview

There are two main components: the edit distance finder, and the trigram model.

Edit distance is, appropriately, implemented by the `EDIT-DISTANCE` function, which accepts two words and returns the edit distance between them (scores being defined by the `EDIT-SCORES` struct).

The trigram model is trained using a suitable corpus (currently the Brown corpus, although that has some problems discussed below). `TRAIN-TRIGRAM-MODEL` accepts a filename and returns a hash table of trigram counts of type `Map<String, Map<String, Map<String, Number>>>`. `counts["foo"]["bar"]["baz"]` is therefore the number of times "baz" occurred when preceded by "foo" and "bar".

This count is more useful when treated as a probability. `PROBABILITY` accepts three words and, using Laplace smoothing, returns the probability of the third word occurring when preceded by the first two.

The true meat of the logic occurs in `CORRECT2`, which corrects a word given a frequency table and its two predecessors. If the word is in the dictionary (`/usr/share/dict/words`) and is deemed sufficiently probable, it is marked correct. Otherwise (that is, it is either an orthographic or usage mistake), the following happens:

* A list of candidate replacements are made based on edit distance from the source word.
* The most probable of those candidates is selected.

Note that the precise edit distance is not a factor--with the current configuration, an edit distance of 1 is treated the same way as an edit distance of 6. This is because all small edit distances are likely to occur and need correcting, so it is best to put more emphasis on the language model.

## Testing

Testing was very *ad hoc*. A trigram was found in the corpus, and the third word would be misspelled with both a misspelled word and a real (but improbable) word. The parameters (particularly `*alpha*`) were tuned until satisfactory results were fonud.

## Results

Results vary wildly depending on the usefulness of the corpus. When trained with the Baum corpus, the spell-checker performed very well with Baumish phrases (even correcting "said the drab" to "said the crab"), but did not generalize very well. The Brown corpus was then used for a more comprehensive treatment of English, but still suffers notably ("the depressing prat is" => "the repressing writ is", not "the depressing part is"). "prat" being a real word, this means that the trigram model is not tuned well. This would hopefully be solved by more and better data.

Performance is also an issue. Apart from memoizing `EDIT-DISTANCE` and using a mature implementation (SBCL) of a compiled language (Common Lisp), correcting a four-word phrase takes around 64 seconds (although all hardware available was being used for statistical work at the same time, presumably slowing things down by a factor of three (a core split time between two R jobs and Wizard Professor).

## Potential Future Enhancements

* Using a better corpus. Or even sanitizing the current one better.
* Using Good-Turing smoothing instead of Laplace smoothing.
* Tweaking `*alpha*` and `*edit-tolerance*` further.
* Incorporating different edit distances when choosing between candidates (somehow finding p-values for each edit-distance and incorporating that into the trigram probability)
