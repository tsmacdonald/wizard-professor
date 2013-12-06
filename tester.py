from __future__ import division

import re
import sys


def compare(source, target):
  with open(source) as s, open(target) as t:
    original_words = re.findall('\w+', " ".join(s).lower())
    corrected_words = re.findall('\w+', " ".join(t).lower())
    total = len(original_words)
    wrong = 0
    assert total == len(corrected_words)
    for i in range(total):
        if original_words[i] != corrected_words[i]:
            wrong += 1
    return (total - wrong) / total

def print_usage():
    print "%s original-file corrected-file"%sys.argv[0]

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print_usage()
    else:
        print "%f%% right"%(compare(sys.argv[1], sys.argv[2]) * 100)
