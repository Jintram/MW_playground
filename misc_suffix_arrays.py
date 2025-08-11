

# below is written by ChatGPT 4.1

def build_suffix_array(input_string, return_full=True):
    """
    Constructs the suffix array for the given input string.
    Returns a list of starting indices of sorted suffixes.
    """
    suffixes = [(input_string[i:], i) for i in range(len(input_string))]
    suffixes.sort()
    if return_full:
        return [suff for (suff, idx) in suffixes], [idx for (suff, idx) in suffixes]
    else:
        return [idx for (suff, idx) in suffixes]

# Example usage:
# sa = build_suffix_array("banana")
# build_suffix_array("abcabc")