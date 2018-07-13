#!/usr/bin/env python3

""" 
This module implements different sequence alignment methods. 
All the methods are non-statistical in nature: i.e. given two sequences
they find one or many plausible alignments between the items in the 
sequences and a score/number associated with each alignment

Known techniques for this include 
  a) Longest Common Subsequence method
  b) Edit-distance method- notably Levenstein distance

For now, we restrict ourselves to alignment between two and only two sequences.

"""

# Longest Common Subsequence
## -- quadratic dynamic programming ++ traceback
## -- read out all LCSs
## -- generalized alignment structure ?? 
## -- extension to more than 2 sequences ??
### https://en.wikipedia.org/wiki/Longest_common_subsequence_problem
### https://en.wikipedia.org/wiki/Longest_increasing_subsequence
### https://en.wikipedia.org/wiki/Hunt%E2%80%93McIlroy_algorithm
### https://en.wikipedia.org/wiki/Hirschberg%27s_algorithm
### https://en.wikipedia.org/wiki/Method_of_Four_Russians

def lcs(seq1, seq2) :
  """ Longest common subsequence for two inputs """
  # calculate prefixes of length [1..n]
  lenseq1 = len(seq1) + 1 ; 
  lenseq2 = len(seq2) + 1 ;
  C = defaultdict(lambda : 0, [((i,j),0) for i in range(lenseq1) \
                                            for j in range(lenseq2)]);
  for i in range(1, lenseq1) :
    for j in range(1, lenseq2) :
      if seq1[i] == seq2[j] :
        X[(i,j)] = X[(i-1, j-1)] + 1 ; 
      else :
        X[(i,j)] = max(C[(i,j-1)], C[(i-1,j)]) ;
  return C[lenseq1-1, lenseq2-1] ;

# Edit Distance methods
## 

def levenstein(seq1, seq2) :
  return x ; 

# https://en.wikipedia.org/wiki/Edit_distance#Improved_algorithms
# https://en.wikipedia.org/wiki/Levenshtein_distance
# https://github.com/universal-automata/liblevenshtein
# https://en.wikipedia.org/wiki/Levenshtein_automaton
