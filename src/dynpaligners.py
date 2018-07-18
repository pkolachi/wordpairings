#!/usr/bin/env python3

""" 
This module implements different alignment methods primarily based on
dynamic programming. 
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

def needleman_wunsch(seq1, seq2, edit_matrix) : 
  m = len(seq1) + 1 ; 
  n = len(seq2) + 1 ; 
  C = [[0 for j in range(n+1)] for i in range(m+1)] ; # cost matrix 
  for j in range(1, n+1) :
    C[0][j] = C[0][j-1] + edit_matrix[INSERT][seq2[j].desc] ; 
  for i in range(1, m+1) :
    C[i][0] = C[i-1][0] + edit_matrix[DELETE][seq1[i].desc] ; 
    for j in range(1, n+1) :
      s_sub = C[i-1][j-1] + edit_matrix[REPLACE][seq1[i].desc,seq2[j].desc] ;
      s_del = C[i-1][j]   + edit_matrix[DELETE][seq1[i].desc] ; 
      s_ins = C[i][j-1]   + edit_matrix[INSERT][seq2[j].desc] ; 
      C[i][j] = max(s_sub, s_del, s_ins) ; 

  return [C[m][j] for j in range(0, n+1)] ;

def levenstein(seq1, seq2) :
  return x ; 

# https://en.wikipedia.org/wiki/Edit_distance#Improved_algorithms
# https://en.wikipedia.org/wiki/Levenshtein_distance
# https://github.com/universal-automata/liblevenshtein
# https://en.wikipedia.org/wiki/Levenshtein_automaton
