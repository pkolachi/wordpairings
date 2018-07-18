#!/usr/bin/env python3

from __future__ import print_function, division ; 

from functools import partial ; 
from operator import itemgetter ; 
import os.path ; 
import re ; 
import sys ; 

import treeops as AT ; 
import utils ; 
import conll_utils as cu ; 

def align_treeset(trees, traversal=None, langs=None) :
  ctrees    = map(itemgetter(1), trees) ; 
  tree_seqs = [] ;
  if not traversal : 
    traversal = AT.preorder ; 
    
  for lidx, tree in enumerate(ctrees, start=1) : 
    tree_seqs.append( traversal(tree) ) ;
  print("="*100) ;
  for l, seq in zip(langs, tree_seqs) :
    print("{0}\t{1}".format(l, ' '.join(seq))) ;
  print("="*100) ; 
  print("\n\n")  ; 

  pairsc = len(trees)*(len(trees)-1) // 2 ;
  distm  = [0 for _ in range(pairsc)] ; 

  #for tpair in it.combinations(tree_seqs, 2) :
  #  distm.append( lcs(tpair[0], tpair[1]) ) ;

  return distm ; 

def align_treebank(*conllinps) :
  cu.FIELDS = cu.CONLLU_COLUMNS ; 
  treebanks = [None for _ in conllinps] ;
  exLang    = lambda trname : re.findall('^([a-z_]+?)-', trname)[0] ;
  langs     = list(map(exLang, [os.path.split(inp)[1] for inp in conllinps])) ;
  for lidx, inp in enumerate(conllinps) : 
    clines  = utils.lines_from_file(inp) ;
    treebanks[lidx] = [sent for sent in cu.sentences_from_conll(clines) ] ;
  paired_treebanks = list(zip(*treebanks)) ;
  align_treeset_   = partial(align_treeset, langs=langs) ; 
  alignments       = map(align_treeset_, paired_treebanks) ;
  for align in alignments : 
    pass ; 

if __name__ == '__main__':
  align_treebank(*sys.argv[1:]) ;
