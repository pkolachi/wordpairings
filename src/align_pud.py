#!/usr/bin/env python3

def align_treeset(*trees, traversal=None) : 
  tree_seq = {} ;
  if not traversal : 
    traversal = Tree.preorder ; 

  for lidx, tree in enumerate(trees, start=1) : 
    tree_seqs[lidx] = traversal(tree) ; 

  return multiLcs(tree_seqs) ;
    

def align_treebank(*conllinps) :
  treebank = {} ; 
  for lidx, inp in enumerate(conllinps, start=1) : 
    treebank[lidx] = [sent for sent in sentences_from_conll(inp) ] ;

  paired_treebanks = zip(*treebank.values() ) ;
  alignments = map(align_treeset, paired_treebanks) ;
  return alignments ; 


