#!/usr/bin/env python3

from __future__ import print_function, unicode_literals ;  
from merge_wpairings import * ;
import utils ; 

import argparse ; 
import itertools as it ; 
import os ; 

def load_all_dics(opus_dir, sel_pairs=None, preproc=False) : 
  select = lambda X: (X in sel_pairs if sel_pairs else True) 
  for dirpath, dirsList, filesList in os.walk(opus_dir) : 
    if dirpath.endswith('dic') : 
      # all files in there are dictionary files
      for filename in filesList:
        if filename.startswith('.') : 
          # add this just to be safe :) 
          continue ; 
        paircode   = filename.split('.', 1)[0] ; 
        srcc, tgtc = paircode.split('-') ; 
        filepath   = os.path.join(dirpath, filename) ; 
        if select( (srcc, tgtc) ) :
          yield read_dictionary(srcc, tgtc, filepath, delim=' ', lc=preproc) ;

def main(runenv) :
  if not runenv.root_dir : 
    print("OPUS directory not specified. Can't continue. Exiting ..", 
        file=sys.stderr) ; 
    sys.exit(1) ; 

  if runenv.all_langs : 
    sel_lpairs = None ;
  elif runenv.f_lpairs : 
    # runenv.chosen_pairs specify which langs are relevant
    sel_lpairs = [tuple(lpair.split( )) 
                     for lpair in utils.lines_from_file(runenv.f_lpairs) ] ;
  else :
    # pick languages specified using -l option and select all pairs
    sel_lpairs = list( it.combinations(sorted(runenv.langs), 2) ) ;

  dictLs = [dic  for dic in load_all_dics(runenv.root_dir, sel_lpairs, preproc=True)] ;
  merged_graph = merge_pairs(dictLs) ; 
  print("Size of the merged graph\t#nodes:{0}, #edges:{1}".format(len(merged_graph.nodes.words), len(merged_graph.eorigs))) ;

  langs = merged_graph.nodes.langs ; 
  loffs = merged_graph.nodes.offsets ; 
  for lidx, lang in enumerate(langs) :
    start  = loffs[lidx] if lidx > 0 else 0 ; 
    end    = loffs[lidx+1] if lidx < len(langs)-1 else len(merged_graph.nodes.words) ;
    vcblen = end-start ;
    print("Vocabulary of Lang {0}:\t{1}".format(lang, vcblen)) ;

  clusters = (comp for comp in connected_components(merged_graph)) ; 
  clusters = list(clusters) ;
  print("Total number of clusters found: {0}".format(len(clusters))) ;
  groups   = sorted(nodes_from_component(merged_graph.nodes, clusters), key=itemgetter(0, 2)) ;
  output   = (u'\t'.join(u"{0}:{1}".format(l,u','.join(wds)) for (l,wds) in grp) for _,grp,_ in groups) ; 
  utils.lines_to_file(output, runenv.output) ; 
  return True ; 

def cmdline() :
  argparser = argparse.ArgumentParser(
                prog=sys.argv[0], 
                description='Demo for multiple alignment using dictionaries from the OPUS database') ;
  argparser.add_argument(
                '-d', '--tbd', 
                dest='root_dir', required=True, 
                help='Directory where the OPUS corpora are stored. (NOTE: dir. structure resembles the OPUS structure') ;
  argparser.add_argument(
                '--all', nargs='?', 
                dest='all_langs', default=False, const=True, 
                help='should all language pairs be included ') ;
  argparser.add_argument(
                '-l', '--langs', nargs='+', 
                dest='langs', default=['en', 'sv'], 
                help='languages to be included (all pairs will be included)') ;
  argparser.add_argument(
                '--lpairs-file', 
                dest='f_lpairs', default='', 
                help='File with language pairs to be included (one language pair per line)') ; 
  argparser.add_argument(
                '-o', '--output',
                required=False,
                dest='output', default='',
                help='output file to write the lexicon (default: STDOUT)') ;
  return argparser ;

if __name__ == '__main__' :
  runenv = cmdline().parse_args(sys.argv[1:]) ;
  sys.exit( int(main(runenv)) ) ; 
  
