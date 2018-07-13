#!/usr/bin/env python3

from __future__ import print_function, division, unicode_literals ;  
import utils ; 

from array import array ; 
from collections import defaultdict as defdict, namedtuple ; 
from copy import deepcopy ; 
from operator    import itemgetter ; 
import itertools as it ; 

from functools import reduce, partial ; 
from operator import mul ; 
import io ; 
import sys ;

"""
Version log:  
  V0:   simple merge_pairs + connected_components 
  V0.1: use tables only for reprs and keep components and rank in list
    - I am already crushing the RAM on the MAC sticking with simple list/dict
    - This when I use 100 languages with max 10K vocabulary items
    - in this version, edges are represented using hashtable of pairs 
      and nodes using a hashtable of descriptions
    - in a typical instance, (106 from 4950) pairs, graph size is (469513, 3387090) 
    - this is a reasonably sparsely populated graph with not too huge a 
      vocabulary (say 4*Europarl vcb for a language
  V0.2: simply use list.extend to modify components; that way new lists are
      not created for every edge. That way, it doesn't matter if the components
      are built in memory or not. I tested it for upto 20K vocab items on
      set of 100 languages and already from 10K vocab items, it takes more
      time to generate the random alignment matrices then it does to do the
      component construction.
  V0.3: a few changes to optimize the code to run for all opus dics in 10mins.
      high-level: all dict structures are replaced with simple lists and further
      using arrays to improve performance 
      dictionary reading is lazy; i.e. IO operation happens only one to build
      merged graph.
      vocabulary for all languages is stored using a csc-like table 
      structures in connected_components are cleared regularly to make sure
      that memory is never blocked due to redundant items in the structures
  V0.4: TODO: Python2 and Python3 are not exactly the same in terms of the
      output. This is clearly observed when run on all dics both in terms of
      number of nodes in graph and edges and furthermore in number of clusters.
      Hopefully this is not a bug and an artefact of using str/unicode across
      both platforms. Also, using numpy is an option but at the moment I do not
      see the need for it- Py2 consumes roughly 10G+ and Py3 around 6G+. 
      What might be "urgent" is to implement the data-types defined using
      namedtuple() & indexdict as custom collections.
      I don't see any point in implementing disjoint set forests. The 
      weighted union + disjoint sets seems to server the purpose for reasonable
      sizes of the graphs. 

"""

# UGLY HACK 
if sys.version_info >= (3,0) :
  indexdict = lambda: defdict(it.count().__next__) ;
  print     = partial(print, flush=True) ;
elif sys.version_info < (3,0) :
  indexdict = lambda: defdict(it.count().next) ; 
else:
  print("Which version of python are you using mate!!", file=sys.stderr) ; 

WPairing  = namedtuple('WordPairing', ['srcl', 'tgtl', 'dict'] ) ;
# a csc-like format for storing vocabulary for multiple languages
VTable    = namedtuple('VocabTable',  ['langs', 'offsets', 'words'] ) ;
# a coo-like format for storing edge matrices along with node descriptions
Graph     = namedtuple('Graph',  ['nodes', 'eorigs', 'etargs'] ) ;
WGraph    = namedtuple('WGraph', ['nodes', 'eorigs', 'etargs', 'ews'] ) ;
Component = namedtuple('Component', ['nodes', 'strength'] ) ;

def read_dictionary(srclang, tgtlang, dictfile, delim='', lc=True) :
  entries = utils.lines_from_file(dictfile) ;
  if lc :
    entries = (line.lower() for line in entries) ;
  delim   = delim if delim else None ; 
  entries = (item.strip()      for item in entries ) ;
  entries = (item.split(delim) for item in entries ) ;
  # filter things that do not fit the format 
  entries = (item for item in entries if len(item) in [2,3] ) ;
  entries = ((item[0], item[1], (1 if len(item) < 3 else float(item[2]))) \
               for item in entries) ;
  return WPairing(srclang, tgtlang, entries ) ;

def merge_pairs(pairingsList) :
  #pairingsList = pairingsList[:50] ; 
  # each item in the list is a WPairing object
  # this creates a full graph in memory of all the word pairs
  langs  = [lang for bidic in pairingsList for lang in (bidic.srcl, bidic.tgtl)] ;
  langs  = [l  for l  in sorted(set(langs))] ;
  lpairs = [(bidic.srcl,bidic.tgtl) for bidic in pairingsList] ;
  lpairs = [lp for lp in sorted(set(lpairs))] ;
  print("Loading {0} dictionaries for {1} languages and {2} language-pairs".format(len(pairingsList), len(langs), len(lpairs))) ;

  lvocab  = [indexdict()   for _ in langs ] ; 
  lpedges = [defdict(int)  for _ in lpairs] ; 
  for pidx, bidic in enumerate(pairingsList, start=1) :
    if not pidx%100 : 
      print("{0}...".format(pidx), end=' ', file=sys.stderr) ;
    lsrcidx = langs.index(bidic.srcl) ;
    ltgtidx = langs.index(bidic.tgtl) ; 
    pairidx = lpairs.index((bidic.srcl, bidic.tgtl)) ; 
    for entry in bidic.dict :
      w    = float(entry[2]) if len(entry) > 2 else 1 ; 
      sidx = lvocab[lsrcidx][entry[0]] ; 
      tidx = lvocab[ltgtidx][entry[1]] ;
      lpedges[pairidx][(sidx, tidx)] += w ;
      lvocab[lsrcidx][entry[0]] = sidx ; 
      lvocab[ltgtidx][entry[1]] = tidx ; 
  print("{0} !!".format(pidx), file=sys.stderr) ;

  offsets  = [0]*len(langs) ;
  for lidx,_ in enumerate(langs[1:],start=1) :
    offsets[lidx] = offsets[lidx-1] + len(lvocab[lidx-1]) ;
  vtable   = [wd for lt in lvocab for wd,_ in sorted(lt.items(), key=itemgetter(1))] ;
  vocab    = VTable(langs, offsets, vtable) ; 
  pairoffs = lambda X: (offsets[langs.index(X[0])], offsets[langs.index(X[1])]) ;
  poffsts  = list(map(pairoffs, lpairs)) ;
  rids     = array('i', (lnk[0]+poffsts[lpix][0] for lpix,tbl in enumerate(lpedges) for lnk in tbl)) ;
  cids     = array('i', (lnk[1]+poffsts[lpix][1] for lpix,tbl in enumerate(lpedges) for lnk in tbl)) ;
  ws       = array('f', (tbl[lnk]                for lpix,tbl in enumerate(lpedges) for lnk in tbl)) ; 
  return WGraph(vocab, rids, cids, ws ) ; 

def connected_components(congraph, compress=True) :
  """
  Assumes UNDIRECTED WEIGHTED graph.
  Implementation of connected components
  From Chapter 21 of Cormen on Algorithms 
  It uses the weighted-union heuristic 
  TODO: check if the implementation also matches the description of disjoint
  set forests. I "think" one can do that by using a k-length key to index
  everything i.e. replace int in reprs with a tuple of length k 
  that should simulate the effect of having a tree to represent the
  different sets. 
  
  #'''
  #repeat  = it.repeat ;       # make local variables to reduce time 
  #chain   = it.chain ; 
  #update  = dict.update ; 
  #'''
  nidmin = min(i for _ in zip(congraph.eorigs, congraph.etargs) for i in _) ;
  nidmax = max(i for _ in zip(congraph.eorigs, congraph.etargs) for i in _) ;
  nodec  = nidmax-nidmin+1 ; 
  """

  nodec = len(congraph.nodes.words) ;  # number of nodes in graph
  edgec = len(congraph.eorigs) ;       # number of edges in graph
  reprc = len(congraph.nodes.words) ;  # keeps track of number of components
  print("Size of the graph: ({0}, {1})".format(nodec, edgec), file=sys.stderr);
  # all nodes are repr. as indices ; so lists are best
  reprs = array('i',    (i for i in range(nodec))) ; # one representative per node
  linkc = array('f',    (0 for i in range(reprc))) ; # one score per component 
  # components and attest/rank are simple lists of size N
  comps = [array('i', [i]) for i in range(nodec)] ;  # one array per component tracking nodeids

  bsize = 1000000 ;
  csize = reprc ;    # keep track of size of components list seperately
  cf    = 0.80 ;
  rf    = 5 ; 
  edgesList  = ((i,j,w) for i,j,w in zip(congraph.eorigs, congraph.etargs, congraph.ews)) ;
  for edidx, edge in enumerate(edgesList, start=1) :
    si, ti, w  = edge ; 
    rsi, rti   = reprs[si], reprs[ti] ; 
    if rsi != rti :
      # new representative will be rsi
      # if rti is bigger component, swap si and ti
      if len(comps[rsi]) < len(comps[rti]) :
        si , ti  = ti , si  ;
        rsi, rti = rti, rsi ;
      comps[rsi].extend(comps[rti]) ; 
      # reset the representative for all items that have now been merged 
      for item in comps[rti] :
        reprs[item] = rsi ;
      linkc[rsi] += linkc[rti] + w ; 
      reprc     -= 1 ;                     # #reprs reduced by 1
      csize += len(comps[rti]) ;  # #duplicate elements stored in components
    else:
      linkc[rsi] += w ; 

    if not edidx % bsize : 
      print("({0})...".format(edidx), end=' ', file=sys.stderr) ;
    
    if compress and reprc <= cf*len(linkc) :
      # about 20% components have been merged . a good time to 
      # compress data-structures 
      print("[-C:{0},{1}-]".format(reprc/len(linkc), csize/nodec), end=' ', file=sys.stderr) ;
      nrepr = sorted(set(reprs)) ;
      rmap  = dict((r,newr) for newr,r in enumerate(nrepr)) ; 
      reprs = array('i', ( rmap[ridx] for ridx in reprs)) ; 
      linkc = array('f', (linkc[newr] for newr in nrepr)) ; 
      comps = [comps[newr]            for newr in nrepr] ;
  print("({0}). !!".format(edidx), file=sys.stderr) ; 

  protos = sorted(set(reprs), key=lambda X: linkc[X]) ;
  for pid in protos :
    yield Component(comps[pid], linkc[pid]) ; 

def nodes_from_component(nodetbl, components) :
  # end offsets for language 
  endoffs = nodetbl.offsets[1:] + [len(nodetbl.words)]  
  loffs   = [(lang, soff, eoff) for lang, soff, eoff in zip(nodetbl.langs, nodetbl.offsets, endoffs)] ;
  lcidx   = dict((i,l) for l,s,e in loffs for i in range(s,e)) ;
  f_key   = itemgetter(0) ;
  for comp in components :
    nodes = [(lcidx[nid], nodetbl.words[nid]) for nid in comp.nodes] ;
    nodes = sorted(nodes, key=f_key) ; 
    ngrps = it.groupby(nodes, key=f_key) ;
    ngrps = [(grp[0], tuple(sorted(nd[1] for nd in grp[1]))) for grp in ngrps] ;
    yield (comp.strength, ngrps, len(nodes)) ; 

def demo() : 
  """
  Use functions from random module to simulate what 
  bilingual dictionaries would look like. This allows to test how well the
  method scales across #languages vs #lang.pairs 

  At the moment, the simulated word pairings are too dense to test
  how the method would work in real cases. So, switching to a different
  demo case.
  
  Also this is extremely bad way of generating these random matrices.
  Look into parameterized ways of generating graphs that simulate 
  """
  import random as r ;  # most of the values are generated using random
  langc    = 100 ; 
  maxVocab = int(sys.argv[1]) ;
  dF       = 5 ;   # dictionary is always a constant factor of mixed vocabulary
  vcbsize  = [int(r.random()*maxVocab) for _ in range(langc)] ;
  vcbsstr  = ' '.join(str(i) for i in vcbsize) ;
  print("Vocabulary sizes for {0} languages: {1}".format(langc, vcbsstr), file=sys.stderr) ;

  pairc_max = int(langc*(langc-1) / 2) ;   # max number of pairwise pairings possible
  # to make sure that we do not generate data 
  # for too many language pairs or two few pairs
  # max pairs should be constant factor of number of languages (here 2)
  # intuition: pivot systems with constant number of pivot languages is 
  #            always feasible
  while True :
    N = int(r.random()*pairc_max + 1) ; # randomly decide how many pairings you will use
    if 1.5*langc < N < 2*langc : 
      break ; 

  print("Choosing {0} language pairs from {1} possible pairs".format(N, pairc_max), file=sys.stderr) ;
  paircomb  = list(it.combinations(range(langc), 2)) ;
  langpairs = r.sample(paircomb, N) ;
  wordpairs = [ ] ;

  langs = [] ;
  for pair in langpairs :
    src, tgt = pair ; 
    srcvcb, tgtvcb = vcbsize[src], vcbsize[tgt] ; 
    dSmax = srcvcb*tgtvcb ; 
    dS    = (srcvcb+tgtvcb)*dF ;
    # if dS is more than possible combinations; slice them
    dS    = int(0.90*dSmax) if dS > dSmax else dS ; 
    pairing = r.sample(list(it.product(range(srcvcb), range(tgtvcb))), dS) ;
    pairs = [("W_{0}".format(srci), "W_{0}".format(tgti)) \
                     for srci,tgti in pairing] ;
    langs.append("L_{0}".format(src)) ;
    langs.append("L_{0}".format(tgt)) ; 
    wordpairs.append( WPairing("L_{0}".format(src), 
                                  "L_{0}".format(tgt), 
                                   pairs) ) ;

  langs  = sorted(set(langs)) ;
  mgraph = merge_pairs(wordpairs) ; 
  comps  = connected_components(mgraph) ;
  groups = sorted(nodes_from_component(mgraph.nodes, comps), key=itemgetter(0, 3), reverse=True) ;
  for grps_ in groups :
    w     = grps_[0] ;
    ngrps = grps_[1] ; 
    maxw  = reduce(mul, (len(d) for _,d in ngrps if len(d)!=0), 1) ;
    langentry = [u"{0}:{1}".format(l,','.join(wds)) for l,wds in ngrps if len(wds)] ;
    entrysize = [u"{0}:{1}".format(l,len(wds))      for l,wds in ngrps if len(wds)] ;
    cmpstr    = u"{0}\t{1}\t{2}".format(w, maxw, u' ||| '.join(langentry)) ;
    cmpstr    = u"{0}\t{1}\t{2}".format(w, maxw, u' ||| '.join(entrysize)) ;
    if hasattr(cmpstr, 'decode') : 
      cmpstr = cmpstr.encode('utf-8') ;
    print(cmpstr) ; 
  return ; 

def demo2() :
  f_stream  = sys.argv[1] if len(sys.argv) > 1 else '' ; 
  lines     = (line for line in utils.lines_from_file(f_stream)) ;
  entries   = map(lambda X: X.rstrip(u'\n').split(u'\t'), lines) ; 
  entries   = list(entries) ;                    # construct the list in memory
  langs     = [] ;
  langpairs = [] ; 
  pairings  = defdict(list) ;
  uniqfuns  = [] ; 

  langpairs = sorted(set((i[1],i[2]) for i in entries)) ; 
  uniqfuns  = sorted(set(i[0] for i in entries)) ;
  for item in entries : 
    pairings[(item[1],item[2])].append((item[3], item[4])) ;

  wordpairs = [] ;
  for lp in langpairs :
    langs.append(lp[0]) ;
    langs.append(lp[1]) ;
    wordpairs.append( WPairing(lp[0], lp[1], pairings[lp]) ) ;

  langs  = sorted(set(langs)) ;
  mgraph = merge_pairs(wordpairs) ; 
  comps  = [cluster for cluster in connected_components(mgraph)] ;
  groups = sorted(nodes_from_component(mgraph.nodes, comps), key=itemgetter(0, 3), reverse=True) ; 
  print("Number of unique functions from the lexicon: {0}".format(len(uniqfuns)), file=sys.stderr) ;
  print("Total number of clusters found: {0}".format(len(clusters)), file=sys.stderr) ;
  for grps_ in groups :
    w     = grps_[0] ;   
    ngrps = grps_[1] ;
    maxw  = reduce(mul, (len(d) for _,d in ngrps if len(d)!=0), 1) ;
    langentry = [u"{0}:{1}".format(l,','.join(wds)) for l,wds in ngrps if len(wds)] ;
    entrysize = [u"{0}:{1}".format(l,len(wds))      for l,wds in ngrps if len(wds)] ;
    cmpstr    = u"{0}\t{1}\t{2}".format(w, maxw, ' ||| '.join(langentry)) ;
    if hasattr(cmpstr, 'decode') :
      cmpstr = cmpstr.encode('utf-8') ; 
    print(cmpstr) ;
  return ;

if __name__ == '__main__' :
  import cProfile, pstats ;

  #run_fun = demo ;   
  run_fun = demo2 ; 
  try :
    cProfile.run("run_fun()", "profiler") ;
    programStats = pstats.Stats("profiler") ;
    programStats.sort_stats('tottime').print_stats() ;
  except KeyboardInterrupt :
    programStats = pstats.Stats("profiler") ;
    programStats.sort_stats('tottime').print_stats() ;
    sys.exit(1) ;

