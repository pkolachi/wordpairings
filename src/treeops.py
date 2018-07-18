#!/usr/bin/env python3

from globalimports import * ;
import random_utils as ru ;
import conll_utils  as cu ;

class Tree(object) :
  def __init__(self, root, children) :
    self._root     = root ; 
    self._children = [child for child in children] ;

  def __repr__(self, delim=',') :
    if not delim : delim = ',' ;
    crepr = ','.join( map(repr, self._children) ) ;
    return "{0}[{1}]".format(repr(self._root), crepr) ;

  def __str__(self) : 
    return ; 

  def __cmp__(self, tree2) : 
    return ; 

  def preorderT(self, descr=None) : 
    ndescr = [] ;
    ndescr.append( descr(self.root) ) ; 
    for subt in self.children : 
      ndescr.extend( subt.preorderT(descr) ) ; 
    return ndescr ; 

  def postorderT(self, descr=None):
    ndescr = [] ;
    for subt in self.children :
      ndescr.extend( subt.postorderT(descr) ) ;
    ndescr.append( descr(self.root) ) ;
    return ndescr ;

def preorder(ctree) :
  descr  = lambda node : "|".join( [
                            node['id'],
                            node['postag'],
                            node['deprel'],
                            node['deprel'].split(':')[0],
                            node['head']
                            ] )
  root      = [node for node in ctree if node['head'] == '0'][0] ; 
  traversal = [] ; 
  traversal.append(root) ;
  cur   = 0 ; 
  while cur <= len(traversal) :
    item   = traversal[cur] ; 
    child  = [node for node in ctree if node['head'] == item['id']] ; 
    schild = sorted(child, key=lambda X: X['deprel'], reverse=True) ; 
    for node in schild :
      traversal.insert(cur+1, node) ; 
    cur   += 1 ; 
  return [_ for _ in map(descr, traversal)] ; 

def postorder(ctree) :
  descr  = lambda node : "{0}|{1}|{2}|{3}".format(
                            node['postag'], 
                            node['deprel'],
                            node['deprel'].split(':')[0],
                            node['id']
                          )
  root      = [node for node in ctree if node['head'] == '0'][0] ; 
  traversal = [] ; 
  traversal.append(root) ;
  while len(traversal) <= len(ctree) :
    item   =  traversal[0] ; 
    child  = [node for node in ctree if node['head'] == item['id']] ; 
    schild = sorted(child, key=lambda X: X['deprel'], reverse=True) ; 
    for node in schild : 
      stack.insert(0, node) ;
  return [_ for _ in map(descr, stack)] ; 

