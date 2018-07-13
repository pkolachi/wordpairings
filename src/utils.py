#!/usr/bin/env python3

from __future__ import print_function ; 
from collections import defaultdict ; 
from bz2  import BZ2File  ;
from gzip import GzipFile ;
#from lzma import LZMAFile ;
import io ;
import os ;
import sys ; 

FHANDLERS = defaultdict(lambda : io.open, \
             {'.bz2': BZ2File,  \
              '.gz' : GzipFile, \
              '.xz' : GzipFile
             } ) ;
def lines_from_file(filename=''):
  global FHANDLERS ; 
  filename  = filename.strip();
  if filename:    
    _, ext   = os.path.splitext(filename) ;
  else:            # filename is empty. read from STDIN
    filename = sys.stdin.fileno() ;
    ext = '';      # a fake key that does not exist in FHANDLERS 
  with FHANDLERS[ext](filename, mode='rb') as iostream :
    #iobuf = io.BufferedReader(iostream) ;
    iobuf = iostream ;
    for line in iobuf :
      yield line.decode('utf-8').rstrip('\n') ; 
  return ;

def lines_to_file(lines, filename=''):
  global FHANDLERS ;
  filename  = filename.strip();
  if filename:
    _, ext   = os.path.splitext(filename) ;
  else:            # filename is empty. write to STDOUT
    filename = sys.stdout.fileno() ;
    ext = '';      # a fake key that does not exist in FHANDLERS 
  with FHANDLERS[ext](filename, mode='wb') as iostream :
    iobuf = iostream ; 
    #iobuf = io.BufferedWriter(iostream) ;
    for idx, line in enumerate(lines, start=1) :
      line = u"{0}\n".format(line.strip()).encode('utf-8') ;
      iobuf.write(line) ;
    print("Wrote {0} lines of content to <{1}>".format(idx, filename), file=sys.stderr)
  return ;

