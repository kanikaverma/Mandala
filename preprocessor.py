#! /usr/bin/python

# Author: Harsha Vemuri

import os 
import re
import sys  

# Find the best implementation available based on the platform 
try:
  from cStringIO import cStringIO
except:
  from StringIO import StringIO

invalid_characters = ('{', '}', '[', ']', ';')
comment_symbol = '#'
extensions = (".md", ".mndl", ".mandala")

def process(input_file):
  stack = [0]
  output = StringIO()
  newindent = False
  commented = False
  linejoin = False 

  for i, line in enumerate(input_file):
    print i, line 

if __name__ == "__main__":

  # sanitize usage 
  if len(sys.argv) != 2:
    sys.exit("usage: python preprocessor.py <input.mandala>")

  # open the file 
  try:
    infile = open(sys.argv[1], 'r')
  except IOError:
    sys.stderr.write("Cannot read input file." + '\n')
    sys.exit(1)

  # get the path 
  filename = os.path.basename(infile.name)
  directory = os.path.dirname(infile.name)

  # get the filename without extension 
  if filename.lower().endswith(extensions):
    new_filename = os.path.splitext(filename)[0] 
  else:
    sys.stderr.write("Input file must have Mandala file extension")

  # process the input file 
  output = process(infile)

  outfile = open(directory + new_filename + ".proc.md", 'w')
  outfile.write(output.getvalue())