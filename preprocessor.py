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
    clean_line = sanitize(line) # remove comments 

    if clean_line:
      # throw error on invalid characters 
      for char in invalid_characters:
        if char in clean_line:
          sys.exit("Invalid character: {0}. Found on line: {1}".format(char, i))

# removes comments from the line 
def sanitize(line):
  if comment_symbol in line:
    regex_pattern = "^(.*?)#.*|.*"
    match = re.match(regex_pattern, line)
    sans_comments = match.group(1)
  else:
    sans_comments = line
  return sans_comments.rstrip()

# main 
if __name__ == "__main__":

  # sanitize usage 
  if len(sys.argv) != 2:
    sys.exit("usage: python preprocessor.py <input.mandala>")

  # open the file 
  try:
    infile = open(sys.argv[1], 'r')
  except IOError:
    sys.exit("Cannot read input file.")

  # get the path 
  filename = os.path.basename(infile.name)
  directory = os.path.dirname(infile.name)

  # get the filename without extension 
  if filename.lower().endswith(extensions):
    new_filename = os.path.splitext(filename)[0] 
  else:
    sys.exit("Input file must have Mandala file extension.")

  # process the input file 
  output = process(infile)

  outfile = open(directory + new_filename + ".proc.md", 'w')
  outfile.write(output.getvalue())