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

invalid_characters = (';', '?', '~') # characters not in the language 
comment_symbol = '#' # character for commenting 
blockcomment = ['/#', '#/']
extensions = (".mndl", ".mandala") # file extensions for the language 

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

      stripped_line = clean_line.lstrip()

      if len(stripped_line) > 1 and blockcomment[0] == stripped_line[:2]:
        commented = True

      if commented:
        if len(clean_line) > 1 and blockcomment[1] == clean_line[-2:]:
          commented = False 

      else:

        if not linejoin:
          wcount = len(clean_line) - len(clean_line.lstrip(' '))

          if newindent:
            if wcount > stack[-1]:
              stack.append(wcount)
              newindent = False
            else:
              sys.exit("Indentation error on line {}".format(i))

          if wcount > stack[-1]:
            print clean_line
            sys.exit("Indentation error on line {}".format(i))

          else:
            while wcount < stack[-1]:
              clean_line = "};\n" + clean_line
              stack.pop()
            if wcount != stack[-1]:
              sys.exit("Indentation error on line {}".format(i))

        if clean_line[-1] == ':':
          newindent = True
          clean_line = clean_line + "{\n"
          
        elif clean_line[-1] == "\\":
          linejoin = True
          clean_line = clean_line[:-1]

        else:
          linejoin = False 
          clean_line = clean_line + ";\n"
        
        output.write(clean_line)

  while 0 < stack[-1]:
    output.write("}")
    stack.pop()

  output = StringIO(remove_semis(output))

  return output 

def remove_semis(text_io):
  text = text_io.getvalue()
  in_braces = False 
  output_text = ""
  queue = []

  for line in text.splitlines():
    if '{' in line:
      in_braces = True
    if in_braces:
      if '}' in line:
        in_braces = False
    if in_braces:
      if ':' in line:
        line += '{'
      output_text += line[:-1] 
      output_text += "\n"
    else:
      output_text += line
      output_text += "\n"
 
  return output_text
  
# removes comments from the line 
def sanitize(line):
  if blockcomment[0] not in line and blockcomment[1] not in line and comment_symbol in line:
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
  directory = os.path.dirname(infile.name) + '/'

  # get the filename without extension 
  if filename.lower().endswith(extensions):
    new_filename = os.path.splitext(filename)[0] 
  else:
    sys.exit("Input file must have Mandala file extension.")

  # process the input file 
  output = process(infile)

  # create output file 
  outfile = open(directory + new_filename + ".mandala.proc", 'w')
  outfile.write(output.getvalue())