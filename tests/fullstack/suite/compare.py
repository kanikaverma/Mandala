import sys

hashmap = {} 
hashmap2 = {} 
f = file(sys.argv[1], 'r') # generated program 
f2 = file(sys.argv[2], 'r') # expected output 

def main():
  build_hash_1()
  build_hash_2()
  if hashmap == hashmap2:
    print 0 # equal 
  else:
    print -1 # unequal

def build_hash_1():
  for line in f:
    line = line.strip()
    if line in hashmap:
      hashmap[line] += 1
    else:
      hashmap[line] = 1

def build_hash_2():
  for line in f2:
    line = line.strip()
    if line in hashmap2:
      hashmap2[line] += 1
    else:
      hashmap2[line] = 1

if __name__ == "__main__":
  main()