f = file("log.txt", 'r')
f2 = file("clean.txt", 'w')

for line in f:
  if 'commit' in line or 'Author' in line or 'Date' in line or 'Merge' in line:
    if 'pull' not in line and 'conflict' not in line and 'branch' not in line and 'Initial' not in line: 
      f2.write(line)

f.close()
f2.close()