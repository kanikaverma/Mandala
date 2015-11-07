# Compiles and executes programs for testing purposes

import os

for file in os.listdir('TestSuite'):
    if file.endswith('.ml'):
        exe = str(file)[:-3]
        os.system("ocamlc graphics.cma TestSuite/" + file + " -o TestSuite/" + exe)
        os.system("./TestSuite/" + exe)
        os.system("rm TestSuite/*.cmi*")
        os.system("rm TestSuite/*.cmo*")
        os.system("rm TestSuite/" + exe)
