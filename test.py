# Compiles and executes programs for testing purposes

import os

result_file = open("test_results.txt", 'w')

for file in os.listdir('TestSuite'):
    if file.endswith('.ml'):
        exe = str(file)[:-3]
        os.system("ocamlc graphics.cma TestSuite/" + file + " -o TestSuite/" + exe)
        os.system("./TestSuite/" + exe)

        result_file.write("[ ] Running Test: " + exe + "\n")

        os.system("rm TestSuite/*.cmi*")
        os.system("rm TestSuite/*.cmo*")
        os.system("rm TestSuite/" + exe)
