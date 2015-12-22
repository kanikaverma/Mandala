# saves the expected output of all tests in the test suite

mandala_files=$(find suite -name *\.mandala)
for m_file in $mandala_files
do
  ./save_output.sh ${m_file##*/}
done