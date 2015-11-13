filename=$1
sanitized_filename=${filename%.*.*}$".cpp"

echo "#include <iostream>" > $sanitized_filename
echo "using namespace std;" >> $sanitized_filename
echo "" >> $sanitized_filename
echo "int main(){" >> $sanitized_filename
echo "  cout << 5 << endl;" >> $sanitized_filename
echo "};" >> $sanitized_filename
