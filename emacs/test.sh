echo "Testing init.el"
echo
time emacs -l init.el -batch --eval '(message "Hello, world!")'

echo
echo "--------------------------------"
echo

echo "testing init.elc"
echo
time emacs -l init.elc -batch --eval '(message "Hello, world!")'
