for executable in $(ls | grep -v "\.")
do
    (time ./$executable) &> time.txt
    echo "" >> time.txt
    (/usr/bin/time -v ./$executable) &>> time.txt
    sed -i '7d' time.txt
done

for lisp_file in $(ls | grep "\.lisp")
do
    (time clisp ./$lisp_file) &> time.txt
    echo "" >> time.txt
    (/usr/bin/time -v clisp ./$lisp_file) &>> time.txt
    sed -i '7d' time.txt
done
