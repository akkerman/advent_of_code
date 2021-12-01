BEGIN {
    sum=0
}

{
    if (n2 && n1) 
        current = n2 + n1 + $0
    if (prev && prev < current)
        sum+=1

    prev=current
    n2=n1
    n1=$0
}

END {
    print sum
}
