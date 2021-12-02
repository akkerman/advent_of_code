BEGIN {
    f=0
    d=0
    a=0
}

$1 == "down" { a = a + $2}
$1 == "up" { a = a - $2 }
$1 == "forward" { f = f + $2; d = d +  (a * $2)  }

END {
    print f*d
}
