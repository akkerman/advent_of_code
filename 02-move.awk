BEGIN {
    f=0
    d=0
}

$1 == "forward" { f = f + $2 }
$1 == "down" { d = d + $2 }
$1 == "up" { d = d - $2 }

END {
    print f*d
}
