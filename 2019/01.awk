BEGIN {
    sum1=0
    sum2=0
}

function calc1(mass) {
    return int(mass / 3) - 2
}

function calc2(fuel) {
    if (fuel <= 0) return 0
    return fuel + calc2(calc1(fuel))
}

{
    fuel=calc1($0)
    sum1+=fuel
    sum2+=calc2(fuel)
}

END {
    print "partOne", sum1
    print "partTwo", sum2
    print ""
    print "the elfs will be better of taking", calc2(sum1)
}
