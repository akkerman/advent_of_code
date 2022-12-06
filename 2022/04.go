package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    // "sort"
    re "regexp"
)

var a1 = 0
var a2 = 1
var b1 = 2
var b2 = 3

func partOne(sections [][]int64) int {
    var count = 0
    for _, s := range sections {
        if (s[a1] <= s[b1] && s[b2] <= s[a2]) || (s[b1] <= s[a1] && s[a2] <= s[b2]) {
            count = count + 1
        }
    }
    return count
}

func partTwo(sections [][]int64) int {
    var count = 0
    for _, s := range sections {
        if (s[a1] <= s[b1] && s[a2] >= s[b1]) || (s[b1] <= s[a1] && s[b2] >= s[a1]) {
            count = count + 1
        }
    }
    return count
}


func main() {
    dashcomma := re.MustCompile("[-,]")
    var lines [][]int64
    scanner := bufio.NewScanner(os.Stdin)
    for scanner.Scan() {
        var sections []int64
        var sectionsStr = dashcomma.Split(scanner.Text(), -1)
        for _, str := range sectionsStr {
            boundary, _ := strconv.ParseInt(str, 10, 0)
            sections = append(sections, boundary)
        }
        lines = append(lines, sections)
    }
    fmt.Println(partOne(lines))
    fmt.Println(partTwo(lines))
}
