package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "sort"
)

func sumSort(lines []string) []int64 {
    var nums = []int64{0}
    var last = 0

    for _, line := range lines {
        if line == "" {
          nums = append(nums, 0)
          last = len(nums)-1
       } else {
           i, _ := strconv.ParseInt(line, 10, 0) 
           nums[last] = nums[last] + i
       }
    }

    sort.Slice(nums, func(i,j int) bool { return nums[i] > nums[j] })

    return nums
}

func partOne(lines []string) int64 {
    return sumSort(lines)[0]
}

func partTwo(lines []string) int64 {
    var nums = sumSort(lines)

    var total int64 = 0
    for _, n := range nums[0:3] {
        total = total + n
    }

    return total
}

func main() {
    var lines []string
    scanner := bufio.NewScanner(os.Stdin)
    for scanner.Scan() {
        lines = append(lines, scanner.Text())
    }
    fmt.Println(partOne(lines))
    fmt.Println(partTwo(lines))
}
