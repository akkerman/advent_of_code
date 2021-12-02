package main

import (
    "bufio"
    "os"
    "strings"
    "strconv"
    "fmt"
)

func asCommand(s string) (string, int) {
    result := strings.Split(s, " ")
    x, err := strconv.Atoi(result[1])
    if err != nil { x=0 }
    return result[0], x
}

func main() {
    forward:=0
    depth:=0
    aim:=0

    file, err := os.Open("./02-input.txt")
    if err != nil { os.Exit(1) }
    scanner := bufio.NewScanner(file)

    for scanner.Scan() {
        command, x := asCommand(scanner.Text())

        switch command {
        case "down":
            aim = aim + x
        case "up":
            aim = aim - x
        case "forward":
            forward = forward + x
            depth = depth + aim * x
        }
    }
    fmt.Println(forward * depth)
}
