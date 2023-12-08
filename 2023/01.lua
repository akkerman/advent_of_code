function partOne (lines) 
    sum = 0
    for _,line in pairs(lines) do
        line =  string.gsub(line, "%a", "")
        num = string.sub(line, 1, 1) .. string.sub(line, -1)
        sum = sum + num
    end
    return sum
end

translations = {
  one = "one1one",
  two = "two2two",
  three = "three3three",
  four = "four4four",
  five = "five5five",
  six = "six6six",
  seven = "seven7seven",
  eight = "eight8eight",
  nine = "nine9nine",
}

function partTwo (lines) 
    translated = {}
    for _,line in pairs(lines) do
        for n,r in pairs(translations) do
            line = string.gsub(line,n,r)
        end
        table.insert(translated, line)
    end
    return partOne(translated)
end

function main() 
    lines = {}
    for line in io.lines() do
        table.insert(lines, line)
    end
    print('partOne', partOne(lines))
    print('partTwo', partTwo(lines))
end
main()
