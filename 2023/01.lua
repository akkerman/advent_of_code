function partOne (lines) 
    sum = 0
    for _,line in pairs(lines) do
        line =  string.gsub(line, "%a", "")
        num = string.sub(line, 1, 1) .. string.sub(line, -1)
        sum = sum + num
    end
    return sum
end

function partTwo (lines) 
    return 'todo'
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
