cubes = {
    red = 12,
    green = 13,
    blue = 14,
}

function partOne (lines)
    gid = 0
    sum = 0
    for _, game in pairs(lines) do
        gid = gid + 1
        possible = true
        for number, color in string.gmatch(game, "(%d+) (%w+)") do
            if tonumber(number) > cubes[color] then
                possible = false
                break
            end
        end
        if possible then
            sum = sum + gid
        end
    end
    return sum
end

function partTwo (lines) 
    gid = 0
    sum = 0
    for _, game in pairs(lines) do
        gid = gid + 1
        bag = { red=0, green=0, blue=0 }
        for number, color in string.gmatch(game, "(%d+) (%w+)") do
            if tonumber(number) > bag[color] then
                bag[color] = tonumber(number)
            end
        end
        sum = sum + bag['red'] * bag['green'] * bag['blue']
    end
    return sum
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
