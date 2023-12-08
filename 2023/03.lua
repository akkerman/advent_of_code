function getPartnumberAt (lines,r, c) 
    local start
    local partNr = ''
    local line = lines[r]
    local maxCol = #line

    for dc = c, 1, -1 do
        start = dc
        if nil == line[dc]:match('%d') then
            start = start + 1
            break 
        end
    end

    for dc = start, maxCol, 1 do
        ch = line[dc]
        if nil == ch:match('%d') then
            break
        end
        partNr = partNr .. ch
    end

    coord = r .. ',' .. start
    nr = tonumber(partNr)
    return coord, nr
end

function partOne (lines) 
    partNumbers = {}
    maxRow = #lines
    maxCol = #lines[1]

    for r, chars in pairs(lines) do
        for c, ch in pairs(chars) do
            if ch == ch:match('%d') or ch == '.' then
                goto nextchar
            end
            for dr = r-1, r+1 do
                for dc = c-1, c+1 do
                    if (dr < 0 and dc < 0 and dr > maxRow and dc > maxCol) then
                        goto nextcoords
                    end

                    ch = lines[dr][dc]

                    if ch:match('%d') then
                        coord, partNr = getPartnumberAt(lines, dr, dc)
                        partNumbers[coord] = partNr
                    end

                    ::nextcoords::
                end
            end
            ::nextchar::
        end
    end

    sum = 0
    for _, partNr in pairs(partNumbers) do
        sum = sum + partNr
    end
    return sum
end

function partTwo (lines) 
    return 'todo'
end

function main() 
    lines = {}
    for line in io.lines() do
        chars = {}
        line:gsub(".",function(c) table.insert(chars,c) end)
        table.insert(lines, chars)
    end
    print('partOne', partOne(lines))
    print('partTwo', partTwo(lines))
end
main()
