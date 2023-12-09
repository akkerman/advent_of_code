require "util"

function allZeros (history) 
    for _,e in pairs(history) do
        if e ~= 0 then return false end
    end
    return true
end
function sequencer (history)
    local last = {}
    local h = history
    while true do
       table.insert(last, h[#h]) 
       local nh = {}
       for i=2, #h do
           table.insert(nh, h[i] - h[i-1])
       end
       h = nh
       if allZeros (h) then break end
    end
    local seqSum = 0
    for _, e in pairs(last) do
        seqSum = seqSum + e
    end
    return seqSum
end 

function partOne (lines) 
    local sum = 0
    for k,history in pairs(lines) do
        ans = sequencer(history) 
        sum = sum + ans
    end
    return sum
end

function partTwo (lines) 
    return 'todo'
end

function main() 
    lines = {}
    for line in io.lines() do
        nums = {}
        -- k** regex! vooral de negatives niet vergeten 
        for s in line:gmatch("(-?%d+) ?") do
            table.insert(nums,tonumber(s))
        end
        table.insert(lines, nums)
    end
    print('partOne', partOne(lines))
    print('partTwo', partTwo(lines))

end
main()
