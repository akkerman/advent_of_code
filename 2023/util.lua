function printTable(table) 
    local out = '['
    for k,v in pairs(table) do
        out = out .. v
        if k < #table then out = out .. ', ' end
    end
    out = out .. ']'
    print(out)
end
