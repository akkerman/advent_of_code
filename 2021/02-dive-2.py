forward=0
depth=0
aim=0

with open('02-input.txt') as file:
    for line in file.readlines():
      [command, x] = line.split()
      x = int(x)
        
      if command == 'down':
          aim = aim + x
      elif command == 'up':
          aim = aim - x
      elif command == 'forward':
          forward = forward + x
          depth = depth + (aim * x)

print(forward*depth)
