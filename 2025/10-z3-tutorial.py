from z3 import Ints, Solver

x1, x2, x3, x4, x5, x6, x7 = Ints('x1 x2 x3 x4 x5 x6 x7')

s = Solver()
s.add(x1 >= 0, x2 >= 0, x3 >= 0, x4 >= 0, x5 >= 0, x6 >= 0, x7 >= 0)

s.add(x5 + x6 == 3)
s.add(x2 + x6 == 5)
s.add(x3 + x4 + x5 == 4)
s.add(x1 + x2 + x4 == 7)

print(s.check())
print(s.model())
