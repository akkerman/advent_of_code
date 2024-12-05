from util_05 import get_rules, check_order

rules = [
      (47,53),
      (97,13),
      (97,61),
      (97,47),
      (75,29),
      (61,13),
      (75,53),
      (29,13),
      (97,29),
      (53,29),
      (61,53),
      (97,53),
      (61,29),
      (47,13),
      (75,47),
      (97,75),
      (47,61),
      (75,61),
      (47,29),
      (75,13),
      (53,13),
]

def test_get_rules():
   assert(get_rules(rules, 97) == [(97,13), (97,61), (97,47), (97,29), (97,53), (97,75)])
   assert(get_rules(rules, 75) == [(75,29), (75,53), (75,47), (75,61), (75,13)])
   assert(get_rules(rules, 29) == [(29,13)])
   assert(get_rules(rules, 13) == [])

def test_check_order():
   assert(check_order(rules, [75,47,61,53,29]) == True)
   assert(check_order(rules, [97,61,53,29,13]) == True)
   assert(check_order(rules, [75,29,13]) == True)
   assert(check_order(rules, [75,97,47,61,53]) == False)
   assert(check_order(rules, [61,13,29]) == False)
   assert(check_order(rules, [97,13,75,29,47]) == False)
