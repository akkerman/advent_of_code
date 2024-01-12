# pylint: disable=missing-module-docstring,missing-function-docstring,missing-class-docstring, too-many-instance-attributes
# pylint: disable=invalid-name,too-many-arguments
import sys
import logging

log = logging.getLogger('Wizard Simulator 20XX')
log.addHandler(logging.StreamHandler(sys.stdout))
log.setLevel(logging.DEBUG)


class Fighter:
    def __init__(self, name:str, hp:int, damage:int=0, mana:int=0):
        self.name = name
        self.hp=hp
        self.damage=damage
        self.mana=mana
        self.armor = 0

    def __repr__(self):
        if self.name == 'Player':
            # pylint: disable=line-too-long
            return f'- {self.name} has {self.hp} hit point{"s" if self.hp>1 else ""}, {self.armor} armor, {self.mana} mana'
        return f'- {self.name} has {self.hp} hit points'


    def is_alive(self):
        return self.hp > 0

class Spell:
     # pylint: disable=line-too-long
    def __init__(self, name:str, cost:int, damage:int=0, armor:int=0, heal:int=0, mana:int=0, duration:int=1):
        self.name = name
        self.cost = cost
        self.damage = damage
        self.armor = armor
        self.heal = heal
        self.mana = mana
        self.duration = duration
        self.timer = 0

    def __repr__(self):
        return f'{self.name} costs {self.cost}'


    def is_active(self):
        return self.timer > 0

    def cast(self, player, boss):
        if self.is_active() or player.mana < self.cost:
            return 0

        player.mana -= self.cost

        if self.duration == 1:
            if self.damage and self.heal:
                log.debug('%s casts %s, dealing %s damage, and healing %s hit points.',
                          player.name, self.name, self.damage, self.heal)
                boss.hp -= self.damage
                player.hp += self.heal
            elif self.damage:
                log.debug('%s casts %s, dealing %s damage.', player.name, self.name, self.damage)
                boss.hp -= self.damage
            else:
                assert False
        elif self.armor:
            player.armor += self.armor
            self.timer = self.duration
            log.debug('%s casts %s, increasing armor by %s.', player.name, self.name, self.armor)
        else:
            log.debug('%s casts %s.', player.name, self.name)
            self.timer = self.duration

        return self.cost

    def apply(self, player, boss):
        if not self.is_active():
            return

        self.timer -= 1


        if self.damage:
            log.debug('%s deals %s damage; its timer is now %s.', self.name, self.damage, self.timer)
            boss.hp -= self.damage
        elif self.mana:
            log.debug('%s provides %s mana; its timer is now %s.', self.name, self.mana, self.timer)
            player.mana += self.mana
        else:
            log.debug('%s\'s timer is now %s.', self.name, self.timer)

        if  not self.is_active():
            if self.armor:
                log.debug('%s wears off, decreasing armor by %s.', self.name, self.armor)
                player.armor -= self.armor
            else:
                log.debug('%s wears off.', self.name)


spells = {
    'Magic Missile': Spell('Magic Missile', cost=53, damage=4),
    'Drain': Spell('Drain', cost=73, damage=2, heal=2),
    'Shield': Spell('Shield', cost=113, armor=7, duration=6),
    'Poison': Spell('Poison', cost=173, damage=3, duration=6),
    'Recharge': Spell('Recharge', cost=229, mana=101, duration=5),
}

def turn(player, boss, cast_spell):
    log.debug('\n-- Player turn --')
    log.debug(player)
    log.debug(boss)

    for spell in spells.values():
        spell.apply(player, boss)
        if not boss.is_alive():
            log.info('Boss dies')
            return

    cast_spell.cast(player, boss)

    if boss.hp <= 0:
        log.info('Boss dies')
        return


    log.debug('\n-- Boss turn --')
    log.debug(player)
    log.debug(boss)
    for spell in spells.values():
        spell.apply(player, boss)
        if not boss.is_alive():
            log.info('Boss dies')
            return

    damage = boss.damage - player.armor
    if player.armor:
        log.debug('Boss attacks for %s - %s = %s damage!', boss.damage, player.armor, damage)
    else:
        log.debug('Boss attacks for %s damage!', damage)
    player.hp -= max(1, damage)
    if not player.is_alive():
        log.info('Player dies')
        return


def example_game1():
    player = Fighter('Player', hp=10, mana=250)
    boss = Fighter('Boss', hp=13, damage=8)
    turn(player, boss, spells['Poison'])
    turn(player, boss, spells['Magic Missile'])

def example_game2():
    player = Fighter('Player', hp=10, mana=250)
    boss = Fighter('Boss', hp=14, damage=8)
    turn(player, boss, spells['Recharge'])
    turn(player, boss, spells['Shield'])
    turn(player, boss, spells['Drain'])
    turn(player, boss, spells['Poison'])
    turn(player, boss, spells['Magic Missile'])

def part_one(boss_stats):
    """ part one """
    # boss = Fighter(*boss_stats)
    # player = Fighter(hp=50, mana=500)

    example_game2()

    return 'todo'


def part_two(boss_stats):
    """ part two """
    return 'todo'


def main():
    """ main """
    boss_stats = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        line = int(line.split(': ')[1])
        boss_stats.append(line)

    print('part_one', part_one(boss_stats))

    print('part_two', part_two(boss_stats))


main()
