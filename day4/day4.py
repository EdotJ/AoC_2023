import re

with open("input.txt") as f:
    cards = f.read().splitlines()
#PART 1
total = 0
#PART 2
instances = [0] * len(cards)

for card in range(len(cards)):
    raw = re.findall("[0-9]+|\|", cards[card])
    pipe = raw.index("|")
    wins = sum(i in raw[pipe+1:] for i in raw[1:pipe])
    #PART 1
    total += int(2 ** (wins - 1))
    #PART 2
    instances[card] += 1
    for j in range(1, wins+1):
        if card+j < len(instances):
            instances[card+j] += instances[card]
print(total)
print(sum(instances))