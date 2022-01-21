#!/usr/bin/python3
import sys
import texttable

path = sys.argv[1][1:-2]

t = texttable.Texttable(max_width=0)

output = open('%s/commands.list' % path, 'w')

output.write(' === Commands list: ===\n')

t.set_cols_width([25, 105])
t.header(['Command', 'Description'])
t.set_cols_align(["l", "l"])
t.set_deco(texttable.Texttable.HEADER)
for x in open('commands.list').readlines():
    row = []
    for z in x.split('#'):
        row.append(z.strip())
    t.add_row(row)
output.write(t.draw())
output.close()

