#!/usr/bin/python
from datetime import datetime
fbuild = open('build', 'r').readlines()
build_pars = fbuild[0].split('_')
xFile = open('VERSION', 'w')
build_num = int(build_pars[1]) + 1
xFile.write('Tab_calc version %s build %s (%s)\n' % (build_pars[0], build_num, str(datetime.now())))
xFile.write('Written by A.A.Mints')
xFile.close()

xFile = open('build', 'w')
xFile.write(build_pars[0]+'_'+str(build_num))
xFile.close()
