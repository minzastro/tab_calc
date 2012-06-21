require "date.rb"
fbuild = IO.readlines('build')
build_pars = fbuild.at(0).split('_')
xFile = File.new('VERSION', 'w')
build_num = build_pars.at(1).to_i + 1
stroka = 'Tab_calc version ' + build_pars.at(0) + ' build ' + build_num.to_s+' ('+Date.today.to_s+')'
xFile.puts stroka
xFile.puts 'Written by A.A.Mints'
xFile.close


xFile = File.new('build', 'w')
xFile.puts build_pars.at(0)+'_'+build_num.to_s
xFile.close
