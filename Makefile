INSTALL_PATH=\'$(CURDIR)/bin/\'

F90=gfortran -Jlib -Ilib -O3 -fbacktrace
#F90=pgf90 -Llib -Ilib
SRCS=file_io.f90 operators.f90 logs.f90 comline.f90 quickSort.f90 StringUtils.f90 array_works.f90 \
ini_file.f90 StringArray.f90 histograms.f90 tcOutput.f90 tcMath.f90 tcFit.f90 tcUtils.f90 tcPower.f90
OBJS=$(SRCS:%.f90=lib/%.o)
XSRCS=$(SRCS:%.f90=src/%.f90)
PROG=tab_calc
EXECUTABLE=$(PROG)
#COMMANDS=

$(PROG): src/tcGlobals.F90 lib/tcGlobals.o src/$(PROG).F90 $(XSRCS) $(OBJS) src/commands.i #src/vars.i
	mkdir -p lib
	mkdir -p bin
	python make_vers.py
	egrep -h "\!\+" commands/* src/tab_calc.F90| sed 's/^ *//g' | colrm 1 2 | sed 's/^ *//g' | sort > commands.list
	python get_commands.py $(INSTALL_PATH)
	cat params/*.param > src/params.i
	$(F90) -o $(EXECUTABLE) src/$(PROG).F90 $(OBJS) lib/tcGlobals.o -DINSTALL_PATH=$(INSTALL_PATH)

profile:
	$(MAKE) $(MAKEFILE) F90="gfortran -Jlib -Ilib -pg"

huge:
	$(MAKE) -B $(MAKEFILE) F90="gfortran -Jlib -Ilib -O3 -DHUGE" EXECUTABLE="tab_calc_huge"

intel:
	$(MAKE) $(MAKEFILE) F90="ifort -module ./lib -Ilib -O3 -limf -static"

update: src/commands.i #src/vars.i
	python make_vers.py
	cat params/*.param > src/params.i
	$(F90) -o $(PROG) src/$(PROG).F90 $(OBJS) lib/tcGlobals.o -DINSTALL_PATH=$(INSTALL_PATH)

inline: src/tcGlobals.F90 lib/tcGlobals.o src/$(PROG).F90 $(XSRCS) $(OBJS) src/commands.i #src/vars.i
	python make_vers.py
	cat params/*.param > src/params.i
	Fortran_wrap_print.sh < USAGE > src/USAGE.wrap
	Fortran_wrap_print.sh < VERSION > src/VERSION.wrap
	$(F90) -o $(PROG) src/$(PROG).F90 $(OBJS) lib/tcGlobals.o -DINCLUDE_TEXTFILES
	rm src/*.wrap

tests: $(XSRCS)
	export FSFLAG=-I
	funit -s src $(ls src/*.fun)

commands/%.commands: ;

variables/%.vars: ;

src/commands.i: commands/%.commands
	cat commands/*.command > src/commands.i

lib/tcGlobals.o: src/tcGlobals.F90 src/vars.i src/StringArray.f90 lib/StringArray.o lib/operators.o
	$(F90) -c -o $@ src/tcGlobals.F90

lib/StringArray.o: src/StringArray.f90 lib/operators.o lib/logs.o
	$(F90) -c -o $@ src/StringArray.f90

lib/histograms.o: src/histograms.f90 lib/tcGlobals.o
	$(F90) -c -o $@ $?

src/vars.i: variables/*.vars
	cat variables/*.vars > src/vars.i

lib/%.o: src/%.f90
	$(F90) -c -o $@ $?


# This entry allows you to type " make clean " to get rid of
# all object and module files
clean:
	 rm -f -r f_{files,modd}* lib/*.o lib/*.mod *.M *.d V*.inc *.vo  V*.f *.dbg album F.err
