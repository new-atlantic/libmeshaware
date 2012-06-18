all:
	if [ ! -d obj ]; then mkdir obj; fi
	if [ ! -d bin ]; then mkdir bin; fi
	gprbuild batman_adv_tests.gpr

test: all
	su -c "./runner.sh"

clean:
	if [ -d bin ]; then rm -r bin; fi
	if [ -d obj ]; then rm -r obj; fi
