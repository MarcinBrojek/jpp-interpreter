.PHONY : all bad_run good_run clean
.SILENT : bad_run good_run

all:
	-mkdir build
	-cp -f src/* build/
	cd build && ghc Main.hs -o ../interpreter

bad_run:
	for bad_example in bad/*; do\
		echo $${bad_example};\
		./interpreter $${bad_example};\
		echo "\n";\
	done

good_run:
	for good_example in good/*; do\
		echo $${good_example};\
		./interpreter $${good_example};\
		echo "\n";\
	done

clean:
	-rm -rf build
	-rm -f interpreter
