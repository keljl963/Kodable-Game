objects = generate.o solution.o representations.o kogame.o
ghcObjects = generate.hi solution.hi representations.hi kogame.hi
exe = kogame

CC = stack ghc -- -fforce-recomp -Wall

all:
	${CC} --make *.hs
clean:
	rm -f ${objects} ${exe} ${ghcObjects}