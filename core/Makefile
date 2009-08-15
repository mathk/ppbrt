PROGRAM = 
DLIBS = $(PROGRAM:%Test=%.so) Shape.so QuadraticSolver.so
LIBS = $(DLIBS:%.so=%.o)
PATHLIB = ../libs
PATHGEO =  ../geometry
SLIB = $(PATHLIB)/libs.a $(PATHGEO)/geometry.a
IFLAGS = -Wc,'-DDO_ASSERTION -DID_DEBUG' -k -I$(PATHLIB) -I$(PATHGEO)
IINC = 
IDC = idc

all : core.a

dlib : $(PROGRAM) 

test : all dlib

%Test : %Test.st $(SLIB)
	$(IDC) $(IFLAGS) $< $(SLIB) -o $@

%.o : %.st
	$(IDC) $(IINC) $(IFLAGS) -c $< -o $@

%.so : %.st
	$(IDC) $(IFLAGS) -s $< -o $@

core.a : $(LIBS)
	ar rc $@.new $(LIBS)
	mv $@.new $@	

tidy: .FORCE
	rm -f *~

clean : tidy .FORCE
	rm -f $(PROGRAM) *.exe *.o *.so *.c *.a

spotless : clean .FORCE

.PHONY : dlib all runtest clean tidy

.FORCE :
