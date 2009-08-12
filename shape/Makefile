PROGRAM = SphereTest
DLIBS = $(PROGRAM:%Test=%.so)
LIBS = $(DLIBS:%.so=%.o)
PATHLIB = ../libs
PATHGEO =  ../geometry
PATHCORE = ../core
SLIB = $(PATHLIB)/libs.a $(PATHGEO)/geometry.a $(PATHCORE)/core.a shape.a
IFLAGS = -Wc,'-DDO_ASSERTION -DID_DEBUG' -k -I$(PATHLIB) -I$(PATHCORE) -I$(PATHGEO)
IINC = 
IDC = idc

all : shape.a

dlib : $(PROGRAM) 

runtest : all dlib
	./SphereTest

%Test : %Test.st $(SLIB)
	$(IDC) $(IFLAGS) $< $(SLIB) -o $@

%.o : %.st
	$(IDC) $(IINC) $(IFLAGS) -c $< -o $@

%.so : %.st
	$(IDC) $(IFLAGS) -s $< -o $@

shape.a : $(LIBS)
	ar rc $@.new $(LIBS)
	mv $@.new $@	

tidy: .FORCE
	rm -f *~

clean : tidy .FORCE
	rm -f $(PROGRAM) *.exe *.o *.so *.c

spotless : clean .FORCE

.PHONY : dlib all runtest clean tidy

.FORCE :
