PROGRAM = 
DLIBS = $(PROGRAM:%Test=%.so) Shape.so
LIBS = $(DLIBS:%.so=%.o)
PATHLIB = ../libs
PATHGEO =  ../geometry
SLIB = $(PATHLIB)/libs.a $(PATHGEO)/geometry.a
IFLAGS = -Wc,'-DDO_ASSERTION -DID_DEBUG' -k -I$(PATHLIB) -I$(PATHGEO)
IINC = 
IDC = idc

all : core.a

dlib : $(PROGRAM) 

runtest : all dlib

%Test : %Test.st
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
	rm -f $(PROGRAM) *.exe *.o *.so *.c

spotless : clean .FORCE

.PHONY : dlib all runtest clean tidy

.FORCE :
