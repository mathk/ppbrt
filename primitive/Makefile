PROGRAM = 
DLIBS = $(PROGRAM:%Test=%.so) GeometricPrimitive.so Aggregate.so \
	    GridAccelerator.so Voxel.so
LIBS = $(DLIBS:%.so=%.o)
PATHLIB = ../libs
PATHGEO =  ../geometry
PATHSHAPE = ../shape
PATHCORE = ../core
SLIB = $(PATHLIB)/libs.a $(PATHGEO)/geometry.a $(PATHSHAPE)/shape.a
IFLAGS = -Wc,'-DDO_ASSERTION -DID_DEBUG' -k -I$(PATHLIB) -I$(PATHGEO) -I$(PATHSHAPE) -I$(PATHCORE)
IINC = 
IDC = idc

all : primitive.a

dlib : $(PROGRAM) 

test : all dlib

%Test : %Test.st $(SLIB)
	$(IDC) $(IFLAGS) $< $(SLIB) -o $@

%.o : %.st
	$(IDC) $(IINC) $(IFLAGS) -c $< -o $@

%.so : %.st
	$(IDC) $(IFLAGS) -s $< -o $@

primitive.a : $(LIBS)
	ar rc $@.new $(LIBS)
	mv $@.new $@	

tidy: .FORCE
	rm -f *~

clean : tidy .FORCE
	rm -f $(PROGRAM) *.exe *.o *.so *.c *.a

spotless : clean .FORCE

.PHONY : dlib all runtest clean tidy

.FORCE :
