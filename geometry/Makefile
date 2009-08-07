PROGRAM = VectorTest CoordinateSystemTest\
	  NormalTest PointTest RayTest RayDifferentialTest \
	  BBoxTest MatrixTest TransformTest
DLIBS = $(PROGRAM:%Test=%.so) BSphere.so DifferentialGeometry.so
LIBS = $(DLIBS:%.so=%.o)
PATHLIB = ../libs
SLIB = $(PATHLIB)/libs.a geometry.a
IFLAGS = -Wc,'-DDO_ASSERTION -DID_DEBUG' -k -I$(PATHLIB)
IINC = 
IDC = idc

all : geometry.a

dlib : $(PROGRAM) 

runtest : all dlib
	./NormalTest
	./VectorTest
	./PointTest
	./CoordinateSystemTest
	./RayTest
	./MatrixTest
	./TransformTest

%Test : %Test.st
	$(IDC) $(IFLAGS) $< $(SLIB) -o $@

%.o : %.st
	$(IDC) $(IINC) $(IFLAGS) -c $< -o $@

%.so : %.st
	$(IDC) $(IFLAGS) -s $< -o $@

geometry.a : $(LIBS)
	ar rc $@.new $(LIBS)
	mv $@.new $@	

tidy: .FORCE
	rm -f *~

clean : tidy .FORCE
	rm -f $(PROGRAM) *.exe *.o *.so *.c

spotless : clean .FORCE

.PHONY : dlib all runtest clean tidy

.FORCE :