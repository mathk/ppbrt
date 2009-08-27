PROGRAM = main 
DLIBS = $(PROGRAM:%Test=%.so)
LIBS = $(DLIBS:%.so=%.o)
PATHLIB = ../libs
PATHGEO =  ../geometry
PATHCORE = ../core
PATHSHAPE = ../shape
PATHPRIM = ../primitive
SLIB = $(PATHLIB)/libs.a $(PATHGEO)/geometry.a $(PATHPRIM)/primitive.a \
	   $(PATHSHAPE)/shape.a $(PATHCORE)/core.a
IFLAGS = -Wc,'-DDO_ASSERTION -DID_DEBUG' -k -I$(PATHLIB) -I$(PATHGEO) \
		 -I$(PATHSHAPE) -I$(PATHPRIM) -I$(PATHCORE)
IINC = 
IDC = idc

all : $(PROGRAM)

main : main.st $(SLIB)
	$(IDC) $(IFLAGS) $< $(SLIB) -o $@

%.o : %.st
	$(IDC) $(IINC) $(IFLAGS) -c $< -o $@

%.so : %.st
	$(IDC) $(IFLAGS) -s $< -o $@

tidy: .FORCE
	rm -f *~

clean : tidy .FORCE
	rm -f $(PROGRAM) *.exe *.o *.so *.c *.a

spotless : clean .FORCE

.PHONY : dlib all runtest clean tidy

.FORCE :
