IDC		= idc -O
IINC		= 
IDFLAGS		= -k -Wc,'-DDO_ASSERTION -DID_DEBUG'

FILES		= $(BIN)ObjectLib.o \
			  $(BIN)NumberLib.o \
			  $(BIN)FloatLib.o \
			  $(BIN)IntegerLib.o \
			  $(BIN)SmallIntegerLib.o \
			  $(BIN)Angle.o

PREFIX		= 

CC		= gcc
OBJEXT		= 
OBJEXT_O	= .o
OBJEXT_SO	= .so

all : $(BIN)libs.a

run : $(BIN)libs.so $(BIN)test-st80
	$(SHELL) -ec '( cd $(BIN).; ./test-st80 )'

$(BIN)libs.so : libs.st $(FILES)
	$(IDC) $(IDFLAGS) -s libs.st $(FILES) -o $@

.SUFFIXES :

$(BIN)%$(OBJEXT_SO) : %.st
	$(IDC) $(IINC) $(IDFLAGS) -s $< -o $@

$(BIN)%$(OBJEXT_O) : %.st
	$(IDC) $(IINC) $(IDFLAGS) -c $< -o $@

$(BIN)%$(OBJEXT) : %.st
	$(IDC) $(IINC) $(IDFLAGS) $< -o $@

libs.a : $(FILES)
	ar rc $@.new $(FILES)
	mv $@.new $@


install : all .FORCE
	-mkdir -p $(PREFIX)
	-cp -pr *.st $(BIN)st80.so $(PREFIX)

tidy : .FORCE
	-rm -f *.o *.d *~ .gdb* *.stackdump *.c

clean : tidy
	-rm -f $(FILES) $(BIN)*.exe $(BIN)*.c $(BIN)*.a

distclean spotless : clean
	-rm -f Makefile

.FORCE :