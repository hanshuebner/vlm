#
# Builds the Alpha Ivory Emulator (VLM)
#
#   The command line to build an emulator is
#
#      make TARGET {OPTIONS="option1 option2 ..."} {NONSHARED=YES}
#
#   where TARGET is either "genera", "minima", or "iverify".  The makefile will
#   automatically include the -DGENERA, -DMINIMA, or -DIVERIFY preprocessor option
#   when invoking the compiler and assembler.
#
#   On the make command line, use OPTIONS="option1 option2 ..." to specify compiler
#   and assembler options, notably preprocessor options.
#
#   The main preprocessor options used throughout the sources are
#      GENERA to build an emulator that is intended to run Genera
#      MINIMA to build an emulator that is intended to run Minima
#      IVERIFY to build an emulator that is intended to run the instruction test suite
#      TRACING to enable instruction tracing and counting
#      CACHEMETERING to enable instruction cache metering facilities
#      TRAPMETERING to enable trap metering facilities
#      STATISTICS to enable other statistics-gathering facilities
#      DEBUGGING to enable debugging facilities
#      DEBUG* to enable other, more specific debugging facilities
#      AUTOSTART to immediately start execution of the loaded image without waiting for
#         a :Start Interactor command from the Minima Debugger.  (This option is defined
#         automatically when the TARGET is "genera".)
#      USE_CPU_FOR_MICROSECOND_CLOCK to use the process CPU time rather than elapsed time
#         as the value of Ivory's microsecond clock
#
#   For example, to create an emulator that runs Genera but waits for a :Start Interactor
#   command from the Minima Debugger before booting and also has debugging features enabled,
#
#      make genera OPTIONS="-DDEBUGGING -UAUTOSTART"
#
#   If you specify a target different from your last build, you must remove all object
#   files first to ensure that everything is properly compiled.  The command line
#      rm {.,life-support,emulator}/*.o
#   will do the trick.
#
#   In fact, if you change any settings between builds, it's a good idea to build from scratch.
#      
#   On the make command line, use PROFILE=YES to build the VLM using the profiling option
#   to gather statistics on execution frequencies.
#
#   On the make command line, use NONSHARED=YES to build the VLM using the non-shared
#   libraries.  (The default is to use the shared libraries.)
#


LIFE = ./life-support
EMULATOR = ./emulator
ALPHAEMULATOR = ./alpha-emulator
OTHER = ./other

genera := MAINOPTIONS = -DGENERA -DAUTOSTART
minima := MAINOPTIONS = -DMINIMA
iverify := MAINOPTIONS = -DIVERIFY

CFLAGS = -std1 -g2 -I. -I$(LIFE) -I$(EMULATOR) -I$(ALPHAEMULATOR) $(MAINOPTIONS) $(OPTIONS) -o $@
AFLAGS = -g2 -I. -I$(LIFE) -I$(EMULATOR) -I$(ALPHAEMULATOR) $(MAINOPTIONS) $(OPTIONS) -o $@

.SUFFIXES:
.SUFFIXES: .o .c .s .S

.c.o:
	cc $(CFLAGS) -c $<

.s.o:
	as $(AFLAGS) $<

.S.o:
	as $(AFLAGS) $<

SRCS = main.c spy.c world_tools.c utilities.c \
       $(LIFE)/cold_load.c $(LIFE)/console.c $(LIFE)/disks.c $(LIFE)/initialization.c \
       $(LIFE)/network.c $(LIFE)/message_channels.c $(LIFE)/polling.c $(LIFE)/queues.c \
       $(LIFE)/signals.c $(LIFE)/network-osf.c \
       $(EMULATOR)/interfac.c $(EMULATOR)/interpds.c $(EMULATOR)/externals.c \
       $(EMULATOR)/memory.c

COMPONENTS = $(FAKEEMULATOR? \
                : $(ALPHAEMULATOR)/idispat.s $(ALPHAEMULATOR)/ifuncom1.s \
                  $(ALPHAEMULATOR)/ifuncom2.s $(ALPHAEMULATOR)/ifungene.s \
                  $(ALPHAEMULATOR)/ifunfcal.s $(ALPHAEMULATOR)/ifunloop.s \
                  $(ALPHAEMULATOR)/ifunlist.s $(ALPHAEMULATOR)/ifuninst.s \
                  $(ALPHAEMULATOR)/ifunmath.s $(ALPHAEMULATOR)/ifunarra.s \
                  $(ALPHAEMULATOR)/ifunmove.s $(ALPHAEMULATOR)/ifunpred.s \
                  $(ALPHAEMULATOR)/ifunsubp.s $(ALPHAEMULATOR)/ifunfext.s \
                  $(ALPHAEMULATOR)/ifunlexi.s $(ALPHAEMULATOR)/ifunbits.s \
                  $(ALPHAEMULATOR)/ifunblok.s $(ALPHAEMULATOR)/ifunbind.s \
                  $(ALPHAEMULATOR)/ifunfull.s $(ALPHAEMULATOR)/ifunbnum.s \
                  $(ALPHAEMULATOR)/ifuntrap.s $(ALPHAEMULATOR)/ihalt.s \
                  $(ALPHAEMULATOR)/idouble.s $(ALPHAEMULATOR)/ifunjosh.s \
                  $(ALPHAEMULATOR)/ifuntran.s)

ASMS = $(FAKEEMULATOR? $(EMULATOR)/fake_emulator.c \
                     : $(EMULATOR)/emulator.S)
EMULATOROBJ = $(FAKEEMULATOR? $(EMULATOR)/fake_emulator.o \
                            : $(EMULATOR)/emulator.o)

OBJS = $(LIFE)/cold_load.o $(LIFE)/console.o $(LIFE)/disks.o $(LIFE)/initialization.o \
       $(LIFE)/network.o $(LIFE)/message_channels.o $(LIFE)/polling.o $(LIFE)/queues.o \
       $(LIFE)/signals.o \
       world_tools.o utilities.o spy.o \
       $(EMULATOR)/interfac.o $(EMULATOR)/interpds.o $(EMULATOR)/externals.o \
       $(EMULATOR)/memory.o $(EMULATOROBJ)

EMULATORINCLUDES =  $(EMULATOR)/aihead.h $(ALPHAEMULATOR)/aistat.h $(EMULATOR)/ivoryrep.h

OTHEROBJS = $(OTHER)/pfopen.o \
            $(NONSHARED? \
                       :)

LIBRARIES = $(NONSHARED?-non_shared -lpthread -lpthreads -lmach -lc_r -lexc -lX11 -ldnet_stub \
                       :-lpthread -lpthreads -lmach -lc_r -lexc -lX11)

all: genera

$(EMULATOROBJ): $(ASMS) $(COMPONENTS)

$(EMULATOR)/externals.o: $(EMULATOR)/externals.c $(EMULATORINCLUDES)
$(EMULATOR)/interpds.o: $(EMULATOR)/interpds.c $(EMULATORINCLUDES) $(EMULATOR)/asmfuns.h
$(EMUALTOR)/interfac.o: $(EMULATOR)/interfac.c $(EMULATORINCLUDES)
spy.o: spy.c $(EMULATORINCLUDES)

$(LIFE)/network.o: $(LIFE)/network.c $(LIFE)/network-osf.c

genera: main.o byteswap_world.o $(OBJS) $(OTHEROBJS)
	cc $(PROFILE?-p:) -o genera $(NONSHARED?-non_shared -lc_r:) $(OTHEROBJS) main.o $(OBJS) $(LIBRARIES)
	cc $(PROFILE?-p:) -o byteswap_world $(NONSHARED?-non_shared -lc_r:) $(OTHEROBJS) byteswap_world.o $(OBJS) $(LIBRARIES)

minima: main.o $(OBJS) $(OTHEROBJS)
	cc $(PROFILE?-p:) -o minima $(NONSHARED?-non_shared -lc_r:) $(OTHEROBJS) main.o $(OBJS) $(LIBRARIES)

iverify: main.o $(OBJS) $(OTHEROBJS)
	cc $(PROFILE?-p:) -o iverify $(NONSHARED?-non_shared -lc_r:) $(OTHEROBJS) main.o $(OBJS) $(LIBRARIES)

clean:
	rm -f main.o byteswap_world.o $(OBJS)
