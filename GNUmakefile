#
# Builds the Ivory Emulator (VLM)
#
#   The command line to build an emulator is
#
#      make {OPTIONS="option1 option2 ..."} {NONSHARED=YES} TARGET
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
#   files first to ensure that everything is properly compiled.  Use the "clean" target.
#
#   In fact, if you change any settings between builds, it's a good idea to build from scratch
#      
#   On the make command line, use PROFILE=YES to build the VLM using the profiling option
#   to gather statistics on execution frequencies.
#
#   On the make command line, use NONSHARED=YES to build the VLM using the non-shared
#   libraries.  (The default is to use the shared libraries.)
#


LIFE = ./life-support
EMULATOR = ./emulator
G5EMULATOR = ./g5-emulator
OTHER = ./other

genera: MAINOPTIONS = -DGENERA -DAUTOSTART
minima: MAINOPTIONS = -DMINIMA
iverify: MAINOPTIONS = -DIVERIFY

CFLAGS = -m64 -std=gnu99 -g2 -I. -I$(LIFE) -I$(EMULATOR) -I$(G5EMULATOR) $(MAINOPTIONS) $(OPTIONS)
AFLAGS = -m64 -g2 -I. -I$(LIFE) -I$(EMULATOR) -I$(G5EMULATOR) $(MAINOPTIONS) $(OPTIONS)

.SUFFIXES:
.SUFFIXES: .o .c .S

.c.o:
	$(CC) $(CFLAGS) -o $@ -c $<

.S.o:
	$(CC) $(AFLAGS) -o $@ -c $<

SRCS = main.c spy.c world_tools.c utilities.c \
       $(LIFE)/cold_load.c $(LIFE)/console.c $(LIFE)/disks.c $(LIFE)/initialization.c \
       $(LIFE)/network.c $(LIFE)/message_channels.c $(LIFE)/polling.c $(LIFE)/queues.c \
       $(LIFE)/signals.c \
       $(EMULATOR)/interfac.c $(EMULATOR)/interpds.c $(EMULATOR)/externals.c \
       $(EMULATOR)/memory.c

NETWORKSOURCES = $(LIFE)/network-osf.c $(LIFE)/network-linux.c $(LIFE)/network-darwin.c

ifndef FAKEEMULATOR
  ASMS = $(EMULATOR)/emulator.S
  EMULATOROBJ = $(EMULATOR)/emulator.o
  COMPONENTS = $(G5EMULATOR)/idispat.s $(G5EMULATOR)/ifuncom1.s $(G5EMULATOR)/ifuncom2.s \
               $(G5EMULATOR)/ifungene.s $(G5EMULATOR)/ifunfcal.s $(G5EMULATOR)/ifunloop.s \
               $(G5EMULATOR)/ifunlist.s $(G5EMULATOR)/ifuninst.s $(G5EMULATOR)/ifunmath.s \
               $(G5EMULATOR)/ifunarra.s $(G5EMULATOR)/ifunmove.s $(G5EMULATOR)/ifunpred.s \
               $(G5EMULATOR)/ifunsubp.s $(G5EMULATOR)/ifunfext.s $(G5EMULATOR)/ifunlexi.s \
               $(G5EMULATOR)/ifunbits.s $(G5EMULATOR)/ifunblok.s $(G5EMULATOR)/ifunbind.s \
               $(G5EMULATOR)/ifunfull.s $(G5EMULATOR)/ifunbnum.s $(G5EMULATOR)/ifuntrap.s \
               $(G5EMULATOR)/ihalt.s $(G5EMULATOR)/idouble.s $(G5EMULATOR)/ifunjosh.s \
               $(G5EMULATOR)/ifuntran.s
else
  ASMS = $(EMULATOR)/fake_emulator.c
  EMULATOROBJ = $(EMULATOR)/fake_emulator.o
  COMPONENTS = 
endif

OBJS = $(LIFE)/cold_load.o $(LIFE)/console.o $(LIFE)/disks.o $(LIFE)/initialization.o \
       $(LIFE)/network.o $(LIFE)/message_channels.o $(LIFE)/polling.o $(LIFE)/queues.o \
       $(LIFE)/signals.o \
       world_tools.o utilities.o spy.o \
       $(EMULATOR)/interfac.o $(EMULATOR)/interpds.o $(EMULATOR)/externals.o \
       $(EMULATOR)/memory.o $(EMULATOROBJ)

EMULATORINCLUDES = $(EMULATOR)/aihead.h $(G5EMULATOR)/aistat.h $(EMULATOR)/ivoryrep.h

OTHEROBJS = 

ifdef NONSHARED
  OTHEROBJS += 
  LIBRARIES = -lpthread -lc -lX11 -lm
  EARLYLIBS = -L/usr/X11R6/lib64
else
  OTHEROBJS +=
  LIBRARIES = -lpthread -lc -lX11 -lm
  EARLYLIBS = -L/usr/X11R6/lib64
endif

ifdef PROFILE
  override PROFILE = -p
endif

$(LIFE)/network.o: $(NETWORKSOURCES)

$(EMULATOROBJ): $(ASMS) $(COMPONENTS)

$(EMULATOR)/externals.o: $(EMULATOR)/externals.c $(EMULATORINCLUDES)
$(EMULATOR)/interpds.o: $(EMULATOR)/interpds.c $(EMULATORINCLUDES) $(EMULATOR)/asmfuns.h
$(EMUALTOR)/interfac.o: $(EMULATOR)/interfac.c $(EMULATORINCLUDES)
spy.o: spy.c $(EMULATORINCLUDES)

genera: main.o byteswap_world.o $(OBJS) $(OTHEROBJS)
	cc -m64 $(PROFILE) -o genera $(EARLYLIBS) $(OTHEROBJS) main.o $(OBJS) $(LIBRARIES)
	cc -m64 $(PROFILE) -o byteswap_world $(EARLYLIBS) $(OTHEROBJS) byteswap_world.o $(OBJS) $(LIBRARIES)

minima: main.o $(OBJS) $(OTHEROBJS)
	cc -m64 $(PROFILE) -o minima $(EARLYLIBS) $(OTHEROBJS) main.o $(OBJS) $(LIBRARIES)

iverify: main.o $(OBJS) $(OTHEROBJS)
	cc -m64 $(PROFILE) -o iverify $(EARLYLIBS) $(OTHEROBJS) main.o $(OBJS) $(LIBRARIES)

clean:
	rm -f main.o byteswap_world.o $(OBJS)
