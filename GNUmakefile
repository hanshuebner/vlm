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
X86EMULATOR = ./x86_64-emulator
OTHER = ./other

CPU = $(X86EMULATOR)

genera: MAINOPTIONS = -DGENERA -DAUTOSTART -DUSE_TUN
minima: MAINOPTIONS = -DMINIMA
iverify: MAINOPTIONS = -DIVERIFY

OPT = -O -mtune=nocona \
-fforce-mem -foptimize-sibling-calls -fstrength-reduce \
-fexpensive-optimizations \
-fsched-interblock -fsched-spec -fpeephole2 \
-freorder-blocks  -freorder-functions \
-funit-at-a-time \
-falign-functions  -falign-jumps -falign-loops -falign-labels \
-fcrossjumping \
-finline-functions -fweb, -frename-registers -funswitch-loops \
-fregmove \
-fcse-follow-jumps \
-fcse-skip-blocks -frerun-cse-after-loop  -frerun-loop-opt -fgcse \
-fgcse-lm  -fgcse-sm  -fgcse-las -fdelete-null-pointer-checks \
-foptimize-sibling-calls -fcaller-saves 

# broken
#-fstrict-aliasing
#-fschedule-insns  -fschedule-insns2

#-fforce-mem -foptimize-sibling-calls -fstrength-reduce -fcse-follow-jumps
#-fcse-skip-blocks -frerun-cse-after-loop  -frerun-loop-opt -fgcse
#-fgcse-lm  -fgcse-sm  -fgcse-las -fdelete-null-pointer-checks
#-fexpensive-optimizations -fregmove -fschedule-insns  -fsched-ule-insns2
#-fsched-interblock  -fsched-spec -fcaller-saves -fpeep-hole2
#-freorder-blocks  -freorder-functions -fstrict-aliasing
#-funit-at-a-time -falign-functions  -falign-jumps -falign-loops
#-falign-labels -fcrossjumping

#-finline-functions, -fweb, -frename-registers and -funswitch-loops

CFLAGS = $(OPT) -std=gnu99 -g2 -I/usr/X11R6/include -I. -I$(LIFE) -I$(EMULATOR) -I$(X86EMULATOR) $(MAINOPTIONS) $(OPTIONS)
AFLAGS = -g2 -I. -I$(LIFE) -I$(EMULATOR) -I$(X86EMULATOR) $(MAINOPTIONS) $(OPTIONS)

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

NETWORKSOURCES = $(LIFE)/network-osf.c \
	$(LIFE)/network-linux.c $(LIFE)/network-tun-linux.c \
	$(LIFE)/network-darwin.c $(LIFE)/network-libpcap.c

FAKEEMULATOR=y
ifndef FAKEEMULATOR
  ASMS = $(EMULATOR)/emulator.S
  EMULATOROBJ = $(EMULATOR)/emulator.o
  COMPONENTS = $(CPU)/idispat.s $(CPU)/ifuncom1.s $(CPU)/ifuncom2.s \
               $(CPU)/ifungene.s $(CPU)/ifunfcal.s $(CPU)/ifunloop.s \
               $(CPU)/ifunlist.s $(CPU)/ifuninst.s $(CPU)/ifunmath.s \
               $(CPU)/ifunarra.s $(CPU)/ifunmove.s $(CPU)/ifunpred.s \
               $(CPU)/ifunsubp.s $(CPU)/ifunfext.s $(CPU)/ifunlexi.s \
               $(CPU)/ifunbits.s $(CPU)/ifunblok.s $(CPU)/ifunbind.s \
               $(CPU)/ifunfull.s $(CPU)/ifunbnum.s $(CPU)/ifuntrap.s \
               $(CPU)/ihalt.s $(CPU)/idouble.s $(CPU)/ifunjosh.s \
               $(CPU)/ifuntran.s
else
#  ASMS = $(EMULATOR)/fake_emulator.c
#  EMULATOROBJ = $(EMULATOR)/fake_emulator.o
  ASMS = stub/stub.c
  EMULATOROBJ = stub/stub.o
  COMPONENTS = 
endif

OBJS = $(LIFE)/cold_load.o $(LIFE)/console.o $(LIFE)/disks.o $(LIFE)/initialization.o \
       $(LIFE)/network.o $(LIFE)/message_channels.o $(LIFE)/polling.o $(LIFE)/queues.o \
       $(LIFE)/signals.o \
       world_tools.o utilities.o spy.o \
       $(EMULATOR)/interfac.o $(EMULATOR)/interpds.o $(EMULATOR)/externals.o \
       $(EMULATOR)/memory.o $(EMULATOROBJ)

EMULATORINCLUDES = $(EMULATOR)/aihead.h $(CPU)/aistat.h $(EMULATOR)/ivoryrep.h

OTHEROBJS = 

ifdef NONSHARED
  OTHEROBJS += 
  LIBRARIES = -lpthread -lc -lX11 -lm -lpcap
  EARLYLIBS = -L/opt/ppc64/X11R6/lib
else
  OTHEROBJS +=
#  LIBRARIES = -lpthread -lc -lX11 -lm
#  EARLYLIBS = -L/opt/ppc64/X11R6/lib
  LIBRARIES = -lpthread -lc -lX11 -lm -lpcap
  EARLYLIBS = -L/usr/X11R6/lib64 -L/usr/X11R6/lib
endif

ifdef PROFILE
  override PROFILE = -p
endif

all: genera

$(LIFE)/network.o: $(NETWORKSOURCES)

stub/stub.o: glob(stub/output*)

$(EMULATOROBJ): $(ASMS) $(COMPONENTS)

$(EMULATOR)/externals.o: $(EMULATOR)/externals.c $(EMULATORINCLUDES)
$(EMULATOR)/interpds.o: $(EMULATOR)/interpds.c $(EMULATORINCLUDES) $(EMULATOR)/asmfuns.h
$(EMUALTOR)/interfac.o: $(EMULATOR)/interfac.c $(EMULATORINCLUDES)
spy.o: spy.c $(EMULATORINCLUDES)

genera: main.o byteswap_world.o $(OBJS) $(OTHEROBJS)
	cc $(PROFILE) -o genera $(EARLYLIBS) $(OTHEROBJS) main.o $(OBJS) $(LIBRARIES)
	cc $(PROFILE) -o byteswap_world $(EARLYLIBS) $(OTHEROBJS) byteswap_world.o $(OBJS) $(LIBRARIES)

minima: main.o $(OBJS) $(OTHEROBJS)
	cc $(PROFILE) -o minima $(EARLYLIBS) $(OTHEROBJS) main.o $(OBJS) $(LIBRARIES)

iverify: main.o $(OBJS) $(OTHEROBJS)
	cc $(PROFILE) -o iverify $(EARLYLIBS) $(OTHEROBJS) main.o $(OBJS) $(LIBRARIES)

clean:
	rm -f main.o byteswap_world.o $(OBJS)
