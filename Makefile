include Makefile.config

BDIR := _build

# OCaml Kernel
SRC := src
KERNEL_MLIS := $(wildcard $(SRC)/*.mli)
KERNEL_CMXS := $(patsubst $(SRC)/%.ml,$(BDIR)/%.cmx,$(addprefix $(SRC)/,$(ML)))
KERNEL_STUBS += $(patsubst $(SRC)/%.c,$(BDIR)/%.o,$(wildcard $(SRC)/*.c))
KERNEL := $(BDIR)/kernel.o

# librpi-boot-ocaml
SRC_BOOT := src-boot
BOOT_OBJS := $(patsubst $(SRC_BOOT)/%.S,$(BDIR)/%.o,$(wildcard $(SRC_BOOT)/*.S))
BOOT_OBJS += $(patsubst $(SRC_BOOT)/%.c,$(BDIR)/%.o,$(wildcard $(SRC_BOOT)/*.c))
BOOT_A += $(BDIR)/librpi-boot-ocaml.a

ifeq ($(LIBC_USE_NEWLIB),true)
	LIBC_DIR := libc-newlib/src
	LIBC_CLIBS := -lc
else
	LIBC_DIR := libc-ocaml/src
	LIBC_CLIBS :=
endif

LIBC_SRCS := $(wildcard $(LIBC_DIR)/*.c)
LIBC_OBJS := $(patsubst $(LIBC_DIR)/%.c,$(BDIR)/%.o,$(LIBC_SRCS))
LIBC_A := $(BDIR)/libc-ocaml.a

# Build tools
OCAMLOPT := ocamlfind -toolchain armv7_none_eabihf ocamlopt
CROSS ?= arm-none-eabi-
CC := $(CROSS)gcc
AR := $(CROSS)ar
LD := $(CROSS)ld
OBJCOPY := $(CROSS)objcopy
OBJDUMP := $(CROSS)objdump

# C and assembly compiler options
DEBUG :=
DEPEND_FLAGS := -MD -MP
ARCH_FLAGS := -march=armv7-a -mtune=cortex-a7 -mfpu=neon-vfpv4 -mfloat-abi=hard

ASFLAGS := $(ARCH_FLAGS) -ffreestanding

CINCS := -I src-boot
CFLAGS := $(CINCS) $(DEPEND_FLAGS) $(DEBUG) $(ARCH_FLAGS)
CFLAGS += -ffreestanding -std=c99 -O2 -Wall

LINKER := rpi-boot-ocaml.ld
LASMRUN_PATH := $(shell $(OCAMLOPT) -where)
LDFLAGS := -T$(LINKER)
LDFLAGS += -L $(LASMRUN_PATH) # For -lasmrun
LDFLAGS += -L $(LC_PATH)      # For -lc -lm for now
LDFLAGS += -L $(LGCC_PATH)    # For -lgcc
CLIBS= -lbigarray -lasmrun -lgcc -lm

# Build rules

mk_bdir := $(shell mkdir -p $(BDIR))

.PHONY: doc
doc: $(KERNEL_CMXS) $(KERNEL_MLIS)
	mkdir -p $(BDIR)/doc
	ocamldoc -colorize-code -charset utf-8 -I $(BDIR) -html \
           -d $(BDIR)/doc $(KERNEL_MLIS)
	cp doc/style.css $(BDIR)/doc/

clean:
	$(RM) -r $(BDIR)

rpi-boot-ocaml: $(BDIR)/rpi-boot-ocaml.img $(BDIR)/rpi-boot-ocaml.dasm

$(LIBC_A): $(LIBC_OBJS)
	$(AR) rc -o $@ $^

$(BOOT_A): $(BOOT_OBJS)
	$(AR) rc -o $@ $^

$(KERNEL): $(KERNEL_CMXS)
	$(OCAMLOPT) -output-obj -o $@ bigarray.cmxa $^

$(BDIR)/rpi-boot-ocaml.elf: $(BOOT_A) $(KERNEL_STUBS) $(KERNEL) $(LINKER) \
                            $(LIBC_A)
	$(LD) -o $@ -Map $(BDIR)/rpi-boot-ocaml.map \
   $(LDFLAGS) $(KERNEL_STUBS) $(KERNEL) $(CLIBS) $(LIBC_CLIBS) \
   $(LIBC_A) $(BOOT_A)

$(BDIR)/rpi-boot-ocaml.img: $(BDIR)/rpi-boot-ocaml.elf
	$(OBJCOPY) $(BDIR)/rpi-boot-ocaml.elf -O binary $@

$(BDIR)/rpi-boot-ocaml.dasm : $(BDIR)/rpi-boot-ocaml.elf
	$(OBJDUMP) -d $< > $@

$(BDIR)/%.o: $(LIBC_DIR)/%.c
	$(CC) $(CFLAGS) -I $(LIBC_DIR)/../include -c -o $@ $<

$(BDIR)/%.o: $(SRC_BOOT)/%.c
	$(CC) $(CFLAGS) -c -o $@ $<

$(BDIR)/%.o: $(SRC_BOOT)/%.S
	$(CC) $(ASFLAGS) -c -o $@ $<

$(BDIR)/%.o: $(SRC)/%.c
	$(OCAMLOPT) -ccopt -std=c99 -ccopt -I$(SRC) -o $@ $<
	mv $(shell basename $@) $(BDIR)/

$(BDIR)/%.o $(BDIR)/%.cmx: $(SRC)/%.ml
	$(eval MLI := $(wildcard $(patsubst $(SRC)/%.ml,$(SRC)/%.mli,$<)))
	@echo $(MLI)
	if [ "$(MLI)" ]; then\
	   $(OCAMLOPT) -I $(BDIR) -c \
       -o $(patsubst $(SRC)/%.ml,$(BDIR)/%.cmi,$<) $(MLI); \
  fi
	$(OCAMLOPT) -I $(BDIR) -c -o $@ $<

.PRECIOUS: %.cmx

include $(wildcard $(BDIR)/*.d)
