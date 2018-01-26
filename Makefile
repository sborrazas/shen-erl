INSTALL = install
INSTALL_DIR = $(INSTALL) -m755 -d
INSTALL_DATA = $(INSTALL) -m644
INSTALL_BIN = $(INSTALL) -m755

CSRCDIR = c_src

BINDIR = bin
EBINDIR = ebin
SRCDIR = src
INCDIR = include

PREFIX ?= /usr/local
DESTLIBDIR := $(PREFIX)/lib/lfe
DESTEBINDIR := $(DESTLIBDIR)/$(EBINDIR)
DESTBINDIR := $(DESTLIBDIR)/$(BINDIR)

CSRCS = $(notdir $(wildcard $(CSRCDIR)/*.c))
BINS = $(CSRCS:.c=)

ESRCS = $(notdir $(wildcard $(SRCDIR)/*.erl))
EBINS = $(ESRCS:.erl=.beam)

ERLCFLAGS = -W1 +debug_info
ERLC = erlc

EXE := shen-erlang

## Compile C files
$(BINDIR)/%: $(CSRCDIR)/%.c
	mkdir -p $(BINDIR)
	$(CC) -c -o $@ $< -Wall -Wextra -pedantic $(CFLAGS)


## Compile .erl files
$(EBINDIR)/%.beam: $(SRCDIR)/%.erl
	@$(INSTALL_DIR) $(EBINDIR)
	$(ERLC) -I $(INCDIR) -o $(EBINDIR) $(ERLCFLAGS) $<

all: $(EXE)

## Compile Erlang files using erlc
.PHONY: erlc-compile
erlc-compile: $(addprefix $(EBINDIR)/, $(EBINS)) $(addprefix $(BINDIR)/, $(BINS))

## shen-erlang compile
$(EXE): erlc-compile
