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

EXE := shen-erl

## Compile C files
$(BINDIR)/%: $(CSRCDIR)/%.c
	mkdir -p $(BINDIR)
	$(CC) -o $@ $^ -Wall -Wextra -pedantic $(CFLAGS)

## Compile .erl files
$(EBINDIR)/%.beam: $(SRCDIR)/%.erl
	@$(INSTALL_DIR) $(EBINDIR)
	$(ERLC) -I $(INCDIR) -o $(EBINDIR) $(ERLCFLAGS) $<

all: $(EXE)

## Compile Erlang files using erlc
.PHONY: erlc-compile
erlc-compile: $(addprefix $(EBINDIR)/, $(EBINS)) $(addprefix $(BINDIR)/, $(BINS))

.PHONY: clean
clean:
	rm -rf $(EBINDIR)/*.beam bin/*

## shen-erlang compile
$(EXE): erlc-compile

## Lexer & parser
lexer:
	erl -noshell -eval 'leex:file("src/kl_scan"), init:stop().'

parser:
	erl -noshell -eval 'yecc:file("src/kl_parse"), init:stop().'
