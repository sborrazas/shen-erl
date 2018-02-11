BASE_DIR = $(shell pwd)

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
	rm -rf $(EBINDIR)/*.beam bin/* erl_crash.dump

## shen-erlang compile
$(EXE): erlc-compile

## Lexer & parser
lexer:
	erl -noshell -eval 'leex:file("src/shen_erl_kl_scan"), init:stop().'

parser:
	erl -noshell -eval 'yecc:file("src/shen_erl_kl_parse"), init:stop().'

################################################################################
## DOCKER
################################################################################

DOCKER_ERLANG_IMAGE = erlang:20.0.2

.PHONY: docker-test
docker-test:
	@docker run --rm \
							--volume "$(BASE_DIR)":/app \
							--volume "$(BASE_DIR)/Erlmakefile":/app/Makefile \
							--workdir /app \
							$(DOCKER_ERLANG_IMAGE) \
							/bin/bash -c "make tests"
