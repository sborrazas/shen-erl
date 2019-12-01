BASE_DIR = $(shell pwd)

SHENVERSION = 21.0

INSTALL = install
INSTALL_DIR = $(INSTALL) -m755 -d
INSTALL_DATA = $(INSTALL) -m644
INSTALL_BIN = $(INSTALL) -m755

CSRCDIR = c_src

BINDIR = bin
EBINDIR = ebin
SRCDIR = src
INCDIR = include
KLSRCDIR = kl

CSRCS = $(notdir $(wildcard $(CSRCDIR)/*.c))
BINS = $(CSRCS:.c=)

ESRCS = $(notdir $(wildcard $(SRCDIR)/*.erl))
EBINS = $(ESRCS:.erl=.beam)

KL_SRCS = $(notdir $(wildcard $(KLSRCDIR)/*.kl))

ERLCFLAGS = -W1 +debug_info
ERLC = erlc

EXE ?= shen-erl

.PHONY: all
.DEFAULT: all
all: $(EXE)

## Shen sources
ShenOSKernel-$(SHENVERSION):
	curl -LO 'https://github.com/Shen-Language/shen-sources/releases/download/shen-$(SHENVERSION)/ShenOSKernel-$(SHENVERSION).tar.gz'
	tar xzf ShenOSKernel-$(SHENVERSION).tar.gz

$(KLSRCDIR): ShenOSKernel-$(SHENVERSION)
	cp -r ShenOSKernel-$(SHENVERSION)/klambda $(KLSRCDIR)

## Compile C files
$(BINDIR)/%: $(CSRCDIR)/%.c
	mkdir -p $(BINDIR)
	$(CC) -o $@ $^ -Wall -Wextra -pedantic $(CFLAGS)

## Compile .erl files
$(EBINDIR)/%.beam: $(SRCDIR)/%.erl
	@$(INSTALL_DIR) $(EBINDIR)
	$(ERLC) -I $(INCDIR) -o $(EBINDIR) $(ERLCFLAGS) $<

## Compile Erlang files using erlc
.PHONY: erlc-compile
erlc-compile: $(addprefix $(EBINDIR)/, $(EBINS)) $(addprefix $(BINDIR)/, $(BINS))

.PHONY: clean
clean:
	rm -rf $(EBINDIR)/*.beam $(BINDIR)/* erl_crash.dump test/shen test/logs

## shen-erlang compile
$(EXE): erlc-compile

## Lexer & parser
.PHONY: lexer
lexer:
	erl -noshell -eval 'leex:file("src/shen_erl_kl_scan"), init:stop().'

.PHONY: parser
parser:
	erl -noshell -eval 'yecc:file("src/shen_erl_kl_parse"), init:stop().'

## Compile .kl files
.PHONY: shen-kl
shen-kl: $(EXE)
	@$(INSTALL_DIR) $(EBINDIR)
	SHEN_ERL_ROOTDIR=$(BASE_DIR) $(BINDIR)/$(EXE) --kl $(addprefix $(KLSRCDIR)/, $(KL_SRCS)) --output-dir $(EBINDIR)

## Tests
test/shen: ShenOSKernel-$(SHENVERSION)
	cp -r ShenOSKernel-$(SHENVERSION)/tests test/shen

.PHONY: shen-tests
shen-tests: shen-kl test/shen
	SHEN_ERL_ROOTDIR=$(BASE_DIR) $(BINDIR)/$(EXE) --script scripts/run-shen-tests.shen


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

.PHONY: docker-dialyze
docker-dialyze:
	@docker run --rm \
							--volume "$(BASE_DIR)":/app \
							--volume "$(BASE_DIR)/Erlmakefile":/app/Makefile \
							--workdir /app \
							$(DOCKER_ERLANG_IMAGE) \
							/bin/bash -c "make dialyze"
