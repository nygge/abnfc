include ../../include.mk

ABNF_SOURCES := $(wildcard *.abnf)
ABNF_OBJECTS := $(ABNF_SOURCES:%.abnf=./%.erl)
ERL_OBJECTS := $(ABNF_SOURCES:%.abnf=$(EBIN_DIR)/%.$(EMULATOR))

all: $(ERL_OBJECTS)

clean:
	-rm $(ERL_OBJECTS) $(ABNF_OBJECTS)

$(EBIN_DIR)/rfc4566.beam: rfc4566.hrl
