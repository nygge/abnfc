include ../vsn.mk

VSN=$(ABNFC_VSN)

include ../include.mk

all: $(ERL_OBJECTS) ../ebin/abnfc.app

clean:
	-rm $(ERL_OBJECTS)

$(EBIN_DIR)/abnf_gen.beam: ../include/abnfc_ast.hrl
$(EBIN_DIR)/abnfc_rfc4234.beam: abnfc_rfc4234.hrl ../include/abnfc_ast.hrl
$(EBIN_DIR)/rfc4234_core.beam: rfc4234_core.hrl
