
ifeq ($(SUBDIRS),)
SUBDIRS	= src
endif

all clean:
	@set -e ; \
	  for d in $(SUBDIRS) ; do \
	    if [ -f $$d/Makefile ]; then \
              ( cd $$d && $(MAKE) $@ ) || exit 1 ; \
            fi ; \
	  done

docs:
	erl -noshell -pa $(BINDIR) -run edoc_run application \
            "'$(APPNAME)'" '"."' '$(DOC_OPTS)' -s init stop
