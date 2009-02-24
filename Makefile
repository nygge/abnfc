SUBDIRS=src \
	samples/abnf \
#	samples/sdp \
#	samples/sip

include ./subdir.mk
include ./vsn.mk

APPNAME=abnf
DOC_OPTS=[{def,{vsn,"$(ABNF_VSN)"}}]
