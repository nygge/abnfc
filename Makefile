SUBDIRS=src \
	samples/sip
#	samples/sdp \

include ./subdir.mk
include ./vsn.mk

APPNAME=abnf
DOC_OPTS=[{def,{vsn,"$(ABNF_VSN)"}}]
