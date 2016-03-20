#
# Common library
# src-level makefile
#
# (C) 2007 Enrico Zeffiro
#

PACKAGES    = unix str num stdlib http

SRCS 	    = prelude.ml \
              xList.ml xString.ml env.ml xStream.ml math.ml time.ml xThread.ml containers.ml \
              log.ml device.ml security.ml logLib.ml io.ml timer.ml pool.ml comm.ml \
              cleanup.ml \
              poolLib.ml commLib.ml apiComm.ml apiCommLib.ml cache.ml utility.ml deviceLib.ml xml.ml \
			  httpComm.ml httpCommLib.ml \
			  externalConfigAbsyn.ml externalConfigParser.ml externalConfigLexer.ml externalConfig.ml
              
TARGET      = common.cmxa

MLPP		= -pp "camlp4o pa_macro.cmo"
MLPKGS      = -package "$(PACKAGES)"
MLLIBS      = $(addprefix -I $(LIBBASE)/src/, $(LIBS))
MLDEP		= ocamlfind ocamldep -native $(MLPP) $(MLLIBS) $(MLPKGS)
MLC		    = ocamlfind ocamlopt -thread
MLCOPTS 	= $(MLPP) -w A -w e -warn-error A -inline 100 $(MLLIBS) $(MLPKGS)
OBJS		= $(SRCS:.ml=.cmx)


all: $(TARGET)

$(TARGET): $(OBJS)
	$(MLC) -a $(OBJS) -o $@

%.cmx: %.ml
	$(MLC) -c $(MLCOPTS) $<

%.cmi: %.mli
	$(MLC) -c $(MLCOPTS) $<

externalConfigLexer.ml: externalConfigLexer.mll
	ocamllex $<

externalConfigParser.mli externalConfigParser.ml: externalConfigParser.mly	
	ocamlyacc $<

doc:
	rm -f ../doc/* && ocamldoc.opt -v $(MLPP) -I +threads -I . -all-params -short-functors -html -d ../doc -sort -t "Common Library 2.3" $(SRCS)
	
clean:
	rm -f externalConfigParser.mli externalConfigParser.ml externalConfigLexer.ml *.o *.cm* a.out *.bak *.obj *~ *.annot *.stackdump *.a $(TARGET)
	
.depend: $(SRCS) $(MAKEFILE)
	$(MLDEP) -native $(SRCS) $(MAKEFILE) >.depend

include .depend
