
PACKAGES = unix str num

MLPP		= -pp "camlp4o pa_macro.cmo"
MLDEP		= ocamlfind ocamldep -native $(MLPP)
MLC	        = ocamlfind ocamlopt -thread
MLCOPTS 	= $(MLPP) -I .. -ffast-math -warn-error A -inline 23
MLLINKOPTS      = unix.cmxa threads.cmxa nums.cmxa str.cmxa cryptokit.cmxa
COMMONLIB       = ../common.cmxa

.PHONY: $(COMMONLIB)

$(COMMONLIB):
	$(MAKE) -C ..
	
%: $(COMMONLIB) %.ml
	$(MLC) $(MLLINKOPTS) $(MLCOPTS) -o $@ $(COMMONLIB) $@.ml

clean:
	rm -f *.o *.cm* a.out *.bak *.obj *~ *.annot *.stackdump *.a $(LIBCMXA)
