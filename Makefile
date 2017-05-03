# Ocaml defs
OCAML = ocamlc -g
OCAMLOPT = ocamlopt
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc
OCAMLFLAGS = # -labels
OCAMLLIBS =

OCAMLMKTOP = ocamlmktop -g
###str.cma unix.cma -cclib -lstr -cclib -lunix

##########

# interfaces
INTERFACES =
#id.mli hsparser.mli

# basic definitions
SRCS_COMMON = global.ml helper.ml locat.ml proc.ml valexp.ml \
intSets.ml simplQueue.ml abstractSimplicialComplex.ml directedGraph.ml \
printer.ml dfmVect.ml graph.ml hyperGraphImpl.ml bipartite.ml setSystem.ml \
hyperGraph.ml dfmVectField.ml carrierMap.ml carrierCpl.ml checker.ml \
deformin.ml

# parser front end
SRCS_PARSER =  cmaplexer.ml cmapparser.ml

## all *.ml source files
SRCS_CORE = $(SRCS_COMMON) $(SRCS_PARSER) main.ml

OBJS = $(SRCS_CORE:.ml=.cmo)
OPTOBJS = $(SRCS_CORE:.ml=.cmx)

TARGET = cont.exe
OPTTARGET = cont.opt.exe

#### test files

SRCS_TEST = $(SRCS_COMMON)
OBJS_TEST = $(SRCS_TEST:.ml=.cmo)


##########

default::
	$(MAKE) all

all:: $(TARGET) $(OPTTARGET)
noopt:: $(TARGET)

######################################################################

.SUFFIXES: .ml .mli .cmi .cmo .cmx .mly .mll .java .class

#.ml.mli:
#	$(OCAML) $(OCAMLFLAGS) $(OCAMLLIBS) -i $<

.ml.cmo:
	$(OCAML) $(OCAMLFLAGS) $(OCAMLLIBS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLFLAGS) $(OCAMLLIBS) -c $<

.mli.cmi:
	$(OCAML) $(OCAMLFLAGS) $(OCAMLLIBS) -c $<

cmaplexer.ml: cmaplexer.mll
	$(OCAMLLEX) cmaplexer.mll

cmapparser.ml cmapparser.mli: cmapparser.mly
	$(OCAMLYACC) cmapparser.mly

cmapparser.cmi: cmapparser.mli
cmapparser.cmo: cmapparser.cmi
cmapparser.cmx: cmapparser.cmi
cmaplexer.cmo: cmapparser.cmi
cmaplexer.cmx: cmapparser.cmi


DEPENDS = $(SRCS_COMMON) $(INTERFACES) cmaplexer.ml cmapparser.mly main.ml
depend: $(DEPENDS)
	ocamldep $(DEPENDS) > depend

# clean up
clean:: clean-cwd

clean-cwd::
	rm -f *~ *.o *.cm* *.exe cmaplexer.ml cmapparser.ml cmapparser.mli depend

# archiver
PACKNAME=contdeform
package::
	zip $(PACKNAME) Makefile *.md *.ml *.ml? CMaps/*.map


######################################################################
# object code entries
######################################################################
cont.exe: $(OBJS)
	$(OCAML) -o $@ $(OBJS)
cont.opt.exe: $(OPTOBJS)
	$(OCAMLOPT) -o $@ $(OPTOBJS)

#test.exe: $(OBJS)
#	$(OCAMLMKTOP) -o $@ $(OBJS)

include depend
