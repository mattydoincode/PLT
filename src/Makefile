OBJS = ast.cmo sast.cmo parser.cmo scanner.cmo analyzer.cmo javagen.cmo pc.cmo

SRCS = $(wildcard *.*)
JAVA_SRCS = $(shell find java -name '*')
TESTS := $(shell find tests -name '*.pc' -o -name "*.out")
DEMOS = $(shell find finaldemo *.pc -name '*')

TARFILES = Makefile $(SRCS) $(JAVA_SRCS) $(TESTS) $(DEMOS)

pc : $(OBJS)
	ocamlc -g -o pc $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -g -c $<

%.cmi : %.mli
	ocamlc -g -c $<

pubCrawl.tar.gz : $(TARFILES)
	tar czf pubCrawl.tar.gz $(TARFILES)

.PHONY : cleanJava
cleanJava :
	rm -f java/function_* java/output.java java/*.class

.PHONY : clean
clean : cleanJava
	rm -f pc parser.ml parser.mli parser.output scanner.ml \
	*.cmo *.cmi *.out *.diff *.log *.gz

.PHONY : all
all : clean pc


# Generated by ocamldep *.ml *.mli
analyzer.cmo : sast.cmo ast.cmo
analyzer.cmx : sast.cmx ast.cmx
javagen.cmo : sast.cmo
javagen.cmx : sast.cmx
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmx parser.cmi
pc.cmo : scanner.cmo sast.cmo parser.cmi javagen.cmo ast.cmo analyzer.cmo
pc.cmx : scanner.cmx sast.cmx parser.cmx javagen.cmx ast.cmx analyzer.cmx
sast.cmo : ast.cmo
sast.cmx : ast.cmx
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx
parser.cmi : ast.cmo
