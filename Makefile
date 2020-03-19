
.PHONY: all generate clean run run_d

ifeq ($(OS), Windows_NT)
	RM=del
	FS=fsi --exec
else
	RM=rm
	FS=fsharpi
	MONO=mono
endif


all:
	@echo "Help:"
	@echo "  make generate - generates parser and lexer files"
	@echo "  make clean    - deletes generated files"


run:
	$(FS) Program.fsx


run_d:
	$(FS) Program.fsx -d


generate:

	$(MONO) FsLexYacc.10.0.0/build/fslex/net46/fslex.exe GCLLexer.fsl --unicode
	@echo "\n\e[32mDONE 1 of 2\e[0m\n"
	$(MONO) FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe GCLParser.fsp --module GCLParser
	@echo "\n\e[32mDONE 2 of 2\e[0m\n"


clean:

	$(RM) GCLLexer.fs GCLParser.fs GCLParser.fsi
