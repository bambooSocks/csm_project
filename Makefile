
.PHONY: all generate clean run run_d


all:
	@echo "Help:"
	@echo "  make run      - run the program in non-deterministic mode"
	@echo "  make run_d    - run the program in deterministic mode"
	@echo "  make generate - generates parser and lexer files"
	@echo "  make clean    - deletes generated files"


run:
ifeq ($(OS), Windows_NT)
	fsi --exec Program.fsx
else
	fsharpi Program.fsx
endif


run_d:
ifeq ($(OS), Windows_NT)
	fsi --exec Program.fsx -d
else
	fsharpi Program.fsx -d
endif


generate:
ifeq ($(OS), Windows_NT)
	FsLexYacc.10.0.0/build/fslex/net46/fslex.exe GCLLexer.fsl --unicode
	@echo -e "\n\e[32mDONE 1 of 2\e[0m\n"
	FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe GCLParser.fsp --module GCLParser
	@echo -e "\n\e[32mDONE 2 of 2\e[0m\n"
else
	mono FsLexYacc.10.0.0/build/fslex/net46/fslex.exe GCLLexer.fsl --unicode
	@echo -e "\n\e[32mDONE 1 of 2\e[0m\n"
	mono FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe GCLParser.fsp --module GCLParser
	@echo -e "\n\e[32mDONE 2 of 2\e[0m\n"
endif


clean:
ifeq ($(OS), Windows_NT)
	del GCLLexer.fs GCLParser.fs GCLParser.fsi
else
	rm GCLLexer.fs GCLParser.fs GCLParser.fsi
endif
