
all:
ifdef OS
	FsLexYacc.10.0.0\build\fslex\net46\fslex.exe GCLLexer.fsl --unicode
	FsLexYacc.10.0.0\build\fsyacc\net46\fsyacc.exe GCLParser.fsp --module GCLParser
else
	mono FsLexYacc.10.0.0/build/fslex/net46/fslex.exe GCLLexer.fsl --unicode
	mono FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe GCLParser.fsp --module GCLParser
endif
