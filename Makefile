
RESULT			= 	tama
SOURCES			= 	util.ml \
					character.ml \
					tama.ml \
					window.ml \
					save.ml \
					main.ml

LIBS			=	bigarray sdl sdlttf sdlloader sdlmixer
INCDIRS			=	~/.opam/4.02.0/lib/sdl/
OCAMLLDFLAGS	=	-custom -cclib "-framework Cocoa"
OCAMLFLAGS		=	-w -a
THREADS			=	true

include OCamlMakefile
