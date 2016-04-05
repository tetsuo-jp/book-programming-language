PACKAGE = MyInterpreter
FILES   = Interpreter.hs Main.hs Parser.hs Syntax.hs examples \
	  Lexer.hs Makefile Parser.y Testing.hs
VER     = 1.0-`date +%Y%m%d`

# etc
RM	= rm -f

OPTS =
# OPTS = -prof -auto
# OPTS = -O -fvia-C -O2-for-C
OBJS = Syntax.o Lexer.o Parser.o Interpreter.o Main.o Testing.o

ifeq ($(OSTYPE),cygwin)
 EXT = .exe
else
 EXT =
endif


all: interpreter

interpreter: parser $(OBJS)
	ghc $(OPTS) -o interpreter $(OBJS)
	strip interpreter$(EXT)

parser: Parser.hs Parser.info

Parser.hs:
	happy Parser.y

Parser.info:
	happy Parser.y -i

.SUFFIXES : .hs .o

.hs.o:
	ghc ${OPTS} -c $<

clean:
	$(RM) Parser.info Parser.hs
	$(RM) *.o *.hi *~ *.bak core
	$(RM) interpreter$(EXT)
	$(RM) tags TAGS

tags:
	find -name \*.\*hs | xargs hasktags

zip:
	zip -9 $(PACKAGE)-$(VER).zip $(FILES)

tar:
	echo $(PACKAGE)-$(VER) > .package
	$(RM) -r `cat .package`
	mkdir `cat .package`
	ln $(FILES) `cat .package`
	tar cvf - `cat .package` | gzip -9 > `cat .package`.tar.gz
	$(RM) -r `cat .package` .package
