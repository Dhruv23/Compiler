CXX		= c++ -std=c++17
CXXFLAGS	= -g -Wall
EXTRAS		= lexer.cpp
OBJS		= Register.o Scope.o Symbol.o Tree.o Type.o allocator.o \
		  checker.o generator.o lexer.o parser.o string.o writer.o Label.o
PROG		= scc


all:		$(PROG)

$(PROG):	$(EXTRAS) $(OBJS)
		$(CXX) -o $(PROG) $(OBJS)

clean:;		$(RM) $(PROG) core *.o

clobber:;	$(RM) $(EXTRAS) $(PROG) core *.o

lexer.cpp:	lexer.l
		$(LEX) $(LFLAGS) -t lexer.l > lexer.cpp

clall: clean all