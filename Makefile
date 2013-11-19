CXXFLAGS = -Wall -std=c++11
LDFLAGS = 
LDLIBS =
#CXX = clang++

.SUFFIXES:
.SUFFIXES: .cpp .o

BINNAME = test_parser
OBJS = parser.o scanner.o test_parser.o bada.o

$(BINNAME): $(OBJS)
	$(CXX) -o $(BINNAME) $(OBJS) $(LDFLAGS) $(LDLIBS)

test_parser.o: parser.hpp scanner.hpp
parser.o: parser.hpp scanner.hpp scoped_symbol_table.hpp bada.hpp splaytree.hpp ci_string.hpp
scanner.o: scanner.hpp ci_string.hpp bada.hpp
bada.o: bada.hpp

.PHONY: clean
clean:
	-rm -f $(BINNAME) *.o *~ gmon.out
