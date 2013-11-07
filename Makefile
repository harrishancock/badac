CXXFLAGS = -Wall -std=c++11
LDFLAGS = 
LDLIBS =
#CXX = clang++

.SUFFIXES:
.SUFFIXES: .cpp .o

BINNAME = test_parser
OBJS = parser.o scanner.o test_parser.o

$(BINNAME): $(OBJS)
	$(CXX) -o $(BINNAME) $(OBJS) $(LDFLAGS) $(LDLIBS)

test_parser.o: parser.hpp scanner.hpp
parser.o: parser.hpp scanner.hpp
scanner.o: scanner.hpp

.PHONY: clean
clean:
	-rm -f $(BINNAME) *.o *~ gmon.out
