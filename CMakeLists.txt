# Harris' Baby Ada Compiler CMake build script

cmake_minimum_required(VERSION 2.8)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

##############################################################################
# Targets

set(SRCS src/bada.cpp src/parser.cpp src/scanner.cpp src/main.cpp)

add_executable(badac ${SRCS})
install(TARGETS badac RUNTIME DESTINATION bin)

##############################################################################
# Testing

enable_testing()

set(TESTDIR ${CMAKE_SOURCE_DIR}/tests)

add_test(AssignToConstantBoolean badac ${TESTDIR}/AssignToConstantBoolean.ada)
add_test(AssignToConstantInteger badac ${TESTDIR}/AssignToConstantInteger.ada)
add_test(AssignToConstantReal badac ${TESTDIR}/AssignToConstantReal.ada)
add_test(ReadToConstantBoolean badac ${TESTDIR}/ReadToConstantBoolean.ada)
add_test(ReadToConstantInteger badac ${TESTDIR}/ReadToConstantInteger.ada)
add_test(ReadToConstantReal badac ${TESTDIR}/ReadToConstantReal.ada)
add_test(ConstantDeclarationWithoutAssignment badac ${TESTDIR}/ConstantDeclarationWithoutAssignment.ada)
add_test(NonConstantDeclarationWithAssignment badac ${TESTDIR}/NonConstantDeclarationWithAssignment.ada)
add_test(NonBooleanIfCondition badac ${TESTDIR}/NonBooleanIfCondition.ada)
add_test(NonBooleanWhileCondition badac ${TESTDIR}/NonBooleanWhileCondition.ada)
add_test(ProgramIdentifierMismatch badac ${TESTDIR}/ProgramIdentifierMismatch.ada)
add_test(TypeMismatchAddOp badac ${TESTDIR}/TypeMismatchAddOp.ada)
add_test(TypeMismatchMulOp badac ${TESTDIR}/TypeMismatchMulOp.ada)
add_test(TypeMismatchRelOp badac ${TESTDIR}/TypeMismatchRelOp.ada)
add_test(TypeMismatchNot badac ${TESTDIR}/TypeMismatchNot.ada)
add_test(TypeMismatchAssignment badac ${TESTDIR}/TypeMismatchAssignment.ada)

set_tests_properties(
    AssignToConstantBoolean
    AssignToConstantInteger
    AssignToConstantReal
    ReadToConstantBoolean
    ReadToConstantInteger
    ReadToConstantReal
    ConstantDeclarationWithoutAssignment
    NonConstantDeclarationWithAssignment
    NonBooleanIfCondition
    NonBooleanWhileCondition
    ProgramIdentifierMismatch
    TypeMismatchAddOp
    TypeMismatchMulOp
    TypeMismatchRelOp
    TypeMismatchNot
    TypeMismatchAssignment
    PROPERTIES WILL_FAIL TRUE)
