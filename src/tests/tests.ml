(** This file should be used to write your tests of your other code. *)

open Batteries;;

open OUnit2;;

open TestUtils;;

let all_tests =
  [
    test_success "test_code/4.bird" "4";
    test_success "test_code/after_4.bird" "5";
    test_success "test_code/before_4.bird" "3";
    test_success "test_code/before_after_4.bird" "4";
    test_success "test_code/arithmetic.bird" "-9";
    test_success "test_code/minus1.bird" "3";
    test_success "test_code/minus2.bird" "-2";
    test_success "test_code/minus3.bird" "2";
    test_success "test_code/plus1.bird" "2";
    test_success "test_code/plus2.bird" "6";
    test_success "test_code/plus3.bird" "6";
    test_success "test_code/times1.bird" "8";
    test_success "test_code/times2.bird" "15";
    test_success "test_code/let1.bird" "5";
    test_success "test_code/let2.bird" "10";
    test_success "test_code/hardArithmetic.bird" "1728";
    test_compile_failure "test_code/testUnboundVariable.bird" "Unbound variable a.";
    test_success "test_code/testScope.bird" "21";
    test_success "test_code/true.bird" "true";
    test_success "test_code/false.bird" "false";
    test_success "test_code/isBoolTrue1.bird" "true";
    test_success "test_code/isBoolTrue2.bird" "true";
    test_success "test_code/isBoolFalse1.bird" "false";
    test_success "test_code/isIntFalse.bird" "false";
    test_success "test_code/isIntTrue.bird" "true";
    test_success "test_code/equal1.bird" "true";
    test_success "test_code/equal2.bird" "false";
    test_success "test_code/lessThan1.bird" "true";
    test_success "test_code/lessThan2.bird" "false";
    test_success "test_code/greaterThan1.bird" "true";
    test_success "test_code/greaterThan2.bird" "false";
    test_success "test_code/and1.bird" "true";
    test_success "test_code/and2.bird" "false";
    test_success "test_code/or1.bird" "true";
    test_success "test_code/or2.bird" "false";
    test_success "test_code/or3.bird" "true";
    test_success "test_code/if1.bird" "true";
    test_success "test_code/if2.bird" "4";
    test_success "test_code/if3.bird" "false";
    test_success "test_code/if4.bird" "false";
    test_success "test_code/print5.bird" "5
5";
    test_success "test_code/mediumPrinting.bird" "2
9
-7
-7";
    test_success "test_code/hardPrinting.bird" "32
32
55
1760
9
1751
1728";
    test_runtime_failure "test_code/wrongArithm.bird" 1;
    test_runtime_failure "test_code/intError.bird" 1;
    test_runtime_failure "test_code/boolError.bird" 2;
    test_runtime_failure "test_code/boolError2.bird" 2;
    test_runtime_failure "test_code/orError.bird" 2;
    test_runtime_failure "test_code/orError2.bird" 2;
    test_runtime_failure "test_code/greaterThanError.bird" 1;
    test_runtime_failure "test_code/greaterThanError2.bird" 1;
    test_runtime_failure "test_code/equalToError.bird" 1;
    test_runtime_failure "test_code/equalToError2.bird" 1;
    test_runtime_failure "test_code/lessThanError.bird" 1;
    test_runtime_failure "test_code/errorCheckPrint.bird" 1;
    test_success "test_code/f4.bird" "5";
    test_success "test_code/varyingParameters.bird" 
      "true
false
true";
    test_success "test_code/nestedDeclarations.bird" "18";
    test_success "test_code/summate.bird" "15";
    test_success "test_code/mutualRecursion.bird" "1";
    test_success "test_code/testNoArg.bird" "true";
    test_compile_failure "test_code/gNotDefined.bird" "Unbound variable g.";
    test_compile_failure "test_code/duplicateX.bird" "Function f declares a duplicate parameter x.";
    test_compile_failure "test_code/duplicateFunction.bird" "Duplicate definition of function f.";
    test_compile_failure "test_code/unboundY.bird" "Unbound variable y.";
    test_compile_failure "test_code/reportAllError.bird" "Unbound variable g.
Unbound variable y.
Unbound variable z.";
    test_success "test_code/evaluationOrder.bird" "1
2
-1";
    test_success "test_code/test1.bird" "5";
   (*  test_success "test_code/big_addition.bird" "1000";  *)
    test_success "test_code/callerSavedRegs.bird" "51";
    test_compile_failure "test_code/palmerEdit2.bird" "Unbound variable x.";
    test_success "test_code/palmerEdit.bird" "2";
    test_compile_failure "test_code/palmerError2.bird" "Duplicate definition of function f.
Duplicate definition of function g.";
    test_compile_failure "test_code/unboundInMain.bird" "Unbound variable y.";
    test_success "test_code/allowedLeft.bird" "2";
    test_compile_failure "test_code/nestedLeftError.bird" "Unbound variable x.";
    test_success "test_code/isTuple.bird" "(true, false)";
    test_success "test_code/printTuple.bird" "1
2
1";
    test_success "test_code/indexTuple.bird" "6";
    test_success "test_code/tuple.bird" "1
2
(1, 2)";
    test_runtime_failure "test_code/runtimeTuple.bird" 1;
    test_runtime_failure "test_code/runtimeTuple2.bird" 3;
    test_runtime_failure "test_code/runtimeTuple3.bird" 3;
    test_runtime_failure "test_code/runtimeTuple4.bird" 4;
    test_success "test_code/testTupleBehavior.bird" "3";
    test_success "test_code/testTupleBehavior2.bird" "7";
    test_runtime_failure "test_code/testTupleBehavior3.bird" 1;
    test_success "test_code/nestedTuple.bird" "2";
    test_runtime_failure "test_code/runtimeTuple5.bird" 3;
    test_runtime_failure "test_code/runtimeTuple6.bird" 4;
    test_runtime_failure "test_code/runtimeTuple7.bird" 1;
    test_success "test_code/hardTuple.bird" "(1, (2, (3, (4, true))), 6)";
    test_compile_failure "test_code/compileErrorTuple.bird" "Unbound variable h.
Unbound variable x.
Unbound variable y.
Unbound variable z.
Unbound variable d.";
    test_compile_failure "test_code/unboundFuncs.bird" "Unbound variable f.
Unbound variable g.";
    test_success "test_code/closures.bird" "6";
    test_success "test_code/partialApplication.bird" "(4, 8)";
    test_success "test_code/closureIdentity.bird" "false";
    test_runtime_failure "test_code/closureExpected.bird" 5;
    test_runtime_failure "test_code/integerExpected.bird" 1;
    test_success "test_code/initialClosuresTest.bird" "1";
    test_success "test_code/passingFunctions.bird" "6";
    test_success "test_code/bindingClosures.bird" "<closure@000000000040119d>[0/2](?, ?)";
    test_success "test_code/testIsTuple.bird" "(false, true)";
    test_success "test_code/joeysTest.bird" "true";
    test_runtime_failure "test_code/joeysTest2.bird" 3; 
    test_success "test_code/gull1.bird" "12";
    test_runtime_failure "test_code/gull3.bird" 3;
    test_runtime_failure "test_code/gull4.bird" 1;
    test_runtime_failure "test_code/gull5.bird" 4;
    test_success "test_code/garbageCollector.bird" "1048576";
    test_runtime_failure "test_code/garbageCollector2.bird" 7;
    test_runtime_failure "test_code/garbageCollector3.bird" 0; 
    test_success "test_code/hoopoe1.bird" "1";
    test_success "test_code/hoopoe2.bird" "(10000003, true)";
    test_success "test_code/hoopoe3.bird" "12";
    test_success "test_code/hoopoe4.bird" "1";
    test_success "test_code/hoopoe5.bird" "6";

  ];;

let suite = "compiler test suite" >::: all_tests;;

run_test_tt_main suite;;
