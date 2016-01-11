let _ = Kaputt.Test.run_tests
  (MatchObjects_unit.tests @
   MatchOperations_unit.tests @
   MatchTraces_matcher_unit.tests @
   (* MatchTraces_unit.tests @ *)
   MatchTypes_unit.tests)

