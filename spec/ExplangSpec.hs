module ExplangSpec (spec) where

import Test.Hspec hiding (Expectation)
import Language.Explang


simple scope inspection binding = simpleMatching scope inspection binding Unmatching
simpleNegated scope inspection binding = (Expectation noFlags scope True inspection binding Unmatching AnyCount)
simpleMatching scope inspection binding matcher = (Expectation noFlags scope False inspection binding matcher AnyCount)


spec :: Spec
spec = do
  describe "parseExpectation" $ do
    let run = parseExpectation
    let test code expectation = it (code ++ " shouldBe " ++ show expectation) (run code `shouldBe` expectation)

    test "calls" (simple Unscoped "calls" Any)
    test "calls `foo`" (simple Unscoped "calls" (Named "foo"))
    test "calls something like `foo`" (simple Unscoped "calls" (Like "foo"))
    test "calls something distinct of `foo`" (simple Unscoped "calls" (Except "foo"))
    test "calls any of (`foo`, `bar`, `baz`)" (simple Unscoped "calls" (AnyOf ["foo", "bar", "baz"]))

    test "not calls" (simpleNegated Unscoped "calls" Any)
    test "not calls `foo`" (simpleNegated Unscoped "calls" (Named "foo"))
    test "not calls something like `foo`" (simpleNegated Unscoped "calls" (Like "foo"))
    test "not calls something distinct of `foo`" (simpleNegated Unscoped "calls" (Except "foo"))
    test "not calls any of (`foo`, `bar`, `baz`)" (simpleNegated Unscoped "calls" (AnyOf ["foo", "bar", "baz"]))

    test "intransitively calls" (Expectation intransitiveFlag Unscoped False "calls" Any Unmatching AnyCount)
    test "intransitively calls `foo`" (Expectation intransitiveFlag Unscoped False "calls" (Named "foo") Unmatching AnyCount)
    test "intransitively calls something like `foo`" (Expectation intransitiveFlag Unscoped False "calls" (Like "foo") Unmatching AnyCount)
    test "intransitively calls something distinct of `foo`" (Expectation intransitiveFlag Unscoped False "calls" (Except "foo") Unmatching AnyCount)
    test "intransitively calls any of (`foo`, `bar`, `baz`)" (Expectation intransitiveFlag Unscoped False "calls" (AnyOf ["foo", "bar", "baz"]) Unmatching AnyCount)

    test "intransitively declares class like `Foo`" (Expectation intransitiveFlag Unscoped False "declares class" (Like "Foo") Unmatching AnyCount)
    test "intransitively declares class distinct of `Foo`" (Expectation intransitiveFlag Unscoped False "declares class" (Except "Foo") Unmatching AnyCount)
    test "intransitively declares method like `foo`" (Expectation intransitiveFlag Unscoped False "declares method" (Like "foo") Unmatching AnyCount)
    test "intransitively declares method distinct of `foo`" (Expectation intransitiveFlag Unscoped False "declares method" (Except "foo") Unmatching AnyCount)

    test "intransitively not calls" (Expectation intransitiveFlag Unscoped True "calls" Any Unmatching AnyCount)
    test "intransitively not calls `foo`" (Expectation intransitiveFlag Unscoped True "calls" (Named "foo") Unmatching AnyCount)
    test "intransitively not calls something like `foo`" (Expectation intransitiveFlag Unscoped True "calls" (Like "foo") Unmatching AnyCount)
    test "intransitively not calls something distinct of `foo`" (Expectation intransitiveFlag Unscoped True "calls" (Except "foo") Unmatching AnyCount)
    test "intransitively not calls any of (`foo`, `bar`, `baz`)" (Expectation intransitiveFlag Unscoped True "calls" (AnyOf ["foo", "bar", "baz"]) Unmatching AnyCount)

    test "intransitively within `bar` not calls" (Expectation intransitiveFlag (Scoped "bar") True "calls" Any Unmatching AnyCount)
    test "intransitively within `bar` not calls `foo`" (Expectation intransitiveFlag (Scoped "bar") True "calls" (Named "foo") Unmatching AnyCount)
    test "intransitively within `bar` not calls something like `foo`" (Expectation intransitiveFlag (Scoped "bar") True "calls" (Like "foo") Unmatching AnyCount)
    test "intransitively within `bar` not calls something distinct of `foo`" (Expectation intransitiveFlag (Scoped "bar") True "calls" (Except "foo") Unmatching AnyCount)
    test "intransitively within `bar` not calls any of (`foo`, `bar`, `baz`)" (Expectation intransitiveFlag (Scoped "bar") True "calls" (AnyOf ["foo", "bar", "baz"]) Unmatching AnyCount)

    test "within `bar` not calls" (Expectation noFlags (Scoped "bar") True "calls" Any Unmatching AnyCount)
    test "within `bar` not calls `foo`" (Expectation noFlags (Scoped "bar") True "calls" (Named "foo") Unmatching AnyCount)
    test "within `bar` not calls something like `foo`" (Expectation noFlags (Scoped "bar") True "calls" (Like "foo") Unmatching AnyCount)
    test "within `bar` not calls something distinct of `foo`" (Expectation noFlags (Scoped "bar") True "calls" (Except "foo") Unmatching AnyCount)
    test "within `bar` not calls any of (`foo`, `bar`, `baz`)" (Expectation noFlags (Scoped "bar") True "calls" (AnyOf ["foo", "bar", "baz"]) Unmatching AnyCount)

    test "within `bar` calls exactly 3 times" (Expectation noFlags (Scoped "bar") False "calls" Any Unmatching (Exactly 3))
    test "within `bar` calls `foo` exactly 3 times" (Expectation noFlags (Scoped "bar") False "calls" (Named "foo") Unmatching (Exactly 3))
    test "within `bar` calls something like `foo` exactly 3 times" (Expectation noFlags (Scoped "bar") False "calls" (Like "foo") Unmatching (Exactly 3))
    test "within `bar` calls something distinct of `foo` exactly 3 times" (Expectation noFlags (Scoped "bar") False "calls" (Except "foo") Unmatching (Exactly 3))
    test "within `bar` calls any of (`foo`, `bar`, `baz`) exactly 3 times" (Expectation noFlags (Scoped "bar") False "calls" (AnyOf ["foo", "bar", "baz"]) Unmatching (Exactly 3))

    test "within `bar` calls at least 3 times" (Expectation noFlags (Scoped "bar") False "calls" Any Unmatching (AtLeast 3))
    test "within `bar` calls `foo` at least 3 times" (Expectation noFlags (Scoped "bar") False "calls" (Named "foo") Unmatching (AtLeast 3))
    test "within `bar` calls something like `foo` at least 3 times" (Expectation noFlags (Scoped "bar") False "calls" (Like "foo") Unmatching (AtLeast 3))
    test "within `bar` calls something distinct of `foo` at least 3 times" (Expectation noFlags (Scoped "bar") False "calls" (Except "foo") Unmatching (AtLeast 3))
    test "within `bar` calls any of (`foo`, `bar`, `baz`) at least 3 times" (Expectation noFlags (Scoped "bar") False "calls" (AnyOf ["foo", "bar", "baz"]) Unmatching (AtLeast 3))

    test "within `bar` calls at most 3 times" (Expectation noFlags (Scoped "bar") False "calls" Any Unmatching (AtMost 3))
    test "within `bar` calls `foo` at most 3 times" (Expectation noFlags (Scoped "bar") False "calls" (Named "foo") Unmatching (AtMost 3))
    test "within `bar` calls something like `foo` at most 3 times" (Expectation noFlags (Scoped "bar") False "calls" (Like "foo") Unmatching (AtMost 3))
    test "within `bar` calls something distinct of `foo` at most 3 times" (Expectation noFlags (Scoped "bar") False "calls" (Except "foo") Unmatching (AtMost 3))
    test "within `bar` calls any of (`foo`, `bar`, `baz`) at most 3 times" (Expectation noFlags (Scoped "bar") False "calls" (AnyOf ["foo", "bar", "baz"]) Unmatching (AtMost 3))

    test "within `bar` returns with 0" (simpleMatching (Scoped "bar") "returns" Any (Matching [IsNumber 0]))
    test "within `bar` returns with \"hello\"" (simpleMatching (Scoped "bar") "returns" Any (Matching [IsString "hello"]))
    test "within `bar` returns with `hello`" (simpleMatching (Scoped "bar") "returns" Any (Matching [IsSymbol "hello"]))
    test "within `bar` returns with 'a'" (simpleMatching (Scoped "bar") "returns" Any (Matching [IsChar 'a']))
    test "within `bar` returns with true" (simpleMatching (Scoped "bar") "returns" Any (Matching [IsTrue]))
    test "within `bar` returns with false" (simpleMatching (Scoped "bar") "returns" Any (Matching [IsFalse]))
    test "within `bar` returns with nil" (simpleMatching (Scoped "bar") "returns" Any (Matching [IsNil]))
    test "within `bar` returns with self" (simpleMatching (Scoped "bar") "returns" Any (Matching [IsSelf]))
    test "within `bar` returns with math" (simpleMatching (Scoped "bar") "returns" Any (Matching [IsMath]))
    test "within `bar` returns with logic" (simpleMatching (Scoped "bar") "returns" Any (Matching [IsLogic]))

    test "within `bar` calls `foo` with 0 and with self" (simpleMatching (Scoped "bar") "calls" (Named "foo") (Matching [IsNumber 0, IsSelf]))
    test "within `bar` calls `foo` with \"hello\" and with self" (simpleMatching (Scoped "bar") "calls" (Named "foo") (Matching [IsString "hello", IsSelf]))
    test "within `bar` calls `foo` with `hello` and with self" (simpleMatching (Scoped "bar") "calls" (Named "foo") (Matching [IsSymbol "hello", IsSelf]))
    test "within `bar` calls `foo` with 'a' and with self" (simpleMatching (Scoped "bar") "calls" (Named "foo") (Matching [IsChar 'a', IsSelf]))
    test "within `bar` calls `foo` with true and with self" (simpleMatching (Scoped "bar") "calls" (Named "foo") (Matching [IsTrue, IsSelf]))
    test "within `bar` calls `foo` with false and with self" (simpleMatching (Scoped "bar") "calls" (Named "foo") (Matching [IsFalse, IsSelf]))
    test "within `bar` calls `foo` with nil and with self" (simpleMatching (Scoped "bar") "calls" (Named "foo") (Matching [IsNil, IsSelf]))
    test "within `bar` calls `foo` with self and with self" (simpleMatching (Scoped "bar") "calls" (Named "foo") (Matching [IsSelf, IsSelf]))
    test "within `bar` calls `foo` with math and with self" (simpleMatching (Scoped "bar") "calls" (Named "foo") (Matching [IsMath, IsSelf]))
    test "within `bar` calls `foo` with logic and with self" (simpleMatching (Scoped "bar") "calls" (Named "foo") (Matching [IsLogic, IsSelf]))

    test "calls `foo` with self and with something that (returns with math)" (
      simpleMatching Unscoped "calls" (Named "foo") (Matching [IsSelf, That (simpleMatching Unscoped "returns" Any (Matching [IsMath]) )]))

    test "calls `foo` with self and with something that (declares method `baz`)" (
      simpleMatching Unscoped "calls" (Named "foo") (Matching [IsSelf, That (simple Unscoped "declares method" (Named "baz"))]))

  describe "parseExpectations" $ do
    let run = parseExpectations
    let test code expectation = it (code ++ " shouldBe " ++ show expectation) (run code `shouldBe` expectation)

    test "declares class `Baz`" [simple Unscoped "declares class" (Named "Baz")]
    test "declares class `Baz`;" [simple Unscoped "declares class" (Named "Baz")]
    test "declares class `Baz`;\n" [simple Unscoped "declares class" (Named "Baz")]
    test "declares class `Baz`;\nwithin `Baz` sends `foo`" [simple Unscoped "declares class" (Named "Baz"), simple (Scoped "Baz") "sends" (Named "foo")]
    test "declares class `Baz`;\nwithin `Baz` sends `foo`;" [simple Unscoped "declares class" (Named "Baz"), simple (Scoped "Baz") "sends" (Named "foo")]
    test "declares class `Baz`;\n\
         \\n\
         \within `Baz`\n\
         \sends `foo`;\n" [simple Unscoped "declares class" (Named "Baz"), simple (Scoped "Baz") "sends" (Named "foo")]

  describe "handles errors" $ do
    let run = either id (error.show) . parseExpectations'
    let test code expectation = it (code ++ " shouldBe " ++ show expectation) (run code `shouldBe` expectation)

    test "declares class `Baz" "Lexical error"
    test "declares class `Baz` exoctly 3 times" "Parse Error: Unexpected keyword exoctly"
    test "declares class `Baz` exactly 3 time" "Parse Error: Unexpected keyword time"
    test "declares class `Baz`\n within `Baz` sends `foo`" "Parse Error: within is not expected here"
    test "declares class of distinct `Baz`\n" "Parse Error: of is not expected here"
    test "declares class distinct `Baz`\n" "Parse Error: symbol Baz is not expected here"
    test "declares class `Baz` 3 times" "Parse Error: number 3.0 is not expected here"
    test "declares class `Baz` not exactly 3 times" "Parse Error: not is not expected here"
    test "declares class `Baz`;\n\
         \\n\
         \Within `Baz`\n\
         \sends `foo`;\n" "Parse Error: Unexpected keyword sends"
