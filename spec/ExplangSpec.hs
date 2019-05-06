module ExplangSpec (spec) where

import Test.Hspec hiding (Expectation)
import Language.Explang
import Language.Explang.Expectation


simple scope inspection binding = simpleMatching scope inspection binding Unmatching
simpleNegated scope inspection binding = (Expectation noFlags scope (Not (Inspection inspection binding Unmatching)) AnyCount)
simpleCount scope inspection binding count = (Expectation noFlags scope (Inspection inspection binding Unmatching) count)
simpleIntransitive scope inspection binding = (Expectation intransitiveFlag scope (Inspection inspection binding Unmatching) AnyCount)
intransitiveNegated scope inspection binding = (Expectation intransitiveFlag scope (Not (Inspection inspection binding Unmatching)) AnyCount)
simpleMatching scope inspection binding matcher = (Expectation noFlags scope (Inspection inspection binding matcher) AnyCount)

simpleTest name scope inspection binding = Test name (simple scope inspection binding)

spec :: Spec
spec = do
  describe "parseExpectation" $ do
    let run = parseExpectation :: String -> Expectation
    let test code expectation = it ("test " ++ code ++ " shouldBe " ++ show expectation) (run code `shouldBe` expectation)

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

    test "intransitively calls" (simpleIntransitive Unscoped "calls" Any)
    test "intransitively calls `foo`" (simpleIntransitive Unscoped "calls" (Named "foo"))
    test "intransitively calls something like `foo`" (simpleIntransitive Unscoped "calls" (Like "foo"))
    test "intransitively calls something distinct of `foo`" (simpleIntransitive Unscoped "calls" (Except "foo"))
    test "intransitively calls any of (`foo`, `bar`, `baz`)" (simpleIntransitive Unscoped "calls" (AnyOf ["foo", "bar", "baz"]))

    test "intransitively declares class like `Foo`" (simpleIntransitive Unscoped "declares class" (Like "Foo"))
    test "intransitively declares class distinct of `Foo`" (simpleIntransitive Unscoped "declares class" (Except "Foo"))
    test "intransitively declares method like `foo`" (simpleIntransitive Unscoped "declares method" (Like "foo"))
    test "intransitively declares method distinct of `foo`" (simpleIntransitive Unscoped "declares method" (Except "foo"))

    test "intransitively not calls" (intransitiveNegated Unscoped "calls" Any)
    test "intransitively not calls `foo`" (intransitiveNegated Unscoped "calls" (Named "foo"))
    test "intransitively not calls something like `foo`" (intransitiveNegated Unscoped "calls" (Like "foo"))
    test "intransitively not calls something distinct of `foo`" (intransitiveNegated Unscoped "calls" (Except "foo"))
    test "intransitively not calls any of (`foo`, `bar`, `baz`)" (intransitiveNegated Unscoped "calls" (AnyOf ["foo", "bar", "baz"]))

    test "intransitively within `bar` not calls" (intransitiveNegated (Scoped "bar") "calls" Any)
    test "intransitively within `bar` not calls `foo`" (intransitiveNegated (Scoped "bar") "calls" (Named "foo"))
    test "intransitively within `bar` not calls something like `foo`" (intransitiveNegated (Scoped "bar") "calls" (Like "foo"))
    test "intransitively within `bar` not calls something distinct of `foo`" (intransitiveNegated (Scoped "bar") "calls" (Except "foo"))
    test "intransitively within `bar` not calls any of (`foo`, `bar`, `baz`)" (intransitiveNegated (Scoped "bar") "calls" (AnyOf ["foo", "bar", "baz"]))

    test "within `bar` not calls" (simpleNegated (Scoped "bar") "calls" Any)
    test "within `bar` not calls `foo`" (simpleNegated (Scoped "bar") "calls" (Named "foo"))
    test "within `bar` not calls something like `foo`" (simpleNegated (Scoped "bar") "calls" (Like "foo"))
    test "within `bar` not calls something distinct of `foo`" (simpleNegated (Scoped "bar") "calls" (Except "foo"))
    test "within `bar` not calls any of (`foo`, `bar`, `baz`)" (simpleNegated (Scoped "bar") "calls" (AnyOf ["foo", "bar", "baz"]))

    test "within `bar` calls exactly 3 times" (simpleCount (Scoped "bar") "calls" Any (Exactly 3))
    test "within `bar` calls `foo` exactly 3 times" (simpleCount (Scoped "bar") "calls" (Named "foo") (Exactly 3))
    test "within `bar` calls something like `foo` exactly 3 times" (simpleCount (Scoped "bar") "calls" (Like "foo") (Exactly 3))
    test "within `bar` calls something distinct of `foo` exactly 3 times" (simpleCount (Scoped "bar") "calls" (Except "foo") (Exactly 3))
    test "within `bar` calls any of (`foo`, `bar`, `baz`) exactly 3 times" (simpleCount (Scoped "bar") "calls" (AnyOf ["foo", "bar", "baz"]) (Exactly 3))

    test "within `bar` (calls `foo`)" (run "within `bar` calls `foo`")
    test "within `bar` (calls `foo`) or (calls `foo`)" (run "within `bar` calls `foo` or calls `foo`")
    test "within `bar` (calls `foo` or calls `foo`)" (run "within `bar` calls `foo` or calls `foo`")

    test "within `bar` calls `foo` or calls `baz`" (Expectation noFlags (Scoped "bar") (Or (Inspection "calls" (Named "foo") Unmatching) (Inspection "calls" (Named "baz") Unmatching)) AnyCount)
    test "within `bar` calls `foo` and calls `baz`" (Expectation noFlags (Scoped "bar") (And (Inspection "calls" (Named "foo") Unmatching) (Inspection "calls" (Named "baz") Unmatching)) AnyCount)
    test "within `bar` calls `foo` or calls `baz` at least 2 times" (Expectation noFlags (Scoped "bar") (Or (Inspection "calls" (Named "foo") Unmatching) (Inspection "calls" (Named "baz") Unmatching)) (AtLeast 2))
    test "within `bar` calls `a` and calls `b` or calls `c`" (run "within `bar` (calls `a` and calls `b`) or calls `c`")
    test "within `bar` calls `a` and calls `b` or calls `c` or calls `d`" (run "within `bar` ((calls `a` and calls `b`) or calls `c`) or calls `d`")
    test "within `bar` calls `a` and calls `b` or calls `c` and calls `d`" (run "within `bar` (calls `a` and calls `b`) or (calls `c` and calls `d`)")

    test "declares body `foo` that (calls `baz` or calls `bar`)" (run "declares body `foo` with something that (calls `baz` or calls `bar`)")

    test "within `bar` calls at least 3 times" (simpleCount (Scoped "bar") "calls" Any (AtLeast 3))
    test "within `bar` calls `foo` at least 3 times" (simpleCount (Scoped "bar") "calls" (Named "foo") (AtLeast 3))
    test "within `bar` calls something like `foo` at least 3 times" (simpleCount (Scoped "bar") "calls" (Like "foo") (AtLeast 3))
    test "within `bar` calls something distinct of `foo` at least 3 times" (simpleCount (Scoped "bar") "calls" (Except "foo") (AtLeast 3))
    test "within `bar` calls any of (`foo`, `bar`, `baz`) at least 3 times" (simpleCount (Scoped "bar") "calls" (AnyOf ["foo", "bar", "baz"]) (AtLeast 3))

    test "within `bar` calls at most 3 times" (simpleCount (Scoped "bar") "calls" Any (AtMost 3))
    test "within `bar` calls `foo` at most 3 times" (simpleCount (Scoped "bar") "calls" (Named "foo") (AtMost 3))
    test "within `bar` calls something like `foo` at most 3 times" (simpleCount (Scoped "bar") "calls" (Like "foo") (AtMost 3))
    test "within `bar` calls something distinct of `foo` at most 3 times" (simpleCount (Scoped "bar") "calls" (Except "foo") (AtMost 3))
    test "within `bar` calls any of (`foo`, `bar`, `baz`) at most 3 times" (simpleCount (Scoped "bar") "calls" (AnyOf ["foo", "bar", "baz"]) (AtMost 3))

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

    test "within `bar` calls `foo` with (0, self)" (simpleMatching (Scoped "bar") "calls" (Named "foo") (Matching [IsNumber 0, IsSelf]))
    test "within `bar` calls `foo` with (\"hello\", self)" (simpleMatching (Scoped "bar") "calls" (Named "foo") (Matching [IsString "hello", IsSelf]))
    test "within `bar` calls `foo` with (`hello`, self)" (simpleMatching (Scoped "bar") "calls" (Named "foo") (Matching [IsSymbol "hello", IsSelf]))
    test "within `bar` calls `foo` with ('a', self)" (simpleMatching (Scoped "bar") "calls" (Named "foo") (Matching [IsChar 'a', IsSelf]))
    test "within `bar` calls `foo` with (true, self)" (simpleMatching (Scoped "bar") "calls" (Named "foo") (Matching [IsTrue, IsSelf]))
    test "within `bar` calls `foo` with (false, self)" (simpleMatching (Scoped "bar") "calls" (Named "foo") (Matching [IsFalse, IsSelf]))
    test "within `bar` calls `foo` with (nil, self)" (simpleMatching (Scoped "bar") "calls" (Named "foo") (Matching [IsNil, IsSelf]))
    test "within `bar` calls `foo` with (self, self)" (simpleMatching (Scoped "bar") "calls" (Named "foo") (Matching [IsSelf, IsSelf]))
    test "within `bar` calls `foo` with (math, self)" (simpleMatching (Scoped "bar") "calls" (Named "foo") (Matching [IsMath, IsSelf]))
    test "within `bar` calls `foo` with (logic, self)" (simpleMatching (Scoped "bar") "calls" (Named "foo") (Matching [IsLogic, IsSelf]))

    test "calls `foo` with something that (returns with math)" (
      simpleMatching Unscoped "calls" (Named "foo") (Matching [That (simpleMatching Unscoped "returns" Any (Matching [IsMath]) )]))

    test "declares `foo` that (returns with math)" (
      simpleMatching Unscoped "declares" (Named "foo") (Matching [That (simpleMatching Unscoped "returns" Any (Matching [IsMath]) )]))

    test "calls `foo` with (self, something that (returns with math))" (
      simpleMatching Unscoped "calls" (Named "foo") (Matching [IsSelf, That (simpleMatching Unscoped "returns" Any (Matching [IsMath]) )]))

    test "calls `foo` with (self, that (returns with math))" (
      simpleMatching Unscoped "calls" (Named "foo") (Matching [IsSelf, That (simpleMatching Unscoped "returns" Any (Matching [IsMath]))]))

    test "calls `foo` with (self, something that (declares method `baz`))" (
      simpleMatching Unscoped "calls" (Named "foo") (Matching [IsSelf, That (simple Unscoped "declares method" (Named "baz"))]))

  describe "parseTests" $ do
    let run = parseTests
    let test code expectation = it ("test " ++ code ++ " shouldBe " ++ show expectation) (run code `shouldBe` expectation)

    test "test: declares class `Baz`" [simpleTest "E0" Unscoped "declares class" (Named "Baz")]
    test "test: declares class `Baz`;" [simpleTest "E0" Unscoped "declares class" (Named "Baz")]
    test "test: declares class `Baz`;\n" [simpleTest "E0" Unscoped "declares class" (Named "Baz")]

    test "test: declares class `Baz`;\ntest: within `Baz` sends `foo`" [
      simpleTest "E0" Unscoped "declares class" (Named "Baz"),
      simpleTest "E1" (Scoped "Baz") "sends" (Named "foo")]
    test "test: declares class `Baz`;\ntest: within `Baz` sends `foo`;" [
      simpleTest "E0" Unscoped "declares class" (Named "Baz"),
      simpleTest "E1" (Scoped "Baz") "sends" (Named "foo")]
    test "test: declares class `Baz`;\n\
         \test : within `Baz`\n\
         \sends `foo`;\n" [
           simpleTest "E0" Unscoped "declares class" (Named "Baz"),
           simpleTest "E1" (Scoped "Baz") "sends" (Named "foo")]

    test "test \"a test\":\n\
         \  declares class `Baz`" [simpleTest "a test" Unscoped "declares class" (Named "Baz")]
    test "test \"a test\":\n\
         \  declares class `Baz`;" [simpleTest "a test" Unscoped "declares class" (Named "Baz")]
    test "test \"a test\":\n\
         \  declares class `Baz`;\n" [simpleTest "a test" Unscoped "declares class" (Named "Baz")]

    test "test \"a test\":\n\
         \  declares class `Baz`;\n\
         \test \"another test\":\n\
         \  within `Baz` sends `foo`" [
      simpleTest "a test" Unscoped "declares class" (Named "Baz"),
      simpleTest "another test" (Scoped "Baz") "sends" (Named "foo")]
    test "test \"a test\":\n\
         \  declares class `Baz`;\n\
         \test \"another test\":\n\
         \  within `Baz` sends `foo`;" [
      simpleTest "a test" Unscoped "declares class" (Named "Baz"),
      simpleTest "another test" (Scoped "Baz") "sends" (Named "foo")]
    test "test \"a test\":\n\
         \  declares class `Baz`;\n\
         \test \"another test\":\n\
         \  within `Baz`\n\
         \  sends `foo`;\n" [
           simpleTest "a test" Unscoped "declares class" (Named "Baz"),
           simpleTest "another test" (Scoped "Baz") "sends" (Named "foo")]

  describe "handles errors" $ do
    let run = either id (error.show) . parseTests'
    let test code expectation = it ("test " ++ code ++ " shouldBe " ++ show expectation) (run code `shouldBe` expectation)

    test "test: declares class `Baz" "Lexical error"
    test "test: declares class `Baz` exoctly 3 times" "Parse Error: Unexpected keyword exoctly"
    test "test: declares class `Baz` exactly 3 time" "Parse Error: Unexpected keyword time"
    test "test: declares class `Baz`\n within `Baz` sends `foo`" "Parse Error: within is not expected here"
    test "test: declares class of distinct `Baz`\n" "Parse Error: of is not expected here"
    test "test: declares class distinct `Baz`\n" "Parse Error: symbol Baz is not expected here"
    test "test: declares class `Baz` 3 times" "Parse Error: number 3.0 is not expected here"
    test "test: declares class `Baz` not exactly 3 times" "Parse Error: not is not expected here"
    test "test: declares class `Baz`;\n\
         \test: Within `Baz`\n\
         \sends `foo`;\n" "Parse Error: Unexpected keyword sends"
