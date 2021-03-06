module Test.Generated.Main109981679 exposing (main)

import Tests

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    [     Test.describe "Tests" [Tests.suite] ]
        |> Test.concat
        |> Test.Runner.Node.run { runs = Nothing, report = (ConsoleReport UseColor), seed = 248209273001977, processes = 8, globs = [], paths = ["F:\\Poli\\An3_sem1\\FP\\Project_elm\\tests\\Tests.elm"]}