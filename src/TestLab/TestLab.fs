namespace TestLab

module TestLab =
    open TestTree
    open Helpers

    let private octToTestReport lvl oct infos =
        let result = runOutcome oct
        let tcReport =
            match result with
            | Success succ ->
                (   { TimeSpent = succ.TimeSpent
                      LevelWithinTree = lvl
                      AccFailure = None
                    },infos)
                |> TestCaseReport
            | Failure failure ->
                ({ TimeSpent = failure.TimeSpent
                   LevelWithinTree = lvl
                   AccFailure = (failure :> IFailure).GetStackTrace() |> Some
                 }, infos)
                |> TestCaseReport
        tcReport
    
    let private findAccumulatedOutput (report : TestReportTree list) = 
        let outputAcc (acc : TestOutput) (output : TestOutput) = 
            let acc = 
                { TimeSpent = acc.TimeSpent + output.TimeSpent
                  LevelWithinTree = output.LevelWithinTree - 1
                  AccFailure = 
                      match output.AccFailure with
                      | Some _ -> Some ""
                      | None -> acc.AccFailure }
            acc
        
        let rec aux listReport (acc : TestOutput) = 
            match listReport with
            | [] -> acc
            | hd :: tl -> 
                match hd with
                | TestCaseReport(output, _) | TestReport(output, _, _) | TestSuiteReport(output, _, _) -> 
                    aux tl (outputAcc acc output)
        
        aux report { TimeSpent = System.TimeSpan()
                     LevelWithinTree = 0
                     AccFailure = None }


    let private summaryLine (kind:TestReportTree) =
        let space n = "   " |> String.replicate n
        let sumLine n = sprintf "%s%s [%s] ran in (%s) : [%s]%s" (space n)
        let createFromKind (out:TestOutput) (infos:TestInformation) (testKind) =
            sumLine 
                (out.LevelWithinTree) 
                (testKind)
                (infos.Name) 
                (out.TimeSpent.ToString()) 
                (match out.AccFailure with
                    | None -> "PASS"
                    | Some _ -> "FAIL")
                (match out.AccFailure with
                    | None -> ""
                    | Some reason -> sprintf "-{%s}" reason)
                
                        
        match kind with
        | TestCaseReport (out,infos) -> createFromKind out infos "Test Case"
        | TestReport (out,_,infos) -> createFromKind out infos "Test"
        | TestSuiteReport (out,_,infos) -> createFromKind out infos "Test Suite"

    let testCase state name (oct:Outcome<'a>) =
        TestCase.TestCase (oct,{Name = name; State = state})    
        |> Test.TestCase

    let testList state name (testlist : Test<'a> list) =
        Test.Test (testlist,{Name = name; State = state})    
    
    let testSuite state name (testlist : Test<'a> list) =
        TestSuite.TestSuite (testlist,{Name = name; State = state})    
        
    
    let internal runTests (test: TestSuite<'a>) =
        let rec run (tests: TestTree<'a>) (lvl:int) =
            match tests with
            | TestTree.TestCase (oct,infos) ->
                octToTestReport lvl oct infos
            | TestTree.Test (testTreeList, infos) ->
                let testReportList =
                    [for testTree in testTreeList do
                        yield run testTree (lvl + 1) 
                    ]
                let accOutput = findAccumulatedOutput testReportList
                let report = TestReport (accOutput,testReportList,infos)
                summaryLine report
                |> cprintf 
                    (match accOutput.AccFailure with
                     | None -> System.ConsoleColor.DarkGreen
                     | Some _ -> System.ConsoleColor.DarkRed)
                    "%s"
                report
            | TestTree.TestSuite (testTreeList,infos) ->
                let testReportList =
                    [for testTree in testTreeList do
                        yield run testTree (lvl + 1) 
                    ]
                let accOutput = findAccumulatedOutput testReportList
                let report = TestSuiteReport (accOutput,testReportList,infos)
                summaryLine report
                |> cprintf 
                    (match accOutput.AccFailure with
                     | None -> System.ConsoleColor.DarkGreen
                     | Some _ -> System.ConsoleColor.DarkRed)
                    "%s"
                report


        in run (TestTree.FromTestSuite test) 0 


    let internal outputTestReport (testReportTree:TestReportTree) =                       
        let rec generateOutput (reports:TestReportTree) =
            match reports with
            | TestCaseReport (_,_) ->
                summaryLine reports
                
            | TestReport (out,aList,infos)
            | TestSuiteReport (out,aList,infos) ->
                let str = summaryLine reports
                aList
                |> List.map (generateOutput)
                |> String.concat "\n"
                |> sprintf "%s\n%s" str

        in generateOutput testReportTree       
    
    let run (test: TestSuite<'a>) =
        let testReport = runTests test
        let output = outputTestReport testReport
        output 
        |> cprintf
            (System.ConsoleColor.DarkGray)
            "%s"
