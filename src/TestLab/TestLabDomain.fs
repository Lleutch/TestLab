namespace TestLab


module internal Helpers =
    open System.Text
    open System.Diagnostics
    open System

    let private hour (date:DateTime) = date.ToLongTimeString()

    let timeNow = hour (DateTime.Now)

    let cprintf c fmt = 
        Printf.kprintf
            (fun s ->
                let old = System.Console.ForegroundColor
                try
                    System.Console.ForegroundColor <- c
                    System.Console.Write s
                finally
                    System.Console.ForegroundColor <- old)
            fmt     

    let appendLine data (sb:StringBuilder) = sb.AppendLine data

    type Stopwatch with
        member x.HighPrecisionSeconds =
            (x.ElapsedTicks |> float) / (Stopwatch.Frequency |> float)

        member x.HighPrecisionMilliseconds =
            (x.ElapsedTicks * 1000L |> float) / (Stopwatch.Frequency |> float)

        member x.HighPrecisionMicroseconds =
            (x.ElapsedTicks * 1000L * 1000L |> float) / (Stopwatch.Frequency |> float)
    
    type TimeSpan with
        member x.HighPrecisionSeconds =
            (x.Ticks |> float) / (Stopwatch.Frequency |> float)

        member x.HighPrecisionMilliseconds = 
            (x.Ticks * 1000L |> float) / (Stopwatch.Frequency |> float)

        member x.HighPrecisionMicroseconds = 
            (x.Ticks * 1000L * 1000L |> float) / (Stopwatch.Frequency |> float)



[<AutoOpen>]
module OCT =
    open Helpers
    open System.Text
    open System.Diagnostics

    type IFailure =
        abstract member Reason : unit -> string
        abstract member GetStackTrace : unit -> string
        abstract member GetStackTraceUpToLevel : int -> string 

    [<NoComparison>]
    [<NoEquality>]
    type TestFailure = 
        {
            TimeSpent : System.TimeSpan;
            Reason : string;
            NestedIFailure : IFailure option;
        }
        interface IFailure with
            member this.Reason() = this.Reason
            member this.GetStackTrace() = 
                let strBuilder =
                    match this.NestedIFailure with
                    | None -> 
                        StringBuilder()
                        |> appendLine this.Reason
                    | Some nestedFailure ->
                        StringBuilder()
                        |> appendLine this.Reason
                        |> appendLine (nestedFailure.GetStackTrace())
                
                strBuilder.ToString()

            member this.GetStackTraceUpToLevel(n:int) =
                let strBuilder =
                    match n,this.NestedIFailure with
                    | m,_ when m < 0 ->
                        StringBuilder()
                        |> appendLine this.Reason                        
                    | 0,_ 
                    | _,None -> 
                        StringBuilder()
                        |> appendLine this.Reason
                    | _,Some nestedFailure ->
                        StringBuilder()
                        |> appendLine this.Reason
                        |> appendLine (nestedFailure.GetStackTrace())
                
                strBuilder.ToString()

    let internal combineTestFailure (tf1:TestFailure) (tf2:TestFailure) =
        {
            TimeSpent = tf1.TimeSpent + tf2.TimeSpent;
            Reason = tf1.Reason + "\n" + tf2.Reason ;
            NestedIFailure = 
                match tf1.NestedIFailure, tf2.NestedIFailure with
                | None , None -> None
                | Some x , None -> Some x
                | None , Some x -> Some x
                | Some x , Some _ -> Some x
                    
        }

    type TestResult<'a> =
        {
            TimeSpent : System.TimeSpan;
            Result : 'a;
        }
    
    let internal initTestResult result =
        {
            TimeSpent = System.TimeSpan();
            Result = result;
        }

    let internal zeroTestResult =
        {
            TimeSpent = System.TimeSpan();
            Result = () ;
        }                                
        

    let internal combineTestResult (tr1:TestResult<'a>) (tr2:TestResult<'a>) =
        {
            TimeSpent = tr1.TimeSpent + tr2.TimeSpent
            Result = tr2.Result
        }


    /// Todo : develop more the type itself to contain enough information about a test computation
    [<NoComparison>]
    [<NoEquality>]
    type OutcomeTest<'a> =
        | Success of TestResult<'a>
        | Failure of TestFailure

    [<NoComparison>]
    [<NoEquality>]
    type Outcome<'a> = |Outcome of (unit -> OutcomeTest<'a>)

    let internal replaceTimeSpent  ts (oct:OutcomeTest<'a>) =
        match oct with
        | Success success ->
            {success with
                TimeSpent = ts
            } |> Success
        | Failure failure ->
            {failure with
                TimeSpent = ts
            } |> Failure


    let internal combine combineSuccess combineFailure a b =
        match a,b with
        | Success a1, Success b1 -> Success (combineSuccess a1 b1)
        | Success _ , Failure b1 -> Failure b1
        | Failure a1, Success _  -> Failure a1
        | Failure a1, Failure b1 -> Failure (combineFailure a1 b1)
   

    /// When used as with the outcome keyword, this represents
    /// the Wrapped type : OutcomeTest<'a>.
    /// The last member called being Run, OutcomeTest<'a> is the returned type
    /// of the outcome CE.
    type OutcomeTestBuilder() =

        member __.Bind(Outcome m, f) =
            match m() with
            |Success elem -> f elem.Result
            |Failure s -> Failure s
                

        member __.Return x = 
            Success (initTestResult x)

        member __.ReturnFrom (m:Outcome<'a>) =
            let (Outcome res) = m
            res()

        member __.Zero () = 
            zeroTestResult |> Success
                
        member __.Combine (a:OutcomeTest<'a>,b:unit -> OutcomeTest<'a>)= 
            let runnedB = b()
            combine combineTestResult combineTestFailure a runnedB

        member __.Delay(f:unit -> OutcomeTest<'a>)=
            fun () -> 
                let sw = Stopwatch()
                sw.Start()
                let oct = f()
                sw.Stop()
                replaceTimeSpent (sw.Elapsed) oct

        member __.Run(funToRun:unit -> OutcomeTest<'a>) =
            Outcome funToRun
            
    let outcome = OutcomeTestBuilder()

    let runOutcome (Outcome oct) = oct()


module Outcome =
    
    let catchFailure (failureHandler:IFailure -> OutcomeTest<'a>) (oct:OutcomeTest<'a>) =
        match oct with
        | Success success -> Success success
        | Failure failure -> (failure :> IFailure) |> failureHandler

    let wrapFailure (wrappingFailure:TestFailure) (oct:OutcomeTest<'a>) =
        match oct with
        | Success success -> Success success
        | Failure failure -> 
            { wrappingFailure with
                NestedIFailure = Some (failure :> IFailure)
            } |> Failure


module TestTree =
   
    type TestState =
        | Run
        | Ignore

    type TestInformation =
        {
            Name : string;
            State : TestState;
        }

    [<NoComparison>]
    [<NoEquality>]
    type TestCase<'a> = TestCase of Outcome<'a> * TestInformation

    [<NoComparison>]
    [<NoEquality>]
    type Test<'a> = 
        | TestCase of TestCase<'a>
        | Test of Test<'a> list * TestInformation

    [<NoComparison>]
    [<NoEquality>]
    type TestSuite<'a> = TestSuite of Test<'a> list * TestInformation
    

    [<NoComparison>]
    [<NoEquality>]
    type internal TestTree<'a> =
        | TestCase of Outcome<'a> * TestInformation
        | Test of TestTree<'a> list * TestInformation
        | TestSuite of TestTree<'a> list * TestInformation
        static member FromTestSuite(testSuite:TestSuite<'a>) =
            let (TestSuite.TestSuite (testList,infos)) = testSuite
            let rec aux (tests:Test<'a> list) acc =
                match tests with
                | [] -> List.rev acc
                | hd::tl ->
                    match hd with
                    | Test.TestCase testCase ->
                        let (TestCase.TestCase (oct,infos)) = testCase
                        let testCase = TestCase(oct,infos)
                        aux tl (testCase::acc)
                    | Test.Test (listOfTest,infos) ->
                        let list = aux listOfTest []
                        let test = Test(list,infos)
                        aux tl (test::acc)
            
            let testTreeList = aux testList []
            TestSuite(testTreeList,infos)

    type TestOutput =
        {
            TimeSpent : System.TimeSpan;
            LevelWithinTree : int;
            AccFailure : string option;
        }

    type TestReportTree =
        | TestCaseReport of TestOutput * TestInformation
        | TestReport of TestOutput * TestReportTree list * TestInformation
        | TestSuiteReport of TestOutput * TestReportTree list * TestInformation

