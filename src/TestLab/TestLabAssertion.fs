namespace TestLab

module Check =
    open System.Diagnostics

    let success (sw:Stopwatch) =
        OutcomeTest.Success({ TimeSpent = sw.Elapsed
                              Result = () })
        |> fun x () -> x
        |> Outcome

    let failure (sw:Stopwatch) reason : Outcome<unit> =
        OutcomeTest.Failure({ TimeSpent = sw.Elapsed
                              Reason = reason
                              NestedIFailure = None })
        |> fun x () -> x
        |> Outcome


    
    let private isTrueOrFalse fn elem =
        let sw = Stopwatch()
        sw.Start()
        if fn elem then 
            sw.Stop()
            success sw
        else 
            sw.Stop()
            failure sw (sprintf "the element is %b when it should be %b" elem (not elem))

    let isTrue = isTrueOrFalse id

    let isFalse = isTrueOrFalse not
    
    let isEqualTo expected message actual =
        let sw = Stopwatch()
        sw.Start()
        if actual = expected then
            sw.Stop()
            success sw
        else 
            sw.Stop()
            failure sw  (sprintf "%s.\n Expected %A but Received %A" message expected actual)


    let isNotEqualTo expected message actual =
        let sw = Stopwatch()
        sw.Start()
        if actual = expected then
            sw.Stop()
            failure sw (sprintf "%s.\n Expected something different from %A but Received %A" message expected actual)
        else 
            sw.Stop()
            success sw

    let isNone message x =
        let sw = Stopwatch()
        sw.Start()
        match x with
        | None ->
            sw.Stop()
            success sw
        | Some elem ->
            sw.Stop()
            failure sw (sprintf "%s.\n Expected None but Received Some(%A)" message elem)

    let isSome message x =            
        let sw = Stopwatch()
        sw.Start()
        match x with
        | None ->
            sw.Stop()
            success sw
        | Some elem ->
            sw.Stop()
            failure sw (sprintf "%s.\n Expected Some(%A) but Received None" message elem)


    let isNotNull message x =
        let sw = Stopwatch()
        sw.Start()        
        match x with
        | null ->
            sw.Stop()
            failure sw (sprintf "%s.\n Expected a value but Received null" message)
        | _ ->
            sw.Stop()
            success sw

    let isNull message x =
        let sw = Stopwatch()
        sw.Start()        
        match x with
        | null ->
            sw.Stop()
            success sw
        | elem ->
            sw.Stop()
            failure sw (sprintf "%s.\n Expected null but Received %A" message elem)

    let isLessThan a message b =
        let sw = Stopwatch()
        sw.Start()        
        if b < a then
            sw.Stop()
            failure sw (sprintf "%s. Expected b = (%A) to be less than a = (%A)."
                            message b a)
        else
            sw.Stop()
            success sw

    let isLessThanOrEqual a message b =
        let sw = Stopwatch()
        sw.Start()        
        if b <= a then
            sw.Stop()
            failure sw (sprintf "%s. Expected b = (%A) to be less than Or equal to a = (%A)."
                            message b a)
        else
            sw.Stop()
            success sw

    let isGreaterThan a message b =
        let sw = Stopwatch()
        sw.Start()        
        if b > a then
            sw.Stop()
            failure sw (sprintf "%s. Expected b = (%A) to be greater than a = (%A)."
                            message b a)
        else
            sw.Stop()
            success sw

    let isGreaterThanOrEqual a message b =
        let sw = Stopwatch()
        sw.Start()        
        if b >= a then
            sw.Stop()
            failure sw (sprintf "%s. Expected b = (%A) to be greater than Or equal to a = (%A)."
                            message b a)
        else
            sw.Stop()
            success sw

    let isOfType expected message actual =
        let sw = Stopwatch()
        sw.Start()        
        if actual.GetType() = expected then
            sw.Stop()
            success sw
        else
            sw.Stop()
            failure sw (sprintf "%s. Expected a value of type %A but Received an object of type %A"
                            message expected (actual.GetType()))

    let isNotOfType expected message actual =
        let sw = Stopwatch()
        sw.Start()        
        if actual.GetType() = expected then
            sw.Stop()
            failure sw (sprintf "%s. Expected a value of type different to %A but Received an object of the same type"
                            message expected )
        else
            sw.Stop()
            success sw
                

    let inline isEmpty message (collection : ^T when ^T: ( member Length : int)) =
        let sw = Stopwatch()
        sw.Start()        
        let size = (^T : (member Length : int) (collection))
        if size > 0 then
            sw.Stop()
            failure sw (sprintf "%s. Expected an empty collection but its size was %i"
                            message size )
        else
            sw.Stop()
            success sw
        
    let inline isNotEmpty message (collection : ^T when ^T: ( member Length : int)) =
        let sw = Stopwatch()
        sw.Start()        
        let size = (^T : (member Length : int) (collection))
        if size = 0 then
            sw.Stop()
            failure sw (sprintf "%s. Expected a Non-empty collection but its size was %i"
                            message size )
        else
            sw.Stop()
            success sw

    let pass () = 
        let sw = Stopwatch()
        success sw

    let fail message =
        let sw = Stopwatch()
        failure sw message

