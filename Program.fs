namespace HCRD

module Dormant = 
    open System
    ///<Description> </Description>
    type TableMessage< ^T > = 
        | Add of ^T 
        | Remove of ^T 

    ///<Description> </Description>
    type Table< ^T > = ^T []
    
    //     let mutable private tables = Array.empty
    //     let agent = 
    //         MailboxProcessor.Start(fun inbox->
    //             // the message processing function
    //             let rec messageLoop() = async{

    //                 // read a message
    //                 let! msg = inbox.Receive()

    //                 match msg with 
    //                 | Add item -> 
    //                     tables <- Array.append tables [| item |]
    //                 | Remove item -> 
    //                     tables <- Array.filter ( fun x -> x = item |> not ) tables

    //                 printfn "%A" tables

    //                 // loop to top
    //                 return! messageLoop()
    //                 }

    //             // start the loop
    //             messageLoop()
    //         )
    //     member _.Post = agent.Post
    
    
    
    ///<Description> </Description>
    type DatabaseMessage =
        | AddTable of Type
        | AddToTable of ( Type * obj )
        | GetItems of ( Type * ( obj -> bool ) )// (Type * id) 
        | Print of Type
        | List 
    
    type Message = DatabaseMessage * AsyncReplyChannel< (Type * obj[]) option >

    ///<Description>Wrapper for </Description>
    module Database = 
        let agent = 
            MailboxProcessor<Message>.Start(fun inbox -> 
                // the message processing function
                let rec messageLoop tables = 
                    async {
                        // read a message
                        let! ( msg, replyChannel ) = inbox.Receive()
                        printfn "%A %A" msg replyChannel
                        let newTables, reply = 
                            match msg with 
                            | AddTable _type -> 
                                Map.add _type.Name [| |] tables, None
                            | AddToTable item -> 
                                let _type = (fst item)
                                Map.change _type.Name ( fun x ->
                                    match x with
                                    | Some data -> Array.append data [| snd item |] |> Some
                                    | None -> None
                                ) tables, None
                            | GetItems (_type, getter) ->
                                tables,
                                Map.find _type.Name tables
                                |> Array.filter ( getter )
                                |> fun x -> _type, x
                                |> Some
                            | Print _type -> 
                                let mutable table = [| |]
                                if Map.containsKey _type.Name tables
                                then printfn "%A" ( Map.find _type.Name tables )
                                else printfn "No table assigned to type %A." _type
                                tables, None
                            | List -> 
                                printfn "%A" tables
                                tables, None

                        replyChannel.Reply reply

                        // loop to top
                        return! messageLoop newTables
                    }

                // start the loop
                Map.empty
                |> messageLoop 
            )
        let post msg = 
            agent.PostAndAsyncReply ( fun rc -> msg, rc) 

module Main = 
    open Dormant
    
    
    type Other = 
        { id: int 
          name: string
        }

    // let myOther = Other< someOtherType >
    // typeof<myOther> => Type<Other< someOtherType >> | typedefof<myOther> => Type< Other< 't > > 

    type Simple = 
        { id: int 
          name: string
          other: Other
        }


    type MyContainer< 't, 's > = | T

    [<EntryPoint>]
    let main args = 
        // let otherTable = Table< Other > ()
        // let simpleTable = Table< Simple > ()
        // let test = Database()
        // let other = { id = 1; name = "foo" }
        // let other_other_other_other = { id = 2; name = "bar" }
        // let other_other = { id = 3; name = "fizz" }
        // let other_other_other = { id = 4; name = "buzz" }
        
        // let simple = { id = 1; name = "smitty"; other = other }

        // Database.post ( AddTable typeof< Other > )
        // [other; other_other; other_other_other; other_other_other_other]
        // |> List.map (fun row -> Database.post ( AddToTable ( typeof< Other >, row) ) |> Async.RunSynchronously )
        // |> printfn "%A"
        
        // Database.post ( AddTable typeof< Simple >) |> Async.RunSynchronously
        // Database.post ( AddToTable (typeof< Simple >, simple) ) |> Async.RunSynchronously
 
        // Database.post List |> Async.RunSynchronously

        // Database.post ( GetItems (typeof<Other>, fun x -> (unbox<Other> x).id = 2 ) )
        // |> Async.RunSynchronously
        // |> function 
        // | Some data -> printfn "%A" data 
        // | None -> printfn "Error trying to get item Other where id = 1"
        
        let myContainer = MyContainer< int, string >.T 
        typeof< MyContainer< int, string > > |> printfn "Typeof : %A"
        typedefof< MyContainer< int, string > > |> printfn "Typedefof : %A"
        myContainer.GetType().GetGenericArguments() |> printfn "GetTypeArguments : %A"



        // Database.post ( GetItems (typeof<Other>, fun x -> (unbox<Other> x ).id = 1 ))
        // otherTable.Post (Add other)

        // printerAgent.Post "test message"

        // System.Threading.Thread.Sleep 6000
        0

