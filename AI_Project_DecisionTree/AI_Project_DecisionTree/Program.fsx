// Learn more about F# at http://fsharp.org

open System
#r "../../../bin/lib/net45/FSharp.Data.dll"
open FSharp.Data

let attr_file = JsonValue.Load("/Users/thomas/Desktop/ai_project_2018/parser/res/car.names.json")

//type AttrList = JsonProvider<""" {attr_list: ["attr_1", "attr_2"]} """>

let attrString = System.IO.File.ReadAllText "/Users/thomas/Desktop/ai_project_2018/parser/res/car.names.json"

//type Simple = JsonProvider<""" { "name":"John", "age":94 } """>
//let simple = Simple.Parse(""" { "name":"Tomas", "age":4 } """)
//simple.Age
//simple.Name

//type Numbers = JsonProvider<""" [1, 2, 3, 3.14] """>
//let nums = Numbers.Parse(""" [1.2, 45.1, 98.2, 5] """)
//let total = nums |> Seq.sum

//type Mixed = JsonProvider<""" [1, 2, "hello", "world"] """>
//let mixed = Mixed.Parse(""" [4, 5, "hello", "world" ] """)

//mixed.Numbers |> Seq.sum
//mixed.Strings |> String.concat ", "


//type People = JsonProvider<""" 
//  [ { "name":"John", "age":94 }, 
//    { "name":"Tomas" } ] """>

//for item in People.GetSamples() do 
  //printf "%s " item.Name 
  //item.Age |> Option.iter (printf "(%d)")
  //printfn ""


//[<Literal>]
//let nameSample = """
//{
//    "class":"unacc"
//}
//"""
//type Names = JsonProvider<nameSample>
//// type Strings = JsonProvider<""" ["hello", "yay", "testing"] """, SampleIsList=true>
//let path = "/Users/thomas/Desktop/ai_project_2018/parser/res/car.names.json"

//let names = Names.Parse("""
//{
//    "class":"unacc"
//}
//""")

// let newNames = Names.Name

//type Names = JsonProvider<"/Users/thomas/Desktop/ai_project_2018/parser/res/car.data.json">
//let doc = Names.GetSamples

//let rec printList (a: string list): string list =
    //match a with
    //  [] -> 0
    //| h :: t -> (printf "%s" h) :: printList t

// let toString a = 
let arrayToList (myArray: string array) = myArray |> Array.toList
type Simple = JsonProvider<""" { "name":"John", "age":94 } """>
let simple = Simple.Parse(""" { "name":"Tomas", "age":4 } """)

[<EntryPoint>]
let main argv =
    printf "%A" simple
    0 // return an integer exit code
