// Learn more about F# at http://fsharp.org

open System.IO
open FSharp.Data

let path = "/Users/Huangzexian/Projects/parser/res/car.data.json"

let getData(filePath:string) = 
            JsonValue.Load(filePath)

let carInfo = getData(path)

[<EntryPoint>]

let main argv =
    let carInfo = getData(path)

    printf "%A" carInfo
    0




