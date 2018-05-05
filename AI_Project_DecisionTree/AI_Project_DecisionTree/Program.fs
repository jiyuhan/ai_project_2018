// Learn more about F# at http://fsharp.org

open System
open FSharp.Data

[<Literal>]

let DataFilePath = "/Users/Huangzexian/Projects/parser/res/car.data.json"

type CarData = JsonProvider<DataFilePath>



