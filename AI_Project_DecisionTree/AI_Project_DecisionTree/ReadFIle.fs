// Learn more about F# at http://fsharp.org
module ReadFile
open System.IO
open FSharp.Data

// "/Users/Huangzexian/Projects/parser/res/car.names.json"

let getattributeData(filePath:string) = JsonValue.Load(filePath)


// "/Users/Huangzexian/Projects/parser/res/car.data.json"

let getData(filePath:string) = JsonValue.Load(filePath)






