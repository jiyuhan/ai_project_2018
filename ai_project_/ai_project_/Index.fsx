#r "C:/Users/Thomas/Documents/Visual Studio 2017/Projects/DecisionTree/packages/FSharp.Data.2.4.6/lib/net45/FSharp.Data.dll"
open System
open FSharp.Data
open FSharp.Data.JsonExtensions
open System.IO

(* ================================================================================= *)

[<Literal>]
let namesPath = "C:\Users\Thomas\Documents\Visual Studio 2017\Projects\DecisionTree\DecisionTree\car.names.json"

[<Literal>]
// let dataPath = "C:\Users\Thomas\Documents\Visual Studio 2017\Projects\DecisionTree\DecisionTree\car.data.json"
// this path is recommended while developing because it relieves the workload on debugger
let dataPath = "C:\Users\Thomas\Documents\Visual Studio 2017\Projects\DecisionTree\DecisionTree\car.data.short.json"
(* ================================================================================= *)

type NamesType = JsonProvider<namesPath>
let parseToNamesObj stringObj= NamesType.Parse(stringObj)

type DataType = JsonProvider<dataPath>
let parseToDataObj (stringObj:string) = DataType.Parse(stringObj)

let namesString = File.ReadAllText(namesPath)
let dataString = File.ReadAllText(dataPath)

let names = parseToNamesObj namesString
let data = parseToDataObj dataString

let attributes = names.AttrList |> Array.toList
let class_ = names.Class |> Array.toList

let valuesOfAttr (a: string) : string list = 
    let rec arrayStringArray (a: JsonValue list): string list =
        match a with
        | [] -> []
        | h :: t -> [h.AsString()] @ (arrayStringArray t)
    in
    arrayStringArray ((JsonExtensions.Item(names.Attr.JsonValue, a).AsArray()) |> Array.toList)

(* ================================================================================= *)

type Attribute =
    {
        Name: string
        Info: string
        Options: string list
    }

    override this.ToString() =
        sprintf
            "%s = %s"
            this.Name
            this.Info

let rec namesObj (attributes: string list) : Attribute list = 
    match attributes with
    | [] -> []
    | h :: t -> [{Name = h; Info = ""; Options= (valuesOfAttr h)}] @ (namesObj t)

let namesInAttributes = namesObj attributes

(* ================================================================================= *)
type AttributeSimple =
    {
        Name: string
        Info: string
    }

    override this.ToString() =
        sprintf
            "%s = %s"
            this.Name
            this.Info

// this is to get the attribute's value (instead of options) in a given datum
// this is used as a recursive helper function for type Datum
let rec getAttrVal (attrName: string) (attributes: AttributeSimple list) : string =
    match (attrName, attributes) with
    | (attrName, []) -> failwithf "Invalid attribute name %s" attrName
    | (attrName, h :: t) -> if h.Name.Equals attrName then h.Info else getAttrVal attrName t

type Datum = 
    {
        Attributes  : AttributeSimple list
        Decision    : AttributeSimple 
    }

    /// Given an attribute name return its value
    member this.GetAttributeValue(attrName) =
        getAttrVal attrName this.Attributes

    /// Make the %o format specifier look all pretty like
    override this.ToString() =
        sprintf
            "{ %A %A }"
            this.Attributes
            this.Decision

let rec validAttribute stringToCheck (attributes: string list) : bool =
    match attributes with
        | [] -> false
        | h :: t -> if h.Equals stringToCheck then true else validAttribute stringToCheck t

// let constructOneAttribute (attribute: string) (value: string) : Attribute = 
    

// let rec constructAllAttributes (attributes:)

let rec assemblesAttributes (datum: JsonProvider<dataPath>.Root) (attributes: Attribute list): AttributeSimple list =
    match attributes with
    | [] -> []
    // get the specific attribute for this piece of data
    | h :: t -> [{Name = h.Name; Info = JsonExtensions.Item(datum.JsonValue, h.Name).AsString()}] @ (assemblesAttributes datum t)

let rec parseDatumInToDatumTypeHelper (datum: JsonProvider<dataPath>.Root) (attributes: Attribute list): Datum =
    {Attributes = (assemblesAttributes datum attributes)  ; Decision = { Name = "class"; Info = JsonExtensions.Item(datum.JsonValue, "class").AsString()}}

let rec parseDataIntoDatumList (data: JsonProvider<dataPath>.Root list) (attributes: Attribute list): Datum list =
    match data with
    | [] -> []
    | h :: t -> [(parseDatumInToDatumTypeHelper h attributes)] @ parseDataIntoDatumList t attributes

let dataInDatumList = parseDataIntoDatumList (data |> Array.toList) namesInAttributes

(* ================================================================================= *)

(* == So you are here! All the data has been loaded!! Please call namesInAttributes and dataInDatumList for the according data == *)

type DecisionTreeNode =
    // attr * ( selected * nested node)
    | DecisionNode of string * (string * DecisionTreeNode) seq
    // decision * original piece of data
    | Leaf         of string * Datum seq

[<EntryPoint>]
let main argv =
    
    0
