#r "C:/Users/Thomas/Documents/Visual Studio 2017/Projects/DecisionTree/packages/FSharp.Data.2.4.6/lib/net45/FSharp.Data.dll"
open System
open FSharp.Data
open FSharp.Data.JsonExtensions
open System.IO

(* ================================================================================= *)

[<Literal>]
let namesPath = "C:\Users\Thomas\Documents\Visual Studio 2017\Projects\DecisionTree\DecisionTree\car.names.json"

[<Literal>]
let dataPath = "C:\Users\Thomas\Documents\Visual Studio 2017\Projects\DecisionTree\DecisionTree\car.data.json"
// this path is recommended while developing because it relieves the workload on debugger
// let dataPath = "C:\Users\Thomas\Documents\Visual Studio 2017\Projects\DecisionTree\DecisionTree\car.data.short.json"
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

let rec contains stringToCheck (attributes: string list) : bool =
    match attributes with
        | [] -> false
        | h :: t -> if h.Equals stringToCheck then true else contains stringToCheck t

let rec containsFloat floatToCheck (float_list: float list) : bool =
    match float_list with
        | [] -> false
        | h :: t -> if h = floatToCheck then true else containsFloat floatToCheck t

let rec containsInt intToCheck (int_list: int list) : bool =
    match int_list with
        | [] -> false
        | h :: t -> if h = intToCheck then true else containsInt intToCheck t

let rec indexAt (stringToCheck: string) (attributes: string list) (countAt: int): int =
    match attributes with
        | [] -> -1
        | h :: t -> if h.Equals stringToCheck then countAt else (indexAt stringToCheck t (countAt + 1))

indexAt "5" ["1"; "2"; "3"; "4"; "5"] 0

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
    | DecisionNode of string * (string * DecisionTreeNode) list
    // decision * original piece of data
    | Leaf         of string * Datum list

// attributes: string can be used for pattern matching, but we have no discriminator in our case
/// Return the total true, total false, and total count for a set of Records
let listToTuple l =
    let l' = List.toArray l
    let types = l' |> Array.map (fun o -> o.GetType())
    let tupleType = Microsoft.FSharp.Reflection.FSharpType.MakeTupleType types
    Microsoft.FSharp.Reflection.FSharpValue.MakeTuple (l' , tupleType)

let variable_count (class_string: string list): int list = [ for i in 1 .. (class_.Length + 1) -> 0]
let probability_list (class_string: string list): float list = [ for i in 1 .. (class_.Length + 1) -> 0.0]

let rec incrementAtLocationHelper (int_list: int list) (at: int) (counter: int)=
    match int_list with
    | [] -> failwith "It should never reach here, just so you know"
    | h :: t -> if counter = at then [(h + 1)] @ t else [h] @ (incrementAtLocationHelper t at (counter + 1))

let incrementAtLocation (int_list: int list) (at: int) =
    incrementAtLocationHelper int_list at 0

let rec incrementAtMultipleLocation (int_list: int list) (at: int list) =
    match at with
    | [] -> int_list
    | h :: t -> incrementAtMultipleLocation (incrementAtLocation int_list h) t

incrementAtMultipleLocation [0;0;0;0;0] [1;3]

let rec countClassifications (data: Datum list) (var_count: int list)= 
    match data with
    | [] -> var_count
    | h :: t -> countClassifications t (incrementAtMultipleLocation var_count [(indexAt h.Decision.Info class_ 0); class_.Length])

let entropyMathHelper (float_list: float list): float =
    (List.fold
        (fun listToBuild (item: float) ->
            listToBuild @ [(-item * Math.Log(item, 2.0))])
        [] float_list) |> List.sum

countClassifications dataInDatumList (variable_count class_)

let entropy (data: Datum list) : float = 
    let data_stat: int list = countClassifications data (variable_count class_)
    let totalCount: int = data_stat.Item class_.Length
    let input_stat: int list = (List.chunkBySize class_.Length data_stat).Item 0
    // printfn "%A" input_stat
    let rec normalizeListToFloatList (int_list: int list) (divider: int) : float list =
        match int_list with
        | [] -> []
        | h :: t -> [(float h) / (float divider)] @ (normalizeListToFloatList t divider)
    in
    let prob_stat: float list = normalizeListToFloatList input_stat totalCount
    // printfn "%A" prob_stat
    // Log2(0.0) = -infinity, short circuiting this part
    if containsFloat 0.0 prob_stat then
        0.0
    else
        entropyMathHelper prob_stat

entropy dataInDatumList

let informationGain (data : Datum list) attr =
    let divisionsByAttribute = 
        data 
        |> List.groupBy(fun item -> item.GetAttributeValue(attr))

    // printfn "%d" divisionsByAttribute.Length

    let totalEntropy = entropy data
    
        
    let mapping = List.map (fun (attributeValue, rowsWithThatValue: Datum list) -> 
                        // printfn "==============================="
                        // printfn "%A" rowsWithThatValue
                        // printfn "rows' length %d" rowsWithThatValue.Length
                        // printfn "data's length %d" data.Length
                        let local_entropy = entropy rowsWithThatValue
                        // printfn "local entropy is: %f" local_entropy
                        let percentageOfTotalRows = (float rowsWithThatValue.Length) / (float data.Length)
                        // printfn "percentage of total rows: %f" percentageOfTotalRows
                        -1.0 * percentageOfTotalRows * local_entropy) 
                        divisionsByAttribute

    // printfn "%A" mapping
    let entropyBasedOnSplit = mapping |> List.sum
    // printfn "%f" entropyBasedOnSplit
    totalEntropy + entropyBasedOnSplit

informationGain dataInDatumList "buying"
informationGain dataInDatumList "maint"
informationGain dataInDatumList "doors"
informationGain dataInDatumList "persons"
informationGain dataInDatumList "lug_boot"
informationGain dataInDatumList "safety"
// ----------------------------------------------------------------------------

/// Give a list of attributes left to branch on and training data,
/// construct a decision tree node.
let rec createTreeNode data attributesLeft =
    
    let data_stat: int list = countClassifications data (variable_count class_)

    // If we have tested all attributes, then label this node with the 
    // most often occuring instance; likewise if everything has the same value.
    if List.length attributesLeft = 0 || containsInt 0 data_stat then
        let mostOftenOccuring = 
            if totalTrue > totalFalse then true
            else false
        Leaf(mostOftenOccuring, data)
    
    // Otherwise, create a proper decision tree node and branch accordingly
    else
        let attributeWithMostInformationGain =
            attributesLeft 
            |> List.map(fun attrName -> attrName, (informationGain data attrName))
            |> List.maxBy(fun (attrName, infoGain) -> infoGain)
            |> fst
        
        let remainingAttributes =
            attributesLeft |> List.filter ((<>) attributeWithMostInformationGain)

        // Partition that data base on the attribute's values
        let partitionedData = 
            Seq.groupBy
                (fun (d : Datum) -> d.GetAttributeValue(attributeWithMostInformationGain))
                data

        // Create child nodes
        let childNodes =
            partitionedData
            |> List.map (fun (attrValue, subData) -> attrValue, (createTreeNode subData remainingAttributes))

        DecisionNode(attributeWithMostInformationGain, childNodes)
[<EntryPoint>]
let main argv =
    
    0
