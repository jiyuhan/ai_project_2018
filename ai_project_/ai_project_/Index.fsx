// #r "C:/Users/Thomas/Documents/Visual Studio 2017/Projects/DecisionTree/packages/FSharp.Data.2.4.6/lib/net45/FSharp.Data.dll"
#r "../packages/FSharp.Data.2.4.6/lib/net45/FSharp.Data.dll"
open FSharp.Data.Runtime.StructuralInference
#r "../packages/MathNet.Numerics.4.4.1/lib/net40/MathNet.Numerics.dll"
open System
open FSharp.Data
open FSharp.Data.JsonExtensions
open System.IO
open MathNet.Numerics.Distributions

(* ================================================================================= *)
[<Literal>]
let BASE_PATH = "/Users/thomas/Desktop/ai_project_2018/ai_project_/ai_project_"
[<Literal>]
// let namesPath = "C:\Users\Thomas\Documents\Visual Studio 2017\Projects\DecisionTree\DecisionTree\car.names.json"
let NamesPath = BASE_PATH + "/car.names.json"
[<Literal>]
let DataPath = BASE_PATH + "/car.data.json"
// let DataPath = BASE_PATH + "/data.short.json"
// let DataPath = "C:\Users\Thomas\Documents\Visual Studio 2017\Projects\DecisionTree\DecisionTree\car.data.json"
// this path is recommended while developing because it relieves the workload on debugger
// let dataPath = "C:\Users\Thomas\Documents\Visual Studio 2017\Projects\DecisionTree\DecisionTree\car.data.short.json"
(* ================================================================================= *)

type NamesType = JsonProvider<NamesPath>
let parseToNamesObj stringObj= NamesType.Parse(stringObj)

type DataType = JsonProvider<DataPath>
let parseToDataObj (stringObj:string) = DataType.Parse(stringObj)

let namesString = File.ReadAllText(NamesPath)
let dataString = File.ReadAllText(DataPath)

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
let rec assemblesAttributes (datum: JsonProvider<DataPath>.Root) (attributes: Attribute list): AttributeSimple list =
    match attributes with
    | [] -> []
    // get the specific attribute for this piece of data
    | h :: t -> [{Name = h.Name; Info = JsonExtensions.Item(datum.JsonValue, h.Name).AsString()}] @ (assemblesAttributes datum t)

let rec parseDatumInToDatumTypeHelper (datum: JsonProvider<DataPath>.Root) (attributes: Attribute list): Datum =
    {Attributes = (assemblesAttributes datum attributes)  ; Decision = { Name = "class"; Info = JsonExtensions.Item(datum.JsonValue, "class").AsString()}}

let rec parseDataIntoDatumList (data: JsonProvider<DataPath>.Root list) (attributes: Attribute list): Datum list =
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
    | Leaf         of string

// attributes: string can be used for pattern matching, but we have no discriminator in our case
/// Return the total true, total false, and total count for a set of Records
let listToTuple l =
    let l' = List.toArray l
    let types = l' |> Array.map (fun o -> o.GetType())
    let tupleType = Microsoft.FSharp.Reflection.FSharpType.MakeTupleType types
    Microsoft.FSharp.Reflection.FSharpValue.MakeTuple (l' , tupleType)

let initialize_class_count_list (class_string: string list): int list = [ for i in 1 .. (class_.Length + 1) -> 0]
let probability_list (class_string: string list): float list = [ for i in 1 .. (class_.Length + 1) -> 0.0]

let rec incrementAtLocationHelper (int_list: int list) (at: int) (counter: int)=
    match int_list with
    | [] -> []
    | h :: t -> if counter = at then [(h + 1)] @ t else [h] @ (incrementAtLocationHelper t at (counter + 1))
let incrementAtLocation (int_list: int list) (at: int) =
    incrementAtLocationHelper int_list at 0

// this increments multiple variables in the list
let rec incrementAtMultipleLocation (int_list: int list) (at: int list) =
    match at with
    | [] -> int_list
    | h :: t -> incrementAtMultipleLocation (incrementAtLocation int_list h) t
let rec countClassifications (data: Datum list) (var_count: int list)= 
    match data with
    | [] -> var_count
    | h :: t -> countClassifications t (incrementAtMultipleLocation var_count [(indexAt h.Decision.Info class_ 0); class_.Length])
let entropyMathHelper (float_list: float list): float =
    (List.fold
        (fun listToBuild (item: float) ->
            listToBuild @ [ (if(item <> 0.0) then(-item * Math.Log(item, 2.0)) else 0.0)])
        [] float_list) |> List.sum
let entropy (data: Datum list) : float = 
    let data_stat: int list = countClassifications data (initialize_class_count_list class_)
    let totalCount: int = data_stat.Item class_.Length
    let input_stat: int list = (List.chunkBySize class_.Length data_stat).Item 0
    // printfn "%A" input_stat
    let rec normalizeListToFloatList (int_list: int list) (divider: int) : float list =
        match int_list with
        | [] -> []
        | h :: t -> [(float h) / (float divider)] @ (normalizeListToFloatList t divider)
    in
    let probStat: float list = normalizeListToFloatList input_stat totalCount
    // printfn "%A" probStat
    // Log2(0.0) = -infinity, short circuiting this part
    // if containsFloat 0.0 probStat then
    //     0.0
    // else
    entropyMathHelper probStat
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

// ----------------------------------------------------------------------------



/// Give a list of attributes left to branch on and training data,
/// construct a decision tree node.
let rec DTL data attributesLeft (pruning : bool) =
    
    let dataStat: int list = countClassifications data (initialize_class_count_list class_)
    let inputStat: int list = (List.chunkBySize class_.Length dataStat).Item 0
    // If we have tested all attributes, then label this node with the 
    // most often occuring instance; likewise if everything has the same value.
    if List.isEmpty attributesLeft || (List.filter (fun (i, somethingelse) -> (i = 0)) ((List.groupBy (fun (i: int) -> i) inputStat))).Length = 3 then
    // || containsInt 0 dataStat then
        let mostOftenOccuring: string = 
            class_.Item ((List.sort inputStat).Item (class_.Length - 1))

        // calculate s, p, n here
        // calculate chi-square using s, p, n
        // then doing post-pruning here
        //TODO:
        Leaf(mostOftenOccuring)
    
    // Otherwise, create a proper decision tree node and branch accordingly
    else
        let attributeWithMostInformationGain =
            attributesLeft 
            |> List.map(fun attrName -> attrName, (informationGain data attrName))
            |> List.maxBy(fun (attrName, infoGain) -> infoGain)
            |> fst
        // printfn "attribute with most ig: %s" attributeWithMostInformationGain
        let remainingAttributes =
            attributesLeft |> List.filter ((<>) attributeWithMostInformationGain)
        // printfn "remaining attributes are: %A" remainingAttributes
        // Partition that data base on the attribute's values
        let partitionedData = 
            List.groupBy
                (fun (d : Datum) -> d.GetAttributeValue(attributeWithMostInformationGain))
                data
        // printfn "remaining attributes are %A" partitionedData
        // Create child nodes
        let childNodes =
            partitionedData
            |> List.map (fun (attrValue, subData) -> attrValue, (DTL subData remainingAttributes pruning))

        DecisionNode(attributeWithMostInformationGain, childNodes)

let rec decisionTreeToString (decisionTree: DecisionTreeNode): string =
    let rec somethingToStringHelper (toParse: (string * DecisionTreeNode) list) : string = 
        match toParse with
        | [] -> ""
        | [(s, treeNode)] -> "{\"" + s + "\":" + decisionTreeToString treeNode + "}"
        | (s, treeNode) :: t -> "{\"" + s + "\":" + decisionTreeToString treeNode + "}," + (somethingToStringHelper t)
    in
    match decisionTree with
    | DecisionNode (s, node)  -> "{\"" + s + "\": [" + (somethingToStringHelper node) + "]}"
    // | DecisionNode (t1,t2)-> decisionTreeToString(t2)
    | Leaf l -> "\"" + l + "\""

// --------------
// second part of Part I

let rand = new Random()

let whole_names = parseToNamesObj namesString
let whole_data = parseToDataObj dataString

let whole_attributes = whole_names.AttrList |> Array.toList
let whole_names_attributes = namesObj whole_attributes
let whole_data_list = parseDataIntoDatumList (whole_data |> Array.toList) whole_names_attributes

let shuffle (a:'a[]) =
    let swapByIndex i j =
        let tmp = a.[i]
        a.[i] <- a.[j]
        a.[j] <- tmp
    
    let maxIndex = Array.length a - 1
    [|0..maxIndex|] 
    |> Array.iter (fun i -> swapByIndex i (rand.Next maxIndex))
    a

let buildTrainTestIndexes (length:int) (trainPercent:float) :(int[] * int[]) = 
    let splitIndex = int(Math.Floor(trainPercent * float(length-1)))
    let indexes = [|0..length - 1|]
    shuffle indexes |> ignore
    (indexes.[0..splitIndex],  // training indexes
     indexes.[splitIndex+1..])  // testing indexes


// Split a dataset into a trainingset and a testing set (with no overlap)
let splitDataset (d:Datum list) (trainPercent:float) =
    let (trainIndexes, testIndexes) = buildTrainTestIndexes (List.length d) trainPercent
    (trainIndexes |> Array.map (fun i -> List.item i d),  // training set
     testIndexes  |> Array.map (fun i -> List.item i d))  // testing set

buildTrainTestIndexes(whole_data_list.Length)(0.7)
let (trainData, testData) = splitDataset (whole_data_list)(0.7)

let train = trainData |> Array.toList


let myTrainDecisionTree = DTL train whole_attributes false


// let myTrainDecisionTreeString = Printf.sprintf "%s" myTrainDecisionTree
File.AppendAllText(BASE_PATH + "/train_result.json", (decisionTreeToString myTrainDecisionTree))

let test = testData |> Array.toList

let myTestDecisionTree = DTL test whole_attributes false

 // let myTestDecisionTreeString = Printf.sprintf "%A" myTestDecisionTree
File.AppendAllText(BASE_PATH + "/test_result.json", (decisionTreeToString myTestDecisionTree))
 
let toPredict (toPredict: Datum) (decisionTree: DecisionTreeNode): bool =
    let attributesLeft = toPredict.Attributes
    let rec pickFeatures (attr: AttributeSimple list) (feature: string): AttributeSimple list =
        match attr with
        | [] -> failwith "not found"
        | [h] -> if h.Name.Equals feature then [] else failwith "not found"
        | h :: t -> if h.Name.Equals feature then t else [h] @ (pickFeatures t feature)

    let rec pickSelection (attr: AttributeSimple list) (feature: string): string =
        match attr with
        | [] -> failwith "not found"
        | [h] -> if h.Name.Equals feature then h.Info else failwith "not found"
        | h :: t -> if h.Name.Equals feature then h.Info else pickSelection t feature

    let rec getSelectedDecision (selection: string) (decisionList: (string * DecisionTreeNode) list): DecisionTreeNode =
        match decisionList with
        | [] -> failwith "didn't find selected feature"
        | [(s, node)] -> if (s.Equals selection) then node else failwith "didn't find selected feature"
        | (s, node) :: t -> if s.Equals selection then node else (getSelectedDecision selection t)
    in
    // this iterates through DecisionTreeNode to find the right feature
    let rec predictHelper (attributesLeft: AttributeSimple list) (decisionTreeLeft: DecisionTreeNode): DecisionTreeNode =
        match (decisionTreeLeft) with
        | (Leaf(x)) -> Leaf(x)
        | (DecisionNode(x, y)) -> (predictHelper (pickFeatures attributesLeft x) (getSelectedDecision (pickSelection attributesLeft x) y))
    in
    let leafUnpack (decisionTree: DecisionTreeNode): string =
        match (decisionTree) with
        | Leaf(x) -> x
        | (_) -> failwith "not a leaf??"
    in
    // check equality 
    (toPredict.Decision.Info = leafUnpack (predictHelper attributesLeft decisionTree))

let rec getAccuracy (record: int * int) (datumList: Datum list) (decisionTree: DecisionTreeNode): int * int =
    match (datumList, record) with
    | ([], r) -> r
    | ([h], (c, total)) -> if toPredict h decisionTree then (c + 1, total + 1) else (c, total + 1)
    | (h :: t, (c, total)) -> if toPredict h decisionTree then getAccuracy (c + 1, total + 1) t decisionTree else getAccuracy (c, total + 1) t decisionTree

printf "testing accuracy %A" (getAccuracy (0, 0) test myTestDecisionTree)
printf "training accuracy %A" (getAccuracy (0, 0) train myTrainDecisionTree)

(* == part II == *)
let rec getDepth (tree: DecisionTreeNode) : int =
    let rec getLengthsInList (toParse: (string * DecisionTreeNode) list) (current_list: int list) : (int list) = 
        match toParse with
        | [] -> [0]
        | [(s, treeNode)] -> current_list @ [getDepth treeNode]
        | (s, treeNode) :: t ->  current_list @ [getDepth treeNode] @ (getLengthsInList t current_list)
    in
    match tree with
    | DecisionNode (s, node)  -> 1 + (List.maxBy (fun (i) -> i) (getLengthsInList node []))
    // | DecisionNode (t1,t2)-> decisionTreeToString(t2)
    | Leaf _ -> 1


