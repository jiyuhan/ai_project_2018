open ReadFile

[<EntryPoint>]
let main argv =
    printf "Please enter your datafile:"
    let datafileName = System.Console.ReadLine();
    let path = "/Users/Huangzexian/Projects/parser/res/" + datafileName

    printf "Please enter your data model:" 
    let dataModelName = System.Console.ReadLine();
    let attributePath = "/Users/Huangzexian/Projects/parser/res/" + dataModelName

    let carInfo = ReadFile.getData (path)
    printf "%A" carInfo
    printf "The data file is loaded"

    let carModelInfo = ReadFile.getattributeData(attributePath)
    printf "%A" carModelInfo
    printf "The data model is loaded"
    0
