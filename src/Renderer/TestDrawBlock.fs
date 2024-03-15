module TestDrawBlock
open GenerateData
open Optics
open Optics.Operators
open DrawHelpers
open Helpers
open CommonTypes
open ModelType
open DrawModelType
open Sheet.SheetInterface
open GenerateData
open Elmish




/// TODO: apply D1 etc. here
let applyBeautifyAlgorithm (sheet: SheetT.Model) =
    // dummy sheet
    SheetBeautifyD1.sheetAlignScale sheet
    

//let getTestMetrics (sheet : SheetT.Model) : TestMetrics =
//    {
//        numSegments = sheet.Wire.Wires.Values
//                        |> Seq.toList
//                        |> List.sumBy (fun wire -> List.length wire.Segments)

//        numVisibleSegments = SheetBeautifyHelpers.getVisibleSegmentCount sheet

//        visibleLength = SheetBeautifyHelpers.calcVisWireLength sheet

//        rightAngleCount = SheetBeautifyHelpers.numOfVisRightAngles sheet

//        rightAngleCrossings = SheetBeautifyHelpers.numOfWireRightAngleCrossings sheet

//        wireSymbolIntersections = SheetBeautifyHelpers.numOfIntersectSegSym sheet

//        retracingSegments = (SheetBeautifyHelpers.findRetracingSegments sheet).RetraceSegs |> List.length

//        retracingSegmentsInsideSymbol = (SheetBeautifyHelpers.findRetracingSegments sheet).RetraceSegsInSymbol |> List.length

//        intersectingSymbols = SheetBeautifyHelpers.numOfIntersectSegSym sheet
//    }


//let compareTestMetrics (originalMetrics : TestMetrics) (newMetrics : TestMetrics) : string list =
//    let compareInts (name : string) (original : int) (newData : int) =
//        if original <> newData then
//            [ $"{name} changed from {original} to {newData}" ]
//        else
//            []

//    let compareFloats (name : string) (original : float) (newData : float) =
//        if original <> newData then
//            [ $"{name} changed from {original} to {newData}" ]
//        else
//            []

//    let intComparisons =
//        [
//            compareInts "Number of segments" originalMetrics.numSegments newMetrics.numSegments
//            compareInts "Number of visible segments" originalMetrics.numVisibleSegments newMetrics.numVisibleSegments
//            compareInts "Number of right angle segments" originalMetrics.rightAngleCount newMetrics.rightAngleCount
//            compareInts "Number of right angle crossings" originalMetrics.rightAngleCrossings newMetrics.rightAngleCrossings
//            compareInts "Number of wire symbol intersections" originalMetrics.wireSymbolIntersections newMetrics.wireSymbolIntersections
//            compareInts "Number of retracing segments" originalMetrics.retracingSegments newMetrics.retracingSegments
//            compareInts "Number of retracing segments inside symbols" originalMetrics.retracingSegmentsInsideSymbol newMetrics.retracingSegmentsInsideSymbol
//            compareInts "Number of intersecting symbols" originalMetrics.intersectingSymbols newMetrics.intersectingSymbols
//        ]
//        |> List.concat

//    let floatComparisons =
//        [
//            compareFloats "Visible length of all segments" originalMetrics.visibleLength newMetrics.visibleLength
//        ] |> List.concat

//    intComparisons @ floatComparisons


//-------------------------------------------------------------------------------------------//
//--------Types to represent tests with (possibly) random data, and results from tests-------//
//-------------------------------------------------------------------------------------------//
module TestLib =

    /// convenience unsafe function to extract Ok part of Result or fail if value is Error
    let getOkOrFail (res: Result<'a,string>) =
        match res with
        | Ok x -> x
        | Error mess ->
            failwithf "%s" mess


    type TestStatus =
            | Fail of string
            | Exception of string

    type Test<'a> = {
        Name: string
        Samples: Gen<'a>
        StartFrom: int
        /// The 1st argument is the test number: allows assertions that fail on a specific sample
        /// to display just one sample.
        /// The return value is None if test passes, or Some message if it fails.
        Assertion: int -> 'a -> string option
        }

    type TestResult<'a, 'b> = {
        TestName: string
        TestDataCount: int
        TestSucceededCount: int
        TestSucceededValues: 'a list
        TestErrors: ('b * TestStatus) list
    }

    let catchException name func arg =
        try
            Ok (func arg)
        with
            | e ->
                Error ($"Exception when running {name}\n" + e.StackTrace)
            
 
            
(******************************************************************************************
   This submodule contains a set of functions that enable random data generation
   for property-based testing of Draw Block wire routing functions.
   basic idea.
   1. Generate, in various ways, random circuit layouts
   2. For each layout apply smartautoroute to regenerate all wires
   3. Apply check functions to see if the resulting wire routing obeys "good layout" rules.
   4. Output any layouts with anomalous wire routing
*******************************************************************************************)
module HLPTick3 =
    open EEExtensions
    open Optics
    open Optics.Operators
    open DrawHelpers
    open Helpers
    open CommonTypes
    open ModelType
    open DrawModelType
    open Sheet.SheetInterface
    open GenerateData
    open TestLib

    /// create an initial empty Sheet Model 
    let initSheetModel = DiagramMainView.init().Sheet

    /// Optic to access SheetT.Model from Issie Model
    let sheetModel_ = sheet_

    /// Optic to access BusWireT.Model from SheetT.Model
    let busWireModel_ = SheetT.wire_

    /// Optic to access SymbolT.Model from SheetT.Model
    let symbolModel_ = SheetT.symbol_

    /// allowed max X or y coord of svg canvas
    let maxSheetCoord = Sheet.Constants.defaultCanvasSize
    let middleOfSheet = {X=maxSheetCoord/2.;Y=maxSheetCoord/2.}

    /// Used throughout to compare labels since these are case invariant "g1" = "G1"
    let caseInvariantEqual str1 str2 =
        String.toUpper str1 = String.toUpper str2
 

    /// Identify a port from its component label and number.
    /// Usually both an input and output port will mathc this, so
    /// the port is only unique if it is known to be input or output.
    /// used to specify the ends of wires, since tehee are known to be
    /// connected to outputs (source) or inputs (target).
    type SymbolPort = { Label: string; PortNumber: int }

    /// convenience function to make SymbolPorts
    let portOf (label:string) (number: int) =
        {Label=label; PortNumber = number}


    //-----------------------------------------------------------------------------------------------
    // visibleSegments is included here as ahelper for info, and because it is needed in project work
    //-----------------------------------------------------------------------------------------------

    /// The visible segments of a wire, as a list of vectors, from source end to target end.
    /// Note that in a wire with n segments a zero length (invisible) segment at any index [1..n-2] is allowed 
    /// which if present causes the two segments on either side of it to coalesce into a single visible segment.
    /// A wire can have any number of visible segments - even 1.
    let visibleSegments (wId: ConnectionId) (model: SheetT.Model): XYPos list =

        let wire = model.Wire.Wires[wId] // get wire from model

        /// helper to match even and off integers in patterns (active pattern)
        let (|IsEven|IsOdd|) (n: int) = match n % 2 with | 0 -> IsEven | _ -> IsOdd

        /// Convert seg into its XY Vector (from start to end of segment).
        /// index must be the index of seg in its containing wire.
        let getSegmentVector (index:int) (seg: BusWireT.Segment) =
            // The implicit horizontal or vertical direction  of a segment is determined by 
            // its index in the list of wire segments and the wire initial direction
            match index, wire.InitialOrientation with
            | IsEven, BusWireT.Vertical | IsOdd, BusWireT.Horizontal -> {X=0.; Y=seg.Length}
            | IsEven, BusWireT.Horizontal | IsOdd, BusWireT.Vertical -> {X=seg.Length; Y=0.}

        /// Return a list of segment vectors with 3 vectors coalesced into one visible equivalent
        /// if this is possible, otherwise return segVecs unchanged.
        /// Index must be in range 1..segVecs
        let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int)  =
            if segVecs[index] =~ XYPos.zero
            then
                segVecs[0..index-2] @
                [segVecs[index-1] + segVecs[index+1]] @
                segVecs[index+2..segVecs.Length - 1]
            else
                segVecs

        wire.Segments
        |> List.mapi getSegmentVector
        |> (fun segVecs ->
                (segVecs,[1..segVecs.Length-2])
                ||> List.fold tryCoalesceAboutIndex)


//------------------------------------------------------------------------------------------------------------------------//
//------------------------------functions to build issue schematics programmatically--------------------------------------//
//------------------------------------------------------------------------------------------------------------------------//
    module Builder =
        /// Place a new symbol with label symLabel onto the Sheet with given position.
        /// Return error if symLabel is not unique on sheet, or if position is outside allowed sheet coordinates (0 - maxSheetCoord).
        /// To be safe place components close to (maxSheetCoord/2.0, maxSheetCoord/2.0).
        /// symLabel - the component label, will be uppercased to make a standard label name
        /// compType - the type of the component
        /// position - the top-left corner of the symbol outline.
        /// model - the Sheet model into which the new symbol is added.
        let placeSymbol (symLabel: string) (compType: ComponentType) (position: XYPos) (model: SheetT.Model) : Result<SheetT.Model, string> =
            let symLabel = String.toUpper symLabel // make label into its standard casing
            let symModel, symId = SymbolUpdate.addSymbol [] (model.Wire.Symbol) position compType symLabel
            let sym = symModel.Symbols[symId]
            match position + sym.getScaledDiagonal with
            | {X=x;Y=y} when x > maxSheetCoord || y > maxSheetCoord ->
                Error $"symbol '{symLabel}' position {position + sym.getScaledDiagonal} lies outside allowed coordinates"
            | _ ->
                model
                |> Optic.set symbolModel_ symModel
                |> SheetUpdateHelpers.updateBoundingBoxes // could optimise this by only updating symId bounding boxes
                |> Ok
        


    
        /// Place a new symbol onto the Sheet with given position and scaling (use default scale if this is not specified).
        /// The ports on the new symbol will be determined by the input and output components on some existing sheet in project.
        /// Return error if symLabel is not unique on sheet, or ccSheetName is not the name of some other sheet in project.
        let placeCustomSymbol
                (symLabel: string)
                (ccSheetName: string)
                //(project: Project)
                //(scale: XYPos)
                (position: XYPos)
                (model: SheetT.Model)
                    : Result<SheetT.Model, string> =
           let symbolMap = model.Wire.Symbol.Symbols
           //if caseInvariantEqual ccSheetName project.OpenFileName then
           //     Error "Can't create custom component with name same as current opened sheet"        
           // elif not <| List.exists (fun (ldc: LoadedComponent) -> caseInvariantEqual ldc.Name ccSheetName) project.LoadedComponents then
           //     Error "Can't create custom component unless a sheet already exists with smae name as ccSheetName"
           //if symbolMap |> Map.exists (fun _ sym ->  caseInvariantEqual sym.Component.Label symLabel) then
           if false then
                Error "Can't create custom component with duplicate Label"
            else
                let canvas = model.GetCanvasState()
                let ccType: CustomComponentType =
                    {
                        Name = ccSheetName
                        //InputLabels = Extractor.getOrderedCompLabels (Input1 (0, None)) canvas
                        InputLabels = [("A", 0) ; ("B", 1) ; ("S2", 2) ; ("S1", 3)]
                        //OutputLabels = Extractor.getOrderedCompLabels (Output 0) canvas
                        OutputLabels = [("E", 0) ; ("F", 1); ("G", 2)]
                        Form = None
                        Description = None
                    }
                placeSymbol symLabel (Custom ccType) position model
            
        

        
        /// Rotate the symbol given by symLabel by an amount rotate.
        /// Takes in a symbol label, a rotate fixed amount, and a sheet containing the symbol.
        /// Return the sheet with the rotated symbol.
        let rotateSymbol (symLabel: string) (rotate: Rotation) (model: SheetT.Model) : (SheetT.Model) =

            let symbolsMap = model.Wire.Symbol.Symbols
            let getSymbol = 
                mapValues symbolsMap
                |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symLabel)
                |> function | Some x -> Ok x | None -> Error "Can't find symbol with label '{symPort.Label}'"

            match getSymbol with
            | Ok symbol ->
                let rotatedSymbol = SymbolResizeHelpers.rotateSymbol rotate symbol
                let updatedSymbolsMap = Map.add symbol.Id rotatedSymbol symbolsMap
                { model with Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbolsMap } } }

            | _ -> model

        /// Flip the symbol given by symLabel by an amount flip.
        /// Takes in a symbol label, a flip fixed amount, and a sheet containing the symbol.
        /// Return the sheet with the 
        ///  symbol.
        let flipSymbol (symLabel: string) (flip: SymbolT.FlipType) (model: SheetT.Model) : (SheetT.Model) =

            let symbolsMap = model.Wire.Symbol.Symbols
            let getSymbol =
                mapValues symbolsMap
                |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symLabel)
                |> function | Some x -> Ok x | None -> Error "Can't find symbol with label '{symPort.Label}'"

            match getSymbol with
            | Ok symbol ->
                let flippedSymbol = SymbolResizeHelpers.flipSymbol flip symbol
                let updatedSymbolsMap = Map.add symbol.Id flippedSymbol symbolsMap
                { model with Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbolsMap } } }

            | _ -> model


        /// Add a (newly routed) wire, source specifies the Output port, target the Input port.
        /// Return an error if either of the two ports specified is invalid, or if the wire duplicates and existing one.
        /// The wire created will be smart routed but not separated from other wires: for a nice schematic
        /// separateAllWires should be run after  all wires are added.
        /// source, target: respectively the output port and input port to which the wire connects.
        let placeWire (source: SymbolPort) (target: SymbolPort) (model: SheetT.Model) : Result<SheetT.Model,string> =
            let symbols = model.Wire.Symbol.Symbols
            let getPortId (portType:PortType) symPort =
                mapValues symbols
                |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symPort.Label)
                |> function | Some x -> Ok x | None -> Error "Can't find symbol with label '{symPort.Label}'"
                |> Result.bind (fun sym ->
                    match portType with
                    | PortType.Input -> List.tryItem symPort.PortNumber sym.Component.InputPorts
                    | PortType.Output -> List.tryItem symPort.PortNumber sym.Component.OutputPorts
                    |> function | Some port -> Ok port.Id
                                | None -> Error $"Can't find {portType} port {symPort.PortNumber} on component {symPort.Label}")
            
            match getPortId PortType.Input target, getPortId PortType.Output source with
            | Error e, _ | _, Error e -> Error e
            | Ok inPort, Ok outPort ->
                let newWire = BusWireUpdate.makeNewWire (InputPortId inPort) (OutputPortId outPort) model.Wire
                if model.Wire.Wires |> Map.exists (fun wid wire -> wire.InputPort=newWire.InputPort && wire.OutputPort = newWire.OutputPort) then
                        // wire already exists
                        Error "Can't create wire from {source} to {target} because a wire already exists between those ports"
                else
                     model
                     |> Optic.set (busWireModel_ >-> BusWireT.wireOf_ newWire.WId) newWire
                     |> Ok
            

        /// Run the global wire separation algorithm (should be after all wires have been placed and routed)
        let separateAllWires (model: SheetT.Model) : SheetT.Model =
            model
            |> Optic.map busWireModel_ (BusWireSeparate.updateWireSegmentJumpsAndSeparations (model.Wire.Wires.Keys |> Seq.toList))

        /// Copy testModel into the main Issie Sheet making its contents visible
        let showSheetInIssieSchematic (testModel: SheetT.Model) (dispatch: Dispatch<Msg>) =
            let sheetDispatch sMsg = dispatch (Sheet sMsg)
            dispatch <| UpdateModel (Optic.set sheet_ testModel) // set the Sheet component of the Issie model to make a new schematic.
            sheetDispatch <| SheetT.KeyPress SheetT.CtrlW // Centre & scale the schematic to make all components viewable.


        /// 1. Create a set of circuits from Gen<'a> samples by applying sheetMaker to each sample.
        /// 2. Check each ciruit with sheetChecker.
        /// 3. Return a TestResult record with errors those samples for which sheetChecker returns false,
        /// or where there is an exception.
        /// If there are any test errors display the first in Issie, and its error message on the console.
        /// sheetMaker: generates a SheetT.model from the random sample
        /// sheetChecker n model: n is sample number, model is the genrated model. Return false if test fails.
        let runTestOnSheets
            (sheetMaker: 'c -> SheetT.Model)
            (optimizer: SheetT.Model -> SheetT.Model)
            (sheetChecker: ('c -> SheetT.Model) -> (SheetT.Model -> SheetT.Model) -> TestResult<'a, 'b>)
            (dispatch: Dispatch<Msg>)
                : TestResult<'a, 'b> =

            // run sheetChecker on each sample
            let result = sheetChecker sheetMaker optimizer

            // display result info
            printf $"Test {result.TestName}: {result.TestDataCount} samples tested, {result.TestSucceededCount} passed, {result.TestErrors.Length} failed\n"
            printf $"Succeeeds: {result.TestSucceededValues}\n"

            result

//--------------------------------------------------------------------------------------------------//
//----------------------------------------Example Test Circuits using Gen<'a> samples---------------//
//--------------------------------------------------------------------------------------------------//

    open Builder


    /// returns a random position inside a rectangle, adding middle of sheet
    let getRandomPlacePosition (startPoint : XYPos) (endPoint : XYPos) (index : int) =
        //let xRange, yRange = (int range.X, int range.Y)
        //let xRangeList = randomInt (xRange / 2) 5 xRange |> map float
        //let yRangeList = randomInt (-yRange) 5 yRange |> map float
        //let data = product (fun x y -> middleOfSheet + {X=x; Y=y}) xRangeList yRangeList
        //data.Data index
        let x = randomInt (int startPoint.X) 5 (int endPoint.X)
        let y = randomInt (int startPoint.Y) 5 (int endPoint.Y)
        let randomPos = product (fun x y -> {X=float x; Y= float -y}) x y
        randomPos.Data index + middleOfSheet

    /// A circuit that creates two singly connected components, one of which is slightly offset from straight
    let makeSinglyConnectedCircuit sampleNum =

        let outPos = getRandomPlacePosition {X=100.0; Y= -50.0} {X=300.0; Y=50.0} sampleNum
        initSheetModel
        |> placeSymbol "Input 1" (Input1 (1, None)) middleOfSheet
        |> Result.bind (placeSymbol "Output 1" (Output 1) outPos)
        |> Result.bind (placeWire (portOf "Input 1" 0) (portOf "Output 1" 0))
        |> getOkOrFail


    let makeAndGateCircuit_1 sampleNum =

        initSheetModel
        |> placeSymbol "Input 1" (Input1 (1, None)) (getRandomPlacePosition {X = -300; Y = -100} {X = -50; Y = -50} sampleNum)
        |> Result.bind (placeSymbol "Input 2" (Input1 (1, None)) (getRandomPlacePosition {X = -300; Y = 50} {X = -50; Y = 100} sampleNum))
        |> Result.bind (placeSymbol "AND" (GateN (And, 2)) middleOfSheet)
        |> Result.bind (placeSymbol "Output 1" (Output 1) (getRandomPlacePosition {X = 50; Y = -50} {X = 300; Y = 50} sampleNum))
        |> Result.bind (placeWire (portOf "Input 1" 0) (portOf "AND" 0))
        |> Result.bind (placeWire (portOf "Input 2" 0) (portOf "AND" 1))
        |> Result.bind (placeWire (portOf "AND" 0) (portOf "Output 1" 0))
        |> getOkOrFail


    let makeCircuitA3 sampleNum =
        let InputAPos = getRandomPlacePosition {X = -150; Y = 0} {X = -100; Y = 50} sampleNum
        let InputBPos = getRandomPlacePosition {X = -150; Y = 50} {X = -100; Y = 100} sampleNum
        let Mux1Pos = getRandomPlacePosition {X = -50; Y = -50} {X = 50; Y = 50} sampleNum
        let Mux2Pos = getRandomPlacePosition {X = 100; Y = -80} {X = 150; Y = 120} sampleNum
        let InputS1Pos = getRandomPlacePosition {X = -50; Y = -200} {X = 50; Y = -150} sampleNum
        let InputS2Pos = getRandomPlacePosition {X = -100; Y = -100} {X = -50; Y = -50} sampleNum
        let OutputCPos = getRandomPlacePosition {X = 200; Y = 50} {X = 250; Y = 100} sampleNum

        initSheetModel
        |> placeSymbol "Input A" (Input1 (1, None)) InputAPos
        |> Result.bind (placeSymbol "Input B" (Input1 (1, None)) InputBPos)
        |> Result.bind (placeSymbol "Mux1" (Mux2) Mux1Pos)
        |> Result.bind (placeSymbol "Mux2" (Mux2) Mux2Pos)
        |> Result.bind (placeSymbol "Input S1" (Input1 (1, None)) InputS1Pos)
        |> Result.bind (placeSymbol "Input S2" (Input1 (1, None)) InputS2Pos)
        |> Result.bind (placeSymbol "Output C" (Output 1) OutputCPos)
        |> Result.bind (placeWire (portOf "Input A" 0) (portOf "Mux1" 0))
        |> Result.bind (placeWire (portOf "Input B" 0) (portOf "Mux1" 1))
        |> Result.bind (placeWire (portOf "Mux1" 0) (portOf "Mux2" 0))
        |> Result.bind (placeWire (portOf "Input S1" 0) (portOf "Mux1" 2))
        |> Result.bind (placeWire (portOf "Input S2" 0) (portOf "Mux2" 2))
        |> Result.bind (placeWire (portOf "Mux2" 0) (portOf "Output C" 0))
        |> getOkOrFail


    /// Figure A4: two custom components, one of which is slightly offset from straight, each with 4 ports
    let makeCircuitA4 (sampleNum : int)  =
        let cc1Pos = getRandomPlacePosition {X = -200; Y = 0} {X = -100; Y = 100} sampleNum
        let cc2Pos = getRandomPlacePosition {X = 0; Y = 0} {X = 200; Y = 100} sampleNum

        initSheetModel
        |> placeCustomSymbol "main1" "main" cc1Pos
        |> Result.bind (placeCustomSymbol "main2" "main" cc2Pos)
        |> Result.bind (placeWire (portOf "main1" 0) (portOf "main2" 0))
        |> Result.bind (placeWire (portOf "main1" 1) (portOf "main2" 1))
        |> Result.bind (placeWire (portOf "main1" 2) (portOf "main2" 2))
        |> getOkOrFail


    module Asserts =

        (* Each assertion function from this module has as inputs the sample number of the current test and the corresponding schematic sheet.
           It returns a boolean indicating (true) that the test passes or 9false) that the test fails. The sample numbr is included to make it
           easy to document tests and so that any specific sampel schematic can easily be displayed using failOnSampleNumber. *)


        /// Assertion internal function
        let getTestResults
            (sheetMaker : 'a -> SheetT.Model)
            (sheetOptimizer : SheetT.Model -> SheetT.Model)
            (testName: string)
            (testNum: int)
            (getInfofunc: SheetT.Model -> 'b)
            (compareFunc: 'b -> 'b -> 'b)
            (compareFuncBool: 'b -> bool)=

            let testParameters = [0..testNum]
            let testResults = testParameters |> List.map (fun n ->
                let originalSheet = sheetMaker n
                let newSheet = sheetOptimizer originalSheet
                let originalVisSegCount = getInfofunc originalSheet
                let newVisSegCount = getInfofunc newSheet
                let comparedResults = compareFunc originalVisSegCount newVisSegCount
                if compareFuncBool comparedResults then
                    Error (string comparedResults)
                else
                    Ok (comparedResults))

            let testSucceededCount = testResults |> List.filter (function | Ok _ -> true | _ -> false) |> List.length
            let testSucceededValues = testResults |> List.choose (function | Ok x -> Some x | _ -> None)
            let testErrors = testResults |> List.choose (function | Error x -> Some (x, TestStatus.Fail x) | _ -> None)
            {
                TestName = testName
                TestDataCount = testNum
                TestSucceededCount = testSucceededCount
                TestSucceededValues = testSucceededValues
                TestErrors = testErrors
            }


        /// Fail when the optimized sheet is same or even worse than before in terms of visible segments count
        let VisSegCountCheck (sheetMaker : int -> SheetT.Model) (sheetOptimizer : SheetT.Model -> SheetT.Model) : TestResult<int, string> =
            getTestResults
                sheetMaker
                sheetOptimizer
                "Visible Segment Count"
                10
                SheetBeautifyHelpers.getVisibleSegmentCount
                (fun original updated -> original - updated)
                (fun x -> x < 0)

            
        let SymbolOverlap (sheetMaker : int -> SheetT.Model) (sheetOptimizer : SheetT.Model -> SheetT.Model) : TestResult<int, string> =
            getTestResults
                sheetMaker
                sheetOptimizer
                "Symbol Overlap"
                20
                SheetBeautifyHelpers.numOfIntersectSegSym
                (fun original updated -> original - updated)
                (fun x -> x < 0)
    


//---------------------------------------------------------------------------------------//
//-----------------------------Demo tests on Draw Block code-----------------------------//
//---------------------------------------------------------------------------------------//

    module Tests =

        /// Allow test errors to be viewed in sequence by recording the current error
        /// in the Issie Model (field DrawblockTestState). This contains all Issie persistent state.
        let recordPositionInTest (testNumber: int) (dispatch: Dispatch<Msg>) (result: TestResult<'a, 'b>) =
            dispatch <| UpdateDrawBlockTestState(fun _ ->
                match result.TestErrors with
                | [] ->
                    printf "Test finished"
                    None
                | (numb, _) :: _ ->
                    printf $"Sample {numb}"
                    Some { LastTestNumber=testNumber; LastTestSampleIndex= 0})

        let showTestCircuit1 (testNum : int) (firstSample : int) (dispatch: Dispatch<Msg>) (sheet : SheetT.Model) =
            let testCircuit = makeSinglyConnectedCircuit firstSample
            showSheetInIssieSchematic testCircuit dispatch

        let showTestCircuit2 (testNum : int) (firstSample : int) (dispatch: Dispatch<Msg>) (sheet : SheetT.Model) =
            let testCircuit = makeAndGateCircuit_1 firstSample
            showSheetInIssieSchematic testCircuit dispatch

        let showTestCircuit3 (testNum : int) (firstSample : int) (dispatch: Dispatch<Msg>) (sheet : SheetT.Model) =
            let testCircuit = makeCircuitA3 firstSample
            showSheetInIssieSchematic testCircuit dispatch

        let showTestCircuit4 (testNum : int) (firstSample : int) (dispatch: Dispatch<Msg>) (sheet : SheetT.Model) =
            let testCircuit = makeCircuitA4 firstSample
            showSheetInIssieSchematic testCircuit dispatch

        let showOptimizedCircuit (testNum : int) (firstSample : int) (dispatch: Dispatch<Msg>) (sheet : SheetT.Model)=
            // get the current circuit on sheet
            let testCircuit = sheet
            let optimizedCircuit = applyBeautifyAlgorithm testCircuit
            showSheetInIssieSchematic optimizedCircuit dispatch


        /// check visible wires
        let test1 (testNum : int) (firstSample : int) (dispatch: Dispatch<Msg>) (sheet : SheetT.Model) =
            runTestOnSheets
                makeAndGateCircuit_1
                applyBeautifyAlgorithm
                Asserts.VisSegCountCheck
                dispatch
            |> recordPositionInTest testNum dispatch


        /// check for symbol overlap
        let test2 testNum firstSample dispatch sheet =
            runTestOnSheets
                makeAndGateCircuit_1
                applyBeautifyAlgorithm
                Asserts.SymbolOverlap
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Example test: Horizontally positioned AND + DFF: fail on sample 10
        //let test2 testNum firstSample dispatch =
        //    runTestOnSheets
        //        "Horizontally positioned AND + DFF: fail on sample 10"
        //        firstSample
        //        singlyConnectSamples
        //        makeSinglyConnectedCircuit
        //        (Asserts.failOnSampleNumber 10)
        //        dispatch
        //    |> recordPositionInTest testNum dispatch

        ///// Example test: Horizontally positioned AND + DFF: fail on symbols intersect
        //let test3 testNum firstSample dispatch =
        //    runTestOnSheets
        //        "Horizontally positioned AND + DFF: fail on symbols intersect"
        //        firstSample
        //        singlyConnectSamples
        //        makeSinglyConnectedCircuit
        //        Asserts.failOnSymbolIntersectsSymbol
        //        dispatch
        //    |> recordPositionInTest testNum dispatch

        ///// Example test: Horizontally positioned AND + DFF: fail all tests
        //let test4 testNum firstSample dispatch =
        //    runTestOnSheets
        //        "Horizontally positioned AND + DFF: fail all tests"
        //        firstSample
        //        singlyConnectSamples
        //        makeSinglyConnectedCircuit
        //        Asserts.failOnAllTests
        //        dispatch
        //    |> recordPositionInTest testNum dispatch


        //let test5 testNum firstSample dispatch =
        //    runTestOnSheets
        //        "Horizontally positioned AND + DFF: fail all tests"
        //        firstSample
        //        singlyConnectSamples
        //        makeSinglyConnectedCircuit
        //        Asserts.failOnWireIntersectsSymbol
        //        dispatch
        //    |> recordPositionInTest testNum dispatch

        /// List of tests available which can be run ftom Issie File Menu.
        /// The first 9 tests can also be run via Ctrl-n accelerator keys as shown on menu
        ///
        /// ------------------------------------------------------------------------------------
        /// Q8 explanation:
        /// The trace tree from a key being pressed to the test being run is:
        /// Program.mkProgram init update view' -> Program.withSubscription attachMenusAndKeyShortcuts -> attachMenusAndKeyShortcuts
        /// -> fileMenu -> makeMenu -> makeMenuGen -> testsToRunFromSheetMenu -> test1 - test8
        ///
        /// Considering the fact that cross-module forward references are not allowed, Issie navigates this by defining the function that
        /// holds all the references in here, which is complied at a later order. The makeMenuGen function also serves as a higher-order function
        /// that creates a generic, reusable component that elegently handles different use cases including testing like this one.
        /// ------------------------------------------------------------------------------------
        let testsToRunFromSheetMenu : (string * (int -> int -> Dispatch<Msg> -> SheetT.Model -> Unit)) list =
            // Change names and test functions as required
            // delete unused tests from list
            [
                "Optimize current circuit", showOptimizedCircuit
                "Test Circuit 1", showTestCircuit1
                "Test Circuit 2", showTestCircuit2
                "Test Circuit 3", showTestCircuit3
                "Test Circuit 4", showTestCircuit4
                "Test6", fun _ _ _ _-> printf "Test6"
                "Test7", fun _ _ _ _-> printf "Test7"
                "Test8", fun _ _ _ _-> printf "Test8"
                "Next Test Error", fun _ _ _ _-> printf "Next Error:" // Go to the nexterror in a test

            ]

        /// Display the next error in a previously started test
        let nextError (testName, testFunc) firstSampleToTest dispatch =
            let testNum =
                testsToRunFromSheetMenu
                |> List.tryFindIndex (fun (name,_) -> name = testName)
                |> Option.defaultValue 0
            testFunc testNum firstSampleToTest dispatch

        /// common function to execute any test.
        /// testIndex: index of test in testsToRunFromSheetMenu
        let testMenuFunc (testIndex: int) (dispatch: Dispatch<Msg>) (model: Model) =
            let name,func = testsToRunFromSheetMenu[testIndex] 
            printf "%s" name
            //match name, model.DrawBlockTestState with
            //| "Next Test Error", Some state ->
            //    nextError testsToRunFromSheetMenu[state.LastTestNumber] (state.LastTestSampleIndex+1) dispatch
            //| "Next Test Error", None ->
            //    printf "Test Finished"
            //    ()
            //| _ ->
            //    func testIndex (randomInt 0 1 100 |> toList).[0] dispatch
            func testIndex (randomInt 0 1 100 |> toList).[0] dispatch model.Sheet
        


    


