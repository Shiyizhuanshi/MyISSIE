module SheetBeautifyD1

open CommonTypes
open DrawModelType
open BlockHelpers
open BusWire
open SymbolT
open SheetBeautifyHelpers
open DrawModelType
open Optics


// this is the module for team phase work D1 

let mapValuesToList map = 
    map
    |> Helpers.mapValues
    |> Array.toList


let getOrientation (pos: XYPos): BusWireT.Orientation = 
    if pos.X = 0.0 then BusWireT.Vertical else BusWireT.Horizontal

/// Given three visual (non-zero) segments, determine whether combine into a parallel wire
let isParallelWire (wireId: ConnectionId) (model: SheetT.Model): XYPos option= 
    let segsLst = SegmentHelpers.visibleSegments wireId model
    match segsLst.Length with
        | 3 ->  match getOrientation segsLst.[0] = getOrientation segsLst.[2] && getOrientation segsLst.[0] <> getOrientation segsLst.[1] &&
                segsLst.[0].X * segsLst.[2].X >= 0 && segsLst.[0].Y * segsLst.[2].Y >= 0  with //lengths of seg1 and seg3 need to have the same sign
                | true -> Some segsLst.[1]
                | false -> None
        | _ -> None

/// Given a symbol, turn how many wires are connected to it
let connectedWiresCount (sym: Symbol) (sheetModel: SheetT.Model): int = 
    let portsLst = 
        sym.PortMaps.Order 
        |> Map.toList 
        |> List.collect snd

    let wiresModel = 
        sheetModel.Wire.Wires 
        |> mapValuesToList

    let connectedWires = 
        portsLst
        |> List.collect (fun portId ->
            wiresModel
            |> List.filter (fun wire -> 
                wire.InputPort = InputPortId portId || wire.OutputPort = OutputPortId portId))

    connectedWires.Length

    

let hasOnlyOnePort (sym: Symbol) : Edge option= 
    let portMaps = sym.PortMaps.Order |> Map.toList
    let portEdge = 
        portMaps
        |> List.tryFind (fun tuple -> tuple |> snd |> List.length = 1)
        |> Option.map (fun tuple -> tuple |> fst)

    match sym.Component.InputPorts.Length + sym.Component.OutputPorts.Length with
    | 1 -> portEdge
    | _ -> None


let inputPortIdToString (inputPortId: InputPortId) =
        match inputPortId with
        | InputPortId str -> str

let outputPortIdToString (outputPortId: OutputPortId) =
        match outputPortId with
        | OutputPortId str -> str

let hasOnlyOneConnectedParallelWire (sheetModel: SheetT.Model) (sym: Symbol): XYPos option=
    let parallelWiresLst = 
        sheetModel.Wire.Wires 
        |> mapValuesToList 
        |> List.collect (fun wire -> 
            match isParallelWire wire.WId sheetModel with
                | Some seg -> [(wire, seg)]
                | None -> [])

    let wiresPortIds = 
        parallelWiresLst
        |> List.map fst
        |> List.collect (fun wire -> [inputPortIdToString wire.InputPort; outputPortIdToString wire.OutputPort])
    
    let symPortIds = 
        sym.PortMaps.Order
        |> Map.toList
        |> List.collect (fun tuple -> snd tuple)

    let connectedPortCount = 
        wiresPortIds
        |> List.filter (fun  wirePortId -> List.exists (fun  symPortId-> wirePortId = symPortId )  symPortIds) 
        |> List.length
        
    match connectedPortCount = 1 with
    | true -> 
        let connectedWire = 
            parallelWiresLst
            |> List.map fst
            |> List.find (fun wire -> 
                let wirePortIds = [inputPortIdToString wire.InputPort; outputPortIdToString wire.OutputPort]
                List.exists (fun wirePortId -> List.exists (fun symPortId -> wirePortId = symPortId ) symPortIds ) wirePortIds )
        Some (parallelWiresLst |> List.find (fun (wire, _) -> wire = connectedWire) |> snd)
    | false -> None
let negXYPos (pos: XYPos) : XYPos = {X = -pos.X; Y = -pos.Y}

let getFstOfSndTuple (tuple : ('a * ('b * 'c))) = 
    match tuple with
    | (_, snd) -> fst snd

let getSndOfSndTuple (tuple : ('a * ('b * 'c))) = 
    match tuple with
    | (_, snd') -> snd snd'

let updateOneSym (sym: Symbol) (sheetModel: SheetT.Model) ((offset, portType): (XYPos * PortType)) (intersectSymPairCount): Symbol option= 
    printfn "updating one sym with offset %s" (pXY offset)
    let newSym = 
        match portType with
        | PortType.Output -> moveSymbol  offset sym
        | PortType.Input -> moveSymbol  (negXYPos offset) sym
    let newSheet = 
        sheetModel
        |> Optic.set SheetT.symbol_ (SymbolUpdate.replaceSymbol sheetModel.Wire.Symbol newSym newSym.Id)
        |> SheetUpdateHelpers.updateBoundingBoxes
    // if the new symbol intersects with any other symbol, return None
    printfn "intersected symbols after beautify: %d" (numOfIntersectedSymPairs newSheet)
    printfn "intersected symbols before beautify: %d" intersectSymPairCount
    match (numOfIntersectedSymPairs newSheet) > intersectSymPairCount with
    | false -> Some newSym
    | _ -> None


let getParallelWiresLst (sheetModel: SheetT.Model) (wiresCannotStraighten: ConnectionId list): BusWireT.Wire list= 
    sheetModel.Wire.Wires 
    |> mapValuesToList 
    |> List.collect (fun wire -> 
        match isParallelWire wire.WId sheetModel with
            | Some seg -> [wire]
            | None -> [])
    |> List.filter (fun wire -> 
        not (List.exists (fun wireCannotStraighten -> wire.WId = wireCannotStraighten) wiresCannotStraighten))

let getPortType (portId: string) (sym: Symbol): PortType = 
    let port = 
        sym.Component.InputPorts @ sym.Component.OutputPorts
        |> List.find (fun port -> port.Id = portId)
    port.PortType

let isMovable (sym: Symbol) (portId: string) (parallelWirePortsLst : string list): bool= 
    printfn "checking if the symbol %s is movable" (pXY sym.Pos)
    let edge = sym.PortMaps.Orientation[portId]
    //ports on given edge
    sym.PortMaps.Order[edge]
    //if the port is connected to only one parallel wire in one edge, it is movable
    |> List.map (fun portId -> List.exists (fun parallelWirePortId -> parallelWirePortId = portId) parallelWirePortsLst)
    |> List.length = 1


let rec sheetAlignScaleOnce (sheetModel: SheetT.Model) (stubbornWiresLst : ConnectionId list) (intersectSymPairCount : int)=
    printfn "%s" "aligning and scaling start!"
    printfn "stubborn wires: %d" stubbornWiresLst.Length
    let parallelWiresLst = getParallelWiresLst sheetModel stubbornWiresLst
    printfn "parallel wires: %d" parallelWiresLst.Length
    // if there is no parallel wire, return the original sheetModel
    match parallelWiresLst.Length with
    | 0 -> 
        printfn "no parallel wire found, aligning and scaling finished"
        printfn "intersected symbols found: %d" (numOfIntersectedSymPairs sheetModel)
        sheetModel
    | _ ->

        let parallelWirePortsLst=
            parallelWiresLst
            |> List.collect (fun (wire: BusWireT.Wire) -> [inputPortIdToString wire.InputPort; outputPortIdToString wire.OutputPort])
        
        let newNotMovableWires = 
            parallelWiresLst
            |> List.filter (fun wire -> 
                let sourceSym = getSourceSymbol sheetModel.Wire wire
                let targetSym = getTargetSymbol sheetModel.Wire wire
                let inputPortId = inputPortIdToString wire.InputPort
                let outputPortId = outputPortIdToString wire.OutputPort
                not (isMovable sourceSym outputPortId parallelWirePortsLst || isMovable targetSym inputPortId parallelWirePortsLst))
            |> List.map (fun wire -> wire.WId)
        let newStubbonWiresLst = 
            stubbornWiresLst @ newNotMovableWires
            |> List.distinct

        let getRelateWiresCount (wire : BusWireT.Wire) : int =
            connectedWiresCount (getSourceSymbol sheetModel.Wire wire) sheetModel
            + connectedWiresCount (getTargetSymbol sheetModel.Wire wire) sheetModel

        /// Given a wire and a sheetModel, return the wire to move
        let wireToStraighten : BusWireT.Wire option = 
            parallelWiresLst
            |> List.filter (fun wire -> not (List.contains wire.WId newNotMovableWires))
            |> List.map (fun wire -> (wire, getRelateWiresCount wire))
            |> List.sortByDescending snd
            |> List.tryHead
            |> Option.map fst

        match wireToStraighten with
        //none happens when all the parallel wires are not movable, thus return the original sheetModel
        | None -> 
            printfn "%s" "no wire to move, continue to align and scale"
            sheetModel
        // if there is a wire to move, move the symbol and update the sheetModel and call the function recursively
        | Some wireToMove ->
            let offset = isParallelWire wireToMove.WId sheetModel |> Option.defaultValue XYPos.zero
            let sourceSym = getSourceSymbol sheetModel.Wire wireToMove
            let targetSym = getTargetSymbol sheetModel.Wire wireToMove
            let moveInfo = 
                match isMovable sourceSym (outputPortIdToString wireToMove.OutputPort) parallelWirePortsLst, isMovable targetSym (inputPortIdToString wireToMove.InputPort) parallelWirePortsLst with
                | true, false -> [sourceSym, (PortType.Output, offset)]
                | false, true -> [targetSym, (PortType.Input, offset)]
                | true, true -> 
                    if connectedWiresCount sourceSym sheetModel < connectedWiresCount targetSym sheetModel 
                    then [targetSym, (PortType.Input, offset); sourceSym, (PortType.Output, offset)]
                    else [sourceSym, (PortType.Output, offset); targetSym, (PortType.Input, offset)]
                | false, false -> failwith "impossible" // this should not happen

            let newSym =
                match moveInfo.Length with
                | 1 -> 
                    updateOneSym (fst moveInfo.[0]) sheetModel (getSndOfSndTuple moveInfo.[0], getFstOfSndTuple moveInfo.[0]) intersectSymPairCount
                | 2 ->
                    match updateOneSym (fst moveInfo.[0]) sheetModel (getSndOfSndTuple moveInfo.[0], getFstOfSndTuple moveInfo.[0]) intersectSymPairCount with
                    | Some sym -> Some sym
                    | None -> 
                        printfn "%s" "one end sym failed to move, move the other end sym instead"
                        updateOneSym (fst moveInfo.[1]) sheetModel (getSndOfSndTuple moveInfo.[1], getFstOfSndTuple moveInfo.[1]) intersectSymPairCount
                | _ -> failwith "impossible" // this should not happen
            match newSym with
            | None -> 
                printfn "%s" "intersected symbols found, add new wire to stubbornWiresLst" 
                sheetAlignScaleOnce sheetModel (wireToMove.WId::newStubbonWiresLst) (numOfIntersectedSymPairs sheetModel) 
            | Some newSym ->
                let newWireModel = 
                    updateModelWires (updateModelSymbols sheetModel.Wire [newSym] ) [parallelWiresLst |> List.head]
                    |> BusWireSeparate.updateWireSegmentJumpsAndSeparations [(parallelWiresLst |> List.head).WId] 

                let newWireModel' = 
                    BusWireRoute.updateWires newWireModel [newSym.Id] { X = 0.0; Y = 0.0 } 

                let newSheetModel = 
                    sheetModel
                    |> Optic.set SheetT.wire_ (newWireModel')
                    |> SheetUpdateHelpers.updateBoundingBoxes // could optimise this by only updating symId bounding boxes
                
                printfn "%s" "straighten one wire, continue to align and scale"
                sheetAlignScaleOnce newSheetModel newStubbonWiresLst (numOfIntersectedSymPairs newSheetModel)
                // newSheetModel

let rec sheetAlignScale (sheetModel: SheetT.Model) (runTimes : int)=
    if runTimes = 0 then sheetModel
    else
        printfn "runTimes: %d" runTimes
        let newSheetModel = sheetAlignScaleOnce sheetModel [] (numOfIntersectedSymPairs sheetModel)
        sheetAlignScale newSheetModel (runTimes - 1)


let getSymPorts (sym: Symbol) =
    sym.Component.InputPorts @ sym.Component.OutputPorts

let getWiresBetweenTwoSymbol (sym1 : Symbol) (sym2 : Symbol) (sheetModel : SheetT.Model): BusWireT.Wire list = 
    let sym1Ports = getSymPorts sym1
    let sym2Ports = getSymPorts sym2
    let wires = sheetModel.Wire.Wires |> mapValuesToList
    wires
    |> List.filter (fun wire -> 
        let inputPortId = inputPortIdToString wire.InputPort
        let outputPortId = outputPortIdToString wire.OutputPort
        ((List.exists (fun (port: Port) -> port.Id.ToString() = inputPortId) sym1Ports) && (List.exists (fun (port: Port) -> port.Id.ToString() = outputPortId) sym2Ports)) ||
        ((List.exists (fun (port: Port) -> port.Id.ToString() = outputPortId) sym1Ports) && (List.exists (fun (port: Port) -> port.Id.ToString() = inputPortId) sym2Ports)))

let isCustomWithType (componentType: ComponentType) =
    match componentType with
    | Custom customType -> true
    | _ -> false

let getCustomSyms (sheetModel : SheetT.Model) : Symbol list = 
    sheetModel.Wire.Symbol.Symbols
    |> mapValuesToList
    |> List.filter (fun sym -> isCustomWithType sym.Component.Type)

let findPortSymLabel (port : Port) (sheetModel : SheetT.Model) : string = 
    let syms = sheetModel.Wire.Symbol.Symbols |> mapValuesToList
    let sym = 
        syms
        |> List.find (fun sym -> sym.Id.ToString() = port.HostId)
    sym.Component.Label


let calculateDimension (portGap : float) (portDimension : float) (gap : float) (sym : Symbol) : XYPos = 
    let h = portGap * (portDimension + 2.0 * gap)
    {X = sym.Component.W; Y = h}



let printOutPortPositions (sheetModel : SheetT.Model) : SheetT.Model = 
    let wireLst = sheetModel.Wire.Wires |> mapValuesToList
    let wirePortsLst = wireLst |> List.collect (fun wire -> [inputPortIdToString wire.InputPort; outputPortIdToString wire.OutputPort])
    let syms = getCustomSyms sheetModel
    let sym1 = syms |> List.head
    let sym2 = syms |> List.tail |> List.head
    syms
    |> List.iter (fun sym -> printfn "symbol dim: %s pos: %s"  (pXY (getCustomCompDims sym)) (pXY sym.Pos))
    syms
    |> List.collect (fun sym ->  getSymPorts sym)
    |> List.filter (fun port -> List.exists (fun wirePort -> wirePort = port.Id) wirePortsLst)
    |> List.map (fun port -> port, findPortSymLabel port sheetModel)
    |> List.iter (fun (port, label) -> printfn "label %s position: %s" label (pXY (getPortPos port.Id sheetModel.Wire.Symbol)))
    syms
    |> List.pairwise
    |> List.iter (fun (sym1, sym2) -> 
        let wires = getWiresBetweenTwoSymbol sym1 sym2 sheetModel
        printfn "wires between %s and %s: %d" sym1.Component.Label sym2.Component.Label wires.Length)

    let leftPortDimension = float sym1.PortMaps.Order[Right].Length - 1.0
    let rightPortDimension = float sym2.PortMaps.Order[Left].Length - 1.0
    let lefth, leftw = Symbol.getRotatedHAndW sym1
    let righth, rightw = Symbol.getRotatedHAndW sym2
    let gap = Symbol.getPortPosEdgeGap sym1.Component.Type

    printfn "left port gap: %f" (lefth / (leftPortDimension + 2.0 * gap))
    printfn "right port gap: %f" (righth / (rightPortDimension + 2.0 * gap))
    sheetModel

