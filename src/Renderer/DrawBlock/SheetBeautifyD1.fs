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

    /// Return the list of segment vectors with 3 vectors coalesced into one visible equivalent
    /// wherever this is possible
    let rec coalesce (segVecs: XYPos list)  =
        match List.tryFindIndex (fun segVec -> segVec =~ XYPos.zero) segVecs[1..segVecs.Length-2] with          
        | Some zeroVecIndex ->
            let index = zeroVecIndex + 1 // base index as it should be on full segVecs
            segVecs[0..index-2] @
            [segVecs[index-1] + segVecs[index+1]] @
            segVecs[index+2..segVecs.Length - 1]
            |> coalesce
        | None -> segVecs
    
    wire.Segments
    |> List.mapi getSegmentVector
    |> coalesce

    //-----------------------------------------------------------------------------------------------
let mapValuesToList map = 
    map
    |> Helpers.mapValues
    |> Array.toList


let getOrientation (pos: XYPos): BusWireT.Orientation = 
    if pos.X = 0.0 then BusWireT.Vertical else BusWireT.Horizontal

/// Given three visual (non-zero) segments, determine whether combine into a parallel wire
let isParallelWire (wire: BusWireT.Wire) (model: SheetT.Model): XYPos option= 
    let segsLst = visibleSegments wire.WId model
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
            match isParallelWire wire sheetModel with
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

let chooseOffset (offset: XYPos) (edge: Edge): XYPos = 
    match edge with
    | Left-> negXYPos offset
    | Bottom | Top | Right -> offset

let updateOneSym (sym: Symbol) (sheetModel: SheetT.Model) (offset: XYPos): Symbol option= 
    let newSym = moveSymbol offset sym
    let newSheet = 
        sheetModel
        |> Optic.set SheetT.symbol_ (SymbolUpdate.replaceSymbol sheetModel.Wire.Symbol newSym sym.Id)
    // if the new symbol intersects with any other symbol, return None
    match numOfIntersectedSymPairs newSheet with
    | 0 -> Some newSym
    | _ -> None
    

    
let sheetAlignScale (sheetModel: SheetT.Model)=
    let parallelWiresLst = 
        sheetModel.Wire.Wires 
        |> mapValuesToList 
        |> List.collect (fun wire -> 
            match isParallelWire wire sheetModel with
                | Some seg -> 
                    printfn "offset %s" (pXY seg)
                    [wire]
                | None -> [])
    printfn "%d parallel wires found" parallelWiresLst.Length

    let parallelWirePorsLst=
        parallelWiresLst
        |> List.collect (fun wire -> [wire.InputPort, wire.OutputPort])
        |> List.collect (fun ports -> [inputPortIdToString (fst ports); outputPortIdToString (snd ports)])
        |> List.distinct
    
    printfn "%d parallel wire ports found" parallelWirePorsLst.Length

    let isMovable (sym: Symbol) (portId: string): bool= 
        let edge = sym.PortMaps.Orientation[portId]
        //ports on given edge
        sym.PortMaps.Order[edge]
        //if the port is connected to only one parallel wire in one edge, it is movable
        |> List.map (fun portId -> List.exists (fun wirePortId -> wirePortId = portId) parallelWirePorsLst)
        |> List.length = 1

    
    /// Given a wire and a sheetModel, return the symol with more connected wires
    let chooseSymToMove (sheetModel: SheetT.Model) (wire: BusWireT.Wire) : Symbol option= 
        let inputPortId = inputPortIdToString wire.InputPort
        let outputPortId = outputPortIdToString wire.OutputPort
        let sourceSym = getSourceSymbol sheetModel.Wire wire 
        let targetSym = getTargetSymbol sheetModel.Wire wire
        match isMovable sourceSym outputPortId, isMovable targetSym inputPortId with
        | true, false -> Some sourceSym
        | false, true -> Some targetSym
        | true, true -> 
            if connectedWiresCount sourceSym sheetModel > connectedWiresCount targetSym sheetModel then Some sourceSym
            else Some targetSym
        | false, false -> None

    let symsToMove = 
        parallelWiresLst
        |> List.map (fun wire -> wire, (chooseSymToMove sheetModel wire).Value)
        |> List.distinct
        // sort the list by the nnumber of connected wires, so that the symbol with more connected wires will be moved first
        |> List.sortByDescending (fun (wire, sym) -> connectedWiresCount sym sheetModel)

    printfn "%d symbols to move" symsToMove.Length

    symsToMove
    |> List.fold (fun acc (wire, symToMove) -> 
        let offset = 
            match isParallelWire wire sheetModel with
            | Some seg -> seg
            | None -> {X = 0.0; Y = 0.0}
        printfn "%s" "offset:"
        printfn "%s" (pXY offset)
        let newSym = updateOneSym symToMove sheetModel offset
        match newSym with
        | Some sym -> 
            let newSheet = 
                sheetModel
                |> Optic.set SheetT.symbol_ (SymbolUpdate.replaceSymbol sheetModel.Wire.Symbol sym symToMove.Id)
                |> SheetUpdate.updateBoundingBoxes
            newSheet
        | None -> 
            printfn "%s" "symbol cannot be moved"
            acc) sheetModel

    // let symsLst = sheetModel.Wire.Symbol.Symbols |> mapValuesToList

    // let symModel = sheetModel.Wire.Symbol
    // let wireModel = sheetModel.Wire
    // let updateSymsLst=
    //     symsLst
    //     |> List.collect (fun sym -> 
    //         match hasOnlyOnePort sym with
    //             | Some edge -> 
    //                 match hasOnlyOneConnectedParallelWire sheetModel sym with
    //                 | Some seg -> 
    //                     printfn "%s" "found a sym with a parallel wire:"
    //                     printfn "%s" (pXY sym.Pos)
    //                     [(sym, seg, edge)]
    //                 | None -> []
    //             | None -> [])
    //     |> List.map (fun (sym: Symbol, offset, edge) -> 
    //         printf "%s" "old position: "
    //         printfn "%s" (pXY sym.Pos)
    //         moveSymbol (chooseOffset offset edge) sym)

    // updateSymsLst
    // |> List.iter (fun sym -> 
    //     printf "%s" "new position: "
    //     printfn "%s" (pXY sym.Pos))
            
    // let updateSymIdLst = 
    //     updateSymsLst
    //     |> List.map (fun sym -> sym.Id)

    // let newSymModel =
    //     updateSymsLst
    //     |> List.fold (fun acc sym -> 
    //         let symId = sym.Id
    //         let symModel = acc
    //         let symModel = SymbolUpdate.replaceSymbol symModel sym symId 
    //         symModel) symModel

    // newSymModel.Symbols
    // |> mapValuesToList
    // |> List.iter (fun sym ->
    //         printf "%s" "new symModel positions: " 
    //         printfn "%s" (pXY sym.Pos))

    // let newParallelWiresLst = 
    //     parallelWiresLst
    //     // |> List.map (fun wire -> BusWireRoute.updateWire newWireModel wire false) 
    //     |> List.map (fun wire -> BusWireRoute.updateWire sheetModel.Wire wire false )
    //     // |> List.map (fun wire -> wire.WId)

    // let newWireModel = 
        // updateModelWires (updateModelSymbols wireModel (mapValuesToList newSymModel.Symbols) ) newParallelWiresLst 
        // |> BusWireSeparate.updateWireSegmentJumpsAndSeparations newParallelWiresLst 

    // let newWireModel' = 
    //     BusWireRoute.updateWires newWireModel updateSymIdLst { X = 0.0; Y = 0.0 } 

    // let newSheetModel = 
    //     sheetModel
    //     |> Optic.set SheetT.wire_ (newWireModel')
    //     // |> SheetUpdate.updateBoundingBoxes // could optimise this by only updating symId bounding boxes
        
    
    // let newSymsLst = newSheetModel.Wire.Symbol.Symbols |> mapValuesToList
    // printf "%s" "updated positions: "
    // newSymsLst
    // |> List.iter (fun sym -> printfn "%s" (pXY sym.Pos))

    // newSheetModel
    // sheetModel
