module Model
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Audio
open System
open Microsoft.Xna.Framework.Content

[<Struct>]
type Position = {
    X:int
    Y:int
}

[<Struct>]
type Direction =
    | Up
    | Down
    | Left
    | Right

type StaticEntity =
| Stone of stonePos:Point
| RiverPart of riverPos:Point
| Bridge of bridgePos:Point
| Food of foodPos:Point
| AntHill of antHilPos:Point

type MovingEntity =
| WorkerAnt
| FightingAnt
| ResearchAnt
with
    static member Cases = [
        WorkerAnt
        FightingAnt
        ResearchAnt
    ]

type EntityKind =
| StaticEntity of StaticEntity
| MovingEntity of MovingEntity

type ViewSelection = Point * Point

type Camera = {
    position: Vector2
    bounds: Rectangle
    zoom: float32
} with
    static member Default =
        {
            position = Vector2.Zero 
            bounds = Rectangle(0, 0, 100, 100)
            zoom = 1.f
        }

// This class really needs tests to ensure the internal datastructure behaves correctly!
type FastIdStore<'T>() =
    let entityIdToInnerIdDict = System.Collections.Generic.Dictionary<_,_>()
    let innerIdToEntityIdDict = System.Collections.Generic.Dictionary<_,_>()
    let idStore = ResizeArray()

    member __.Add(data:'T) =
        let id = idStore.Count
        idStore.Add(data)
        entityIdToInnerIdDict.Add(id, id)
        innerIdToEntityIdDict.Add(id, id)
        id

    // This remove is in O(1) thanks to the heavy caching system.
    member __.Remove(publicId) =
        if entityIdToInnerIdDict.ContainsKey publicId then
            let innerId = entityIdToInnerIdDict.[publicId]
            let lastIndex = idStore.Count - 1
            
            // Swap the innerId with the lastIndex to achieve
            // a O(1) remove
            let temp = idStore.[lastIndex]
            idStore.[lastIndex] <- idStore.[innerId]
            idStore.[innerId] <- temp
            idStore.RemoveAt(lastIndex)

            // Remove the public id from the entityId dictionary
            entityIdToInnerIdDict.Remove(publicId) |> ignore

            // Correct the id mapping of the public id of lastIndex
            // and remove the entry for the innerId in the innerIdToEntityIdDict
            let lastIndexPublicId = innerIdToEntityIdDict.[lastIndex]
            entityIdToInnerIdDict.[lastIndexPublicId] <- innerId
            innerIdToEntityIdDict.Remove(innerId) |> ignore
            true
        else
            false

    member self.Item
        with get(id) =
            match entityIdToInnerIdDict.TryGetValue id with
            | true, innerId -> idStore.[innerId] |> Some
            | false, _ -> None
        and set id (data:'T) =
            match entityIdToInnerIdDict.TryGetValue id with
            | true, innerId -> idStore.[innerId] <- data
            | false, _ ->
                sprintf "id %d was not found and can not be set." id
                |> System.Collections.Generic.KeyNotFoundException
                |> raise

    member self.Count = idStore.Count

    member __.Contains id = entityIdToInnerIdDict.ContainsKey id

    interface System.Collections.Generic.IEnumerable<struct(int * 'T)> with
        member this.GetEnumerator() :  System.Collections.IEnumerator =
            seq {
                for i=0 to idStore.Count - 1 do
                    yield struct(entityIdToInnerIdDict.[i], idStore.[i])
            } :?> _

        member __.GetEnumerator() : System.Collections.Generic.IEnumerator<struct(int * 'T)> =
            seq {
                for publicId=0 to idStore.Count - 1 do
                    let innerId = entityIdToInnerIdDict.[publicId]
                    yield struct(publicId, idStore.[innerId])
            } :?> _

type MovingEntityData = unit
type StaticEntityData = unit
type EntityData =
    | StaticEntityData of StaticEntityData
    | MovingEntityData of MovingEntityData

type EntityId = int
type EntitySelection = Map<MovingEntity, EntityId[]>
type EntityKindWithId = EntityKind * EntityId

type Grid(width, height) =
    let grid : Option<EntityKindWithId>[,] = Array2D.init width height (fun x y -> None)
    let idToPosDict = System.Collections.Generic.Dictionary<_,_>()
    
    member __.Item
        with get(x, y) =
            grid.[x, y]
        and set (x, y) (pair:EntityKindWithId) =
            idToPosDict.[pair] <- struct(x, y)
            grid.[x, y] <- pair |> Some

    member __.RemoveAt(x, y) =
        match grid.[x, y] with
        | Some pair ->
            idToPosDict.Remove(pair) |> ignore
            grid.[x, y] <- None
        | None -> ()

    member __.Occupied(x, y) = grid.[x, y] |> Option.isSome

    member __.TryGetPositionFromEntityKindWithId(pair:EntityKindWithId) =
        match idToPosDict.TryGetValue pair with
        | true, (x, y) ->
            (x, y) |> Some
        | false, _ ->
            None

    member self.RemoveGivenEntityKindWithId(pair:EntityKindWithId) =
        match self.TryGetPositionFromEntityKindWithId pair with
        | Some(x, y) ->
            self.RemoveAt(x, y)
            true
        | None ->
            false


type World(worldWidth, worldHeight) =
    let staticEntityStore = System.Collections.Generic.Dictionary<StaticEntity, FastIdStore<StaticEntityData>>()
    let movableEntityStore = System.Collections.Generic.Dictionary<MovingEntity, FastIdStore<MovingEntityData>>()
    let grid = Grid(worldWidth, worldHeight)

    do
        for kind in MovingEntity.Cases do
            movableEntityStore.[kind] <- FastIdStore()

    member __.Width = worldWidth
    
    member __.Height = worldHeight

    member __.AddEntity(kind:EntityKind, x, y, data:EntityData) : EntityId option =
        if grid.Occupied(x, y) |> not then
            match kind, data with
            | StaticEntity innerKind, StaticEntityData data ->
                let id = staticEntityStore.[innerKind].Add data
                grid.[x, y] <- (kind, id)
                Some id
            | MovingEntity innerKind, MovingEntityData data ->
                let id = movableEntityStore.[innerKind].Add data
                grid.[x, y] <- (kind, id)
                Some id
            | _ ->
                System.InvalidOperationException("The entity kind and entity data does not match")
                |> raise
        else
            None

    member __.Remove(kind:EntityKind, id:EntityId) =
        if grid.RemoveGivenEntityKindWithId(kind, id) then
            match kind with
            | StaticEntity innerKind ->
                staticEntityStore.[innerKind].Remove id
            | MovingEntity innerKind ->
                movableEntityStore.[innerKind].Remove id
        else
            false
        
    member __.Set(kind:EntityKind, id, data:EntityData) =
        match kind, data with
        | StaticEntity innerKind, StaticEntityData data ->
            if staticEntityStore.[innerKind].Contains id then
                staticEntityStore.[innerKind].[id] <- data
                true
            else
                false
        | MovingEntity innerKind, MovingEntityData data ->
            if movableEntityStore.[innerKind].Contains id then
                movableEntityStore.[innerKind].[id] <- data
                true
            else
                false
        | _ ->
            System.InvalidOperationException("The entity kind and entity data does not match")
            |> raise
    
    member __.MoveEntity(kind:EntityKind, id:EntityId, x, y) =
        match grid.TryGetPositionFromEntityKindWithId(kind, id) with
        | Some(oldX, oldY) ->
            if(x >= worldWidth || x < 0 || y >= worldHeight || y < 0) ||
              grid.Occupied(x, y) then
                false
            else
                grid.RemoveAt(oldX, oldY)
                grid.[x, y] <- (kind, id)
                true
        | None -> false



    member __.EntityCount(kind:EntityKind) =
        match kind with
        | StaticEntity innerKind -> staticEntityStore.[innerKind].Count
        | MovingEntity innerKind -> movableEntityStore.[innerKind].Count

    member __.MovingEntityCompleteCount
        with get() =
            let mutable sum = 0
            for kind in MovingEntity.Cases do
                sum <- sum + movableEntityStore.[kind].Count
            sum

    member __.EntityAt
        with get(x, y) =
            grid.[x, y]

    member __.TryGetPosOf(kind, id) =
        grid.TryGetPositionFromEntityKindWithId(kind, id)

    member __.MovingEntitiesAsList(kind:MovingEntity) = movableEntityStore.[kind] :> System.Collections.Generic.IEnumerable<struct(EntityId * MovingEntityData)>
    
    member self.MovingEntitiesAsListWithPos(kind:MovingEntity) =
        seq {
            let entities = movableEntityStore.[kind] :> System.Collections.Generic.IEnumerable<struct(EntityId * MovingEntityData)>
            for (id, data) in entities do
                match self.TryGetPosOf(kind |> MovingEntity, id) with
                | Some (x, y) ->
                    yield (id, data, x, y)
                | None ->
                    failwith "An object has no position."
        }

type Zoom =
    | Near
    | Middle
    | Far

type ViewModel = {
    selection: ViewSelection option
    zoomState : Zoom
}

type InGameModel = {
    world: World
    camera: Camera
    viewModel : ViewModel
    selection: EntitySelection option
    counter: int
    movingEntities: (MovingEntity * EntityId * Vector2)[]
} with
    static member Empty = {
        world = World(1000, 1000)
        camera = Camera.Default
        viewModel = {
            selection = None
            zoomState = Near
        }
        counter = 0
        movingEntities = [||]
        selection = None
    }

type GameState =
    | GameOver
    | GameIsInMainScreen
    | GameIsRunning of InGameModel

type UserInteraction =
    | CameraUp
    | CameraDown
    | CameraLeft
    | CameraRight
    | CameraZoomIn
    | CameraZoomOut
    | DoExitGame
    
type Collision =
    | Wall
    | Food of newFoodPosition:Position
    | Tail of remainingSnakeParts:Position list

[<Struct>]
type MainMenuMsg =
    | StartGame
    | ExitGame

[<Struct>]
type UpdateTickMsg = {
    gameTime:GameTime
    keyboardState:Input.KeyboardState
    mouseState:MouseState 
}

[<Struct>]
type InGameMessage =
| AntsSelected
| AntsCreated
| AntTypeSelected
| AntTargetChosen
| AutomaticBehaviourSelected
| AntSelectFood
| AntGoResearch
| HillUpgradeSelected
| UpdateTick of updateTick:UpdateTickMsg
| ExitPressed
| RedrawRequested
| ColorChanged of colorChanged:struct {| colorChangedColor:Color |}

type Actions =
// Collision detection / In range detection
| AntInFight
// Could be a performance problem
| AntDying

[<Struct>]
type InGameMsg =
    | UserInteraction of UserInteraction
    | SelectionStarted of start:Point
    | SelectionOngoing of corner:Point
    | SelectionEnded of pEnd:Point
    | UpdateTick of gameTime:GameTime

[<Struct>]
type Msg = 
    | MainMenuMsg of mainMenu:MainMenuMsg
    | InGameMsg of inGame:InGameMsg

[<Struct>]
type Model = {
    mutable gameState: GameState
    assets: Xelmish.Model.LoadedAssets
    screenSize: Point
}
