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

type ViewSelection = Point * Point

type Camera = {
    position: Vector2
    bounds: Rectangle
} with
    member self.Translate vec =
        { self with position = Vector2.Add(vec, self.position) }

    static member Default =
        { position = Vector2.Zero 
          bounds = Rectangle(0, 0, 100, 100) }

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

type MovingEntityData = Point
type EntityId = int
type EntitySelection = Map<MovingEntity, EntityId[]>

type World(worldWidth, worldHeight) =

    let movableEntityStore = System.Collections.Generic.Dictionary<MovingEntity, FastIdStore<MovingEntityData>>()

    do
        for kind in MovingEntity.Cases do
            movableEntityStore.[kind] <- FastIdStore()

    member __.AddMovingEntity(kind:MovingEntity, data:MovingEntityData) : EntityId =
        let id = movableEntityStore.[kind].Add data
        id

    member __.RemoveMovingEntity(kind:MovingEntity, id:EntityId) =
        movableEntityStore.[kind].Remove id
        
    member __.SetMovingEntity(kind:MovingEntity, id, data:MovingEntityData) =
        movableEntityStore.[kind].[id] <- data

    member __.MovingEntityCount(kind:MovingEntity) = movableEntityStore.[kind].Count

    member __.MovingEntityCompleteCount
        with get() =
            let mutable sum = 0
            for kind in MovingEntity.Cases do
                sum <- sum + movableEntityStore.[kind].Count
            sum

    member __.MovingEntitiesAsList(kind:MovingEntity) = movableEntityStore.[kind] :> System.Collections.Generic.IEnumerable<struct(EntityId * MovingEntityData)>

type ViewModel = {
    selection: ViewSelection option
}

type InGameModel = {
    world: World
    camera: Camera
    viewModel : ViewModel
    selection: EntitySelection option
} with
    static member Empty = {
        world = World(100, 100)
        camera = Camera.Default
        viewModel = {
            selection = None
        }
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
