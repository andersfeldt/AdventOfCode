module Day20

open System

type Particle =
    {
        position:     int64 * int64 * int64;
        velocity:     int64 * int64 * int64;
        acceleration: int64 * int64 * int64;
    }

let parseParticles input =
    let parse (line:string) =
        let values =
            line.Split("pva, <=>".ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
            |> List.ofArray
            |> List.map int64
        {
            position = values.[0], values.[1], values.[2];
            velocity = values.[3], values.[4], values.[5];
            acceleration = values.[6], values.[7], values.[8];
        }
    List.map parse input

let update particles =
    let (++) (a1, a2, a3) (b1, b2, b3) =
        a1 + b1, a2 + b2, a3 + b3
    let updateSingle particle =
        { particle with
            velocity = particle.velocity ++ particle.acceleration;
            position = particle.position ++ particle.velocity ++ particle.acceleration;
        }
    List.map updateSingle particles

let getResultA (input:string list) =
    let initialParticles = parseParticles input

    let folder (particles, currentClosest, ticksSinceChangeOfClosest) _ =
        let getDistance particle =
            let x, y, z = particle.position
            List.sumBy abs [x; y; z]
        let updatedParticles = update particles
        let distances = List.map getDistance updatedParticles
        let minDistance = List.min distances
        let newClosest = List.findIndex (fun d -> d = minDistance) distances
        let newTicksSinceChangeOfClosest =
            if currentClosest = newClosest
                then ticksSinceChangeOfClosest + 1
                else 0
        updatedParticles, newClosest, newTicksSinceChangeOfClosest

    let ``arbitrarily chosen value for "long term"`` = 1000

    Seq.initInfinite id
    |> Seq.scan folder (initialParticles, 0, 0)
    |> Seq.find (fun (_, _, x) -> x > ``arbitrarily chosen value for "long term"``)
    |> (fun (_, x, _) -> x)

let getResultB (input:string list) =
    let initialParticles = parseParticles input

    let folder (particles, ticksSinceLastCollision) _ =
        let updatedParticles = update particles
        let collisionPositions =
            updatedParticles
            |> List.groupBy (fun p -> p.position)
            |> List.filter (fun (_, particles) -> Seq.length particles > 1)
            |> List.map fst

        if List.isEmpty collisionPositions
            then updatedParticles, ticksSinceLastCollision + 1
            else
                let particlesNotColliding =
                    updatedParticles
                    |> List.filter (fun p -> collisionPositions |> List.contains p.position |> not)
                particlesNotColliding, 0

    let ``arbitrarily chosen value for "long term"`` = 1000

    Seq.initInfinite id
    |> Seq.scan folder (initialParticles, 0)
    |> Seq.find (snd >> ((<) ``arbitrarily chosen value for "long term"``))
    |> fst
    |> List.length

let getResult part (input:string list) =
    match part with
    | A -> getResultA input
    | B -> getResultB input
    |> string
