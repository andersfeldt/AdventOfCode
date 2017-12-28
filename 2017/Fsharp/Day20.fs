module Day20

open System

type Particle =
    {
        px: int64;
        py: int64;
        pz: int64;
        vx: int64;
        vy: int64;
        vz: int64;
        ax: int64;
        ay: int64;
        az: int64;
    }

let getResultA (input:string list) =
    let parseParticles input =
        let parse (line:string) =
            let values =
                line.Split("pva, <=>".ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
                |> List.ofArray
                |> List.map int64
            {
                px = values.[0];
                py = values.[1];
                pz = values.[2];
                vx = values.[3];
                vy = values.[4];
                vz = values.[5];
                ax = values.[6];
                ay = values.[7];
                az = values.[8];
            }

        input
        |> List.map parse

    let initialParticles = parseParticles input

    let folder (particles, currentClosest, ticksSinceChangeOfClosest) _ =
        let update particles =
            let update' particle =
                { particle with
                    px = particle.px + particle.vx + particle.ax;
                    py = particle.py + particle.vy + particle.ay;
                    pz = particle.pz + particle.vz + particle.az;
                    vx = particle.vx + particle.ax;
                    vy = particle.vy + particle.ay;
                    vz = particle.vz + particle.az;
                }
            particles
            |> List.map update'

        let getDistance particle =
            [particle.px; particle.py; particle.pz]
            |> List.sumBy abs

        let updatedParticles = update particles
        let distances = updatedParticles |> List.map getDistance
        let minDistance = distances |> List.min
        let newClosest = distances |> List.findIndex (fun d -> d = minDistance)
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

let getResult part (input:string list) =
    match part with
    | A -> getResultA input
    | B -> failwith "Not implemented yet"
    |> string
